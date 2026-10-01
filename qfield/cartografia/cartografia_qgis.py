"""Renderizador QGIS incorporado ao R. Sem dependência de rede na cartografia."""
import os,sys,json,math,shutil,hashlib,datetime,re,unicodedata,textwrap
from xml.sax.saxutils import escape
from pathlib import Path
os.environ['QT_QPA_PLATFORM']='windows' if os.name=='nt' else 'offscreen'
from qgis.core import *
from qgis.PyQt.QtCore import QSize,Qt
from qgis.PyQt.QtGui import QColor,QFont,QImage
from osgeo import gdal,osr
app=QgsApplication([],False);app.initQgis();gdal.UseExceptions()

def dump(data,path): Path(path).write_text(json.dumps(data,ensure_ascii=False,indent=2),encoding='utf-8')
def sha(path):
 h=hashlib.sha256()
 with open(path,'rb') as f:
  for b in iter(lambda:f.read(1024*1024),b''):h.update(b)
 return h.hexdigest()
def check_pdf(path,crs):
 ds=gdal.OpenEx(str(path),gdal.OF_RASTER,open_options=['DPI=72']);assert ds is not None,'PDF sem driver raster'
 wkt=ds.GetProjection();assert wkt,'PDF sem CRS';sr=osr.SpatialReference();sr.ImportFromWkt(wkt);expected=osr.SpatialReference();expected.ImportFromWkt(crs.toWkt());assert sr.IsSame(expected),'CRS divergente no PDF'
 gt=ds.GetGeoTransform(can_return_null=True);assert gt and gt!=(0,1,0,0,0,1),'PDF sem transformação'
 result={'crs':sr.GetAuthorityCode(None),'geotransform':gt,'largura_px_72dpi':ds.RasterXSize,'altura_px_72dpi':ds.RasterYSize};ds=None;return result

def probe(output):
 assert gdal.GetDriverByName('PDF'), 'GDAL sem PDF'
 project=QgsProject();project.setCrs(QgsCoordinateReferenceSystem(31983));layout=QgsPrintLayout(project);layout.initializeDefaults()
 m=QgsLayoutItemMap(layout);layout.addLayoutItem(m);m.attemptMove(QgsLayoutPoint(20,20));m.attemptResize(QgsLayoutSize(100,100));m.setCrs(project.crs());m.zoomToExtent(QgsRectangle(600000,8000000,601000,8001000));layout.setReferenceMap(m)
 f=Path(output).with_suffix('.pdf');settings=QgsLayoutExporter.PdfExportSettings();settings.appendGeoreference=True;settings.dpi=96
 assert QgsLayoutExporter(layout).exportToPdf(str(f),settings)==QgsLayoutExporter.Success,'Exportação PDF indisponível'
 evidence=check_pdf(f,project.crs());f.unlink();dump({'status':'PASS','QGIS':Qgis.QGIS_VERSION,'GDAL':gdal.VersionInfo(),'PDF':evidence},output)

def label(layout,text,x,y,w,h,size=9,bold=False,color='#174b3b'):
 item=QgsLayoutItemLabel(layout);item.setText(text);font=QFont('Arial');font.setPointSizeF(size);font.setBold(bold);item.setFont(font);item.setFontColor(QColor(color));layout.addLayoutItem(item);item.attemptMove(QgsLayoutPoint(x,y));item.attemptResize(QgsLayoutSize(w,h));return item

def box(layout,x,y,w,h):
 item=QgsLayoutItemShape(layout);item.setShapeType(QgsLayoutItemShape.Rectangle);item.setSymbol(QgsFillSymbol.createSimple({'color':'248,250,248,255','outline_color':'120,140,130,255','outline_width':'0.15'}));layout.addLayoutItem(item);item.attemptMove(QgsLayoutPoint(x,y));item.attemptResize(QgsLayoutSize(w,h));return item

def extent_of(layer, fallback):
 if layer and layer.featureCount()>0:
  e=QgsRectangle(layer.extent())
 else:e=QgsRectangle(fallback)
 if e.width()<100:e.setXMinimum(e.center().x()-50);e.setXMaximum(e.xMinimum()+100)
 if e.height()<100:e.setYMinimum(e.center().y()-50);e.setYMaximum(e.yMinimum()+100)
 e.scale(1.06);return e

def style_labels(layer,size):
 p=layer.labeling().settings();fmt=p.format();fmt.setSize(size);fmt.setFont(QFont('Arial',size));fmt.setColor(QColor('white'));buf=QgsTextBufferSettings();buf.setEnabled(True);buf.setSize(.45);buf.setColor(QColor('#202820'));fmt.setBuffer(buf);p.setFormat(fmt)
 p.displayAll=False;p.priority=8;p.dist=1.5
 call=QgsSimpleLineCallout();call.setEnabled(True);p.setCallout(call)
 layer.setLabeling(QgsVectorLayerSimpleLabeling(p));layer.setLabelsEnabled(True)

def choose_labels(layer,extent,w,h,project):
 if not layer or layer.featureCount()==0:return {'fonte_pt':None,'rotulos_exibidos':0,'rotulos_suprimidos':0,'total':0}
 best=(-1,0);visible=0;total=layer.featureCount()
 for size in (9,8,7):
  style_labels(layer,size);ms=QgsMapSettings();ms.setLayers([layer]);ms.setDestinationCrs(project.crs());ms.setExtent(extent);ms.setOutputSize(QSize(round(w*150/25.4),round(h*150/25.4)));ms.setOutputDpi(150)
  job=QgsMapRendererParallelJob(ms);job.start();job.waitForFinished();assert not job.errors(),job.errors();results=job.takeLabelingResults();visible=len({p.featureId for p in results.labelsWithinRect(ms.extent()) if p.layerID==layer.id()})
  if (visible,size)>best:best=(visible,size)
  if visible==total:break
 style_labels(layer,best[1]);return {'fonte_pt':best[1],'rotulos_previa':best[0],'total':total}

def uc_title(feature):
 # Sigla e categoria vêm da mesma base federal; não inferir sigla pelo nome.
 fields=feature.fields().names()
 assert all(n in fields for n in ['nomeuc','sigla_cate','categoria_']),'Base federal sem campos de nome/categoria/sigla'
 name=str(feature['nomeuc']).strip();sigla=str(feature['sigla_cate']).strip();category=str(feature['categoria_']).strip()
 assert name and sigla and sigla!='NULL','UC federal sem identificação'
 suffix=re.sub(r'^'+re.escape(category)+r'\s*','',name,flags=re.I)
 suffix=re.sub(r'^'+re.escape(sigla)+r'\s*','',suffix,flags=re.I).title()
 suffix=re.sub(r'\b(Da|Das|De|Do|Dos|E)\b',lambda m:m[0].lower(),suffix)
 return sigla.upper()+' '+suffix

def normalize_name(value):
 return ''.join(c for c in unicodedata.normalize('NFD',value.upper()) if not unicodedata.combining(c))

def locator_layers(project,out,aes,crs):
 path=out/'contexto/contexto_ibge.gpkg';assert path.is_file(),'Base IBGE do localizador ausente'
 states=QgsVectorLayer(str(path)+'|layername=estados','Estados — IBGE 2025','ogr');biomes=QgsVectorLayer(str(path)+'|layername=biomas','Biomas — IBGE 2025','ogr')
 assert states.isValid() and biomes.isValid()
 palette={'AMAZONIA':'#C6DFC5','CAATINGA':'#EAD6B1','CERRADO':'#DDE3B2','MATA ATLANTICA':'#BFD8C8','PAMPA':'#D5DFBB','PANTANAL':'#C7DCE1'}
 cats=[]
 for name in sorted({str(f['nome']) for f in biomes.getFeatures()}):
  color=palette.get(normalize_name(name),'#ECE8DC');cats.append(QgsRendererCategory(name,QgsFillSymbol.createSimple({'color':color,'outline_color':'#8A806D','outline_width':'0.12'}),name))
 biomes.setRenderer(QgsCategorizedSymbolRenderer('nome',cats))
 states.setRenderer(QgsSingleSymbolRenderer(QgsFillSymbol.createSimple({'color':'255,255,255,0','outline_color':'#455a64','outline_width':'0.15'})))
 labels=QgsPalLayerSettings();labels.fieldName='nome';fmt=QgsTextFormat();fmt.setFont(QFont('Arial',6));fmt.setSize(6);fmt.setColor(QColor('#455a64'));buf=QgsTextBufferSettings();buf.setEnabled(True);buf.setColor(QColor('white'));buf.setSize(.35);fmt.setBuffer(buf);labels.setFormat(fmt);states.setLabeling(QgsVectorLayerSimpleLabeling(labels));states.setLabelsEnabled(True)
 geom=QgsGeometry.unaryUnion([f.geometry() for a in aes for f in a.getFeatures()]);geom.transform(QgsCoordinateTransform(crs,states.crs(),project))
 original=QgsVectorLayer(str(path)+'|layername=estados_identificacao','Identificação de estados','ogr');assert original.isValid()
 selected=[f for f in original.getFeatures() if f.geometry().intersects(geom)]
 extent=QgsRectangle(selected[0].geometry().boundingBox()) if selected else QgsRectangle(geom.boundingBox())
 for f in selected[1:]:extent.combineExtentWith(f.geometry().boundingBox())
 if not selected:extent=QgsRectangle(extent.center().x()-2,extent.center().y()-2,extent.center().x()+2,extent.center().y()+2)
 states.setCustomProperty('monitora_uf_contexto',','.join(str(f['nome']) for f in selected) or 'AEs sem interseção com a malha estadual original')
 extent.scale(1.15)
 marker=QgsVectorLayer('MultiPolygon?crs='+crs.authid(),'Áreas Elegíveis — localização','memory');
 for a in aes:
  for f in a.getFeatures():
   item=QgsFeature();item.setGeometry(f.geometry());assert marker.dataProvider().addFeatures([item])[0]
 marker.updateExtents()
 # Persistir referência reduzida no projeto de edição; não modifica os polígonos de campo.
 opts=QgsVectorFileWriter.SaveVectorOptions();opts.driverName='GPKG';opts.layerName='areas_elegiveis';dest=out/'contexto/localizacao_ae.gpkg'
 assert QgsVectorFileWriter.writeAsVectorFormatV3(marker,str(dest),project.transformContext(),opts)[0]==QgsVectorFileWriter.NoError
 marker=QgsVectorLayer(str(dest)+'|layername=areas_elegiveis','Áreas Elegíveis — localização','ogr');marker.setRenderer(QgsSingleSymbolRenderer(QgsFillSymbol.createSimple({'color':'#e31a1c','outline_color':'#a40000','outline_width':'0.6'})))
 group=project.layerTreeRoot().addGroup('Contexto dos localizadores')
 for layer in [marker,states,biomes]:
  layer.setCustomProperty('monitora_contexto',True);project.addMapLayer(layer,False);group.addLayer(layer)
 group.setExpanded(False)
 return states,biomes,marker,extent,palette

def visible_biomes(project,biomes,m,out,name,threshold):
 # Mesmo recorte/generalização no desenho e na legenda; base IBGE original preservada.
 frame=QgsGeometry.fromPolygonXY([[QgsPointXY(v.x(),v.y()) for v in m.visibleExtentPolygon()]])
 mem=QgsVectorLayer('MultiPolygon?crs='+biomes.crs().authid(),'Biomas — '+name,'memory');mem.dataProvider().addAttributes(biomes.fields());mem.updateFields();audit=[]
 for f in biomes.getFeatures():
  cut=f.geometry().intersection(frame);area=cut.area()/frame.area()*m.sizeWithUnits().width()*m.sizeWithUnits().height();show=area>0 and area>=threshold
  audit.append({'bioma':str(f['nome']),'area_papel_mm2':area,'representado':show})
  if show:
   cut.convertToMultiType();item=QgsFeature(mem.fields());item.setAttributes(f.attributes());item.setGeometry(cut);assert mem.dataProvider().addFeatures([item])[0]
 mem.updateExtents();dest=out/'contexto'/('biomas_'+name+'.gpkg');opts=QgsVectorFileWriter.SaveVectorOptions();opts.driverName='GPKG';opts.layerName='biomas'
 assert QgsVectorFileWriter.writeAsVectorFormatV3(mem,str(dest),project.transformContext(),opts)[0]==QgsVectorFileWriter.NoError
 layer=QgsVectorLayer(str(dest)+'|layername=biomas',mem.name(),'ogr');assert layer.isValid();layer.setRenderer(biomes.renderer().clone());layer.setCustomProperty('monitora_contexto',True)
 project.addMapLayer(layer,False);project.layerTreeRoot().findGroup('Contexto dos localizadores').addLayer(layer)
 return layer,[v['bioma'] for v in audit if v['representado']],audit

def build(config_file):
 cfg=json.loads(Path(config_file).read_text(encoding='utf-8'));root=Path(cfg['root']);out=root/'05_qgis';out.mkdir(exist_ok=True);assets=out/'recursos';assets.mkdir(exist_ok=True)
 project=QgsProject.instance();assert project.read(str(root/'01_qfield/projeto.qgs'))
 crs=project.crs();project.setFileName(str(out/'projeto_edicao.qgz'));project.setFilePathStorage(Qgis.FilePathType.Relative)
 source_hash={};by_name={};all_layers=[]
 for node in project.layerTreeRoot().findLayers():
  layer=node.layer();assert layer and layer.isValid(),node.name();all_layers.append(layer);by_name[layer.name()]=layer
  if isinstance(layer,QgsVectorLayer):
   src=Path(layer.source().split('|')[0]);source_hash[str(src)]=sha(src);dest=out/'dados'/src.name;dest.parent.mkdir(exist_ok=True);shutil.copy2(src,dest)
   layer.setDataSource(str(dest)+'|'+layer.source().split('|',1)[1],layer.name(),'ogr');layer.setReadOnly(False)
   form=layer.editFormConfig()
   for name in ['chave_grade','id_grade','id_malha','codigo_pa']:
    idx=layer.fields().indexFromName(name)
    if idx>=0:form.setReadOnly(idx,True)
   layer.setEditFormConfig(form)
   if layer.geometryType()==Qgis.GeometryType.Point:
    for name,expr in {'qf_lon':"x(transform($geometry,@layer_crs,'EPSG:4326'))",'qf_lat':"y(transform($geometry,@layer_crs,'EPSG:4326'))",'qf_x_m':'x($geometry)','qf_y_m':'y($geometry)'}.items():
     idx=layer.fields().indexFromName(name)
     if idx>=0:layer.setDefaultValueDefinition(idx,QgsDefaultValue(expr,True))
   assert layer.isValid(),layer.name()
  else:
   # Resolve caminho absoluto antes de salvar o projeto em outra subpasta.
   if layer.providerType()=='gdal':layer.setDataSource(str(Path(layer.source()).resolve()),layer.name(),'gdal')
 for p in (root/'.temporarios').glob('logo_*.png'):shutil.copy2(p,assets/p.name)
 north=assets/'norte.svg';north.write_text('<svg xmlns="http://www.w3.org/2000/svg" width="50" height="95" viewBox="0 0 50 95"><text x="25" y="16" text-anchor="middle" font-family="Arial" font-size="18" font-weight="bold">N</text><path d="M25 22 L8 88 L25 70 Z" fill="white" stroke="#111" stroke-width="2"/><path d="M25 22 L42 88 L25 70 Z" fill="#111" stroke="white" stroke-width="1"/></svg>')
 aes=[by_name[v['nome']] for v in cfg.get('camadas',[{'nome':'AE','papel':'areas_elegiveis'}]) if v['papel']=='areas_elegiveis'];assert aes,'Áreas Elegíveis ausentes';ae=aes[0];ae_extent=QgsRectangle(ae.extent())
 for v in aes[1:]:ae_extent.combineExtentWith(v.extent())
 uc=by_name.get('UC');grid=by_name.get('grade_amostral');regional=by_name.get('sat_escala_regional')
 assert regional is not None,'Base regional ausente'
 uc_names=[]
 if uc:
  ae_union=QgsGeometry.unaryUnion([f.geometry() for a in aes for f in a.getFeatures()]);uc_features=[f for f in uc.getFeatures() if str(f['esferaadm']).casefold()=='federal' and f.geometry().intersection(ae_union).area()>0]
  if not uc_features:project.removeMapLayer(uc.id());uc=None
  else:
   uc_names=[uc_title(f) for f in uc_features];ids=','.join("'"+str(f['cnuc']).replace("'","''")+"'" for f in uc_features)
   if len(uc_features)!=uc.featureCount():
    assert uc.setSubsetString('"cnuc" IN ('+ids+')');uc.reload()
   symbol=uc.renderer().symbol().clone()
   if len(uc_names)>1:uc.setRenderer(QgsCategorizedSymbolRenderer('cnuc',[QgsRendererCategory(str(f['cnuc']),symbol.clone(),uc_title(f)) for f in uc_features]))
   uc.setName(uc_names[0] if len(uc_names)==1 else 'Unidades de Conservação federais')
 for original,friendly in [('grade_amostral','Grade Amostral'),('PA_priorit','Pontos Amostrais prioritários'),('PA_altern','Pontos Amostrais alternativos')]:
  if original in by_name:by_name[original].setName(friendly)
 for layer in aes:layer.setName('Áreas Elegíveis'+(' — '+layer.name() if len(aes)>1 else ''))
 states,biomes,ae_marker,state_extent,palette=locator_layers(project,out,aes,crs)
 uc_locator=None
 if uc:
  uc_locator=uc.clone();uc_locator.setName(uc.name()+' — localizador');uc_locator.setCustomProperty('monitora_contexto',True);project.addMapLayer(uc_locator,False);project.layerTreeRoot().findGroup('Contexto dos localizadores').addLayer(uc_locator)

 for v in aes+[uc,grid]:
  if v:v.setLabelsEnabled(False)
 if grid:
  symbol=grid.renderer().symbol();symbol.setSize(.7);symbol.symbolLayer(0).setStrokeWidth(.12)
 scene_file=root/'02_relatorio/sentinel_fonte.json';scene_info=json.loads(scene_file.read_text(encoding='utf-8')) if scene_file.exists() else {};dates=sorted({c['data'][:10] for c in scene_info.get('cenas',[]) if c.get('data')});date_label=', '.join(dates) if len(dates)<=2 else dates[0]+' a '+dates[-1]
 specs=[('01_areas_elegiveis','Áreas Elegíveis',ae,aes+[uc]),('02_grade_amostral','Grade Amostral',grid,[grid]+aes+[uc]),('03_pa_prioritarios','Pontos amostrais prioritários',by_name.get('PA_priorit'),[by_name.get('PA_priorit'),grid]+aes+[uc]),('04_pa_alternativos','Pontos amostrais alternativos',by_name.get('PA_altern'),[by_name.get('PA_altern'),grid]+aes+[uc])]
 audits=[];dpi=int(cfg['dpi']);factor=math.sqrt(2) if cfg['papel']=='A3' else 1
 for idx,(name,title,target,vectors) in enumerate(specs):
  print(f'Cartografia {idx+1}/4: {title}',flush=True)
  layout=QgsPrintLayout(project);layout.initializeDefaults();layout.setName(name);page=layout.pageCollection().page(0);page.setPageSize(QgsLayoutSize(210*factor,297*factor));layout.renderContext().setDpi(dpi)
  e=extent_of(None,ae_extent) if idx==0 else extent_of(target,ae_extent);landscape=e.width()>e.height()*1.15
  width,height=(297*factor,210*factor) if landscape else (210*factor,297*factor);page.setPageSize(QgsLayoutSize(width,height));fx=width/210;mx,my,mw,mh=13*factor,29*factor,width-26*factor,height-105*factor
  label(layout,title,13*factor,5*factor,mw,9*factor,15,True);label(layout,cfg['projeto'],13*factor,15*factor,mw,9*factor,10)
  m=QgsLayoutItemMap(layout);layout.addLayoutItem(m);m.setId(name+'_principal');m.attemptMove(QgsLayoutPoint(mx,my));m.attemptResize(QgsLayoutSize(mw,mh));m.setCrs(crs);m.setFrameEnabled(True);m.zoomToExtent(e);layout.setReferenceMap(m)
  vectors=[v for v in vectors if v is not None];maplayers=vectors+[regional]
  m.setLayers(maplayers);m.setKeepLayerSet(True)
  label_info=choose_labels(target,m.extent(),mw,mh,project) if name.startswith(('03','04')) else {'total':0,'rotulos_exibidos':0,'rotulos_suprimidos':0}
  if grid:grid.setLabelsEnabled(False)
  styles={}
  for v in maplayers:
   st=QgsMapLayerStyle();st.readFromLayer(v);styles[v.id()]=st.xmlData()
  m.setLayerStyleOverrides(styles);m.setKeepLayerStyles(True)
  # Tema e marcador de extensão mantêm os quatro estados reabríveis no QGIS.
  for node in project.layerTreeRoot().findLayers():node.setItemVisibilityChecked(node.layer() in maplayers)
  theme=QgsMapThemeCollection.createThemeFromCurrentState(project.layerTreeRoot(),project.layerTreeModel() if hasattr(project,'layerTreeModel') else QgsLayerTreeModel(project.layerTreeRoot()))
  project.mapThemeCollection().insert(title,theme)
  bookmark=QgsBookmark();bookmark.setName(title);bookmark.setGroup('Monitora');bookmark.setExtent(QgsReferencedRectangle(m.extent(),crs));project.bookmarkManager().addBookmark(bookmark)
  gr=m.grid();gr.setEnabled(True);gr.setCrs(QgsCoordinateReferenceSystem(4326));ge=QgsCoordinateTransform(crs,QgsCoordinateReferenceSystem(4326),project).transformBoundingBox(m.extent());interval=max(ge.width(),ge.height())/4
  power=10**math.floor(math.log10(interval));interval=min([v*power for v in [1,2,5,10]],key=lambda x:abs(x-interval));gr.setIntervalX(interval);gr.setIntervalY(interval);gr.setStyle(QgsLayoutItemMapGrid.FrameAnnotationsOnly);gr.setAnnotationEnabled(True);gr.setAnnotationFormat(QgsLayoutItemMapGrid.DecimalWithSuffix);gr.setAnnotationPrecision(max(2,-int(math.floor(math.log10(interval)))+1));gr.setAnnotationFont(QFont('Arial',7));gr.setFrameStyle(QgsLayoutItemMapGrid.ExteriorTicks);gr.setAnnotationDirection(QgsLayoutItemMapGrid.VerticalDescending,QgsLayoutItemMapGrid.Left);gr.setAnnotationDirection(QgsLayoutItemMapGrid.VerticalDescending,QgsLayoutItemMapGrid.Right)
  scale=QgsLayoutItemScaleBar(layout);scale.setStyle('Single Box');scale.setLinkedMap(m);scale.applyDefaultSize();scale.setUnits(Qgis.DistanceUnit.Kilometers);scale.setUnitsPerSegment(max(.1,10**math.floor(math.log10(m.extent().width()/6000))));scale.setNumberOfSegments(2);scale.setNumberOfSegmentsLeft(0);scale.setUnitLabel('km');scale.setFont(QFont('Arial',8));scale.setBackgroundEnabled(True);scale.setBackgroundColor(QColor(255,255,255,210));layout.addLayoutItem(scale);scale.attemptMove(QgsLayoutPoint(mx+2,my+mh-11))
  pic=QgsLayoutItemPicture(layout);layout.addLayoutItem(pic);pic.attemptMove(QgsLayoutPoint(mx+mw-12,my+2));pic.attemptResize(QgsLayoutSize(9,18));pic.setPicturePath(str(north));pic.setLinkedMap(m);pic.setNorthMode(QgsLayoutItemPicture.TrueNorth)
  fy=height-64*factor;fh=56*factor
  # Faixa inferior: dois localizadores quando existe UC; caixas adaptadas à página.
  margin=8*factor;gap=2*factor;available=width-2*margin
  weights=([42,32,45,55,18] if uc else [62,52,60,18]);unit=(available-gap*(len(weights)-1))/sum(weights);slots=[];xx=margin
  for ww in weights:slots.append((xx,ww*unit));xx+=ww*unit+gap
  for xx,ww in slots:box(layout,xx,fy,ww,fh)
  rx,rw=slots[0];label(layout,'Estados e biomas',rx+1,fy+1,rw-2,5,7,True)
  context_map=QgsLayoutItemMap(layout);layout.addLayoutItem(context_map);context_map.setId(name+'_estados_biomas');context_map.attemptMove(QgsLayoutPoint(rx+2,fy+7*factor));context_map.attemptResize(QgsLayoutSize(rw-4,35*factor));context_map.setCrs(states.crs());context_map.setLayers([ae_marker,states,biomes]);context_map.setKeepLayerSet(True);context_map.zoomToExtent(state_extent);context_map.setFrameEnabled(True)
  # Destaque da extensão das AEs garante localização legível mesmo em áreas pequenas.
  overview=context_map.overview();overview.setLinkedMap(m);overview.setEnabled(True);overview.setFrameSymbol(QgsFillSymbol.createSimple({'color':'227,26,28,45','outline_color':'#e31a1c','outline_width':'0.5'}))
  local_biomes,names_biomes,biome_audit=visible_biomes(project,biomes,context_map,out,name,float(cfg.get('bioma_min_mm2',.5)));names_biomes.sort();context_map.setLayers([ae_marker,states,local_biomes])
  for j,bname in enumerate(names_biomes):
   bx=rx+2+(j%2)*(rw-4)/2;by=fy+43*factor+(j//2)*3.5*factor
   chip=QgsLayoutItemShape(layout);chip.setShapeType(QgsLayoutItemShape.Rectangle);chip.setSymbol(QgsFillSymbol.createSimple({'color':palette.get(normalize_name(bname),'#ECE8DC'),'outline_color':'#8A806D','outline_width':'0.1'}));layout.addLayoutItem(chip);chip.attemptMove(QgsLayoutPoint(bx,by+.6));chip.attemptResize(QgsLayoutSize(2,2));label(layout,bname,bx+2.5,by,(rw-4)/2-2.5,3.5,5.7)
  marker_label=label(layout,'Áreas Elegíveis em vermelho',rx+2,fy+38*factor,rw-4,4,5.7,color='#a40000');marker_label.setBackgroundEnabled(True);marker_label.setBackgroundColor(QColor(255,255,255,220))
  if uc:
   ux,uw=slots[1];label(layout,'Localização na UC',ux+1,fy+1,uw-2,5,7,True)
   loc=QgsLayoutItemMap(layout);layout.addLayoutItem(loc);loc.setId(name+'_localizador_uc');loc.attemptMove(QgsLayoutPoint(ux+2,fy+7*factor));loc.attemptResize(QgsLayoutSize(uw-4,46*factor));loc.setCrs(crs);loc.setLayers(aes+[uc_locator,regional]);loc.setKeepLayerSet(True);local_styles={}
   for v in aes+[uc_locator,regional]:
    style=QgsMapLayerStyle();style.readFromLayer(v);local_styles[v.id()]=style.xmlData()
   loc.setLayerStyleOverrides(local_styles);loc.setKeepLayerStyles(True);loc.zoomToExtent(extent_of(uc,ae_extent));loc.overview().setLinkedMap(m);loc.overview().setEnabled(True);loc.setFrameEnabled(True)
  lx,lw=slots[-3];ix,iw=slots[-2];gx,gw=slots[-1]
  legend=QgsLayoutItemLegend(layout);legend.setId(name+'_legenda');legend.setTitle('Legenda');legend.setLinkedMap(m);legend.setAutoUpdateModel(False);legend.model().rootGroup().clear();legend.setWrapString('\n')
  legend_names=[];ae_added=False
  for v in vectors:
   if v in aes:
    if ae_added:continue
    ae_added=True;title_legend='Áreas Elegíveis'
   else:title_legend=v.name()
   node=legend.model().rootGroup().addLayer(v);node.setName(textwrap.fill(title_legend,width=max(22,int((lw-9)/1.05))));legend_names.append(title_legend)
   if v==uc and len(uc_names)>1:QgsLegendRenderer.setNodeLegendStyle(node,QgsLegendStyle.Hidden)
  legend.setStyleFont(QgsLegendStyle.Title,QFont('Arial',8,QFont.Bold));legend.setStyleFont(QgsLegendStyle.SymbolLabel,QFont('Arial',7));layout.addLayoutItem(legend);legend.attemptMove(QgsLayoutPoint(lx+1,fy+1));legend.attemptResize(QgsLayoutSize(lw-2,fh-2));legend.setResizeToContents(False)
  note=f"{crs.authid()} · escala 1:{round(m.scale()):,}".replace(',','.')
  info=note+'\nAEs e pontos: dados fornecidos.\n'+('UCs federais: ICMBio.\n' if uc else '')+'Estados e biomas: IBGE, 2025.\nSentinel-2 L2A · RGB nativo 10 m.\nDatas: '+(date_label or 'não informadas')+'.\nCopernicus Sentinel / AWS Earth Search.\nPA: local planejado; não é UA instalada.\nElaboração: '+cfg['elaboracao']+'\n'+datetime.date.today().isoformat()+' · Monitora QField v0.4.2\nFontes e métodos: 02_relatorio.'
  label(layout,'Informações do mapa',ix+1,fy+1,iw-2,5,7,True);label(layout,info,ix+1.5,fy+7,iw-3,fh-8,6.5)
  for j,n in enumerate(['monitora','cbc','icmbio']):
   im=QgsLayoutItemPicture(layout);im.setId(name+'_logo_'+n);layout.addLayoutItem(im);im.attemptMove(QgsLayoutPoint(gx+1.5,fy+(2+j*18)*factor));im.attemptResize(QgsLayoutSize(gw-3,15*factor));im.setPicturePath(str(assets/f'logo_{n}.png'));im.setPictureAnchor(QgsLayoutItemPicture.Middle)
  missing=target is None or target.featureCount()==0
  if missing:label(layout,'Sem feições fornecidas para este tema; referência: Áreas Elegíveis.',mx+2,my+2,mw-4,8,9,True)
  project.layoutManager().addLayout(layout)
  pdf=root/'mapas_pdf'/f'{name}.pdf';png=root/'mapas_png'/f'{name}.png'
  exporter=QgsLayoutExporter(layout);ps=QgsLayoutExporter.PdfExportSettings();ps.dpi=dpi;ps.appendGeoreference=True;ps.exportMetadata=True;ps.textRenderFormat=Qgis.TextRenderFormat.AlwaysText
  assert exporter.exportToPdf(str(pdf),ps)==QgsLayoutExporter.Success,exporter.errorMessage()
  evidence=check_pdf(pdf,crs)
  gt=evidence['geotransform'];px=(mx+mw/2)/width*evidence['largura_px_72dpi'];py=(my+mh/2)/height*evidence['altura_px_72dpi'];cx=gt[0]+px*gt[1]+py*gt[2];cy=gt[3]+px*gt[4]+py*gt[5]
  error=math.hypot(cx-m.extent().center().x(),cy-m.extent().center().y());assert error<2*max(abs(gt[1]),abs(gt[5])),('Georreferência do mapa principal divergente',error);evidence['erro_centro_m']=error
  controls=[(mx,my,m.extent().xMinimum(),m.extent().yMaximum()),(mx+mw,my,m.extent().xMaximum(),m.extent().yMaximum()),(mx,my+mh,m.extent().xMinimum(),m.extent().yMinimum()),(mx+mw,my+mh,m.extent().xMaximum(),m.extent().yMinimum())];errors=[]
  for xx,yy,gx,gy in controls:
   px=xx/width*evidence['largura_px_72dpi'];py=yy/height*evidence['altura_px_72dpi'];errors.append(math.hypot(gt[0]+px*gt[1]+py*gt[2]-gx,gt[3]+px*gt[4]+py*gt[5]-gy))
  assert max(errors)<2*max(abs(gt[1]),abs(gt[5])),errors;evidence['erros_cantos_m']=errors
  ims=QgsLayoutExporter.ImageExportSettings();ims.dpi=dpi;ims.generateWorldFile=False;ims.exportMetadata=False
  assert exporter.exportToImage(str(png),ims)==QgsLayoutExporter.Success,exporter.errorMessage()
  a,b,c,d,e,f=exporter.computeWorldFileParameters(dpi)
  png.with_suffix('.pgw').write_text('\n'.join(format(v,'.16g') for v in [a,d,b,e,c,f])+'\n')
  # Rotulagem efetivamente exportada, não apenas uma estimativa por número de pontos.
  results=exporter.labelingResults();visible=0
  if name.startswith(('03','04')) and target:
   for key,result in results.items():
    if str(key)==m.uuid():visible=len({p.featureId for p in result.labelsWithinRect(m.extent()) if p.layerID==target.id()})
   label_info.update(rotulos_exibidos=visible,rotulos_suprimidos=target.featureCount()-visible)
  (png.with_suffix('.prj')).write_text(crs.toWkt(),encoding='utf-8')
  gt_png=[c-a/2-b/2,a,b,f-d/2-e/2,d,e];Path(str(png)+'.aux.xml').write_text('<PAMDataset><SRS>'+escape(crs.toWkt())+'</SRS><GeoTransform>'+','.join(format(v,'.16g') for v in gt_png)+'</GeoTransform></PAMDataset>',encoding='utf-8')
  ds=gdal.Open(str(png));assert ds.GetProjection() and ds.GetGeoTransform();ds=None
  audits.append({'mapa':name,'tema':title,'sem_feicoes':missing,'escala':m.scale(),'extent':[m.extent().xMinimum(),m.extent().yMinimum(),m.extent().xMaximum(),m.extent().yMaximum()],'mapa_principal_uuid':m.uuid(),'rotulos':label_info,'pdf':evidence,'camadas':[v.name() for v in maplayers],'legenda':legend_names,'ucs':uc_names,'localizadores':(['estados_biomas','uc'] if uc else ['estados_biomas']),'contexto_uf':states.customProperty('monitora_uf_contexto'),'biomas_localizador':biome_audit,'biomas_legenda':names_biomes,'bioma_min_mm2':float(cfg.get('bioma_min_mm2',.5))})
 # Exibição inicial de edição inclui todas as referências e fundos offline.
 for node in project.layerTreeRoot().findLayers():node.setItemVisibilityChecked(node.layer().name()!='Google Satellite' and not node.layer().customProperty('monitora_contexto',False))
 project.viewSettings().setDefaultViewExtent(QgsReferencedRectangle(extent_of(None,ae_extent),crs))
 assert project.write(),'Falha ao salvar QGZ'
 for p,h in source_hash.items():assert sha(p)==h,'Edição alterou fonte QField'
 dump({'status':'PASS','QGIS':Qgis.QGIS_VERSION,'mapas':audits,'vetores_isolados':True,'projeto':'05_qgis/projeto_edicao.qgz'},root/'02_relatorio/cartografia.json')
 print('Cartografia: quatro PDF georreferenciados e quatro PNG verificados.',flush=True)

if __name__=='__main__':
 if sys.argv[1]=='probe':probe(sys.argv[2])
 elif sys.argv[1]=='build':build(sys.argv[2])
 else:raise ValueError('Modo inválido')
 # QGIS/Qt pode encerrar com referências gráficas pendentes; SO libera o processo.
 sys.stdout.flush();sys.stderr.flush();os._exit(0)
