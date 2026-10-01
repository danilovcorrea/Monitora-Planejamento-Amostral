"""Casos sem UC/com múltiplas UCs e inspeção do produto QGIS final."""
import sys,json,shutil,importlib.util
from pathlib import Path
renderer,source,tests=map(Path,sys.argv[1:4]);spec=importlib.util.spec_from_file_location('cart',renderer);cart=importlib.util.module_from_spec(spec);spec.loader.exec_module(cart)
from qgis.core import *
project=QgsProject.instance();reports=[]
for case in ['sem_uc','multiplas_ucs']:
 root=tests/case;root.mkdir(parents=True,exist_ok=True)
 for folder in ['01_qfield','02_relatorio','05_qgis/contexto','.temporarios','mapas_pdf','mapas_png']:(root/folder).mkdir(parents=True,exist_ok=True)
 for p in (source/'05_qgis/recursos').glob('logo_*.png'):shutil.copy2(p,root/'.temporarios'/p.name)
 shutil.copy2(source/'05_qgis/contexto/contexto_ibge.gpkg',root/'05_qgis/contexto/contexto_ibge.gpkg')
 project.clear();assert project.read(str(source/'01_qfield/projeto.qgs'));uc=project.mapLayersByName('UC')[0];base=next(uc.getFeatures());mem=QgsVectorLayer('MultiPolygon?crs='+uc.crs().authid(),'fixture','memory');mem.dataProvider().addAttributes(uc.fields());mem.updateFields()
 features=[]
 for j in range(1 if case=='sem_uc' else 4):
  f=QgsFeature(base);f.setId(-1);f['fid']=j+1;g=f.geometry()
  if case=='sem_uc' or j==2:g.translate(1000000,0)
  if j==1:f['cnuc']='TESTE-REBIO';f['nomeuc']='RESERVA BIOLÓGICA TESTE';f['categoria_']='Reserva Biológica';f['sigla_cate']='REBIO'
  if j==2:f['cnuc']='TESTE-DISTANTE'
  if j==3:f['cnuc']='TESTE-ESTADUAL';f['esferaadm']='Estadual'
  f.setGeometry(g);features.append(f)
 assert mem.dataProvider().addFeatures(features)[0];mem.updateExtents();opts=QgsVectorFileWriter.SaveVectorOptions();opts.driverName='GPKG';opts.layerName='UC';dest=root/'01_qfield/UC.gpkg'
 assert QgsVectorFileWriter.writeAsVectorFormatV3(mem,str(dest),project.transformContext(),opts)[0]==QgsVectorFileWriter.NoError
 uc.setDataSource(str(dest)+'|layername=UC','UC','ogr');project.setFileName(str(root/'01_qfield/projeto.qgs'));assert project.write();project.clear()
 cfg={'root':str(root),'projeto':case,'dpi':96,'papel':'A4','elaboracao':'Homologação','camadas':[{'nome':'AE','papel':'areas_elegiveis'}]};config=root/'.temporarios/config.json';config.write_text(json.dumps(cfg),encoding='utf-8');cart.build(config)
 result=json.loads((root/'02_relatorio/cartografia.json').read_text(encoding='utf-8'))
 for m in result['mapas']:
  assert m['localizadores']==(['estados_biomas'] if case=='sem_uc' else ['estados_biomas','uc'])
  assert m['ucs']==([] if case=='sem_uc' else ['PARNA das Sempre-Vivas','REBIO Teste']),m['ucs']
  assert 'Áreas Elegíveis' in m['legenda'] and 'AE' not in m['legenda']
  assert m['contexto_uf']=='MG' and m['pdf']['crs']=='31983'
 if case=='multiplas_ucs':
  from qgis.PyQt.QtGui import QImage
  layout=project.layoutManager().layouts()[0];loc=next(i for i in layout.items() if isinstance(i,QgsLayoutItemMap) and i.id().endswith('_localizador_uc'))
  im=QImage(str(root/'mapas_png/01_areas_elegiveis.png'));ratio=im.width()/layout.pageCollection().page(0).pageSize().width();pos=loc.positionWithUnits();sz=loc.sizeWithUnits();yellow=0
  for xx in range(round(pos.x()*ratio)+3,round((pos.x()+sz.width())*ratio)-3):
   for yy in range(round(pos.y()*ratio)+3,round((pos.y()+sz.height())*ratio)-3):
    c=im.pixelColor(xx,yy);yellow+=c.red()>220 and c.green()>220 and c.blue()<100
  assert yellow>20,('Limites das UCs invisíveis no localizador',yellow)
 reports.append({'caso':case,'status':'PASS','mapas':4})
# Seis classes, inclusive Pampa/Pantanal; fragmento abaixo do mínimo excluído do desenho e legenda.
lo=QgsPrintLayout(project);lo.initializeDefaults();m=QgsLayoutItemMap(lo);lo.addLayoutItem(m);m.attemptResize(QgsLayoutSize(100,100));m.setCrs(QgsCoordinateReferenceSystem(4674));m.zoomToExtent(QgsRectangle(0,0,10,10))
from qgis.PyQt.QtCore import QVariant
v=QgsVectorLayer('MultiPolygon?crs=EPSG:4674','fixture_biomas','memory');v.dataProvider().addAttributes([QgsField('nome',QVariant.String)]);v.updateFields()
for i,name in enumerate(['Amazônia','Caatinga','Cerrado','Mata Atlântica','Pampa','Pantanal']):
 f=QgsFeature(v.fields());f['nome']=name;f.setGeometry(QgsGeometry.fromRect(QgsRectangle(1+i,1,1+i+(.01 if i==0 else .5),1+(.01 if i==0 else .5))));assert v.dataProvider().addFeatures([f])[0]
v.updateExtents();v.setRenderer(QgsSingleSymbolRenderer(QgsFillSymbol.createSimple({'color':'green'})))
layer,names,audit=cart.visible_biomes(project,v,m,root/'05_qgis','fixture_seis',.5)
assert set(names)=={'Caatinga','Cerrado','Mata Atlântica','Pampa','Pantanal'} and layer.featureCount()==5
reports.append({'caso':'legenda_e_desenho_sincronizados_seis_biomas','status':'PASS'})
project.clear();assert project.read(str(source/'05_qgis/projeto_edicao.qgz'))
assert project.mapLayersByName('PARNA das Sempre-Vivas') and project.mapLayersByName('Pontos Amostrais prioritários')
for layout in project.layoutManager().layouts():
 maps=[i for i in layout.items() if isinstance(i,QgsLayoutItemMap)];assert len(maps)==3
 for item in layout.items():
  if isinstance(item,QgsLayoutItemPicture) and '_logo_' in item.id():assert item.pictureAnchor()==QgsLayoutItemPicture.Middle
for layer in project.mapLayers().values():assert layer.isValid(),layer.name()
cart.dump({'status':'PASS','casos':reports,'projeto_final':'camadas válidas; nomes completos; três mapas por layout; logos centralizadas'},tests/'validacao_v042.json')
print('PASS: sem UC; múltiplas UCs; exclusão de UC distante/estadual; nomes; localizadores; logos; PDFs georreferenciados',flush=True)
cart.os._exit(0)
