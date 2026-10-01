"""QGIS Python: test_footer_v047.py ENTREGA ANTERIORES renderer.py."""
import sys,os,json,math,ast,hashlib
from pathlib import Path
from qgis.core import *
app=QgsApplication([],False);app.initQgis();root=Path(sys.argv[1]);base=Path(sys.argv[2]);ns={}
source=ast.parse(Path(sys.argv[3]).read_text(encoding='utf-8'));fn=next(n for n in source.body if isinstance(n,ast.FunctionDef) and n.name=='footer_slots');exec(compile(ast.Module(body=[fn],type_ignores=[]),'<footer_slots>','exec'),ns)
for factor in [1,math.sqrt(2)]:
 for width in [210*factor,297*factor]:
  a=ns['footer_slots'](width,factor,True);b=ns['footer_slots'](width,factor,False)
  assert a[0]==b[0] and b[-1][1]>a[-1][1]
  for slots in [a,b]:
   assert all(slots[i][0]+slots[i][1]<slots[i+1][0] for i in range(len(slots)-1))
   assert abs(slots[-1][0]+slots[-1][1]-(width-8*factor))<1e-6
p=QgsProject.instance();audit=[]
for name,old,has_uc in [('Censipam_100m','ENTREGA_v0.4.5',False),('Noronha','ENTREGA_v0.4.6',True)]:
 folder=root/name;p.clear();assert p.read(str(folder/'05_qgis/projeto_edicao.qgz'));assert all(l.isValid() for l in p.mapLayers().values())
 for layout in p.layoutManager().layouts():
  maps=[x for x in layout.items() if isinstance(x,QgsLayoutItemMap)];assert len(maps)==(3 if has_uc else 2)
  ctx=next(x for x in maps if x.id().endswith('_estados_biomas'));page=layout.pageCollection().page(0);w=page.pageSize().width();factor=1 if w<300 else math.sqrt(2)
  expected=ns['footer_slots'](w,factor,True)[0][1]-4
  assert abs(ctx.sizeWithUnits().width()-expected)<.01 and abs(ctx.sizeWithUnits().height()-35*factor)<.01,(name,layout.name(),w,ctx.sizeWithUnits().width(),expected,ctx.sizeWithUnits().height())
  legend=next(x for x in layout.items() if isinstance(x,QgsLayoutItemLegend));size=QgsLegendRenderer(legend.model(),legend.legendSettings()).minimumSize()
  assert size.width()<=legend.sizeWithUnits().width()+.2,(name,layout.name(),'legenda largura',size.width(),legend.sizeWithUnits().width())
  assert size.height()<=legend.sizeWithUnits().height()+.2,(name,layout.name(),'legenda altura')
  info=next(x for x in layout.items() if isinstance(x,QgsLayoutItemLabel) and x.id().endswith('_informacoes'))
  assert info.font().pointSizeF()==8 and QgsLayoutUtils.textHeightMM(info.font(),info.text())+2<=info.sizeWithUnits().height()
  assert all(QgsLayoutUtils.textWidthMM(info.font(),line)<=info.sizeWithUnits().width()-1 for line in info.text().split('\n'))
  assert legend.legendSettings().style(QgsLegendStyle.SymbolLabel).font().pointSizeF()==9
  main=next(x for x in maps if x.id().endswith('_principal'));assert main.extent().contains(main.layers()[0].extent())
  audit.append({'projeto':name,'mapa':layout.name(),'localizador_mm':[ctx.sizeWithUnits().width(),ctx.sizeWithUnits().height()],'legenda_pt':9,'informacoes_pt':8,'texto_cabe':True})
 for directory in ['01_qfield','03_vetores','04_csv']:
  oldroot=base/old/name/directory
  for f in oldroot.rglob('*'):
   if f.is_file():assert hashlib.sha256(f.read_bytes()).digest()==hashlib.sha256((folder/directory/f.relative_to(oldroot)).read_bytes()).digest(),f
print(json.dumps({'status':'PASS','mapas':audit,'dados_imagens_QField_preservados':True,'dimensoes_A4_A3_com_sem_UC':True},ensure_ascii=False,indent=2),flush=True);os._exit(0)
