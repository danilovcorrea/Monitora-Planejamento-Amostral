"""Executar com Python QGIS: arquivo.py RAIZ_ENTREGA [RAIZ_ANTERIOR]."""
import os,sys,json
from pathlib import Path
from qgis.core import QgsApplication,QgsProject,QgsLayoutItemMap
from qgis.PyQt.QtGui import QImage
app=QgsApplication([],False);app.initQgis()
root=Path(sys.argv[1]);p=QgsProject.instance();assert p.read(str(root/'05_qgis/projeto_edicao.qgz'))
assert all(l.isValid() for l in p.mapLayers().values())
has_detail=any(l.name()=='sat_escala_local' for l in p.mapLayers().values());audit=[]
for layout in p.layoutManager().layouts():
 maps=[x for x in layout.items() if isinstance(x,QgsLayoutItemMap) and x.id().endswith('_principal')];assert len(maps)==1
 m=maps[0];names=[l.name() for l in m.layers()];assert 'sat_escala_regional' in names
 assert ('sat_escala_local' in names)==has_detail
 if has_detail:assert names.index('sat_escala_local')<names.index('sat_escala_regional')
 assert 'Google Satellite' not in names
 image=QImage(str(root/'mapas_png'/f'{layout.name()}.png'));assert not image.isNull()
 changed=None
 if len(sys.argv)>2:
  old=QImage(str(Path(sys.argv[2])/'mapas_png'/f'{layout.name()}.png'));assert old.size()==image.size()
  # Área central do mapa: exclui título, rodapé, legendas e metadados de versão.
  diffs=[sum(abs(image.pixelColor(x,y).getRgb()[i]-old.pixelColor(x,y).getRgb()[i]) for i in range(3)) for x in range(image.width()//4,image.width()*3//4,17) for y in range(image.height()//4,image.height()//2,17)]
  changed=sum(d>30 for d in diffs)/len(diffs);assert changed>.1,(layout.name(),changed)
 audit.append({'mapa':layout.name(),'detalhe':has_detail,'imagens':[n for n in names if n.startswith('sat_')],'fracao_pixels_alterados':changed})
assert len(audit)==4
print(json.dumps({'status':'PASS','mapas':audit},ensure_ascii=False,indent=2),flush=True);os._exit(0)
