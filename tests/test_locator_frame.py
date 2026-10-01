"""QGIS Python: test_locator_frame.py ENTREGA; validação somente leitura."""
from pathlib import Path
import sys,os
from qgis.core import *
app=QgsApplication([],False);app.initQgis();p=QgsProject.instance();count=0
for name in ['Censipam_100m','Noronha']:
 p.clear();assert p.read(str(Path(sys.argv[1])/name/'05_qgis/projeto_edicao.qgz'))
 for layout in p.layoutManager().layouts():
  loc=next(i for i in layout.items() if isinstance(i,QgsLayoutItemMap) and i.id().endswith('_estados_biomas'))
  frame=loc.layers()[0];assert frame.name()=='AE — quadro de localização' and frame.featureCount()==1 and not loc.overview().enabled()
  feat=next(frame.getFeatures());ctx=loc.createExpressionContext();ctx.setFeature(feat);exp=QgsExpression(frame.renderer().symbol().symbolLayer(0).geometryExpression());geom=exp.evaluate(ctx);assert not exp.hasEvalError() and geom.contains(feat.geometry())
  bbox=QgsCoordinateTransform(frame.crs(),loc.crs(),p).transformBoundingBox(geom.boundingBox());safe=QgsRectangle(bbox);safe.grow(.28*loc.extent().width()/loc.sizeWithUnits().width());assert loc.extent().contains(safe)
  assert min(bbox.width()/loc.extent().width()*loc.sizeWithUnits().width(),bbox.height()/loc.extent().height()*loc.sizeWithUnits().height())>=3.5
  scope=QgsExpressionContextScope();scope.setVariable('map_scale',loc.scale()*100);ctx.appendScope(scope);other=exp.evaluate(ctx);assert not exp.hasEvalError() and other.boundingBox().width()>geom.boundingBox().width()
  count+=1
print('PASS:',count,'quadros, extensão AE, traço contido e escala dinâmica',flush=True);os._exit(0)
