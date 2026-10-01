"""Teste PyQGIS com várias AEs nomeadas e temas sem feições; usa cópia descartável."""
import sys, json, shutil, importlib.util
from pathlib import Path
renderer, source, root = map(Path, sys.argv[1:4])
spec = importlib.util.spec_from_file_location('cart', renderer)
cart = importlib.util.module_from_spec(spec); spec.loader.exec_module(cart)
from qgis.core import *
root.mkdir(exist_ok=True)
for name in ['01_qfield', '02_relatorio', '.temporarios', 'mapas_pdf', 'mapas_png']:
 (root/name).mkdir(exist_ok=True)
for p in (source/'.temporarios').glob('logo_*.png'): shutil.copy2(p,root/'.temporarios'/p.name)
p=QgsProject.instance(); assert p.read(str(source/'01_qfield/projeto.qgs'))
ae=p.mapLayersByName('AE')[0]
for layer in list(p.mapLayers().values()):
 if layer.name() not in ['AE','UC','sat_escala_regional']: p.removeMapLayer(layer.id())
ae.setName('campo_norte')
mem=QgsVectorLayer('MultiPolygon?crs='+ae.crs().authid(),'campo_sul','memory')
mem.dataProvider().addAttributes(ae.fields());mem.updateFields()
for feature in ae.getFeatures():
 geom=feature.geometry();geom.translate(20000,0);feature.setGeometry(geom);mem.dataProvider().addFeatures([feature])
mem.updateExtents()
opts=QgsVectorFileWriter.SaveVectorOptions();opts.driverName='GPKG';opts.layerName='campo_sul'
dest=root/'01_qfield/campo_sul.gpkg'
assert QgsVectorFileWriter.writeAsVectorFormatV3(mem,str(dest),p.transformContext(),opts)[0]==QgsVectorFileWriter.NoError
other=QgsVectorLayer(str(dest)+'|layername=campo_sul','campo_sul','ogr');assert other.isValid();p.addMapLayer(other)
expected=QgsRectangle(ae.extent());expected.combineExtentWith(other.extent())
p.setFileName(str(root/'01_qfield/projeto.qgs'));assert p.write();p.clear()
cfg={'root':str(root),'projeto':'Teste: AEs distintas e temas ausentes','dpi':96,'papel':'A4','elaboracao':'Homologação','camadas':[{'nome':n,'papel':'areas_elegiveis'} for n in ['campo_norte','campo_sul']]}
config=root/'.temporarios/cartografia.json';config.write_text(json.dumps(cfg),encoding='utf-8');cart.build(config)
result=json.loads((root/'02_relatorio/cartografia.json').read_text(encoding='utf-8'))
assert len(result['mapas'])==4
for i,m in enumerate(result['mapas']):
 assert {'campo_norte','campo_sul'}<=set(m['camadas'])
 assert m['sem_feicoes']==(i>0)
 assert QgsRectangle(*m['extent']).contains(expected)
 assert m['pdf']['crs']=='31983'
print('PASS multiAE_nomeadas_extent_uniao; PASS tres_temas_ausentes; PASS quatro_PDF_PNG',flush=True)
cart.os._exit(0)
