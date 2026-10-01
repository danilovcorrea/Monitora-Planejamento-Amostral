"""Incorpora helpers e recursos no único R distribuído; não é necessário ao usuário."""
from pathlib import Path
import base64,json
root=Path(__file__).resolve().parents[1];p=root/'monitora_criar_qfield.R';s=p.read_text()
a=s.index('# HELPERS_REVISAO_INICIO');b=s.index('# HELPERS_REVISAO_FIM',a)+len('# HELPERS_REVISAO_FIM')
helpers=(root/'cartografia/revisao_helpers.R').read_text()
resources={f.name:base64.b64encode(f.read_bytes()).decode() for f in (root/'recursos').glob('*.png') if f.is_file()}
if (root/'cartografia/cartografia_qgis.py').exists():resources['cartografia_qgis.py']=base64.b64encode((root/'cartografia/cartografia_qgis.py').read_bytes()).decode()
embedded='\nMQ_RECURSOS <- '+ 'list('+','.join(json.dumps(k)+'='+json.dumps(v) for k,v in resources.items())+')\n'
p.write_text(s[:a]+'# HELPERS_REVISAO_INICIO\n'+helpers+embedded+'# HELPERS_REVISAO_FIM'+s[b:])
print('Incorporados',len(resources),'recursos; R autossuficiente.')
