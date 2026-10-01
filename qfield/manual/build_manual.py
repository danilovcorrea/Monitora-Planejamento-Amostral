"""Materializa HTML autossuficiente a partir do conteúdo e estilo versionados."""
from pathlib import Path
import re
root=Path(__file__).resolve().parent
body=(root/'conteudo.html').read_text(encoding='utf-8')
items=re.findall(r'<section id="([^"]+)"><h1>([^<]+)</h1>',body)
toc=''.join(f'<a href="#{ident}">{title}</a>' for ident,title in items)
body=body.replace('{{SUMARIO}}',toc)
css=(root/'estilo_manual.css').read_text(encoding='utf-8')
html=f'<!doctype html><html lang="pt-BR"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Manual QField Monitora v0.4.0</title><style>{css}</style></head><body>{body}</body></html>'
p=root/'manual_qfield_v0.4.0.html';p.write_text(html,encoding='utf-8')
assert len(items)==14 and '{{' not in body
print(p)
