# Monitora — criação independente de projetos QField

Script `monitora_criar_qfield.R`, versão 0.4.1. Execute em R/RStudio; não é necessário carregar o script biológico do Monitora. QGIS/PyQGIS é necessário para gerar o projeto de edição, os quatro layouts e mapas PDF/PNG (habilitados por padrão). Consulte `HOMOLOGACAO.md` para o alcance e as limitações da validação.

Consulte o manual pronto: [HTML](manual/manual_qfield_v0.4.1.html) · [PDF](manual/manual_qfield_v0.4.1.pdf). Ambos são arquivos versionados para disponibilização junto ao script no GitHub, sem executar R para gerar o manual.

## Início

1. Crie somente `entrada/` ao lado do arquivo R. Subpastas são lidas recursivamente; todas as pastas de saída são automáticas.
2. Coloque na entrada pelo menos uma camada de polígonos identificada como `areas_elegiveis`.
3. Edite o bloco `MQ_CONFIG`, perto do início do script, com caminhos relativos à pasta do script ou absolutos. No Windows, prefira `/` nos caminhos.
4. Instale, caso faltem: `sf`, `terra`, `xml2`, `zip`, `jsonlite`, `digest`, `httr`, `data.table`, `DBI`, `RSQLite`, `cli`, `curl`, `png`, `jpeg`, `lpSolve`.
5. Abra o script no RStudio e use **Source**. A conexão à internet é necessária para a consulta oficial às UCs federais. O MapBiomas também usa internet quando ativado.
6. Consulte o relatório. Importe `01_qfield/pacote_qfield.zip` em uma pasta nova do QField e confira em modo avião.

Para usar como biblioteca: `options(monitora.qfield.somente_funcoes=TRUE)` antes de `source()`, e depois `monitora_criar_qfield(config)`. O script não instala pacotes, envia dados ou publica projetos automaticamente.

## Parâmetros

| Parâmetro | Padrão / regra |
|---|---|
| `entrada`, `saida`, `projeto` | Pastas separadas; título livre; cada execução cria saída própria |
| `modo` | `auto`, `planejar`, `montar`, `expandir` |
| `transecto_m` | 50 m |
| `distancia_min_m` | 100 m, referente a UAs/segmentos; não é garantia do espaçamento de PAs |
| `deslocamento_max_m` | 10 m; regra operacional, não desloca o PA automaticamente |
| `grade_m` | `c(156.25,156.25)`; vértices, não centros |
| `epsg` | `NULL`: SIRGAS2000/UTM adequada; área extensa/multifuso exige configuração explícita |
| `prioritarios` | `list(n=NULL,percentual=20)` |
| `alternativos` | `list(n=NULL,percentual=NULL)` significa dobro dos prioritários |
| `semente` | 20261001; seleção determinística por hash da semente e identidade da célula |
| `mapbiomas` | TRUE; erro não obrigatório fica explícito nos atributos/relatório |
| `mb_produto`, `mb_colecao`, `mb_ano` | `30m`, `11`, `2025`; também homologado `10m`, `4`, anos 2017–2025 |
| `mb_obrigatorio` | FALSE; TRUE bloqueia saída quando extração falha |
| `referencia_anterior` | Pasta `02_relatorio` anterior, obrigatória em `expandir` |
| `max_pontos` | 500000; proteção de volume, sem redução silenciosa |

Informe número **ou** percentual, nunca ambos. Exemplo: `list(n=40,percentual=NULL)`. Percentuais são arredondados para cima, sobre todos os vértices únicos cobertos pela união das AEs (incluindo fronteiras e excluindo interiores de buracos). Cotas inviáveis interrompem a execução; não são reduzidas. Zero explícito é permitido.

`auto` escolhe `montar` se encontrar grade, PAs, UAs, extremos ou transectos fornecidos. `montar` não cria pontos nem completa cotas. `planejar` rejeita essas camadas para impedir substituição acidental. Para adicionar UAs observadas a um planejamento gerado, faça uma montagem posterior com as camadas resultantes e as referências observadas.

## Imagens, confirmação e cache

`baixar_imagem_detalhe=TRUE` ativa o planejamento de aquisição. `confirmar_download=NULL` pergunta antes de baixar detalhe; `confirmar_sentinel=NULL` pergunta separadamente antes de obter Sentinel. `FALSE` recusa novas imagens daquele tipo; `TRUE` representa autorização explícita. Sem interação, uma confirmação necessária e não informada bloqueia a etapa. A falta de Sentinel offline válido bloqueia o pacote; a recusa de detalhe permite o fundo mínimo com a lacuna registrada.

`centros_detalhe="auto"`: sem UAs, raios nos PAs prioritários e alternativos; com UAs, raios nos pontos médios de `verg_ini`/`verg_fin` pareados por identificador e ano. Não inclui toda a grade. Use `"UAs_e_PAs"` explicitamente para cobrir ambos. Somente uma camada genérica UAs sem extremos pareados não permite inventar pontos médios.

Os níveis padrão são 16–18 e a fonte segue o script de download fornecido. Console e relatório informam tiles locais, em cache, faltantes, volume aproximado, tempo mínimo e progresso. Os tempos dependem de rede e processamento. `cache_dir` persiste fora das saídas; `caches_adicionais` permite importar acervos compatíveis. Tiles concluídos, recortes, Sentinel e resultados MapBiomas válidos são reaproveitados; falhas não são armazenadas como sucesso. Desativar download ainda permite aproveitar imagens já disponíveis. `renovar_imagens=TRUE` cria outra versão do acervo e pode exigir nova transferência. Preserve o cache entre execuções.

Sentinel é verificado por proveniência e cobertura de pixels válidos em todo o contexto da UC/AE, com margem de 500 m. Isso não comprova ausência de nuvens. A consulta pode encerrar com insuficiência de cenas; nesse caso forneça um Sentinel documentado compatível, sem renomear outro sensor como Sentinel. Detalhe parcial nunca é apresentado como cobertura completa. Google Satellite inicia desmarcado, para uso opcional com conexão.

## Arquivos e papéis

Formatos vetoriais: KML, KMZ, GeoPackage, shapefile completo diretamente ou em ZIP. O ZIP precisa conter SHP/SHX/DBF/PRJ e pode conter seus auxiliares. KMZ deve conter KML e imagens locais permitidas; referências externas não são executadas. Geometrias inválidas, CRS ausente, tipos mistos incompatíveis e perdas de feições bloqueiam a importação.

Fundos: MBTiles raster e rasters georreferenciados TIF/TIFF, IMG e ASC. MBTiles vetorial não é aceito como imagem. Formatos com arquivos auxiliares devem ser fornecidos completos; prefira GeoTIFF autossuficiente. MBTiles de detalhe são recortados em raios de 500 m, preservando os originais. A camada resultante chama-se `sat_escala_local`. Google Satellite online e Sentinel offline compõem o fundo mínimo obrigatório. Downloads exigem confirmação e os caches compatíveis são reaproveitados. Raster não é uma extensão universal.

Nomes padronizados (arquivo sem extensão ou camada interna):

- `areas_elegiveis`: polígonos obrigatórios; exibidos como `AE`.
- `limites_uc`: limite fornecido de referência, exibido como `UC_forn`; não substitui a consulta federal.
- `grade_amostral`, `PA_priorit`, `PA_altern`: pontos preexistentes.
- `verg_ini`, `verg_fin`, `UAs`: pontos observados.
- `transectos`: segmentos de UAs observadas; não usar para caminhos de acesso.
- `formacao_florestal`, `rodovias`, `estradas`, `trilhas`: referências para diagnóstico de distâncias, quando fornecidas.
- `acessos`: linhas de referência, sem assumir automaticamente o tipo de via.
- `pontos_interesse.gpkg` e `trajeto.gpkg` (ou o legado `apoio_campo.gpkg`): `pontos_interesse` (POINT, campo `ponto_interesse`) e `trajeto` (MULTILINESTRING, campo `trajeto`), ambos com `obs` texto, `data_hora` data/hora e identificador `fid`. São as camadas editáveis no projeto de campo. Campos adicionais fornecidos são preservados.

Para nomes diferentes, forneça `camadas_qfield.csv` com `arquivo;camada;papel` e, opcionalmente, `nome;campo_rotulo;ano`. Um registro por camada. Exemplo:

```csv
arquivo;camada;papel;nome;campo_rotulo;ano
poligonos.gpkg;PN_AE;areas_elegiveis;AE;;
planejamento.gpkg;selecionados;PA_priorit;PA_priorit;PA;
observacoes.gpkg;inicio_2025;verg_ini;verg_ini_2025;UA;2025
vias.gpkg;acesso_norte;acessos;acessos;nome;
```

Não inferir Áreas Elegíveis de polígonos genéricos. Não chamar pontos planejados de UAs. PAs fornecidos exigem rótulos não vazios e únicos; a mesma identificação não pode estar nas duas categorias. `id_grade` ausente é registrado, nunca inventado. Identificadores, atributos e geometria de origem permanecem; transformações de CRS são representações derivadas. Campos derivados `qf_*` são conferidos ao reimportar. Atributos MapBiomas completos preexistentes são preservados, sem reclassificação silenciosa.

Nomes exibidos têm no máximo 26 caracteres, são únicos sem distinguir maiúsculas e preservam exatamente `PA_priorit`, `PA_altern`, `grade_amostral`. Use o ano de observação nos extremos, nunca o ano de geração para preencher um ano desconhecido.

## Grade, expansão e UCs

O serviço WFS federal é consultado em toda execução. As páginas e contagens são verificadas, e depois ocorre interseção geométrica. Falha não significa ausência de UC. O limite completo define a origem da malha; as AEs selecionam os vértices. Nas partes externas utiliza-se a origem da extensão inicial das AEs. Sobreposição positiva de UCs sobre AE exige associação explícita e bloqueia a geração automática.

`referencia_grade.json` e `cadastro_grade.gpkg` preservam origem, índices, limites de referência e IDs. `expandir` mantém projeção, dimensões e seleções existentes, acrescentando IDs sem reciclagem. A consulta territorial atual é registrada, mas atualização do limite de uma UC conhecida não desloca sua malha. Nova UC alcançada ganha referência própria apenas na área nunca processada. O domínio histórico e a malha dos pontos existentes são preservados. Alterar resolução/projeção não é expansão: exige projeto de referência novo. Guarde a pasta inteira `02_relatorio`.

## Protocolo e limites dos diagnósticos

PA = início previsto. O roteiro de 29/04/2026 orienta tentar Norte, Leste, Sul e Oeste, com ajuste de até 10 m quando necessário; instalação inviável leva ao alternativo mais próximo. A simulação usa norte geográfico local e comprimento métrico, sem deslocar o PA. Não afirma que ocorreu instalação.

Os mínimos se aplicam a todo o segmento: floresta e rodovia 100 m; estrada de terra 50 m; trilha 5 m; outra UA conforme `distancia_min_m`. Diagnósticos usam somente geometrias fornecidas e não transformam ausência de camada em ausência de obstáculo. MapBiomas não prova distâncias a floresta nem homogeneidade da transecção. Mudança de formação, obstáculos e relevo precisam ser avaliados em campo. As simulações não excluem vértices, não alteram cotas e não tratam todos os alternativos como UAs simultâneas. Acima de 20 mil PAs as simulações são explicitamente não executadas; a grade continua íntegra.

`alternativos_proximos.csv` usa distância plana do PA prioritário ao PA alternativo, sem comprovar viabilidade. O ponto de partida real do monitor e o terreno podem mudar a opção de navegação. A posição realizada deve ser registrada na UA, mantendo o vínculo ao PA original quando conhecido.

## MapBiomas

Extração do pixel que contém o ponto, sem interpolação de códigos. Campos: `mb_codigo`, `mb_classe`, `mb_formacao`, produto, coleção, ano, resolução, fonte e status. `mb_formacao` só contém classes oficialmente denominadas Formação; outras classes não são forçadas para Campestre/Savânica. Trata-se de classificação cartográfica, separada da observação de campo.

O acesso parcial usa GDAL/terra; a legenda, URL e identificador remoto ETag são registrados. Não mistura 10 m/30 m nem troca coleção como fallback silencioso. Os atributos ficam offline no projeto. Nenhum raster nacional completo é incluído no pacote.

## Saídas e validação

`01_qfield`: QGS, dados, mapas e ZIP portátil. `02_relatorio`: relatório HTML, mapa geral, parâmetros, fontes/checksums, etapas/duração, referência da grade e diagnósticos. `03_vetores`: GPKG/KML/KMZ dos pontos. `04_csv`: todos os atributos dos pontos, coordenadas e dicionário. CSV usa UTF-8 BOM, `;` e ponto decimal; valores ausentes são vazios. KML conserva atributos como texto; GeoPackage conserva os tipos. Os nomes físicos têm índice para evitar colisões.

O projeto exibe coordenadas geográficas WGS84 em graus decimais, seis casas, e processa geometria em CRS métrico. Confirme a exibição em identificação/posicionamento no QField. Trajetos não possuem uma única coordenada: use os vértices e a posição GNSS correspondente.

O relatório distingue geração validada automaticamente de homologação móvel. Cobertura de imagens é verificada por pixel válido nos pontos, com transparência considerada; não prova cobertura do trajeto inteiro, resolução nativa ou ausência de nuvens. Projetos com lacunas permanecem identificados como cobertura parcial. Preserve o apoio preenchido antes de atualizar a pasta do projeto. A ferramenta nunca envia ao QFieldCloud automaticamente.

## Testes

No diretório raiz do repositório: `Rscript --vanilla qfield/tests/test_core.R` e `Rscript --vanilla qfield/tests/test_imagery.R`. Ensaios institucionais são locais e não distribuem bases biológicas. O script é derivado dos leitores espaciais da versão pública v3.0.6, com fonte identificada no cabeçalho.

## Publicação dos manuais

Os dois arquivos prontos em `manual/` devem acompanhar a publicação do script e os assets da release. O GitHub exibe HTML como código na página do arquivo; o usuário pode baixar e abrir o HTML autossuficiente. Para um link HTML navegável, publicar essa pasta no GitHub Pages e atualizar o link do README junto à release. Essa ativação ainda não foi feita; os links atuais apontam aos arquivos entregues. O PDF já pode ser visualizado diretamente pelo GitHub após a publicação.

## Cotas cumulativas (v0.4.1)

Vegetação habilitada por padrão; florestal e atributos adicionais desabilitados. Informe `formacao_campo`/`formacao_mapa` para classificação local ou use MapBiomas conservador. Campos rupestres e classes ambíguas exigem validação local. `cotas_formacao` recebe classe + n OU percentual. `cotas_atributos` é uma lista nomeada por campo, com uma tabela classe + n OU percentual por atributo. As margens são atendidas simultaneamente, não substituídas nem multiplicadas como se fossem independentes.

O solver escolhe arredondamentos conjuntamente, mantém capacidades e PAs preservados e bloqueia conflitos antes das imagens. Alternativos padrão: dobro por combinação. Cotas personalizadas de alternativos acompanham as margens dos prioritários; veja as diferenças no manual, seções 12–13. Na montagem, apenas audita as cotas e preserva campos fornecidos; na expansão, verifica o contrato histórico. Consulte CSVs de cotas, combinações e o JSON de solução em `02_relatorio`.

## Revisão após teste no celular (v0.4.1)

- `codigo_pa`: PA com ao menos cinco algarismos; `id_malha` calculado antes do recorte; originais e correspondência preservados ao migrar.
- Um MBTiles de detalhe, `sat_escala_local`, com alpha e prioridade das fontes; `sat_escala_regional` é o Sentinel offline.
- Um GPKG por camada em `01_qfield/dados`; apoio antigo agrupado continua legível.
- `05_qgis/projeto_edicao.qgz`: vetores de trabalho separados, quatro temas/marcadores/layouts editáveis.
- `mapas_pdf` e `mapas_png`: AEs, grade, prioritários e alternativos. PDFs georreferenciados, PNG com PGW/PRJ.
- `gerar_cartografia=TRUE`, `qgis_python=NULL`, `mapas_papel='A4'`, `mapas_dpi=300`, `elaboracao='Programa Monitora'`.

Abra `ABRA_AQUI.html` na entrega. Edite no produto QGIS; não há sincronização automática dessas edições com o ZIP de campo ou exportações anteriores.

Na revisão cartográfica v0.4.1, o QGIS e as legendas usam nomes completos. UCs são identificadas pelos atributos oficiais ICMBio; o localizador de UC só aparece com interseção federal. Estados e biomas usam IBGE 2025, com cache persistente (~27 MiB na primeira obtenção), e são entregues em `05_qgis/contexto`. A localização de estados considera geometria original; a simplificação serve somente ao desenho regional.
