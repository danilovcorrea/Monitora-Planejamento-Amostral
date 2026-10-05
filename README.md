# Monitora Campestre Savânico - Alvo Global - Planejamento e Desenho Amostral, Cartografia e navegação em campo

**Versão 1.0.2 · script R independente · Programa Monitora**

A versão 1.0.2 oferece compatibilidade com QGIS 3.44 e 4, verifica a cartografia antes dos downloads e explica os valores efetivamente usados e as variáveis a revisar. Reaproveita imagens, recortes e mosaicos; combina fontes Sentinel parciais e adquire somente lacunas autorizadas. Homologada localmente no RStudio pelo responsável pelo projeto.

Ferramenta para planejar o desenho amostral a partir de Áreas Elegíveis, organizar dados espaciais, criar projetos editáveis no QGIS e projetos de navegação no QField, e exportar mapas, vetores, tabelas e relatórios. Inclui os perfis Campestre-Savânico, Ilha e personalizado, com seus critérios próprios.

**[Ler o manual do usuário em HTML](https://danilovcorrea.github.io/Monitora-Planejamento-Amostral/)** · **[Baixar o manual em PDF](https://danilovcorrea.github.io/Monitora-Planejamento-Amostral/manual_usuario.pdf)**

**[Baixar o script R](https://github.com/danilovcorrea/Monitora-Planejamento-Amostral/releases/latest/download/monitora_planejamento_amostral.R)** · [Versão completa e arquivos de verificação](https://github.com/danilovcorrea/Monitora-Planejamento-Amostral/releases/latest)

O manual já está pronto para consulta; não é necessário executar R. O script distribuído é autossuficiente e incorpora os auxiliares cartográficos e recursos. Requer os pacotes R indicados abaixo e QGIS/PyQGIS para gerar os layouts e mapas. A instalação efetiva de UAs depende da avaliação em campo e dos protocolos aplicáveis.

Esta ferramenta deriva da versão homologada 0.4.7 do módulo independente desenvolvido no [Monitora-Campestre-Savanico](https://github.com/danilovcorrea/Monitora-Campestre-Savanico). Este é o repositório canônico do planejamento; o repositório original mantém o tratamento e as análises dos dados biológicos. Consulte [homologação](HOMOLOGACAO.md) e [proveniência](validacao/proveniencia.json).

## Início

1. Crie somente `entrada/` ao lado do arquivo R. Subpastas são lidas recursivamente; todas as pastas de saída são automáticas.
2. Coloque na entrada pelo menos uma camada de polígonos identificada como `areas_elegiveis`.
3. Edite o bloco `MQ_CONFIG`, perto do início do script, com caminhos relativos à pasta do script ou absolutos. No Windows, prefira `/` nos caminhos.
4. O script verifica, instala quando ausentes e carrega: `sf`, `terra`, `xml2`, `zip`, `jsonlite`, `digest`, `httr`, `data.table`, `DBI`, `RSQLite`, `cli`, `curl`, `png`, `jpeg`, `lpSolve`.
5. Abra o script no RStudio e use **Source**. A conexão à internet é necessária para a consulta oficial às UCs federais. O MapBiomas também usa internet quando ativado.
6. Consulte o relatório. Importe `01_qfield/pacote_qfield.zip` em uma pasta nova do QField e confira em modo avião.

Para usar como biblioteca: `options(monitora.qfield.somente_funcoes=TRUE)` antes de `source()`, e depois `monitora_planejamento_amostral(config)` (o nome anterior `monitora_criar_qfield` permanece como alias compatível). Ao executar a função principal, o script prepara os pacotes automaticamente; carregar somente as funções com a opção acima não instala nem anexa pacotes. Usa os repositórios configurados no R, com CRAN HTTPS quando o espelho não foi definido. Pacotes já disponíveis não são reinstalados. Falhas de instalação ou carregamento interrompem a execução com orientação no console. O script não publica projetos automaticamente.

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

Informe número **ou** percentual, nunca ambos. Exemplo: `list(n=40,percentual=NULL)`. Percentuais são arredondados para cima, sobre todos os vértices únicos cobertos pela união das AEs (incluindo fronteiras e excluindo interiores de buracos). O padrão `politica_insuficiencia="usar_disponiveis"` adapta os quantitativos à capacidade e registra déficits. A opção `"bloquear"` exige o atendimento estrito das cotas. Zero explícito é permitido.

`auto` escolhe `montar` se encontrar grade, PAs, UAs, extremos ou transectos fornecidos. `montar` não cria pontos nem completa cotas. `planejar` rejeita essas camadas para impedir substituição acidental. Para adicionar UAs observadas a um planejamento gerado, faça uma montagem posterior com as camadas resultantes e as referências observadas.

## Configuração efetiva e QGIS

Com várias instalações QGIS, a seleção automática testa as versões da mais recente para a mais antiga. O teste inclui fontes, legendas, escala, localizadores, PDF georreferenciado, PNG e reabertura do layout, antes da aquisição de imagens. Informe `qgis_python` para fixar uma instalação; nesse caso ela não será substituída silenciosamente.

`configuracao_efetiva.csv` mostra valor informado, valor utilizado e orientação. Para alterar o espaçamento: `grade_m` no perfil Campestre-Savânico; `parametros_protocolo$grade_m` no personalizado; `padrao_ilha$grade_m` no Ilha, salvo sobrescrita em `parametros_protocolo$grade_m`. Uma grade vazia informa área, envelope, origem, espaçamento e a variável a revisar. O script não altera a malha automaticamente.

## Imagens, confirmação e cache

Preserve `cache_dir` e mantenha `renovar_imagens=FALSE`. Execuções anteriores na mesma saída e `caches_adicionais` ajudam a recuperar fontes compatíveis. Tiles, recortes e mosaicos íntegros são reaproveitados. Fontes Sentinel parciais são combinadas e as cenas recebidas ficam preservadas antes da montagem final; somente lacunas autorizadas são adquiridas. Blocos COG podem incluir pixels vizinhos. Os relatórios registram reutilização, faltantes e fontes rejeitadas.

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

Os mínimos se aplicam a todo o segmento: floresta e rodovia 100 m; estrada de terra 50 m; trilha 5 m; outra UA conforme `distancia_min_m`. Diagnósticos usam somente geometrias fornecidas e não transformam ausência de camada em ausência de obstáculo. MapBiomas não prova distâncias a floresta nem homogeneidade da transecção. Mudança de formação, obstáculos e relevo precisam ser avaliados em campo. Os diagnósticos posteriores à seleção não alteram cotas nem tratam todos os alternativos como UAs simultâneas. As restrições viárias habilitadas são aplicadas antes da seleção e reduzem os candidatos aptos; a grade e o denominador são preservados. Acima de 20 mil PAs as simulações são explicitamente não executadas; a grade continua íntegra.

`alternativos_proximos.csv` usa distância plana do PA prioritário ao PA alternativo, sem comprovar viabilidade. O ponto de partida real do monitor e o terreno podem mudar a opção de navegação. A posição realizada deve ser registrada na UA, mantendo o vínculo ao PA original quando conhecido.

## MapBiomas

Extração do pixel que contém o ponto, sem interpolação de códigos. Campos: `mb_codigo`, `mb_classe`, `mb_formacao`, produto, coleção, ano, resolução, fonte e status. `mb_formacao` só contém classes oficialmente denominadas Formação; outras classes não são forçadas para Campestre/Savânica. Trata-se de classificação cartográfica, separada da observação de campo.

O acesso parcial usa GDAL/terra; a legenda, URL e identificador remoto ETag são registrados. Não mistura 10 m/30 m nem troca coleção como fallback silencioso. Os atributos ficam offline no projeto. Nenhum raster nacional completo é incluído no pacote.

## Saídas e validação

`01_qfield`: QGS, dados, mapas e ZIP portátil. `02_relatorio`: relatório HTML, mapa geral, parâmetros, fontes/checksums, etapas/duração, referência da grade e diagnósticos. `03_vetores`: GPKG/KML/KMZ dos pontos. `04_csv`: todos os atributos dos pontos, coordenadas e dicionário. CSV usa UTF-8 BOM, `;` e ponto decimal; valores ausentes são vazios. KML conserva atributos como texto; GeoPackage conserva os tipos. Os nomes físicos têm índice para evitar colisões.

O projeto exibe coordenadas geográficas WGS84 em graus decimais, seis casas, e processa geometria em CRS métrico. Confirme a exibição em identificação/posicionamento no QField. Trajetos não possuem uma única coordenada: use os vértices e a posição GNSS correspondente.

O relatório distingue geração validada automaticamente de homologação móvel. Cobertura de imagens é verificada por pixel válido nos pontos, com transparência considerada; não prova cobertura do trajeto inteiro, resolução nativa ou ausência de nuvens. Projetos com lacunas permanecem identificados como cobertura parcial. Preserve o apoio preenchido antes de atualizar a pasta do projeto. A ferramenta nunca envia ao QFieldCloud automaticamente.

## Testes

No diretório raiz do repositório: `Rscript --vanilla tests/test_core.R` e `Rscript --vanilla tests/test_imagery.R`. Ensaios institucionais são locais e não distribuem bases biológicas. O script é derivado dos leitores espaciais da versão pública v3.0.6, com fonte identificada no cabeçalho.

## Documentação e manutenção

O [manual HTML](https://danilovcorrea.github.io/Monitora-Planejamento-Amostral/) é servido pelo GitHub Pages; o [PDF](https://danilovcorrea.github.io/Monitora-Planejamento-Amostral/manual_usuario.pdf) corresponde à mesma versão. Fontes editoriais em `manual/`; páginas prontas em `docs/`. Para reconstruir: `python3 manual/build_manual.py`. Para incorporar alterações do renderizador e helpers ao R: `python3 cartografia/build_embedded.py`.

Os testes sintéticos em `tests/test_*.R` usam dados temporários. Os testes PyQGIS de homologação recebem caminhos de produtos locais por argumento; os dados de campo não são distribuídos. Consulte `tests/README.md`.

## Cotas cumulativas (v1.0.0)

Vegetação habilitada por padrão. Campestre-Savânico aceita campos e savanas; Ilha inclui automaticamente floresta (inclusive 100% florestal). `incluir_antropizadas=TRUE` permite pastagem (MapBiomas 15) e degradação declarada no vetor, sempre com ocorrências. Não se infere degradação da classe 25. Classes ambíguas ficam pendentes e seus pontos são excluídos, sem bloquear os demais. Vetores inconsistentes continuam exigindo correção.

`politica_insuficiencia="usar_disponiveis"` é o padrão. Prioriza o quantitativo de prioritários; alternativos chegam até o disponível. Cotas por vegetação e atributos são metas cumulativas conjuntas, com desvios registrados quando a capacidade não permite cumpri-las. Zero explícito continua exclusão; históricos e restrições espaciais são preservados. `"bloquear"` mantém o desenho estrito. Sem cotas explícitas, a preferência é usar nativas primeiro nos prioritários e depois nos alternativos, completando com antropizadas. O denominador é toda a grade recortada às AEs.

`cotas_formacao` recebe classe + n OU percentual. `cotas_atributos` recebe uma lista nomeada por campo. Consulte `selecao_quantidades.csv`, `cotas_realizadas.csv`, `ocorrencias_vegetacao.csv` e o relatório HTML. Quantidades realizadas não equivalem à instalação de UAs nem à conformidade do protocolo. Montagem preserva pontos fornecidos; expansão não migra silenciosamente contratos históricos.

## Revisão após teste no celular (v1.0.0)

- `codigo_pa`: PA com ao menos cinco algarismos; `id_malha` calculado antes do recorte; originais e correspondência preservados ao migrar.
- Um MBTiles de detalhe, `sat_escala_local`, com alpha e prioridade das fontes; `sat_escala_regional` é o Sentinel offline.
- Um GPKG por camada em `01_qfield/dados`; apoio antigo agrupado continua legível.
- `05_qgis/projeto_edicao.qgz`: vetores de trabalho separados, quatro temas/marcadores/layouts editáveis.
- `mapas_pdf` e `mapas_png`: AEs, grade, prioritários e alternativos. PDFs georreferenciados, PNG com PGW/PRJ.
- `gerar_cartografia=TRUE`, `qgis_python=NULL`, `mapas_papel='A4'`, `mapas_dpi=300`, `elaboracao='Programa Monitora'`.

Abra `ABRA_AQUI.html` na entrega. Edite no produto QGIS; não há sincronização automática dessas edições com o ZIP de campo ou exportações anteriores.

Na revisão cartográfica v1.0.0, o QGIS e as legendas usam nomes completos. UCs são identificadas pelos atributos oficiais ICMBio; o localizador de UC só aparece com interseção federal. Estados e biomas usam IBGE 2025, com cache persistente (~27 MiB na primeira obtenção), e são entregues em `05_qgis/contexto`. A localização de estados considera geometria original; a simplificação serve somente ao desenho regional.

## Restrições viárias e protocolo (v1.0.0)

Use arquivos/camadas `estradas_pavimentadas`, `estradas_terra` e `trilhas_preexistentes`. Flags `usar_*`: NULL detecta, TRUE exige, FALSE somente exibe. No perfil campestre, candidatos precisam admitir um segmento N/L/S/O a pelo menos 100/50/5 m das fontes habilitadas. A grade e o denominador percentual são preservados; as cotas usam apenas candidatos disponíveis. Linhas usam eixo + metade de `largura_m` quando informada; polígonos usam a borda. Nomes antigos exigem mapeamento explícito.

O perfil Ilha não aplica os afastamentos campestres. Com `parametros_protocolo=NULL`, usa `padrao_ilha`: transecto 25 m, grade 30 × 30 m; referência experimental de 30 m entre linhas e tentativas N/L/S/O do piloto Noronha. Essas referências não são regras definitivas nem uma verificação automática da implantação. A lista `parametros_protocolo` permite sobrescrever os valores. O perfil personalizado exige também direções e distâncias próprias. Vias são auditadas em `restricoes_viarias.json` e CSVs de pontos/direções; ausência de vetor não equivale à ausência de obstáculo.

Legendas dos localizadores acompanham os biomas efetivamente representados; fragmentos abaixo de `localizador_bioma_min_mm2` (padrão 0,5 mm² no papel) são suprimidos somente nessa representação.

## Área pequena / seleção personalizada

`perfil` define o protocolo; `modo` define planejar/montar/expandir. A política adaptável vale para todos os perfis e tamanhos de AE, dentro ou fora de UC. `estratificar_vegetacao=FALSE` desliga cotas por formação; nos perfis campestre/ilha, mantém o filtro de elegibilidade. No personalizado, desliga também esse filtro: MapBiomas fica informativo, e toda a AE pode participar. Falta parcial reduz o atendimento e gera ocorrências; ausência total de candidatos elegíveis conhecidos bloqueia. Dados inválidos, falhas de fontes obrigatórias e conflitos históricos não são tratados como simples falta de capacidade.

Exemplo sem regras viárias: `parametros_protocolo=list(transecto_m=50,grade_m=c(100,100),direcoes=character(),distancias_viarias_m=numeric())`. Não equivale a cumprir o procedimento campestre. A grade/IDs e as restrições viárias explicitamente ativadas permanecem preservados.

## Piloto Ilha — Noronha (v1.0.0)

O padrão dimensional Ilha foi baseado no pré-projeto Noronha de setembro/2026. Quantidades (60/120), alocação por AE, prioridades, sequência de consulta e quatro pontos complementares são específicos do piloto: use montar para preservá-los. Não se reproduzem apenas com uma semente ou percentuais globais. Categorias restaurada/degradada são condições de manejo; não equivalem automaticamente a formações vegetacionais.

`contexto_uc="componentes_com_AE"` inclui componentes completos das UCs que intersectam as AEs no Sentinel e no localizador de UC; não recorta por 500 m. Os limites oficiais completos continuam disponíveis no projeto e definem a origem da grade quando ela é criada. `"integral"` inclui também setores remotos, podendo exigir volume muito maior. Veja contexto_uc.json e contexto_imagens.gpkg.

Camadas de aves do piloto são referências de planejamento, não UAs instaladas de plantas. Nomes: aves_transectos, aves_Pha_lep, aves_Sul_dac, aves_Sul_sul, AE_com_aves e AE_comuns. O manifesto identifica seus papéis como adicional. grid_id fornecido liga os códigos de exibição entre grade e PAs, preservando os identificadores originais.

Na consulta de homologação Noronha, MapBiomas não forneceu classes válidas nos PAs. A montagem preserva o planejamento fornecido; para planejar novos pontos, forneça classificação local confirmada. Pendências são registradas por ponto, sem apagar classificações válidas do lote.

## Autoria, licença e citação

Autoria e coordenação: **Danilo V. Corrêa**. Código sob [GNU GPL v3](LICENSE), preservada do projeto de origem. As fontes e condições de uso das bases cartográficas e imagens são próprias dos respectivos provedores; a licença do código não as substitui. Marcas institucionais são mantidas como identificação de origem.

Citação sugerida: CORRÊA, Danilo V. *Monitora Campestre Savânico - Alvo Global - Planejamento e Desenho Amostral, Cartografia e navegação em campo*. Versão 1.0.1. GitHub, 2026. https://github.com/danilovcorrea/Monitora-Planejamento-Amostral/releases/tag/v1.0.1.
