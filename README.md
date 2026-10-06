# Monitora Campestre Savânico - Alvo Global - Planejamento e Desenho Amostral, Cartografia e navegação em campo

**Versão 1.1.0 · script R independente · Programa Monitora**

Planejamento do desenho amostral, projetos QGIS/QField, mapas, vetores, tabelas e relatórios. A versão 1.1.0 acrescenta seis perfis de finalidade, configuração unificada, incremento preservando pontos históricos e decisões metodológicas registradas. Mantém compatibilidade com configurações legadas, QGIS 3/4 e reaproveitamento de imagens/cache.

**[Manual do usuário em HTML](https://danilovcorrea.github.io/Monitora-Planejamento-Amostral/)** · **[Manual PDF](https://danilovcorrea.github.io/Monitora-Planejamento-Amostral/manual_usuario.pdf)**

**[Baixar script R](https://github.com/danilovcorrea/Monitora-Planejamento-Amostral/releases/latest/download/monitora_planejamento_amostral.R)** · [Versão completa e verificações](https://github.com/danilovcorrea/Monitora-Planejamento-Amostral/releases/latest)

O manual está pronto para consulta sem executar R. O script é autossuficiente, incluindo auxiliares cartográficos e recursos. A instalação das UAs depende dos procedimentos e da avaliação em campo; escolher um perfil não certifica conformidade integral.

Este é o repositório canônico da ferramenta de planejamento, derivada do [Monitora-Campestre-Savanico](https://github.com/danilovcorrea/Monitora-Campestre-Savanico), que mantém o tratamento e as análises de dados biológicos. Consulte [homologação](HOMOLOGACAO.md) e [proveniência](validacao/proveniencia.json).

## Início

1. Crie somente `entrada/` ao lado do arquivo R. Subpastas são lidas recursivamente; todas as pastas de saída são automáticas.
2. Coloque na entrada pelo menos uma camada de polígonos identificada como `areas_elegiveis`.
3. Edite o bloco `MQ_CONFIG`, perto do início do script, com caminhos relativos à pasta do script ou absolutos. No Windows, prefira `/` nos caminhos.
4. O script verifica, instala quando ausentes e carrega: `sf`, `terra`, `xml2`, `zip`, `jsonlite`, `digest`, `httr`, `data.table`, `DBI`, `RSQLite`, `cli`, `curl`, `png`, `jpeg`, `lpSolve`.
5. Abra o script no RStudio e use **Source**. A conexão à internet é necessária para a consulta oficial às UCs federais. O MapBiomas também usa internet quando ativado.
6. Consulte o relatório. Importe `01_qfield/pacote_qfield.zip` em uma pasta nova do QField e confira em modo avião.

Para usar como biblioteca: `options(monitora.qfield.somente_funcoes=TRUE)` antes de `source()`, e depois `monitora_planejamento_amostral(config)` (o nome anterior `monitora_criar_qfield` permanece como alias compatível). Ao executar a função principal, o script prepara os pacotes automaticamente; carregar somente as funções com a opção acima não instala nem anexa pacotes. Usa os repositórios configurados no R, com CRAN HTTPS quando o espelho não foi definido. Pacotes já disponíveis não são reinstalados. Falhas de instalação ou carregamento interrompem a execução com orientação no console. O script não publica projetos automaticamente.

## Perfis e configuração

| `perfil` | Referências iniciais |
|---|---|
| `treinamento_navegacao` | Qualquer cobertura; sem filtros, cotas ou afastamentos; grade 50 m |
| `treinamento_campestre` | Referências campestres; grade 156,25 m, transecto 50 m; adensamento com aviso |
| `treinamento_ilha` | Referências experimentais do piloto; grade 30 m, transecto 25 m |
| `monitoramento_campestre` | Padrão inicial; referências campestres e critérios ativados |
| `monitoramento_ilha` | Protocolo em construção; inclui floresta por padrão |
| `personalizado` | Controles restritivos/condicionais desligados; ativação individual |

Em todos os perfis da configuração 2: `NULL` herda o padrão; `TRUE` ativa; `FALSE` desativa; valor explícito prevalece. Use os mesmos campos `grade_m`, `transecto_m`, `distancia_min_m` e `deslocamento_max_m`, independentemente do perfil. A grade define candidatos, não a implantação simultânea de UAs.

| `operacao` | Uso |
|---|---|
| `planejar` | Criar grade e novos PAs; UAs existentes podem entrar como referência |
| `montar` | Preservar pontos fornecidos e organizar os produtos |
| `incrementar` | Acrescentar PAs na grade/área existente; exige `referencia_anterior` |
| `expandir` | Ampliar cobertura preservando origem, espaçamento e IDs; exige `referencia_anterior` |

Em incrementar, `quantidade_incremento="novos"` soma ao histórico; `"total"` define o total desejado. Em expandir as quantidades são totais. Para mudar espaçamento/origem, faça outro planejamento sem mover o histórico.

`prioritarios=list(n=NULL,percentual=20)` usa 20% dos vértices da grade cortada pela união das AEs; `alternativos=list(n=NULL,percentual=NULL)` solicita o dobro. Informe n OU percentual. Em incrementar, percentuais de novos pontos usam a grade inteira das AEs atuais; cotas incidem no total final, histórico + novos.

Exemplo para treinamento urbano (substitua os campos no bloco inicial):

```r
perfil = 'treinamento_navegacao',
operacao = 'planejar',
grade_m = c(10,10),
transecto_m = 5,
prioritarios = list(n=20,percentual=NULL),
alternativos = list(n=0,percentual=NULL)
```

## Filtros, cotas e decisões

`filtrar_vegetacao` controla elegibilidade; `aplicar_cotas` distribui por formação/fitofisionomia; `estratificar_por_atributos` acrescenta cotas cumulativas no MESMO conjunto. `mapbiomas` consulta atributos. Os controles são independentes. Campo/savana são o padrão campestre; floresta é incluída por padrão no Ilha, mas `incluir_formacao_florestal` é respeitado em qualquer perfil.

`condicao_campo` registra conservada, degradada, restauracao ou restaurada separadamente da formação. `incluir_antropizadas` permite pastagens e condições declaradas de degradação/restauração, sem convertê-las em vegetação nativa. Use cotas de condição/manejo quando o objetivo exigir esforço específico nesses contextos.

`cotas_formacao` recebe classe + n OU percentual; `cotas_atributos` é uma lista por campo. Margens são resolvidas conjuntamente. Sem cotas explícitas, o algoritmo balanceia nativas e permite complemento por antropizadas. Esse padrão não é obrigação universal do protocolo nem prova representatividade da UC.

`politica_insuficiencia="usar_disponiveis"` propõe quantidades viáveis; déficits exigem decisão. `"bloquear"` tenta metas exatas e permite rever inviabilidade comprovada mediante decisão explícita. Zero explícito é exclusão. Confira `selecao_quantidades.csv`, `cotas_realizadas.csv` e `condicoes_vegetacao.csv`.

`confirmar_desvios=NULL` pergunta; TRUE aceita os desvios descritos e registra; FALSE cancela quando ocorrerem. Sem console interativo, NULL não autoriza. Downloads possuem confirmações separadas. Falhas de integridade, geometria e identificação exigem correção; aceitar aviso não cria vértices ausentes nem comprova conformidade.

`configuracao_efetiva.csv` informa valores solicitados, efetivos e origem; `decisoes_metodologicas.csv` registra avisos e respostas; `configuracao_selecao.json` registra efeitos das decisões na seleção. Os registros aparecem no relatório HTML.

Configurações antigas sem `configuracao_versao=2` preservam sua resolução legada. Para migrar, use o novo bloco: não misture `parametros_protocolo`, `padrao_ilha` ou `estratificar_vegetacao` com os novos controles.

## Afastamentos e limitações

Use vetores `estradas_pavimentadas`, `estradas_terra`, `trilhas_preexistentes`, `formacao_florestal` e `transectos`. Os controles `usar_*` herdam com NULL, ativam com TRUE e desativam com FALSE. `aplicar_afastamentos` define o padrão geral; cada controle explícito prevalece. Sem fonte ativa, a avaliação permanece pendente e exige decisão registrada.

As referências campestres são 100/50/5 m para vias e 100 m para floresta e outra UA, aplicadas à linha inteira contra fontes fornecidas. Linhas viárias consideram metade de `largura_m` quando informada. Uma direção precisa atender conjuntamente às fontes ativas. UAs existentes usam linhas reais em `transectos`; PAs candidatos não equivalem a UAs instaladas. A compatibilidade simultânea das futuras UAs permanece avaliação de campo.

Os perfis Ilha usam referências experimentais do piloto: transecto 25 m, grade 30 m e afastamento entre linhas 30 m, com N/L/S/O. Não ativam automaticamente restrições viárias/florestais campestres. O desenho específico do piloto Noronha deve ser preservado por montagem das camadas fornecidas; não é reproduzido apenas por esses padrões.

O teste inicial QGIS confere fontes, legendas, layouts, PDF/PNG e reabertura antes dos downloads. Com várias instalações, testa da mais recente à mais antiga. `qgis_python` fixa uma instalação sem substituição silenciosa. Grade vazia informa a variável a revisar; o script não altera o espaçamento automaticamente.

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
- `formacao_florestal`: polígonos para afastamento quando ativado; `rodovias`, `estradas`, `trilhas`: nomes legados de referência, exigem papel explícito para triagem viária.
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

## Saídas e validação

`01_qfield`: QGS, dados, mapas e ZIP portátil. `02_relatorio`: relatório HTML, mapa geral, parâmetros, fontes/checksums, etapas/duração, referência da grade e diagnósticos. `03_vetores`: GPKG/KML/KMZ dos pontos. `04_csv`: todos os atributos dos pontos, coordenadas e dicionário. CSV usa UTF-8 BOM, `;` e ponto decimal; valores ausentes são vazios. KML conserva atributos como texto; GeoPackage conserva os tipos. Os nomes físicos têm índice para evitar colisões.

O projeto exibe coordenadas geográficas WGS84 em graus decimais, seis casas, e processa geometria em CRS métrico. Confirme a exibição em identificação/posicionamento no QField. Trajetos não possuem uma única coordenada: use os vértices e a posição GNSS correspondente.

O relatório distingue geração validada automaticamente de homologação móvel. Cobertura de imagens é verificada por pixel válido nos pontos, com transparência considerada; não prova cobertura do trajeto inteiro, resolução nativa ou ausência de nuvens. Projetos com lacunas permanecem identificados como cobertura parcial. Preserve o apoio preenchido antes de atualizar a pasta do projeto. A ferramenta nunca envia ao QFieldCloud automaticamente.

## Documentação e manutenção

O [manual HTML](https://danilovcorrea.github.io/Monitora-Planejamento-Amostral/) é servido pelo GitHub Pages; o [PDF](https://danilovcorrea.github.io/Monitora-Planejamento-Amostral/manual_usuario.pdf) corresponde à mesma versão. Fontes editoriais em `manual/`; páginas prontas em `docs/`. Para reconstruir: `python3 manual/build_manual.py`. Para incorporar alterações do renderizador e helpers ao R: `python3 cartografia/build_embedded.py`.

Os testes sintéticos em `tests/test_*.R` usam dados temporários. Os testes PyQGIS de homologação recebem caminhos de produtos locais por argumento; os dados de campo não são distribuídos. Consulte `tests/README.md`.

## Autoria, licença e citação

Autoria e coordenação: **Danilo V. Corrêa**. Código sob [GNU GPL v3](LICENSE), preservada do projeto de origem. As fontes e condições de uso das bases cartográficas e imagens são próprias dos respectivos provedores; a licença do código não as substitui. Marcas institucionais são mantidas como identificação de origem.

Citação sugerida: CORRÊA, Danilo V. *Monitora Campestre Savânico - Alvo Global - Planejamento e Desenho Amostral, Cartografia e navegação em campo*. Versão 1.1.0. GitHub, 2026. https://github.com/danilovcorrea/Monitora-Planejamento-Amostral/releases/tag/v1.1.0.

## Validação desta versão

16 suítes legadas aprovadas; 37 verificações de perfis no Linux e no Rscript 4.6.0 Windows; 13 verificações novas de integração. A AE_adarquia real, com grade de 10 m, produziu 170 candidatos, 34 prioritários e 68 alternativos no teste de seleção. Integrações completas utilizaram fontes sintéticas locais. Não houve novo ensaio da interface RStudio ou celular nesta rodada. Evidências em [publicação](validacao/publicacao_v1.1.0.json).
