# Correção da candidata 1.0.1-rc3 — 02/10/2026

Erro WKB reproduzido com os dados recebidos e em teste sintético: AE válida em XYZ e raios válidos em XY eram concatenados antes da união, gerando geometria incompatível na serialização. O contexto de imagens passa a normalizar cópias geométricas para XY antes de reunir AE, componentes das UCs e raios; a margem regional permanece a mesma. Geometrias e atributos de origem são preservados.

Quinze suítes passaram com progresso dinâmico, incluindo pipeline com AE XYZ e combinações XY/XYZ/XYM/XYZM, com/sem UC. O novo teste falhou na rc2 com o mesmo erro e passou após a correção. Comparação preservou 109 funções e MQ_CONFIG; alteração funcional restrita à construção do contexto de imagens.

Execução completa no Windows/R 4.6.0 com a entrada real: 12 vértices, 3 prioritários, 6 alternativos; 148 tiles reaproveitados, zero download de detalhe. Recorte anterior reutilizado com integridade conferida. Sentinel obtido para o contexto completo. Gerados QField/ZIP, QGIS editável, quatro PDF e quatro PNG, vetores, CSV e relatórios. IDs, categorias, códigos MapBiomas e coordenadas iguais aos do diagnóstico anterior; AE exportada permanece XYZ e contexto derivado é XY válido. Rodada integral em aproximadamente 74 segundos, sem novo teste físico no celular.

Resultados em `validacao/testes_dimensoes_v1.0.1-rc3.json`. Dados e produtos reais permanecem na entrega local. Candidata sem publicação no GitHub.

---

# Correção da candidata 1.0.1-rc2 — 02/10/2026

Falha reproduzida no backend dinâmico do CLI: a barra criada dentro de mq_progress era removida automaticamente quando esse helper retornava. O primeiro update interrompia a seleção com “Cannot find progress bar”. A cobertura anterior usava console redirecionado e não ativava esse backend.

Correção: ciclo de vida explícito, encerramento pelo on.exit da etapa, sem término automático no retorno do helper ou na chegada a 100%; barras independentes e limpeza idempotente. A formatação usa as variáveis públicas pb_name e pb_status. Totais desconhecidos e vazios têm apresentação sem cálculo indevido de percentual.

O novo teste falhou na rc1 e passou após a correção. Quatorze suítes R passaram com backend dinâmico forçado e renderização imediata no Linux. No Windows/R 4.6.0 passaram o teste específico e a integração adaptativa completa, incluindo a exportação de pacotes com dados sintéticos, sem barras residuais. A comparação confirmou 107 funções e MQ_CONFIG preservados; a alteração funcional se limita aos três helpers de progresso. Os avisos de compilação dos pacotes sob R 4.6.1 são independentes da falha corrigida.

Evidências: `validacao/testes_progresso_v1.0.1-rc2.json`. Diagnóstico original, entradas e cache preservados. Sem novos downloads de imagens, nova rodada real ou publicação no GitHub.

---

# Validação da candidata 1.0.1-rc1 — 02/10/2026

Preparação dos pacotes conforme o script principal: verificação, instalação somente quando indisponíveis e carregamento. Inclui lpSolve; instalação por utils::install.packages e carregamento por base::library. Repositórios configurados são respeitados, com CRAN HTTPS quando indefinido. O carregamento somente de funções permanece sem instalação.

Namespace explícito nas funções de pacotes e nas funções base suscetíveis a conflito ao carregar dependências espaciais. Configuração preservada; comparação de 106 funções confirmou lógica inalterada após normalizar namespaces e identificação da versão. Somente os preparadores de dependências foram substituídos; dois auxiliares foram adicionados.

Treze suítes R aprovadas no Linux. Instalação real de pacote mínimo em repositório e biblioteca temporários, sem acesso à rede; 14 verificações de preparação também aprovadas no Windows. Carga dos 15 pacotes reais conferida nas duas plataformas. No Windows/R 4.6.0, RSQLite, jpeg e lpSolve apresentam avisos de compilação com R 4.6.1, sem impedir o carregamento; as instalações existentes foram preservadas. Manual HTML/PDF atualizado (16 páginas).

Evidências: `validacao/testes_dependencias_v1.0.1-rc1.json`. Candidata local, sem nova publicação no GitHub, geração de pontos ou aquisição de imagens.

---

# Histórico: homologação e publicação 1.0.0

A versão 1.0.0 publica a versão funcional 0.4.7, considerada homologada pelo responsável em 01/10/2026. Mudanças de publicação: nome da ferramenta e do arquivo R, ponto de entrada com alias compatível, identificação de versão, organização do repositório e documentação. Não houve redesenho amostral, troca de fontes, recálculo de pontos ou download de imagens nesta etapa.

## Evidências anteriores preservadas

- PNSV: planejamento e montagem, seleção e exportações, imagens e projetos de campo.
- Censipam: perfil personalizado, grade de 100 m, capacidade reduzida e cartografia sem UC federal.
- Ilha/Noronha: montagem do piloto, parâmetros experimentais próprios, preservação de PAs/UAs e camadas de aves, composição Sentinel com cenas complementares.
- Revisão 0.4.7: oito layouts (Censipam e Noronha), PDFs georreferenciados e PNGs; legendas e fontes, localizadores com/sem UC, quadro vermelho dinâmico das AEs e reabertura de projetos com fontes válidas. Dados, imagens e pacotes de campo preservados por SHA256.

A homologação móvel informada pelo responsável e a validação automatizada são evidências distintas. Esta publicação não representa novo ensaio físico no celular. Os dados e produtos de campo ficam nas entregas locais; o repositório publica código, testes sintéticos e rastreabilidade, sem redistribuir aquelas bases.

## Validação da publicação

Resultados dos testes desta versão em `validacao/testes_publicacao.json`. A migração deve preservar as configurações e funções científicas, os contratos da grade e os caches. O alias `monitora_criar_qfield` e a opção `monitora.qfield.somente_funcoes` continuam disponíveis.

Na revisão dos testes, a configuração do ensaio de integração viária foi explicitada como Campestre-Savânico: ela herdava o perfil personalizado sem restrições do teste-base. A correção é do cenário de teste; o comportamento do script permaneceu preservado. Comparação com a origem confirmou configurações idênticas e lógica preservada em 107 funções (exceto identificação editorial); renderizador preservado exceto o texto de versão.
