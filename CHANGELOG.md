# Histórico

## 1.0.1-rc3 — 2026-10-02 (candidata)

Corrige a união de AEs com coordenadas Z/M e raios de imagem XY, que gerava erro WKB após o recorte. A normalização para XY ocorre somente no contexto derivado de imagens; arquivos de entrada, camadas originais e cache são preservados. Teste de regressão com pipeline XYZ e combinações XY/XYZ/XYM/XYZM.

## 1.0.1-rc2 — 2026-10-02 (candidata)

Corrige o encerramento prematuro das barras CLI em consoles interativos, que interrompia a seleção amostral. O encerramento fica sob controle da etapa; barras aninhadas, atualização a 100% e totais desconhecidos são testados. Formatação usa as variáveis públicas de progresso do CLI. A suíte passa a exercitar explicitamente o modo dinâmico, antes ausente da cobertura.

## 1.0.1-rc1 — 2026-10-02 (candidata)

Preparação automática dos pacotes: verifica disponibilidade, instala somente os ausentes e carrega, incluindo lpSolve. Usa os repositórios configurados com fallback CRAN HTTPS, verifica falhas e mantém o carregamento somente de funções sem instalação. Chamadas de funções qualificadas pelo namespace para evitar ambiguidade ao anexar pacotes. Documentação atualizada; sem publicação desta candidata.

## 1.0.0 — 2026-10-01

Primeira versão pública independente. Publica o comportamento homologado 0.4.7 sob o nome ampliado da ferramenta. Arquivo principal `monitora_planejamento_amostral.R`; função homônima e alias anterior compatível. Manual HTML no GitHub Pages e PDF pronto para download. Histórico do componente preservado por extração dos arquivos pertinentes; hashes de origem em `validacao/historico_origem.txt`.

## Desenvolvimento anterior

Planejamento com cotas cumulativas e adaptação à capacidade; perfis Campestre-Savânico, Ilha e personalizado; montagem e expansão; imagens regionais e locais com cache; QGIS/QField; quatro mapas e exportações; revisões de cartografia e homologação PNSV, Censipam e Noronha. Consulte `HOMOLOGACAO.md`.
