# Homologação e publicação 1.0.0

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
