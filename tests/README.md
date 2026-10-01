# Testes

Execute na raiz do repositório, com as dependências R do manual instaladas:

```sh
Rscript --vanilla tests/test_core.R
Rscript --vanilla tests/test_adaptive.R
Rscript --vanilla tests/test_adaptive_integration.R
Rscript --vanilla tests/test_roads.R
Rscript --vanilla tests/test_island.R
Rscript --vanilla tests/test_sentinel_mosaic.R
```

Os demais `test_*.R` cobrem cotas, montagem, expansão, imagens e compatibilidade. Cada teste usa dados temporários; as chamadas remotas relevantes são substituídas por respostas sintéticas nos testes de integração. A disponibilidade dos serviços reais depende da execução do usuário.

Os testes `.py` requerem o Python do QGIS e produtos locais: argumentos são descritos no cabeçalho de cada arquivo. Não integram a suíte sintética; preservam os verificadores utilizados nas homologações. As bases institucionais não acompanham este repositório.
