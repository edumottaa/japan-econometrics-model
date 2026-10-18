# Correcao local minima de forecast — bsvarSIGNs 3.0.0.1

Autorizada pelo usuario em 18/10/2026. Base: fonte CRAN bsvarSIGNs 3.0, SHA256 EBACED63F64E3A1F843DDFF726508FD75288333F49156BC237A61937EEABF985.

## O que mudou

1. `R/forecast.R`: iniciar os lags no ultimo Y observado, em vez de repetir os regressores da ultima equacao do treino.
2. `src/forecast_bsvarSIGNs.cpp`: deslocar todos os blocos de lags ao acrescentar cada previsao. Preservar constante e usar o regressor exogeno do mes seguinte.
3. `DESCRIPTION`: versao local identificada como 3.0.0.1. Nao e uma atualizacao oficial dos autores.

Estimacao, priors, ponderacao foreign, tratamento de volatilidade COVID e sorteio gaussiano/condicionado nao foram alterados. A instalacao original 3.0 continua separada. Os arquivos originais estao em originals; o tarball oficial tambem foi preservado.

## Uso

Em uma sessao R nova, antes de carregar o pacote:

```r
.libPaths(c("C:/Users/dudup/OneDrive/japan-econometrics-model/software/bsvarSIGNs-3.0-forecast-fix/library", .libPaths()))
library(bsvarSIGNs)
# Continua sendo a mesma funcao e a mesma API:
# previsao <- bsvarSIGNs::forecast(posterior, horizon = 24)
```

Se o pacote 3.0 ja estiver carregado na sessao interativa, reiniciar R primeiro. Os processos independentes do CV abrem sessoes novas e verificam a versao 3.0.0.1.

## Validacao executada

- 10 exemplos contra stats::filter: p1, p2, p3, p6 e p12, com/sem regressores exogenos; todos passaram. Maior erro aproximadamente 6.2e-12.
- Escala futura de covariancia COVID conferida contra a formula original: erro menor que 3e-15.
- Caminho condicionado respeitado exatamente.
- Mesma estimacao foreign nas duas bibliotecas, mesma semente: posteriores iguais com tolerancia 1e-10.
- Exemplo AR(2) estavel: previsao corrigida H24=0.001758771, igual a referencia. Original produzia -3365.622439 nesse exemplo.

Arquivos: validar.R, tests_fixed.csv, tests_original.csv, verificar_estimacao.R, verification.txt e AR2_corrigido.csv. Estes testes validam os defeitos corrigidos; nao constituem auditoria integral do pacote nem garantem a estabilidade economica de todo ajuste.

## Retomada do CV

Saida corrigida: data/processed/cv_covid_corrigido_20261018. Saida defeituosa preservada integralmente em cv_covid_nativo_20261018. Os 94 checkpoints do BVAR condicionado sao reutilizados porque sua previsao utiliza outro pacote e nao foi afetada. Foreign precisa ser reestimado porque a primeira execucao nao guardou os objetos posteriores completos. A nova execucao guarda esses objetos, alem dos sorteios.

O CV mantem nove variaveis, as mesmas 47 origens, sementes, priors, horizontes e CRPS do fable. A correcao nao e uma mudanca de especificacao economica.