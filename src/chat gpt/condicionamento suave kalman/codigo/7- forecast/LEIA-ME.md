# BVAR com satelite de fluxo e condicionamento suave

Configuracao selecionada: Delta do spread 30Y-5Y em pb, explicado por Delta
OIS2Y apenas desde abr/2024 e fluxo estrangeiro de superlongos defasado dois
meses. Coeficientes estimados por ARIMA(0,0,0), com intercepto. Fluxo futuro
igual a media pos-YCC disponivel ate a origem. O fluxo nao e nova endogena
nem novo choque do BVAR. Permanece a tendencia e a estimacao estrutural original.

Os scripts estao distribuidos nas sete etapas existentes:

1. `src/1- data_collection/Satelite fluxo`: leitura das fontes locais.
2. `src/2- data_cleaning/Satelite fluxo`: medias mensais, ordenacao e uniao das series; um unico script.
3. `src/3- feature_engineering/Satelite fluxo`: variacoes do spread/OIS e defasagem do fluxo; um unico script.
4. `src/4- plot data/Satelite fluxo`: residuos, forecasts e metricas do CV.
5. `src/5- models/Satelite fluxo`: regressao satelite e BVAR reduzido para CV.
6. `src/6- evaluation/Satelite fluxo`: CV de 12 meses das cinco domesticas.
7. `src/7- forecast/Satelite fluxo`: condicionamento suave e executor.

Execute `0- rodar.R` para carregar as etapas, reaproveitar os resultados compativeis
e gerar os quatro cenarios. `rodar_cv_satelite <- FALSE` por padrao; mude para TRUE
quando quiser executar/retomar o CV. `REESTIMAR <- FALSE` preserva os checkpoints.
O CV mantem as nove origens trimestrais de jun/2024 a jun/2026, com janelas finais
parciais, mesmos metodos do BVAR reduzido original e benchmarks Naive/Drift.
OIS e bloco externo sao realizados ex post; fluxo futuro usa apenas a media do treino.

Entradas em `data/raw/satelite_fluxo`, alem do posterior/dados BVAR e caminhos
externos originais. Nao ha nova coleta de Bloomberg. A posterior estrutural
continua em `data/processed/bvar_producao_tendencia_jgb30_5_20261112`.
Resultados selecionados em `src/Outputs/Satelite fluxo suave`. Os salvos da
opcao 3 sao importados somente quando os hashes dos insumos e parametros conferem.
Essa importacao nao reestima nem refaz previsoes. Scripts antigos de experimentos
continuam independentes; o executor novo nao importa codigo deles.

O arquivo `resumo_forecast.csv` e a interface para a curva JGB. Valores exportados
sao centros condicionais, sem bandas preditivas. O juro curto terminal e
condicionado a 1,75%-2,25%; a covariancia suave acumula as inovacoes do satelite,
sem incorporar incerteza dos seus coeficientes, da media do fluxo ou das curvas.
