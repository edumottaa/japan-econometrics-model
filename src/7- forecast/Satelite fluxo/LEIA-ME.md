# BVAR com satelite de fluxo (tilting entropico)

Satelite: Delta do spread 30Y-5Y em pb = constante + Delta OIS2Y (so pos-YCC, desde abr/2024)
+ fluxo estrangeiro de superlongos defasado dois meses + erro branco. Fluxo futuro igual a
media pos-YCC ate a origem. OIS2Y futuro pela curva a termo da Bloomberg.

Condicionamento suave do plano inicial (entropic tilting): os 2.500 sorteios salvos pelo
forecast de producao (`trajetorias modelos.rds`, ja com a banda do juro terminal de
1,75%-2,25%) sao reponderados para que a media do spread 30-5 iguale o alvo do satelite em
cada mes. As demais variaveis se ajustam pelos mesmos pesos. O BVAR nao e reestimado.

Execute `0- rodar.R`. Ordem dos scripts:

1. `src/1- data_collection/data_collection_sSpread.R`: leitura das fontes locais.
2. `src/2- data_cleaning/1- data_cleaning sSpread.R`: medias mensais e uniao das series.
3. `src/3- feature_engineering/1- feature_engineering sSpread.R`: variacoes e defasagem do fluxo.
4. `src/5- models/Satelite fluxo/1- modelo satelite.R`: regressao, diagnostico e residuos.
   `src/6- evaluation/Satelite fluxo/1- cv satelite.R`: CV mensal do satelite x Naive e Drift (opcional).
5. `src/7- forecast/Satelite fluxo/1- condicionamento suave.R`: funcao do tilting.
6. `src/7- forecast/Satelite fluxo/2- forecast selecionado.R`: premissas, alvo e reponderacao.
7. `src/4- plot data/Satelite fluxo/1- graficos forecast.R`: graficos por cenario.

Saidas em `src/Outputs/Satelite fluxo suave`. `resumo_forecast.csv` e a interface para a
curva JGB (medias ponderadas). A versao anterior (Kalman) e o CV antigo ficam em
`src/chat gpt/condicionamento suave kalman`.
