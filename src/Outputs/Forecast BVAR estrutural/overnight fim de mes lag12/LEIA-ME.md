# Forecast com 12 lags e overnight de fim de mes

Execute `src/chat gpt/forecast overnight/4- forecast overnight lag12.R`.
Depois execute `5- comparar forecasts lag6 lag12.R` na mesma pasta.

Especificacao: sete variaveis; 12 lags; lambda 0,20; mu 1; delta 1.
Mesmos sinais, narrativas, tratamento da COVID, base ate agosto de 2026 e quatro
cenarios externos usados na versao de seis lags. A taxa-sombra continua ate
fevereiro de 2024; depois entra a overnight de fim de mes do BoJ.

Esta comparacao e entre projecoes. Nao foi executado um novo CV; a diferenca
entre os forecasts nao demonstra, por si so, superioridade preditiva.

Resultados: `src/Outputs/Forecast BVAR estrutural/overnight fim de mes lag12`.
Modelo: `data/processed/forecast_overnight_fim_mes lag12/modelo.rds`.
