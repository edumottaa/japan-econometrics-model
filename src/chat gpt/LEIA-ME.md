# Codigos criados em 25/10/2026

## Forecast overnight

- `1- overnight fim de mes.R`: extrai a overnight de fim de mes do BoJ.
- `3- forecast overnight fim de mes.R`: estima e projeta o BVAR de seis lags.
- `4- forecast overnight lag12.R`: estima e projeta a versao de 12 lags.
- `5- comparar forecasts lag6 lag12.R`: compara as duas projecoes.
- `6- forecast lag12 lambda.R`: 12 lags com lambda 0,10, mesma base congelada.
- `7- comparar lags e lambda.R`: compara seis/12 lags com lambda 0,20 e 12 lags
  com lambda 0,10. Rodar depois do script 6.

Execute o script 3 para seis lags ou o script 4 para 12 lags; ambos chamam a coleta
automaticamente. Depois use o script 5 para comparar. Estes scripts nao rodam CV.

## Testes lag12 e priors

Configuracoes, preparacao da base, funcoes de CV e executor dos pilotos preparados
nesta manha. A bateria de CV nao foi concluida e nao faz parte do forecast atual.

## Apoio

Conferencias de series, diagnostico da amostragem e validacao do forecast.
Nao precisam ser executados para gerar os graficos.

## Premissas

Forecasts com juro de 1,50% apenas em dezembro/2026 e condicionamento suave da
inflacao. O prior de longo prazo de seis lags foi concluido com ESS 87,56; os
graficos e a comparacao estao na pasta de outputs. A versao de 12 lags com prior
nao foi retomada e segue sem resultado validado. Consulte o LEIA-ME da subpasta.

Os scripts usam funcoes anteriores do projeto por `source()`. Os scripts antigos
permanecem em suas pastas; as duas rotinas de coleta chamam o novo extrator nesta
pasta. Os graficos continuam em `src/Outputs/Forecast BVAR estrutural` e os modelos
estimados continuam em `data/processed`.
