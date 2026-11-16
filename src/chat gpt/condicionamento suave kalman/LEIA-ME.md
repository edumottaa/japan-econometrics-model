# Condicionamento suave (Kalman) - versao substituida

Copia do forecast do satelite de fluxo antes da troca pelo entropic tilting do plano inicial (16/11/2026).

- Metodo: o spread do satelite entra como observacao com erro (variancia acumulada do satelite)
  e e combinado com a media e a covariancia do BVAR (atualizacao gaussiana tipo Kalman).
- Resultado Base set/2028: spread 208 pb (BVAR original 189; rigido 224).
- `codigo/`: scripts de `src/7- forecast/Satelite fluxo` e `src/5- models/Satelite fluxo` nessa versao.
- `resultados/`: resumo_forecast.csv, forecasts.csv, premissas.csv e graficos dessa versao.
- `codigo/6- evaluation` e `codigo/5- models/2- BVAR reduzido CV.R`: CV do satelite (versao Kalman),
  retirado de `src` quando o forecast passou a usar o tilting. Depende do `0- configuracao.R` desta pasta.
