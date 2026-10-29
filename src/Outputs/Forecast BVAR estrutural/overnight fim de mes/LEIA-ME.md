# Forecast com overnight de fim de mes

Execute `src/chat gpt/forecast overnight/3- forecast overnight fim de mes.R`.
O script extrai a serie, reestima o BVAR quando os dados mudam, gera os quatro
cenarios e salva os graficos. Nao executa CV.

- Fonte: BoJ, FM02/STRECLUCON, overnight efetiva sem garantia de fim de mes.
- Unidade: % ao ano, mantida em nivel; sem log, diferenca ou divisao por 100.
- Taxa-sombra preservada ate fevereiro de 2024; nova overnight desde marco de 2024.
- Mes corrente incompleto excluido da coleta.
- Mesmas sete variaveis, seis lags, lambda 0,20, mu 1 e delta 1 do estrutural original.
- Mesmos sinais, narrativas e tratamento da COVID do modelo existente.
- Demais series mantidas na base de forecast de janeiro de 2000 a agosto de 2026.
- Observacoes domesticas posteriores e curvas externas mantidas; a condicao de
  juros usa a overnight de fim de mes, inclusive em setembro de 2026.
- Brent e Treasury continuam vindo das mesmas planilhas e rotinas.

As duas rotinas de coleta do projeto tambem passaram a chamar o novo extrator.
Reexecute a estimacao antes de usar essa serie em outros forecasts; o posterior
estimado com a media mensal nao representa a nova variavel.

Saidas: `src/Outputs/Forecast BVAR estrutural/overnight fim de mes`.
Modelo estimado: `data/processed/forecast_overnight_fim_mes/modelo.rds`.

Referencia da serie: https://www.stat-search.boj.or.jp/info/nme_Mdframe_en.html
