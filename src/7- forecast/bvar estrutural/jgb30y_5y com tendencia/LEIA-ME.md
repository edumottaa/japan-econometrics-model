# Forecast principal com âncora terminal

Execute `0- rodar forecast.R`. Ele reutiliza os 2.500 conjuntos de parâmetros do principal: rho 0,995, tendência e spread JGB30Y–5Y. Não reestima o BVAR.

- Horizonte: setembro/2026 a setembro/2028, 25 meses, para todas as variáveis.
- Juro curto em setembro/2028: entre 1,75% e 2,25% nos quatro cenários. A média é determinada pelo modelo dentro desses limites.
- Dezembro/2026 não tem uma taxa imposta; o modelo projeta o juro desse mês.
- Meses intermediários não recebem uma trajetória linear imposta. O condicionamento terminal altera as trajetórias conforme as covariâncias do modelo.
- Nenhum conjunto de coeficientes é excluído. A restrição é aplicada à distribuição futura por conjunto de parâmetros, mantendo seus pesos iguais; não é uma nova estimação da posterior.
- A banda não representa intervalo de confiança nem choque surpresa do BoJ. Não é uma hipótese de convergência permanente depois de setembro/2028.
- O mês adicional usa as curvas de Brent e Treasury já disponíveis e o ARIMA salvo da produção mundial; a curva de mercado conserva sua data original.
- Os cenários de oferta usam as mesmas IRFs. Em seguida, cada cenário recebe a condição terminal do juro.

Saída própria: `src/Outputs/4- Forecast/jgb30y_5y com tendencia/producao/`.
Confira `diagnosticos banda juro.csv` para verificar os limites e os 2.500 sorteios em cada cenário. O arquivo `previsao_base_pacote.rds` guarda a etapa anterior à âncora; os resultados finais estão em `forecast_bvar.rds`, `resumo.csv` e nos gráficos.

Esta é a versão principal do forecast. Os scripts da versão sem âncora foram substituídos.
