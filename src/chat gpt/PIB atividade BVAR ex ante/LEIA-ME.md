# CV do PIB com atividade ex ante do BVAR

Rode `code_run.R`. A preparação foi somente de código: o agente não rodou este CV.

Compara a ponte do PIB alimentada por cinco trajetórias mensais de atividade:
BVAR original, opção 1 (UST30), opção 2 (UST30−UST2), opção 3 (satélite suave)
e opção 4 (satélite rígido). Benchmarks: somente Naive e Drift.

Não reestima o BVAR. Lê os checkpoints de `resultados/CV original/opcao_N` da
suíte dos testes, validando a assinatura. A referência original vem da previsão
`BVAR original ex post` já guardada nas janelas da opção 1.

Em cada origem, a atividade é endógena e foi prevista pelo BVAR estimado naquele
treino. A ponte usa histórico do PIB somente até o trimestre anterior à origem,
como o código original. O trimestre da origem entra como nowcast e a avaliação
começa no trimestre seguinte. Isso mantém a convenção anterior de disponibilidade
do PIB; não é uma reconstrução de vintages ou calendários completos de divulgação.

A atividade futura usada na ponte é exclusivamente prevista. Os meses de atividade
observados até a origem podem compor o trimestre corrente. A agregação respeita:
converter cada sorteio de 100×log para nível, calcular a média prevista mensal,
agregar três meses completos e só depois calcular o crescimento trimestral.
Não preenche trimestres futuros incompletos com atividade realizada. O horizonte
mensal de até 12 meses fornece até quatro trimestres futuros avaliados.

Reaproveita a função existente `ajustar_pontes_pib`: diferenciação por KPSS,
atividade contemporânea e até dois lags, erros ARIMA, escolha por AICc com
preferência pela especificação mais simples entre candidatos a menos de dois pontos.
Tudo é selecionado novamente no treino. Uma única ponte por origem é aplicada
aos cinco caminhos, isolando a diferença entre previsões de atividade.

Este teste é ex ante para a atividade, **condicional ex post aos caminhos externos**
que alimentaram o CV BVAR. Dados revisados da base atual. Não é totalmente ex ante
em tempo real. A distribuição do PIB é a da ponte condicionada à média prevista
da atividade: não propaga toda a incerteza do BVAR para os intervalos do PIB.
Interprete CRPS como avaliação dessa distribuição condicional. RMSE/MAE avaliam
as previsões pontuais da cadeia.

Saídas em `resultados/`: métricas gerais, por período-alvo (2015–2019, 2020–2021,
2022–2026), por horizonte, cobertura e checkpoints. Há duas comparações dos mesmos
escores, sem duas estimações: os cinco BVARs nas mesmas janelas e original/opções
1/2 nas mesmas janelas de histórico amplo. As amostras e datas efetivas aparecem
nas figuras e tabelas. A cobertura disponível de cada caminho é exportada à parte.

`REESTIMAR_PIB <- FALSE` reutiliza as pontes e janelas concluídas. O status indica
falhas e as janelas finais sem trimestre futuro completo. Corrija uma falha e rode
o mesmo executor para retomar. Os resultados antigos do PIB permanecem preservados.

O executor gera gráficos de RMSE/MAE/CRPS, desempenho por período, erro por horizonte
e previsões fora da amostra contra o PIB observado. Depois do CV, pode usar
`code_run_so_graficos.R` para atualizar apenas imagens e tabelas, sem reestimar.
