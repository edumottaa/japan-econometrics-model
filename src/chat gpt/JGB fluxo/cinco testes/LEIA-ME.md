Seis testes para melhorar a projecao do spread JGB30Y-5Y
======================================================

Preparados para execucao manual no R. Nao foram estimados nem projetados na preparacao.
Nao substituem o modelo principal, os forecasts salvos ou os executores antigos.

Para executar tudo, use code_run_todos.R: primeiro os seis forecasts (1,6,2,3,4,5),
depois o CV dos seis testes. O bloco de CV 1 tambem avalia o 6, compartilhando
todos os treinos. Ambos os blocos vem ligados. Um erro interrompe a execucao no ponto
da falha. Para rodar somente os CVs, altere RODAR_FORECAST para FALSE nesse executor.

Cada code_run tem dois blocos: RODAR_FORECAST e RODAR_CV. O forecast vem ligado;
o CV vem desligado para primeiro conferir a coerencia, conforme solicitado.
Depois, troque RODAR_CV para TRUE. Para rodar somente CV, deixe RODAR_FORECAST=FALSE;
isso tambem funciona em sessao nova, sem estimar o forecast prospectivo primeiro.

Configuracoes
-------------

1. code_run_1_satelite_condicionado.R: estima os dois estagios do satelite SEPARADOS
   e condiciona SUAVEMENTE a posterior do BVAR ORIGINAL JA SALVO. Nao reestima
   o BVAR prospectivo, nao troca sua amostra e nao fixa o spread exatamente no alvo.
   UST30 -> fluxo; OIS2Y pos-YCC + fluxo -> Delta spread. Ambos os estagios
   selecionam lags e erros ARIMA pela mesma estrutura do IP mundial, descrita abaixo.
   A covariancia do sinal acumula os erros ARIMA simulados do estagio 2 e preserva
   a correlacao entre horizontes. O BVAR combina
   sua distribuicao com essa trajetoria probabilistica, preservando os nowcasts
   comuns e a banda terminal do juro. SATELITE_VARIANCIA_MULT controla a intensidade:
   maior variancia significa sinal mais fraco; zero nao e permitido.
   A covariancia e condicional aos insumos: nao inclui incerteza de fluxo futuro,
   curvas ou parametros. Os dois modelos compartilham dados: trata-se de cenario
   suave, nao de evidencia adicional comprovadamente independente.
2. code_run_2_fluxo_exogeno.R: adiciona fluxo lag 2, centrado na media de treino,
   como exogena. Coeficiente estimado livremente na equacao do spread; zero exato
   nas outras sete equacoes. Fluxo futuro vem do mesmo estagio selecionado do satelite.
   Nao importa um beta fixo do satelite e nao apenas altera o forecast antigo.
3. code_run_3_fluxo_domestico_ex_ante.R: adiciona fluxo como sexta domestica,
   junto com atividade, CPI, juro, spread e cambio. Futuro projetado pelo BVAR.
4. code_run_4_fluxo_domestico_ex_post.R: MESMA posterior do teste 3; muda somente
   o condicionamento, fixando o fluxo estrangeiro realizado onde disponivel.
5. code_run_5_macro_fluxo_spread.R: fluxo domestico, com macro -> fluxo livre e
   lags de fluxo -> spread livres. Lags de fluxo nas equacoes de atividade, CPI,
   juro e cambio sao exatamente zero. Outros efeitos podem ocorrer pelo spread.
6. code_run_6_satelite_rigido.R: mesma posterior original, mesmo satelite, mesmos
   alvos, externos e ancoras do teste 1. Fixa o spread exatamente na trajetoria
   do satelite, em vez de combina-la probabilisticamente com a previsao do BVAR.
   O juro terminal continua na banda de 1,75%-2,25%. Nao reestima o BVAR futuro.

Os testes 1 e 6 compartilham selecao, coeficientes e posterior, usando cache
validado pelos insumos. A diferenca e somente o condicionamento do forecast.
Nao e preciso estimar modelos duas vezes. Entretanto, as previsoes finais sao
diferentes: o mesmo CV registra previsoes, RMSE, MAE e vies separados para suave
e rigido. Chamar o CV pelo executor 1 ou 6 calcula AMBOS, uma vez por origem.

O teste 5 restringe coeficientes defasados da forma reduzida. A covariancia dos
residuos permanece livre: nao garante uma exclusao causal contemporanea e nao
identifica um choque estrutural de fluxo. A nona inovacao e residual sem rotulo.
Identificacao dos oito choques anteriores preservada, com indices narrativos
recalculados para a amostra de cada estimacao.

Selecao dos dois estagios do satelite, como no IP mundial
--------------------------------------------------------

09_selecao_satelite.R aplica as mesmas familias: A_lag0 (0), A_lag03 (0,3),
A_lag036 (0,3,6), B_1, B_3, B_36, B_6, B_69, B_9 e B_12.
No fluxo, compara estruturas de UST30. No spread, compara estruturas de OIS
pos-YCC e fluxo, seleciona por regressor e estima a equacao conjunta final.

O maior d indicado pelo diagnostico de diferenciacao das series e usado na
regressao de cada estagio. Os termos AR e MA sao selecionados por AICc em
0:3, sem stepwise nem aproximacao. O diagnostico sazonal e exportado; nao sao
impostos termos sazonais. Constante e escolhida pelo ARIMA, sem beta fixado.

As estruturas sao comparadas nas mesmas linhas validas para todos os lags.
Entre delta AICc < 2, favorece menos termos, maior menor lag e menor AICc,
exatamente como no IP mundial. Depois reajusta o modelo final nas linhas
disponiveis para os lags escolhidos. O fluxo lag 12 limita a amostra de selecao
do spread: as datas e quantidades de observacoes de selecao e ajuste sao exportadas.
Nao se comparam AICcs de amostras diferentes para escolher uma estrutura.

Exporta tabela de candidatos, escolha de cada regressor, diferenciacao,
sazonalidade, report do modelo final e Ljung-Box em 12/24 meses, com os graus
de liberdade da estrutura AR/MA selecionada. AICc nao garante residuos brancos:
um diagnostico reprovado gera aviso e permanece visivel; nao se garimpa p-valor.
O CV refaz a selecao em cada treino. Nenhuma escolha usa realizacoes futuras.

Graficos de residuos incluidos no code_run: MOSTRAR_RESIDUOS=TRUE mostra no R
os paineis do fluxo e do spread, com inovacoes no tempo, ACF ate 24 meses e
histograma. Usa residuos finais apos o ARIMA, com Ljung-Box 12/24m no subtitulo.
Salva residuos fluxo.png e residuos spread.png na pasta do respectivo teste.
MOSTRAR_RESIDUOS=FALSE desliga somente a exibicao; os PNGs continuam sendo salvos.
No CV, mantem os diagnosticos em CSV, sem abrir dezenas de graficos por origem.

O condicionamento suave usa SATELITE_SIMULACOES trajetorias do ARIMA do spread,
condicionais aos regressores projetados, para obter a covariancia dos erros futuros.
Assim, a conta nao pressupoe MA(2) no fluxo nem erro branco no spread.

Comparacao justa e criterio de selecao
-------------------------------------

O alvo e somente o spread 30Y-5Y. Melhora de outras variaveis nao seleciona modelo.
Mudanca no forecast nao significa melhora preditiva. Nenhuma conf e aprovada
apenas por elevar o spread ou por gerar um coeficiente significativo.

Os testes 2-5 tem controle sem fluxo REESTIMADO na mesma amostra: jul/2018 ao ultimo mes
do banco BVAR (atualmente ago/2026), com seis defasagens, tendencia, rho=0.995,
Minnesota (mu=1, delta=1, lambda=0.2), COVID recalibrada e os mesmos sinais.
Jul/2018 e o primeiro mes com fluxo lag 2 observado. Nao se inventa fluxo antes
da JSDA. Assim, a diferenca nao e confundida com trocar a amostra 2000-2026.
Nos testes 1 e 6, o controle e o BVAR original salvo, com a amostra original completa.
Sua posterior e usada sem reestimacao. A referencia e recalculada sob os mesmos
cenarios/banda pelo algoritmo conjunto; pode diferir do forecast de producao
salvo, que usa outro algoritmo. Os graficos distinguem os dois controles:
o efeito de cada teste e medido contra a sua propria referencia. Diferencas
entre os testes 1/6 e os testes 2-5 tambem refletem a diferenca de amostra.

Juro curto condicionado NO FINAL do horizonte a [1.75%,2.25%], em todos os seis
testes, controles e quatro cenarios futuros. Nao fixa o caminho inteiro em 2%.
A banda entra no condicionamento multivariado; nao e corte posterior da serie.

Cache guarda posteriores validadas por dados/codigo/opcoes. Os testes 3 e 4
reutilizam a mesma estimacao. Os controles tambem sao reutilizados. Os testes
1/2/6 compartilham o satelite quando os dados e opcoes coincidem.
Diagnosticos de ESS, sorteios distintos, exclusoes e sinais precisam passar
antes de publicar forecasts. Nao ha garantia de ESS suficiente antes de executar.
Para o fluxo domestico, o prior da primeira propria defasagem tem media zero,
apropriado a hipotese de fluxo mensal estacionario; demais macros preservam os
priors de persistencia. A nova equacao tem coeficientes e covariancia estimados.

Ex post nao significa inventar o fluxo futuro
-------------------------------------------

O banco macro termina em ago/2026; a JSDA contem fluxo ate set/2026. Portanto,
no forecast futuro o teste 4 fixa apenas set/2026. Os meses seguintes permanecem
endogenos e sao explicitamente rotulados como NAO ex post. Exporta cobertura.
Isso limita a diferenca prospectiva entre 3 e 4; nao e um oracle de 24 meses.

Para forecast historico completamente ex post, use ORIGEM='2024 Aug',
HORIZONTE=24 e EXIGIR_FLUXO_EX_POST_COMPLETO=TRUE na configuracao comum e rode
os seis executores. Os externos serao realizados (cenario unico, nao real time).
Fora do CV, a banda de juros pedida e mantida inclusive nessa simulacao historica;
nesse caso ela e uma hipotese de cenario, nao um dado que era conhecido em 2024.
Resultados de origens/configuracoes diferentes nao sao misturados nos graficos.

CV incluido nos seis code_run
-----------------------------

CV de 12 meses, janela expandida, origens recentes a partir de out/2024; ultimo
mes observado do banco macro limita a ultima origem. Reestima cada posterior,
hiperparametros COVID, centralizacao e satelite usando somente dados ate a origem.
Nos testes 1/6, essa reestimacao HISTORICA compartilhada mantem a amostra do BVAR original desde
2000 e evita usar a posterior estimada com dados de 2026 para prever o passado.
Ela e necessaria para CV honesto; nao ocorre nos forecasts prospectivos 1/6.
Externos realizados nos seis testes; OIS realizado no satelite. Assim, o termo
ex ante refere-se ao fluxo projetado e nao a um backtest integral em tempo real.

Teste 1: cadeia completa de dois estagios, sinal suave no BVAR original por origem.
Exporta separadamente o antigo
diagnostico EX POST do estagio 2 com OIS e fluxo realizados, sem confundir os dois.
Teste 2: fluxo projetado pelo ARIMA selecionado, sem usar realizacoes futuras do fluxo.
Testes 3 e 5: fluxo previsto endogenamente.
Teste 4: todos os fluxos futuros de cada janela historica sao realizados.
Teste 6: mesma selecao e mesmo BVAR do teste 1, com spread condicionado rigidamente.
O CV do teste 1 ja salva o resultado do 6 em resultados/teste_6/CV, sem repetir
a estimacao do satelite ou BVAR. Comparacao direta: teste_1/CV/comparacao suave rigido.csv.

A meta terminal de 2028 NAO entra no CV historico, evitando impor 2% a janelas
que terminam em 2025/2026. Mesmos condicionantes externos nos seis testes.
Benchmarks: controle BVAR da mesma amostra, naive e drift. Metricas: RMSE, MAE
e vies do spread em pb no horizonte de 12 meses. Origens sobrepostas e poucas
observacoes limitam a evidencia; diferenca pequena nao prova superioridade.

CV inicial usa 500 sorteios/100 centros por origem, configuraveis no arquivo 00.
Mesmo assim pode demorar: ha reestimacao de BVAR em cada origem. Os controles
e modelos domesticos sao reutilizados entre executores. Nenhum CV e disparado
durante a preparacao. Falha de ESS interrompe e exige revisar a estimacao.

Saidas
------

resultados/teste_1 a teste_6: forecast de todas as variaveis/cenarios em CSV e RDS,
valores finais, trajetoria do fluxo, metadados e coeficientes do fluxo no spread.
resultados/comparacao: graficos do spread e das demais variaveis com data labels,
tabela conjunta, triagem do spread, cobertura ex post e verificacao da banda.
A comparacao se atualiza ao terminar cada executor, usando os testes ja rodados.
Pastas CV: previsoes por origem e metricas; comparacao/CV spread.csv reune os
testes completos compativeis. Origem incompleta nao e publicada como CV final.

Graficos mostram centros condicionais e dispersao de parametros nao e rotulada
como intervalo preditivo. Nao foi identificada uma parcela fiscal pura.

Validacao na preparacao: somente leitura de objetos existentes, revisao e parse
dos scripts. Estimacao, previsao, CV e validacao numerica aguardam execucao no R.
