Experimento separado: satelite 30Y-5Y com UST30 no lugar do fluxo.

Treino jan/2014 a ago/2026, origem igual ao BVAR salvo. Intercepto em todas as
configuracoes; OIS2Y em variacao mensal (pb), com efeito zero antes de abr/2024.
Sem nova tendencia na regressao satelite; tendencia do BVAR permanece original.

30 configuracoes: UST30 em nivel (%) ou variacao (pb), lags 0/1/2; erro branco
ou ARIMA(p,0,q), p/q de 0 a 2 por AICc. Para Delta UST, testa tambem dummy de
intercepto pos-YCC, interacao de inclinacao pos-YCC e tres inclinacoes separadas
(pre set/2016, set/2016-mar/2024, desde abr/2024). Mesmas observacoes entre candidatos.
Dummy de intercepto e interacao de inclinacao sao hipoteses diferentes.

Filtro: todos os coeficientes economicos com p nominal <=10%, exceto intercepto.
Nao exige significancia individual AR/MA. Ljung-Box 12/24 com ajuste p+q, ambos
p>=5%. Seleciona AICc entre aprovados; dentro de 2 AICc, prefere menos regressores.
P-valores apos busca sao exploratorios, nao corrigidos por selecao multipla.
ARCH-LM12, Shapiro e graficos tambem exportados. ARCH significativo alerta para
heterocedasticidade: a covariancia homoscedastica usada no suave pode subestimar
incerteza. Nao equivale a falha de autocorrelacao na media nem e corrigido aqui.

code_run_so_diagnosticos.R: apenas ajuste do satelite e diagnosticos.
code_run.R: diagnosticos -> forecast dos quatro cenarios -> CV. Nao execute em
paralelo com outro lote na mesma sessao R. REESTIMAR=FALSE preserva etapas salvas.
O agente executa somente os diagnosticos; forecast BVAR/CV ficam para o usuario.

Forecast: mesmo posterior estrutural original, condicionamento suave conjunto,
BoJ terminal em 1,75%-2,25%; demais caminhos e nowcasts preservados. A covariancia
do satelite vem de 3000 simulacoes ARIMA, preservando correlacao serial e acumulacao.
Nao inclui incerteza dos coeficientes, da escolha do modelo ou das curvas.

CV: BVAR reduzido original, nao estrutural, 2500 draws, pesos/prior/tendencia/COVID
inalterados, 12 meses, janelas trimestrais jun/2024-jun/2026 (nove origens comuns
as quatro opcoes anteriores). Aumentar o treino do satelite para 2014 nao aumenta
automaticamente o numero de origens da avaliacao. UST30/OIS e bloco externo
realizados ex post; demais domesticas previstas. Sem meta futura do BoJ no CV.
Selecao de configuracao e feita novamente em cada treino, sem selecionar com os
dados futuros. Se nenhuma passar, usa o BVAR original naquela janela e registra
isso em resultados/uso do satelite por origem. Nao exclui janelas dificeis nem
relaxa a significancia para conseguir condicionar. O CV avalia a estrategia
"BVAR + satelite quando aprovado", nao um satelite imposto em todas as janelas.
Reaproveita o BVAR reduzido salvo quando a chave da especificacao original confere.
Nao reestima o BVAR apenas por trocar o satelite. Novos resultados em pasta propria.

Um modelo em nivel UST pode induzir drift; o intercepto pode compensa-lo. Usar
Delta UST nao elimina drift se o intercepto for diferente de zero. A curva FWCV e
um condicionante implicito com premios, nao o caminho futuro garantido dos juros.

Regimes mensais aproximam as mudancas dentro do mes; abr/2024 e o primeiro mes
inteiro pos-YCC. QQE nao implicava juro negativo durante todo o periodo 2014-2016.

Teste adicional: code_run_sem_dummy_cointegracao.R executa apenas diagnosticos,
em pasta propria. Compara 24 configuracoes sem dummy adicional: OIS pos-YCC ou
OIS sem qualquer divisao de regime, UST em nivel/Delta, lag 0/1/2 e erros
brancos/ARIMA. Mesmo treino fev/2014-ago/2026 (151 meses) em todos; jan/2014 e
necessario para calcular Delta OIS. Reestima a referencia com dummy nessa mesma
amostra antes de comparar AICc. Nao altera a selecao nem o code_run principal.

Cointegracao: niveis jan/2014-ago/2026, 152 meses. ADF (maximo 12 lags, AIC) e
KPSS em nivel, primeira e segunda diferenca, com constante e tendencia.
Phillips-Ouliaris Pz nos pares spread/UST30, spread/OIS2 e no trio; constante ou
tendencia, banda curta/longa. Valores criticos proprios do teste, H0=ausencia
de cointegracao. A coluna I1_compativel_todas e uma triagem conservadora, nao
um teste conjunto nem prova da ordem de integracao. Resultados discordantes
podem refletir baixa potencia e quebras; nao concluir I(2) automaticamente.
Fonte do procedimento: https://bpfaff.r-universe.dev/urca/doc/manual.html

Na rodada de 16/11/2026: versao sem regimes Delta OIS+Delta UST lag0, AR(1),
AICc 980.007; referencia OIS pos-YCC+dummy na mesma amostra: 985.066.
Ljung-Box 12/24: .942/.959; normalidade rejeitada. ADF/KPSS do spread em nivel
discordam; Delta OIS nao tem estacionariedade confirmada. Nenhum dos testes
Pz rejeita ausencia de cointegracao a 5%. Resultados exploratorios; nao
justificam assumir equilibrio de longo prazo nem aprovacao definitiva do modelo.

code_run_spread_nivel_ust_delta.R: teste separado de spread em nivel (pb) com
Delta UST30, lags 0/1/2, OIS2Y em nivel ou Delta, sem dummy; erros brancos ou
ARMA, d=0 fixado para preservar a resposta em nivel. Mesmo treino fev/2014-ago/2026,
151 observacoes. Nao compara AICc desse teste com o da resposta diferenciada.
Regra de significancia/Ljung-Box e apenas triagem: nao resolve a duvida sobre a
estacionariedade do spread. Raizes AR perto da fronteira unitaria sao reportadas.
Nao altera o condicionamento suave do teste anterior nem executa forecast/CV BVAR.
