# Forecasts com premissas de inflacao e juros

## Situacao em 25/10/2026

Juro em dezembro e condicionamento suave: forecasts concluidos para seis e 12 lags.
Prior de longo prazo com seis lags: concluido e aprovado. Quatro lotes totalizam
10.000 propostas nativas; 3.642 tem peso positivo e 679 tem bloco domestico estavel.
ESS final do prior: 87,56, inclusive agregando parametros distintos. O controle
com a mesma tendencia e estabilidade tem ESS 347,86. Foram gerados os quatro
cenarios, os graficos e a comparacao. As condicoes e a recursao foram verificadas.
O usuario autorizou ESS minimo de 30 para resultado exploratorio; nesta execucao,
o limite original de 50 tambem foi atendido.

A versao de 12 lags com prior permanece sem resultado validado; nao foi retomada
nesta etapa. As tentativas antigas de seis lags que falharam foram substituidas
pelas saidas completas da retomada.

Base em agosto/2028: CPI de 1,41% no original, 2,16% no controle com tendencia e
2,10% com prior; juro de 1,53%, 2,02% e 1,86%, respectivamente. A maior mudanca
frente ao original vem da tendencia e da restricao de estabilidade. O efeito
adicional do prior deve ser comparado ao controle, e nao atribuido a toda a
diferenca frente ao original. Isso nao demonstra maior acuracia fora da amostra.
No controle sem prior, a media aritmetica da inflacao de longo prazo e dominada
por parametros de cauda extrema; nao deve ser interpretada como previsao.
Os forecasts finitos ate 2028 foram verificados separadamente.

O teste atual separado esta em `../forecast overnight/6- forecast lag12 lambda.R`.

Todos os scripts desta pasta geram forecasts, sem CV. Usam a mesma amostra ate
agosto/2026, as observacoes posteriores e as mesmas curvas de Brent/Treasury das
versoes de seis e 12 lags com overnight de fim de mes. A atividade nao foi trocada.

## Execucao

Retomada de seis lags: `6- ampliar sorteios prior lag6.R` recebe o numero do
lote como argumento do Rscript e salva propostas antes da reamostragem nativa.
`7- reunir propostas prior lag6.R` combina os lotes. Depois execute o script 2
com argumento `6`, o script 3 e o script 4 (por padrao, seis lags). O script 2
reutiliza `modelo_propostas.rds` quando disponivel. O cache original fica preservado.

Esta retomada conserva os pesos nativos e combina-os com o fator do prior antes
de reamostrar. Propostas de peso nativo nulo sao descartadas. Isso evita repetir
parametros em uma reamostragem intermediaria. O centro e a dispersao permanecem
fixos em 2% e 0,5 p.p.; os parametros do BVAR continuam sendo estimados. O ESS e
calculado sobre os pesos combinados. O limite de referencia e 50; entre 30 e 50,
o prior recebe identificacao de resultado exploratorio, conforme autorizacao.

1. `1- forecast juro dezembro e inflacao suave.R`: quatro cenarios de Brent, em
   seis e 12 lags, para as duas premissas abaixo.
2. `2- forecast prior longo prazo.R`: por padrao, seis lags. Para 12, executar
   pelo Rscript com argumento `12`. Estima uma vez a versao com tendencia, salva
   o modelo e reutiliza-o nas execucoes seguintes.
3. `3- comparar forecasts.R`: comparacoes de todas as cinco domesticas e de CPI/juros.
4. `4- validar forecasts.R`: verifica as condicoes e a recursao com tendencia.

## 1. Juro em dezembro

Overnight fixada em 1,50% apenas em dezembro/2026, como proxy da premissa para o
alvo. Outubro, novembro e janeiro/2027 em diante ficam livres. A overnight
efetiva pode diferir alguns pontos-base do alvo do BoJ.

## 2. Condicionamento suave

Informacao hipotetica: CPI core em 12 meses de 2%, com erro normal de 0,5 p.p.,
em agosto/2028, ultimo mes do forecast. A verossimilhanca dessa informacao gera
pesos sobre trajetorias futuras completas. O mesmo indice amostral e usado em
todas as variaveis e meses. Isso preserva a distribuicao conjunta e as externas
ja fixadas. A media resultante pode diferir de 2%; esta implementacao e uma
atualizacao por informacao ruidosa, nao uma restricao exata de momentos por
entropic tilting. Nao modifica o prior da estimacao.

## 3. Prior adicional de crescimento de longo prazo

Mantem as sete series nas escalas atuais, incluindo `100 * log(CPI)`, e acrescenta
uma tendencia deterministica em anos, centrada no fim da amostra:

`y_t = c + A1*y_(t-1) + ... + Ap*y_(t-p) + D*s_t + erro_t`.

Na tentativa final, o longo prazo e definido condicionalmente a Treasury e
Brent estabilizados, com crescimento nulo das externas alem do horizonte das
curvas. Para o bloco domestico estavel, o crescimento de tendencia e
`b_dom = solve(I - sum(A_dom_dom), D_dom)`. Dentro do forecast de 24 meses,
as curvas e observacoes originais continuam sendo usadas. A inflacao anual de tendencia e
`100 * (exp(b_CPI/100) - 1)`. Assim, 2% correspondem a `100*log(1.02)`, e nao
a `log(2)` nem ao nivel do indice de precos.

O novo prior e proporcional ao prior nativo multiplicado pelo indicador de
estabilidade e por `dnorm(b_CPI, 100*log(1.02), 0.5/1.02)`. A dispersao equivale
aproximadamente a 0,5 p.p. na escala da inflacao anual. E um prior adicional
coerente sobre uma funcao dos parametros. Nao afirmamos que sua marginal induzida
para a inflacao seja exatamente Normal(2, 0.5^2), nem que esta seja a parametrizacao
independente de steady state de Villani. A forma funcional esta documentada para
evitar confundir esse experimento com uma ancora terminal no forecast.

A primeira tentativa de reponderacao direta teve ESS insuficiente e foi rejeitada.
A implementacao final integra conjuntamente intercepto e tendencia na
condicional normal, incluindo a razao das probabilidades das narrativas. Depois
reamostra os demais parametros e sorteia intercepto e tendencia de sua condicional normal
truncada. E uma atualizacao Rao-Blackwellizada do prior, antes de simular qualquer
futuro. Os pesos nao usam a inflacao futura de um draw. O resultado final atendeu
ESS >= 50, inclusive apos agregar parametros repetidos. Geramos tambem
um controle com a mesma tendencia e estabilidade, sem o fator normal adicional.
Assim, a diferenca para o controle isola o prior; comparar apenas com o modelo
original misturaria os efeitos da tendencia e do prior.

Os sinais, narrativas, hiperparametros mu/delta/lambda e restricoes SOE continuam
os mesmos. A tendencia acrescenta um regressor deterministico, nao uma oitava
serie economica. COVID continua com seu tratamento existente. O prior nao fixa
a inflacao em 2028 e nao garante a melhora do desempenho preditivo.

Compatibilidade com a versao instalada: em bsvarSIGNs 3.0, `shocks` e `sigma`
nao sao reamostrados junto com `A`, `B` e `hyper`. Esses dois campos auxiliares
sao reconstruidos a partir dos parametros correspondentes antes de calcular a
condicional da tendencia. As previsoes anteriores usam A/Sigma/hyper e nao esses
campos auxiliares. As narrativas sao verificadas com os residuos reconstruidos.

Fontes metodologicas:
- Villani (2009): https://doi.org/10.1002/jae.1065
- Giannone, Lenza e Primiceri: https://www.ecb.europa.eu/pub/pdf/scpwps/ecb.wp2132.en.pdf
- bsvarSIGNs: https://bsvars.org/bsvarSIGNs/reference/forecast.PosteriorBSVARSIGN.html

Graficos e tabelas: `src/Outputs/Forecast BVAR estrutural/premissas inflacao e BoJ`.
Modelos com tendencia: `data/processed/premissas_inflacao_boj`.
