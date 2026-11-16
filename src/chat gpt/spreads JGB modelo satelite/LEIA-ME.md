# JGB 30Y–10Y e 30Y–5Y: regressão com erros ARIMA

Os alvos são as variações mensais dos spreads 30Y–10Y e 30Y–5Y do JGB. O método é regressão dinâmica (FPP3, caps. 10.3 e 10.6). A sequência é a mesma do ARIMA da produção mundial no BVAR (`src/7- forecast/bvar estrutural/experimento choque unico Treasury/0- producao mundial.R` e `0b- modelo producao mundial.R`).

## Rodar

Na raiz do projeto, nesta ordem (cada script lê o `.rds` do anterior em `resultados/`):

```r
source("src/JGB30Y - 10Y forecast/0- dados.R", encoding = "UTF-8")
source("src/JGB30Y - 10Y forecast/1- selecao defasagens.R", encoding = "UTF-8")
source("src/JGB30Y - 10Y forecast/2- modelo ARIMA.R", encoding = "UTF-8")
source("src/JGB30Y - 10Y forecast/2b- modelo fluxo estrangeiro.R", encoding = "UTF-8")
source("src/JGB30Y - 10Y forecast/3- validacao cruzada.R", encoding = "UTF-8")
source("src/JGB30Y - 10Y forecast/5- teste regime YCC.R", encoding = "UTF-8")
```

| Script | Etapa FPP3 | Saídas principais |
|---|---|---|
| `0- dados.R` | Dados e visualização | `base mensal.csv`, `series em nivel.png`, `series em variacao.png`, `acf pacf *.png` |
| `1- selecao defasagens.R` | Diferenciação, sazonalidade e defasagens | `testes diferenciacao.csv`, `teste defasagens.csv`, `escolhas defasagens.csv` |
| `2- modelo ARIMA.R` | Estimação e diagnóstico residual dos spreads | `coeficientes ARIMA.csv`, `diagnostico ARIMA.csv`, `residuos ARIMA *.png` |
| `2b- modelo fluxo estrangeiro.R` | Modelo da compra líquida dos estrangeiros | `fluxo coeficientes ARIMA.csv`, `fluxo diagnostico ARIMA.csv`, `fluxo ajuste.png` |
| `3- validacao cruzada.R` | Avaliação com origem móvel | `cv metricas.csv`, `cv RMSE por horizonte.png` |
| `5- teste regime YCC.R` | Robustez: regime YCC e peso exponencial | `regime coeficientes.csv`, `regime cv metricas.csv` |
| `4- forecast.R` | Previsão | **Desatualizado.** Será reescrito quando chegar a curva JGB a termo (ver "Forecast"). Os arquivos `forecast spreads.*` em `resultados/` são da versão antiga. |

## Dados

Todas as séries de mercado entram como **médias mensais dos dados diários**. É o mesmo critério do USD/JPY no BVAR, conferido: a média de set/2026 do DEXJPUS é 156,42, igual ao valor do BVAR. Os níveis aparecem desde jan/2016.

| Variável | Fonte | No modelo |
|---|---|---|
| Spreads 30–10 e 30–5 | MoF, vencimento constante (`src/JGB30Y/teste 1/dados/jgbcme_all_2026-11-04.csv`) | Δ em pb (alvo) |
| UST 30Y | FRED DGS30 | Δ em pb |
| OIS JPY 2Y | Bloomberg `JYSO2 Curncy` (`src/JGB30Y/projecoes/dados/ois2_historico.csv`) | Δ em pb |
| USD/JPY | FRED DEXJPUS (`dados/`) | 100 × Δlog (%) |
| Compra líquida dos estrangeiros | JSDA, superlongos (`src/JGB30Y/resultados/fluxos_jsda_superlongos.csv`) | **Nível**, em trilhões de ienes |
| Fatia estrangeira | JSDA, mesma base | Δ em p.p. |
| JGB30 e diferenciais (sem e com hedge) | MoF, FRED (`DGS30`, `FEDFUNDS`) e BoJ (`STRACLUCON`) | Nível; só no modelo do fluxo |

**Compra líquida em nível.** A compra líquida já é um fluxo, ou seja, a variação do estoque. Diferenciá-la de novo mediria a mudança do fluxo, o que não é a hipótese.

**Fatia estrangeira.** A fórmula é `fatia = 100 × líquido dos estrangeiros / Σ compras líquidas positivas dos investidores finais`. O denominador exclui `Total`, a linha de variação mensal e `Bond Dealers`, que são intermediários. O líquido `Total` da JSDA fica perto de zero e muda de sinal (mediana −0,01 tri), por isso não serve como denominador. O denominador escolhido é sempre positivo: mínimo de 0,72 tri e mediana de 1,79 tri. A fatia fica negativa quando os estrangeiros vendem em termos líquidos. O CSV de fluxos guarda também a fatia nas compras brutas (`fatia_bruta_estr`) como alternativa.

**Amostra do modelo: jun/2018 a set/2026 (100 meses).** A JSDA comparável começa em mai/2018. As publicações anteriores não foram unidas a ela. A série dos superlongos cobre 20, 30 e 40 anos e o 15 anos flutuante pelo prazo original, então não é exclusiva do 30Y.

## Modelo dos spreads

**Diferenciação.** As variações dos spreads pedem d=0. Nenhuma série pede diferença sazonal. A variação do OIS2Y rejeita o KPSS (p=0,01) por causa da troca de regime: juro zero até 2023 e ciclo de altas depois. Por isso, ao contrário do script da produção mundial, **d vem só dos alvos**. Com d=1 no erro, os spreads seriam diferenciados duas vezes.

**Constante.** A constante é obrigatória na estimação (`+ 1`). Sem ela, a compra líquida em nível, que tem média de +0,37 tri/mês, absorve a tendência de inclinação de 2018–26 e o β do fluxo sai viesado. Na projeção, a constante sai, como a tendência no BVAR (ver "Forecast").

**Defasagens.** O teste usa a mesma regra de escolha da produção mundial: ΔAICc < 2, menos termos, maior menor lag e menor AICc. As famílias usadas são A (contemporânea + lags 1–2) e B (só lags 1–3). Erro: ARIMA(1,0,0) nos dois spreads.

| Coeficiente | 30–10 | 30–5 |
|---|---|---|
| ΔUST30 (lag 0), pb por pb | **+0,12** (p<0,001) | **+0,15** (p<0,001) |
| ΔOIS2Y (lag 0), pb por pb | **−0,50** (p<0,001) | **−0,62** (p<0,001) |
| ΔUSD/JPY (lag 0), pb por % | −0,21 (p=0,51) | −0,16 (p=0,65) |
| Compra líquida estrangeira (lag 2), pb por ¥ tri | **+2,0** (p=0,07) | **+3,0** (p=0,04) |
| Δ fatia estrangeira, pb por p.p. | +0,03, lag 3 (p=0,04) | −0,01, lag 2 (p=0,57) |
| Constante, pb/mês | +0,40 (p=0,61) | +0,69 (p=0,43) |

- Uma alta do OIS achata a parte longa, e um Treasury mais alto inclina.
- **A compra estrangeira tem sinal positivo**, contrário à tese de que ela comprime o 30Y. Uma leitura possível é que os estrangeiros compram depois que a curva inclina, quando o 30Y fica barato. O coeficiente mede associação, não causalidade.
- **Diagnóstico:** no Ljung-Box com 12 e 24 lags, os p-valores ficam acima de 0,53. Os resíduos parecem ruído branco.

## Modelo do fluxo estrangeiro (`2b`)

É uma regressão da compra líquida dos estrangeiros (em nível) sobre o juro que a move, com erros ARIMA. O script compara quatro candidatos na mesma amostra: lag 0, d=0 e constante.

| Especificação | Coeficiente (tri por p.p.) | AICc | Erro estacionário? |
|---|---|---|---|
| **JGB30 (nível)** | **+0,21 (p=0,001)** | **121,4** | Sim |
| UST30 e JGB30 separados | UST30 −0,17 (p=0,15); JGB30 +0,38 (p=0,005) | 121,6 | Sim |
| UST30 − JGB30 | −0,18 por 100 pb (p=0,17) | 129,1 | Não (KPSS p=0,01) |
| JGB30 com hedge − UST30 | +0,05 por 100 pb (p=0,34) | 130,1 | Não (KPSS p=0,05) |

O diferencial com hedge é `JGB30 + (Fed Funds − call rate do BoJ) − UST30`. Quem está em dólar e faz hedge do iene ganha a diferença entre os juros curtos. Fontes: FRED `FEDFUNDS` e BoJ FM02/`STRACLUCON`, médias mensais, em `dados/`.

- **Escolha: JGB30 em nível.** O erro é estacionário e o AICc é o menor. Os dois diferenciais ficam cerca de 8 pontos de AICc atrás e têm erro não estacionário.
- **Leitura:** separados, os sinais são os que o diferencial prevê (UST30 mais alto afasta a compra, JGB30 mais alto atrai), mas o efeito do JGB30 é o dobro e o do UST30 não é significativo. A restrição do diferencial (efeitos iguais e opostos) não se sustenta.
- **Ano a ano:** o diferencial com hedge foi máximo em 2023–24 (+250 pb), quando a compra foi a menor da amostra (0,10–0,15 tri/mês). A compra disparou em 2025–26, quando o JGB30 passou de 3%.
- **Modelo final:** `liq_estr ~ lag(jgb30, 3)`, ARIMA(0,0,2) com constante: **+0,22 tri por p.p. do JGB30, com 3 meses de defasagem**.
- **Limitação:** o Ljung-Box ainda rejeita (p=0,01 com 12 lags). O salto das compras em 2025 deixa autocorrelação que o modelo não captura, então as faixas do fluxo projetado ficam estreitas demais.

## Validação cruzada

São 34 origens mensais, de dez/2022 a set/2025, com horizontes de 1 a 12 meses. As ordens ARIMA ficam fixas e os coeficientes são reestimados em cada origem. O erro é medido no nível do spread. "Sem tendência" é a regra da projeção: sem constante e com os regressores medidos como desvio da média do treino.

| RMSE (pb) | 30–10 h=1 | h=6 | h=12 | 30–5 h=1 | h=6 | h=12 |
|---|---|---|---|---|---|---|
| Regressão ex post | 6,9 | 18,6 | **19,5** | **8,0** | **19,9** | **20,3** |
| Regressão ex post, sem tendência | 6,9 | **18,5** | 19,9 | **8,0** | 20,7 | 24,6 |
| Preditores constantes, sem tendência | **6,7** | 21,7 | 37,1 | 8,4 | 27,7 | 46,5 |
| ARIMA sem regressores | 6,8 | 21,1 | 24,1 | 8,3 | 24,0 | 26,8 |
| Variação zero | 7,2 | 20,6 | 23,8 | 8,5 | 23,5 | 26,7 |

> **Ex post** usa os preditores futuros **realizados**. Mede o desempenho condicional, não o que se teria com as curvas a termo disponíveis em cada origem. A escolha dos lags usou a amostra inteira, o que favorece um pouco o modelo. A linha "preditores constantes" mantém o fluxo no último valor da origem; com o fluxo acima da média, isso cria uma tendência e piora em 12 meses.

## Regime YCC e peso exponencial (`5`)

O YCC acabou em 19/03/2024, e abr/2024 é o primeiro mês inteiro sem ele. O script compara quatro especificações:
- amostra completa;
- betas de UST30 e OIS separados por regime;
- estimação só pós-YCC (30 meses);
- MQO com peso exponencial ρ=0,98, igual ao `01_jgb2.R`, sem erros ARIMA e com erro-padrão Newey-West.

| β no 30–10 (30–5) | Completa | Pré-YCC | Pós-YCC (betas por regime) | Só pós-YCC | Ponderada ρ=0,98 |
|---|---|---|---|---|---|
| ΔOIS2Y | −0,50 (−0,62) | −0,14 (−0,18), n.s. | −0,58 (−0,73) | −0,69 (−0,92) | −0,58 (−0,73) |
| ΔUST30 | +0,12 (+0,15) | +0,11 (+0,13) | +0,10 (+0,17) | +0,17 (+0,24) | +0,14 (+0,18) |

- **O teste formal não rejeita betas iguais:** razão de verossimilhança com p=0,18 no 30–10 e p=0,16 no 30–5; o AICc fica 1–2 pontos pior com a separação.
- **Ainda assim, o OIS pesa mais no pós-YCC.** O β pré-YCC é impreciso porque o OIS mal se mexeu até 2023, então o β da amostra completa já vem quase todo do pós-YCC.
- **Validação cruzada:** usa só as 13 origens de set/2024 a set/2025, que têm pelo menos 6 meses pós-YCC no treino, e é ex post.

| RMSE (pb) | 30–10 h=1 | h=6 | h=12 | 30–5 h=1 | h=6 | h=12 |
|---|---|---|---|---|---|---|
| Amostra completa | 7,9 | 20,5 | 21,8 | **10,2** | 24,2 | 25,0 |
| Betas por regime | 8,9 | 25,0 | **13,0** | 11,3 | 27,9 | **22,2** |
| MQO ponderado | **7,4** | **15,4** | 23,4 | 10,3 | **21,0** | 24,7 |
| Variação zero | 10,0 | 27,4 | 26,5 | 11,9 | 31,9 | 29,7 |

**Leitura.** Nenhuma alternativa domina: o peso exponencial ganha até 6 meses, e os betas por regime ganham em 12 meses. O modelo principal continua com a amostra completa. Nos cenários de surpresa, o β pós-YCC entra como sensibilidade: uma surpresa de +25 pb no OIS dá −12 pb (completa) ou −15 pb (pós-YCC) no 30–10.

## Forecast — em reconstrução

Na versão antiga, as altas do OIS **já precificadas** na curva a termo eram tratadas como surpresa (−0,45 × 49 pb ≈ −22 pb no 30–10). Isso conta duas vezes o que já está na curva JGB de hoje. A versão nova terá:

- **Base:** spread a termo da curva JGB, somado ao efeito dos fluxos estrangeiros projetados (β × desvio da média), que não estão no preço da curva. O efeito do USD/JPY do BVAR entra do mesmo jeito. A constante fica fora.
- **Cenários:** **surpresas** de OIS2Y e UST30 em relação ao caminho a termo, multiplicadas pelo β (completo e pós-YCC).
- **Fluxo estrangeiro projetado:** pelo modelo `2b`, com o JGB30 a termo da curva JGB. Os três primeiros meses usam o JGB30 já observado, por causa da defasagem de 3 meses.
- **Média mensal × taxa pontual:** o histórico usa médias do mês e as curvas a termo são de um dia (30/10). O script usa a média entre o início e o fim de cada mês e ancora no nível de 30/10, o que reduz essa diferença.

## Bloomberg — curvas a termo

As planilhas antigas (`OIS jpy fwrd.xlsx`, `US_treasury_actives_forward_2026-11-06.xlsx`) trazem na coluna `Forward Rate` a taxa a termo de cada período da curva, não o 30Y ou o 2Y a termo. **Não servem.**

**Como extrair:** `FWCV <GO>`, aba **Horizon Curve** (não a Implied Forwards). Em Add Columns, adicione os horizontes 1, 2, 3, 6, 9, 12, 15, 18, 22 e 24 Mo e leia a linha do prazo desejado. A data da curva é **30/10/2026**.

| Variável | Curva | Linha | Status |
|---|---|---|---|
| UST 30Y | YCGT0025, US Treasury Actives (taxa par) | 30 Yr | **Recebida.** Fotos da tela transcritas e conferidas pelo usuário. Spot 5,30775, contra 5,31 no FRED em 30/10. |
| OIS JPY 2Y | YCSW0195, JPY OIS (taxa par) | 2 Yr | **Recebida.** Fotos da tela transcritas. Spot 1,87125, igual ao JYSO2 em 30/10. |
| JGB 5Y, 10Y e 30Y | Curva soberana do Japão (procure em Curve List; deve ser a YCGT0018) | 5 Yr, 10 Yr, 30 Yr | **Pendente.** Na conferência, o spot deve bater com o MoF em 30/10: 2,237%, 2,817% e 3,908%. |

**Como o script 4 usa a tabela** (`dados/curvas_bloomberg.csv`, com as colunas `variavel, curva, data_snapshot, horizonte_meses, taxa_pct`):
- interpola linearmente os meses que não foram exportados;
- calcula a média de cada mês como a média entre o início e o fim do mês;
- usa só as **variações** da curva, somadas ao nível diário (FRED, JYSO2 ou MoF) na data do snapshot, para que a diferença entre as fontes não entre como variação.

**Novembro de 2026:** o UST30 projetado cai 15,8 pb porque a média de outubro (com 5,6% no começo do mês) fica acima do nível de 30/10. Isso já aconteceu durante outubro; não é uma previsão de queda.

A curva do Treasury vai só até 30 anos, e a Bloomberg extrapola o trecho acima disso (a tela mostra uma linha de 50 Yr). Os forwards de 30Y com início depois de hoje dependem dessa extrapolação.

**Opcional, para uma validação ex ante:** a mesma extração em cada fim de mês de dez/2022 a set/2025. Com ela, daria para estimar os betas sobre surpresas puras, em vez de variações realizadas.
