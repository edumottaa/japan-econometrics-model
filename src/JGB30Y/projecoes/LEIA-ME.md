# Projecoes da curva JGB

Na raiz do projeto, execute:

```r
source("src/JGB30Y/projecoes/rodar.R", encoding = "UTF-8")
```

O runner reutiliza os resultados do BVAR principal **30Y-5Y com tendencia** em
`src/Outputs/4- Forecast/jgb30y_5y com tendencia/producao/resumo_forecast.csv`.
Nao estima o BVAR. O `code_run.R` da raiz tambem chama esta etapa depois do forecast.

## Ordem dos scripts

1. `00_dados.R`: historico mensal desde 2016, OIS e quatro cenarios do BVAR.
2. `01_jgb2.R`: JGB2Y em OIS2Y defasado dois meses, com erros ARIMA(0,0,1).
3. `02_spreads.R`: spread 30-5 do BVAR e pontes para os segmentos 5-2 e 10-5.
4. `03_curva.R`: JGB5 = JGB2 + (5-2); JGB10 = JGB2 + (10-2); JGB30 = JGB5 + (30-5).
5. `04_graficos.R`: rendimentos e spreads, quatro cenarios, rotulos finais.

## Premissas preservadas

- Estrutura recuperada de `bvar_30_5/8_curva_jgb.R`, que estava na Lixeira.
- Amostra mensal desde janeiro de 2016; fechamento do ultimo dia comum de cada mes.
- OIS2Y futuro: o do mercado (forwards TONA) migra linearmente em 3 meses para o
  OIS2Y implicito no juro curto do BVAR (pesos 1/3, 2/3 e 1). Depois do horizonte
  BVAR, o juro cresce pelo CAGR do ultimo ano do modelo, para fechar a janela de 2 anos.
- OIS2Y aproximado por fatores de desconto mensais, ACT/365 e dois pagamentos
  anuais. Nao e uma cotacao futura observada nem replica exata das convencoes do swap.
- As pontes sao MQO sem intercepto nas variacoes mensais de 5-2 e 10-5, explicadas
  pelas variacoes de JGB2 e 30-5. Coeficientes e p-valores convencionais sao exportados.
- O BVAR usa medias mensais do 30-5; sua variacao em relacao ao Base na origem
  e aplicada ao fechamento observado. Essa ancoragem preserva a convencao original.
- O horizonte agora acompanha os resultados disponiveis do BVAR, limitado pela
  cobertura de 24 meses adicionais necessaria para reconstruir cada OIS2Y.
  Nao se mantem o corte antigo, fixo, em agosto de 2028.
- A curva usa medias condicionais. Nao propaga conjuntamente a incerteza do BVAR,
  das regressoes e do OIS. Nao houve nova selecao de modelos nem novo CV.

## Dados recuperados e datas

Arquivos copiados individualmente da Lixeira, com igualdade de hash conferida:

- `dados/jgbcme_all_2026-11-04.csv`: MoF, taxas de maturidade constante 2/5/10/30 anos,
  nao titulos por ISIN. Fonte: https://www.mof.go.jp/english/policy/jgbs/reference/interest_rate/
- `dados/ois2_historico.csv`: serie diaria Bloomberg JYSO2 Curncy, PX_LAST,
  ja tratada na versao anterior. Agregada pelo ultimo registro mensal disponivel.
- `dados/OIS jpy fwrd.xlsx`: extracao Bloomberg de forwards TONA mensais,
  snapshot de **06/11/2026**; a data e interpretada no formato americano.

A base MoF recuperada termina em **30/10/2026**. Sao os dados da analise original,
nao uma coleta atualizada em novembro. Os insumos do BVAR podem ter outro corte;
nao apresentar os cenarios como um snapshot sincronizado de mercado.

## Saidas

Em `resultados/`: base mensal, modelo JGB2Y, coeficientes, OIS por cenario,
JGB2Y, spreads, curva completa em CSV e dois graficos PNG com rotulos finais.
Spreads no CSV em pontos percentuais; nos graficos em pontos-base.

Os scripts antigos em `teste 1/forecast/09`, `11`, `12` e `13` pertencem ao fluxo
anterior baseado em 30-2 versus 10-2. Nao sao etapas deste runner 30-5.
