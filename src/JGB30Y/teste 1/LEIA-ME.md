# Teste 1 — associação diária do JGB30

## Regressão explicativa com dados públicos

Rodar, na raiz do projeto, [preparo dos dados](<../data collection/01_preparar_dados.R>), [regressão e gráfico de variações](01_rodar_teste1.R) e [gráfico em nível](02_graficos_nivel.R), nessa ordem.

O código estima **uma única regressão explicativa** em toda a amostra. A equação é `ΔJGB30 = α + β₂ ΔJGB2 + βUS ΔUST30 + erro`. São 4.115 pregões de 2010 a outubro/2026. O JGB2 vem da mesma sessão japonesa e o UST30 usa a última observação americana com data anterior. Os erros padrão usam Newey–West.

**Resultado:** β₂ = 0,605 (p < 0,001), βUS = 0,113 (p < 0,001) e R² ajustado = 0,151. São associações condicionais, não efeitos causais. Ver [coeficientes](resultados/teste1_coeficientes_hac.csv) e [diagnósticos](resultados/teste1_diagnosticos.csv).


**Dados já disponíveis.** O JGB2 e o JGB30 vêm da curva de vencimento constante do [MoF japonês](https://www.mof.go.jp/english/policy/jgbs/reference/interest_rate/index.htm). O UST30 vem da série diária [DGS30, Fed H.15](https://fred.stlouisfed.org/series/DGS30). Os arquivos brutos ficam em [dados](dados/). Comparações anteriores ficam em [histórico](historico/).

## Projeção pelo VAR condicionado

O [VAR mensal](../projecoes/03_var_condicionado.R) usa a projeção do JGB2Y com a curva OIS e o cenário do JGB30Y obtido pela soma do 2Y com o spread 30–2. Falta a **curva a termo do UST30Y** para substituir a premissa provisória de 5,32% constante. A extração precisa da taxa par de Treasury soberano de 30 anos com uma data de início por mês, além da data-base e da taxa em % a.a. Para alinhar à curva OIS, a data-base preferida é 06/11/2026. O [gráfico condicionado](../projecoes/resultados/slide1_decomposicao_condicionada.png) já está disponível com a premissa provisória.
