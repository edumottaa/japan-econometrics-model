# Atividade, persistência da inflação e atualização do BVAR

24/10/2026. Esta análise usa o CV e os forecasts existentes. Nenhum novo CV ou estimação do BVAR foi executado.

## 1. Alterações implementadas

- O script estrutural existente atualiza as cinco variáveis domésticas ao ser executado. O corte de referência fica explícito em `data_final_domesticas`, agora setembro/2026.
- A amostra usa somente as sete variáveis do modelo. Produção mundial, VIX e TOPIX não eliminam mais observações desse BVAR.
- Há 320 meses completos, de janeiro/2000 a agosto/2026. Setembro contém CPI, overnight, inclinação e USD/JPY; a atividade ainda não foi divulgada. Essas quatro observações entram como condições no primeiro mês do forecast; a atividade permanece desconhecida, sem preenchimento artificial.
- O CPI de base 2025 é encadeado ao de base 2020 no último mês comum quando houver meses adicionais. O histórico anterior é preservado.
- `lambda`, `mu` e `delta` estão explícitos para ajuste manual, ainda nos valores 0,2; 1; 1.
- Os arquivos brutos, cenários e scripts de petróleo e Treasury foram preservados. A igualdade dos arquivos protegidos foi conferida por hash.

A coleta, a especificação e a matriz de condições 24 × 7 foram verificadas. Os forecasts e gráficos salvos ainda são do ajuste anterior: é necessário reestimar o BVAR antes de gerar novas projeções.

## 2. O índice de atividade é adequado?

É o índice coincidente do Cabinet Office, um indicador legítimo do ciclo, mas sua variação não representa crescimento do PIB. Sua composição pode divergir da economia agregada; o próprio Cabinet Office documentou divergências em relação ao PIB e a influência de produção e expedições industriais.

**Recomendação:** manter a série por enquanto e implementar a ponte trimestral para o PIB da seção 3.6 do plano. Assim se projeta PIB a partir da atividade e se carrega a incerteza por trajetória, sem confundir as métricas. A ponte não corrige automaticamente a fraca previsão da atividade no CV.

A seção 3.1 também sugere o PIB mensal da JCER. É uma candidata mais direta, mas não confirmei nesta análise o acesso reproduzível ao histórico necessário; a página bloqueou a consulta automatizada. Não substituí a série sem verificar cobertura e definição. Produção industrial isolada seria uma medida mais estreita da economia.

Fontes: [índice atual](https://www.esri.cao.go.jp/jp/stat/di/di.html), [discussão do Cabinet Office sobre CI e PIB](https://www5.cao.go.jp/j-j/wp/wp-je20/h01-03.html).

## 3. Priors manuais: fundamento e limite

Regularizar mais pode ser defensável para limitar sobreajuste. Porém, a `lambda` atual é global; não representa um aperto isolado das equações de CPI e atividade. As escalas específicas por variável também afetam a estrutura da covariância e não devem ser tratadas como controles independentes para elevar cada forecast.

O prior atual privilegia passeio aleatório dos log-níveis. Com regularização extrema, isso favorece nível do CPI constante, não inflação próxima de 2%. Alterações moderadas podem elevar ou reduzir a previsão, conforme as relações que são reduzidas.

Para representar a crença de inflação persistente perto da meta, a justificativa adequada é um **prior de crescimento de longo prazo**, ou um condicionamento da distribuição preditiva. Por exemplo, 2% ao ano equivale a uma tendência de aproximadamente 0,165 por mês na escala de 100 × log do CPI. Regularizar desvios dessa tendência é conceitualmente diferente de apenas reduzir lambda. Isso exigiria adaptar as transformações e sua inversão; não foi implementado sem definir essa especificação.

O motivo econômico seria a credibilidade da meta e uma mudança persistente na formação de salários e preços. Isso deve ser declarado como informação econômica incorporada ao modelo, sem apresentar o resultado como confirmação independente dessa mesma crença.

## 4. Uma oitava variável: expectativas ou hiato?

| Opção | Informação adicionada | Limitação principal |
|---|---|---|
| Expectativa do Tankan em 1 ano | Pressão de preços esperada no curto prazo | Pode reagir bastante aos mesmos choques recentes do CPI e do petróleo |
| Expectativa do Tankan em 3 ou 5 anos | Persistência e referência de inflação de médio prazo | Série curta; não é a expectativa de inflação core exatamente em 2028 |
| Hiato do BoJ | Utilização da capacidade e pressão de demanda | Estimado, revisado, trimestral e divulgado com atraso |

Para a questão da persistência da inflação, expectativas são a candidata mais diretamente alinhada. Para melhorar o canal de demanda e a reação dos juros, o hiato pode ser útil, sobretudo se o índice coincidente representar mal a atividade agregada. Nenhuma opção garante alta dos juros ou inflação perto da meta.

O Tankan começa em março/2014, com primeira divulgação em abril. A amostra mensal cairia de 320 para perto de 150 observações, contendo apenas cerca de 50 pesquisas trimestrais. Repetir a última divulgação nos meses seguintes não cria novas pesquisas mensais. Devem ser usadas as datas de publicação, sem antecipar informação.

Com oito variáveis e seis lags são 49 coeficientes por equação, contra 43 hoje. Não acrescentei a oitava variável nem rodei outra comparação. Uma comparação futura precisa usar a mesma amostra nas versões com sete e oito variáveis, separando o efeito da variável do efeito da amostra mais curta.

Há evidência externa favorável a expectativas, mas não uma garantia para o Tankan: estudo do BCE encontrou ganhos com expectativas de profissionais na área do euro, sem melhora sistemática para firmas e famílias. Isso orienta a investigação e não decide o resultado japonês.

Fontes: [primeira pesquisa do Tankan](https://www.boj.or.jp/en/statistics/tk/bukka/2014/tkc1403.pdf), [hiato do BoJ](https://www.boj.or.jp/en/research/research_data/gap/index.htm), [estudo do BCE, revisto em 2023](https://www.ecb.europa.eu/pub/pdf/scpwps/ecb.wp2604~a7dc92b529.en.pdf).

## 5. Precisa construir todo o S2?

Não para incorporar uma visão de inflação persistente. As seções 3.7 e 5 do plano já permitem julgamento por condicionamento suave. Um estudo do Fed de Nova York aplica esse método para aproximar a mediana da inflação de 2%, preservando incerteza e as relações conjuntas do VAR.

**Proposta de menor esforço:** atualizar o núcleo, manter o forecast sem julgamento como referência e produzir uma versão com informação prospectiva explícita: inflação de médio prazo próxima da meta e juros com trajetória ou distribuição fundamentada na OIS/BoJ. A trajetória dos juros ainda precisa ser especificada; não há justificativa para inventar um nível terminal.

O ajuste deve reponderar trajetórias conjuntas de todas as variáveis. Não consiste em elevar a linha do CPI no gráfico e conservar juros, atividade e câmbio inalterados. Também não deve forçar os quatro cenários de petróleo à mesma inflação: isso pode neutralizar o próprio choque que os distingue. A visão deve ser calibrada no base e aplicada aos cenários de maneira coerente.

Esse método aproxima o forecast da visão escolhida; não prova que a visão ocorrerá. É preciso verificar se há trajetórias compatíveis em quantidade suficiente e se os pesos não ficam concentrados em poucos sorteios.

Fonte: [Fed de Nova York, seção 4.2 — incorporação de julgamento](https://www.newyorkfed.org/medialibrary/media/research/staff_reports/sr976.pdf).

## 6. Se o S2 for retomado: ARIMA ou VAR?

Eu começaria com regressões dinâmicas com erros ARIMA, sem criar outro VAR completo:

| Entrada | Projeção proposta | Ligação com o núcleo |
|---|---|---|
| Expectativas | AR(1) trimestral como referência, comparado à persistência da última pesquisa; eventual referência de longo prazo declarada | Usar informação já publicada; não presumir convergência a 2% se isso não estiver especificado |
| Importações em ienes | Regressão da variação do índice sobre variações de USD/JPY e Brent, com poucas defasagens e erros ARMA/ARIMA | Mesmas trajetórias de câmbio e petróleo do BVAR, por sorteio; Brent é uma proxy parcial da cesta importada |
| Hiato | AR(1) trimestral como referência; versão condicional ligada à atividade trimestral projetada ou à ponte do PIB | Não projetar uma atividade alternativa incompatível com o núcleo; respeitar publicação e revisões do hiato |
| Medidas institucionais | Calendário e contribuições baseados em medidas oficiais e hipóteses explícitas de vigência | Não usar ARIMA para prever decisões do governo; medidas futuras desconhecidas são cenários |

A inflação do S2 seria uma regressão dinâmica com esses regressores e erros ARIMA, como nos scripts existentes. Como o S2 trabalha com core-core e o BVAR com core, é necessário tratar energia e demais diferenças de cobertura antes de combinar as projeções.

O iene do S2 deve vir do BVAR, que já reage aos juros japoneses e americanos. Se a inflação do satélite realimentar o núcleo, a integração deve ocorrer na distribuição conjunta; combinar médias independentes ou repetir ajustes sem critério de convergência não resolve a circularidade. Tampouco se deve contar informação compartilhada como se fosse uma fonte estatisticamente independente.

O [fpp3, seção 10.3](https://otexts.com/fpp3/dynamic-regression.html#forecasting) explica que regressões dinâmicas precisam dos caminhos futuros dos regressores e que intervalos condicionais não incluem automaticamente a incerteza desses caminhos. Aqui seria necessário propagá-la por trajetórias.

## Recomendação

Concluir o núcleo atualizado primeiro. Para a melhoria estrutural, investigar uma única medida de expectativas antes de montar o S2 inteiro, considerando o custo da amostra curta. Para incorporar já uma visão de inflação perto da meta e normalização dos juros, o condicionamento suave da seção 3.7 é a opção que preserva o modelo atual com menor mudança. Apertar lambda, isoladamente, não substitui nenhuma dessas escolhas.
