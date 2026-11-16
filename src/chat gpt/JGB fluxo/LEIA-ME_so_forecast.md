# Somente o forecast muda

```r
source('src/chat gpt/JGB fluxo/code_run_2.R', encoding='UTF-8')
```

## O que fica fixo

Todos os coeficientes, priors, covariancias, pesos e os 2.500 sorteios do BVAR
original. O controle e exatamente o forecast salvo em
`src/Outputs/4- Forecast/jgb30y_5y com tendencia/producao/`.
Os hashes do modelo e das trajetorias sao conferidos antes/depois; as medias
do controle sao reconciliadas com `forecast_bvar.rds`.
Nao se executa `estimate()`, `model()` ou o BVAR-X reestimado.

## Calculo do cenario adicional

1. Reutilizar o MA(2) ja estimado para projetar compras liquidas estrangeiras de
   superlongos, usando a curva UST30 existente.
2. Reutilizar o beta do fluxo da regressao do satelite em **variacoes** do spread:
   **3,705 pb por JPY tri/mes**. Ele foi estimado; nao digitado no codigo.
3. Referencia neutra: media historica do fluxo defasado dentro da amostra BVAR,
   **0,3514 tri/mes**, como na proposta inicial de centralizacao.
4. Para os meses apos o snapshot de out/2026:

   `adicional mensal (pb) = beta * (fluxo_lag2 - media historica)`

   `spread alternativo = spread ORIGINAL salvo + cumsum(adicional mensal)`

5. Propagar essa diferenca para as demais domesticas pelas seis defasagens
   ORIGINAIS do BVAR, por sorteio. Somente a diferenca do spread e imposta.
   UST2Y, Brent e IP mundial permanecem identicos aos caminhos salvos.

OIS e constante do satelite **nao sao somados** ao BVAR: cancelam na comparacao
de dois caminhos do satelite que diferem somente no fluxo. Nao se adiciona um
nono choque nem se estima uma nova equacao dentro do BVAR.

## Resultados Base, setembro de 2028

- Original salvo: **186,6 pb**.
- Cenario adicional de fluxo: **212,2 pb**.
- Diferenca: **+25,6 pb**, sem a mudanca de coeficientes de -22,8 pb do experimento antigo.

Esta e uma **sensibilidade incremental**, nao um BVAR-X reestimado. O BVAR sem
fluxo nao identifica qual fluxo ja estaria embutido em seu forecast. Usar a media
historica como referencia e uma hipotese explicita; o adicional pode sobrepor
informacao ja capturada implicitamente pelo modelo. Nao e medida pura de risco fiscal.

A propagacao das demais domesticas e feita pelas defasagens, mantendo as
inovacoes originais dessas equacoes, sem nova inferencia contemporanea por Sigma.
Isso difere de recondicionar conjuntamente toda a distribuicao do BVAR.

Nao sao calculadas novas bandas preditivas: a incerteza do fluxo e do beta
externo nao e incorporada. A banda terminal original do BoJ nao e reimposta
ao cenario alternativo. O centro Base do juro curto passa de **1,925% para 1,828%**;
**36,6%** dos sorteios deslocados ficam fora da antiga banda de 1,75%-2,25%.
Por isso os arquivos mostram centros de sensibilidade, nao uma nova posterior
condicionada aquela banda. O controle original continua integralmente preservado.

## Arquivos

- `04_forecast_coeficientes_fixos.R`: calcula os efeitos e os centros.
- `05_graficos_coeficientes_fixos.R`: graficos do spread e das oito variaveis.
- `resultados/so forecast/`: comparacao, impactos finais, efeito do fluxo,
  diagnosticos, objeto RDS e graficos PNG, para os quatro cenarios.
- `code_run_2_reestimar_BVAR.R`: experimento antigo; nao faz parte desta execucao.
