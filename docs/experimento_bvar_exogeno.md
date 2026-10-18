# Experimento: BVAR com exogeneidade de bloco

Data: 17/10/2026. Teste exploratório; os scripts originais não foram alterados neste experimento.

## Especificação

- Pacotes instalados: bsvarSIGNs 3.0 e bsvars 4.0.
- Mesmas dez variáveis e mesmas transformações em nível/100 × log do núcleo.
- Duas defasagens, escolhidas para uma versão enxuta e para evitar a concentração de pesos observada antes com 12 lags. Não foram selecionadas procurando o menor erro no teste.
- Bloco externo: US2Y, produção industrial mundial, Brent e VIX.
- Bloco doméstico: atividade, CPI core, juro curto, inclinação da curva, USDJPY e TOPIX.
- `foreign` impõe que as variáveis domésticas não afetem o bloco externo por defasagens ou no impacto. As variáveis externas têm equações próprias e são previstas dentro do sistema.
- Lambda fixo em 0,2; mu e delta fixos em 1; psi automático em cada treino e fixo durante a estimação. Nenhum hiperparâmetro é amostrado nesta versão.
- Nenhuma restrição econômica de sinal ou narrativa foi acrescentada.
- A previsão usa a função recursiva existente, já verificada para corrigir a inicialização e a atualização dos lags dos métodos instalados. Não foi alterado nenhum pacote.

## Holdout de 24 meses

Treino: janeiro/2000–julho/2024. Teste: agosto/2024–julho/2026. Foram usados 2.000 sorteios.

| Modelo | RMSE CPI | RMSE atividade |
|---|---:|---:|
| AR | 0,915 | 1,36 |
| BVAR exógeno experimental | 1,25 | 6,29 |
| BVAR existente | 1,35 | 6,55 |
| BVAR condicionado existente | 1,75 | 7,77 |

Os dois BVARs existentes foram comparados a partir de estimações já salvas, com lambda, SOC e SUR estimados e psi fixo. A igualdade do treino foi conferida descontando as observações usadas como defasagens. Não houve nova estimação desses modelos.

No novo BVAR, o tamanho efetivo da ponderação foi aproximadamente 1.025/2.000, com 1.035 conjuntos distintos de coeficientes. Os coeficientes e impactos domésticos nas equações externas foram zero. Esses são diagnósticos básicos; não demonstram validade econômica ou superioridade preditiva.

Também foi testado lambda estimado, mantendo os demais hiperparâmetros fixos. Seu RMSE no holdout foi 1,25 para CPI e 6,70 para atividade; isso não mudou a conclusão do holdout. A versão entregue mantém lambda fixo por simplicidade.

## CV curto

Quatro origens predefinidas: julho/2016, julho/2019, julho/2022 e julho/2024. Cada uma prevê 12 meses completos. Foram usados 500 sorteios por janela. Todos os modelos usam os mesmos treinos e testes. Os parâmetros e as escalas psi são calculados somente no treino.

| Modelo | RMSE CPI | MAE CPI | CRPS CPI | RMSE atividade |
|---|---:|---:|---:|---:|
| BVAR exógeno | **0,828** | **0,625** | **0,431** | 8,26 |
| AR | 0,885 | 0,681 | 0,470 | **7,03** |
| VAR | 1,23 | 0,784 | 0,613 | 8,74 |
| Drift | 1,25 | 0,895 | 0,696 | 7,73 |
| Naive | 1,36 | 0,973 | 0,771 | 7,60 |

O ganho de RMSE do CPI sobre o AR foi de 6,4% nessa seleção de origens. Não se deve comparar diretamente essa tabela com a média do CV anterior de 47 janelas.

Os tamanhos efetivos de ponderação foram 205, 217, 269 e 256 em 500 sorteios, respectivamente. Todas as janelas passaram pelas checagens numéricas e de exogeneidade.

## Conclusão

O modelo é reproduzível e passou pelas checagens básicas de diversidade dos sorteios e das restrições de bloco. É um candidato para estudar a previsão do CPI: ganhou do AR no CV curto, mas perdeu no holdout. Para atividade, não houve melhora sobre o AR em nenhuma das duas avaliações agregadas. A evidência é mista e insuficiente para substituir o núcleo como um todo.

O passo seguinte, caso se queira aprofundar este candidato, é ampliar a avaliação com a mesma especificação e sem selecionar apenas períodos favoráveis. A comparação entre os benchmarks e o novo BVAR deve usar as mesmas origens e horizontes.

## Arquivos

- [Modelo](<C:/Users/dudup/OneDrive/japan-econometrics-model/src/models/bvar exogeno experimental.R>) — função com a especificação e os diagnósticos.
- [Avaliação](<C:/Users/dudup/OneDrive/japan-econometrics-model/src/evaluation/teste bvar exogeno.R>) — holdout, quatro origens de CV, métricas e checkpoints.
- [Previsão recursiva](<C:/Users/dudup/OneDrive/japan-econometrics-model/src/evaluation/prever_bsvarsigns.R>) — função existente reutilizada.
- [Resultados do CV](<C:/Users/dudup/OneDrive/japan-econometrics-model/data/processed/cv_exogeno_experimental.csv>).
- [Comparação do holdout](<C:/Users/dudup/OneDrive/japan-econometrics-model/data/processed/comparacao_holdout_exogeno_experimental.csv>).
- [Diagnósticos do CV](<C:/Users/dudup/OneDrive/japan-econometrics-model/data/processed/diagnosticos_cv_exogeno_experimental.csv>).
- Ajuste do holdout: `data/processed/ajuste_bvar_exogeno_holdout.rds`, com posterior, treino e configuração. Esse ajuste termina em julho/2024; não é uma estimação na amostra completa.

Com `data_bal` já preparado, para repetir a avaliação:

```r
setwd("C:/Users/dudup/OneDrive/japan-econometrics-model")
source("src/evaluation/teste bvar exogeno.R")
```

Para carregar somente a função e estimar depois na amostra completa:

```r
source("src/models/bvar exogeno experimental.R")
ajuste_exogeno <- estimar_bvar_exogeno(data_bal)
```

Referência da interface: [documentação oficial de specify_bsvarSIGN](https://bsvars.org/bsvarSIGNs/reference/specify_bsvarSIGN.html).
