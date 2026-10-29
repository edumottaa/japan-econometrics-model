# Pesos exponenciais no BVAR estrutural

## Premissa

A cópia usa pesos por precisão: `peso_t = rho_peso^(T - t)` e
`Var(erro_t) = escala_COVID_t^2 * Sigma / peso_t`.
Dados antigos são tratados como observações mais ruidosas das mesmas relações.
Os coeficientes continuam constantes. Isso não modela uma quebra de regime explicitamente.

Essa é uma verossimilhança gaussiana heterocedástica. Não é a alternativa
`produto(L_t^peso_t)` (power likelihood): os graus de liberdade da covariância
continuam os da estimação nativa. Manter todos os meses não garante conservar
a precisão do modelo original. A matriz Sigma é estimada novamente.

## Código

O peso entra depois que o pacote constrói Y e X, incluindo defasagens, constante
e tendência. Ambas as matrizes são multiplicadas por `sqrt(peso_t)`, inclusive
na estimação dos parâmetros COVID. Os priors Minnesota e dummy permanecem
nas unidades originais. Ao final, o histórico original é recuperado para o
forecast, e as escalas e os choques são recalculados com os parâmetros reamostrados.

Não há seleção por autovalores nem prior adicional centrado em inflação de 2%.
A tendência temporal estimada, já presente no script de origem, foi mantida.
Ela não é uma meta de inflação. Lambda = 0,2, mu = delta = 1 e as seis defasagens
foram mantidos, assim como as sete variáveis, os sinais e as narrativas.

## Calibração

Mudar somente `rho_peso` na cópia do modelo e estimar novamente:

| rho_peso | Peso de 5 anos atrás | Peso de 10 anos atrás |
|---|---:|---:|
| 1 | 100% | 100% |
| 0,995 | 74% | 55% |
| 0,990 | 55% | 30% |
| 0,980 | 30% | 9% |

O valor 0,995 é inicial, não foi selecionado por desempenho.
Os pesos futuros são 1; a variância futura é a Sigma estimada, com o tratamento
COVID que já existe no pacote. Os intervalos são condicionais a essa hipótese.

## Execução

Depois da coleta, limpeza e feature engineering usuais do BVAR estrutural:

1. `src/5- models/Structural BVAR models/Bvar estrutural pesos exponenciais.R`
2. `src/7- forecast/bvar estrutural/codigos/3- forecast bvar estrutural pesos exponenciais.R`
3. `src/7- forecast/bvar estrutural/codigos/4- graficos forecast bvar estrutural pesos exponenciais.R`

As saídas vão para `src/Outputs/Forecast BVAR estrutural/pesos exponenciais`.
Os scripts e as saídas anteriores não são sobrescritos.
