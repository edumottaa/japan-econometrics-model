# BVAR com prior de inflação, sem tendência

Execute `Bvar estrutural prior inflacao longo prazo.R`. O resultado é `post_boj_prior`; `post_boj_sem_prior` guarda os sorteios usados como proposta.

- Mantém sete variáveis, seis lags, lambda = 0.2, mu = delta = 1 e a atualização doméstica do script original.
- A quarta variável passa de 100 * log(CPI) para inflação mensal anualizada: 12 * diff(100 * log(CPI)). Perde-se a primeira observação. A restrição do BoJ nessa posição passa a se referir à inflação.
- O prior adicional favorece a inflação anual de equilíbrio próxima de 2%, com desvio de 0.5 ponto percentual. Os controles são `centro_inflacao` e `desvio_inflacao`, ao final do script.
- Esse equilíbrio é condicional ao bloco doméstico estável e a Treasury e Brent constantes nos últimos valores observados. As demais variáveis domésticas entram no cálculo conjunto do equilíbrio; seus valores não são fixados manualmente.
- A reponderação dos parâmetros aproxima a posterior do prior original multiplicado pelo fator normal sobre a inflação de equilíbrio e pela condição de estabilidade. Não é um argumento nativo do bsvarSIGNs nem a implementação integral da parametrização de Villani. O fator normal não garante que a distribuição marginal resultante seja exatamente N(2, 0.5²).
- O código verifica estabilidade e diversidade dos sorteios. ESS abaixo de 30 interrompe; entre 30 e 50 gera aviso de resultado exploratório. A reamostragem usa os mesmos índices em todos os parâmetros.
- O forecast e os gráficos antigos esperam CPI em nível. Precisam ser adaptados para este objeto: os sorteios de inflação mensal anualizada devem ser acumulados para reconstruir o CPI, em vez de aplicar diretamente exp(inflacao / 100).

Conferidos: sintaxe, construção das matrizes com a amostra local e cálculos do prior em sorteios sintéticos. Nenhuma estimação, CV ou forecast foi executado nesta criação.

Documentação do pacote: https://bsvars.org/bsvarSIGNs/reference/specify_prior_bsvarSIGN.html
Referência conceitual: https://doi.org/10.1002/jae.1065
