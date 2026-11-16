# Oito opções: UST30, satélite e fluxo estrangeiro

Preparação de código apenas: nenhum forecast, ajuste ou CV foi executado pelo agente.

Abra `code_run.R` e rode no R. Ordem: forecasts das opções 1–4, CVs das opções
1–4, forecast e CV da opção 5, forecasts 6–8 e, por último, CVs 6–8.
Se uma etapa falhar, resolve-se a
falha antes de passar ao próximo bloco. Cada opção tem também um executor individual.
`code_run_6_a_8.R` executa apenas o bloco novo: três forecasts, depois três CVs.
Resultados em `resultados/`, separados por opção, forecast/CV e comparação.
`status.csv` informa falhas. O cache permite retomar sem repetir ajustes compatíveis.
Não altera os modelos e resultados principais do projeto.

Para retomar após interrupção, rode `code_run_retomar.R`. Ele confere os RDS
compatíveis, pula forecasts/CVs já completos e reutiliza as origens concluídas
de um CV parcial. Mantém a mesma ordem dos blocos e `REESTIMAR <- FALSE`.
O plano e o andamento ficam em `resultados/plano retomada.csv`.
O executor de retomada está restrito a **CVs 2, 3 e 4**, com
`CVS_PARA_RODAR <- c(2L,3L,4L)` e `SO_CV_OPCAO_1 <- FALSE`.
Não inicia forecasts prospectivos nem os CVs das outras opções.
Resultados completos são recuperados do cache; os gráficos/tabelas são atualizados.
Mantenha `REESTIMAR <- FALSE`. Para apenas consultar o CV 1 salvo, pode usar
`SO_CV_OPCAO_1 <- TRUE`; um resultado completo compatível também é reaproveitado.
Para gerar somente os gráficos de previsões contra os dados observados, rode
`graficos_previsao_fora_amostra_cv1.R`. Ele lê as origens salvas de dez/2021,
dez/2023 e dez/2025 e não estima modelos nem executa o CV.

## Configurações

1. BVAR com **UST30Y contemporâneo exógeno**, coeficientes estimados livremente
   nas cinco equações domésticas, exatamente zero nas três externas.
2. Mesma estrutura, com **UST30Y − UST2Y** em pontos percentuais.
3. Posterior do BVAR original preservada; spread condicionado **suavemente** ao satélite.
4. Mesmo BVAR e mesmo satélite da opção 3, com trajetória do spread **exata**.
5. Fluxo como **nona variável endógena doméstica**: suas defasagens podem afetar
   as domésticas, e estas podem afetar o fluxo. Sem novo choque de fluxo identificado.
   Pela premissa solicitada, o fluxo futuro é condicionado exatamente à média pós-YCC.
   Portanto, o fluxo é endógeno na estimação, mas sua trajetória futura é um cenário fixado.
6. BVAR com **fluxo estrangeiro contemporâneo exógeno**.
7. BVAR com **fluxo estrangeiro e UST30Y contemporâneos exógenos**.
8. BVAR com **fluxo estrangeiro e UST30Y − UST2Y contemporâneos exógenos**.

Nas opções 6–8 permanecem oito endógenas: três externas e cinco domésticas.
Cada exógena tem coeficientes próprios estimados nas cinco equações domésticas,
exatamente zero nas três equações externas. O fluxo histórico é observado e pode
ser negativo; não recebe log. Seu futuro é a média pós-YCC calculada até a origem,
inclusive no CV (não entra fluxo futuro realizado). UST30 vem da curva no forecast
e do realizado no CV; o diferencial usa o UST2Y do cenário/realizado correspondente.
As exógenas são centradas individualmente pela média do treino; o forecast usa
exatamente as mesmas médias e ordem de colunas. Tendência e intercepto são mantidos.

Não se imputam coeficientes econômicos. Mantêm-se seis lags, tendência linear,
pesos 0,995, Minnesota, volatilidade COVID e sinais/narrativas do modelo original.
Na opção 5, a primeira própria defasagem do fluxo tem média a priori zero,
compatível com a hipótese de fluxo estacionário. Não se aplica log ao fluxo.
UST30/UST2/diferencial permanecem em %/pontos percentuais; as demais transformações
originais, incluindo log dos índices/câmbio, são preservadas.

## Satélite selecionado

`ΔSpread_t(pb) = c + β × ΔOIS2Y_t(pb) × pósYCC_t + γ × Fluxo_(t−2)(¥ tri/mês) + ε_t`

Pós-YCC: abr/2024 em diante. Histórico do fluxo observado; horizonte futuro
constante na média de abr/2024 até a origem. Primeiros dois lags conservam os
fluxos históricos conhecidos. Intercepto e dois coeficientes estimados no treino.
Erro ARIMA(0,0,0), como na especificação selecionada; exporta Ljung-Box 12/24,
resíduos no tempo, ACF e histograma. Não há nova regressão UST30 → fluxo nesta rodada.

O sinal suave usa a covariância acumulada das inovações do satélite, respeitando
os nowcasts de spread já fixados. Não usa desvios mensais independentes para o nível.
Não inclui incerteza de coeficientes/média/curvas. Como compartilha dados com o BVAR,
é uma combinação de cenários; não deve ser interpretada como evidência independente.

## Amostras e referências

- Opções 1/2: mesma amostra original desde jan/2000, após conferir cobertura de UST30.
- Opções 3/4: posterior original salva para o forecast atual. No CV, posterior nova
  estimada somente até cada origem, desde jan/2000, compartilhada pelas duas opções.
- Opções 5–8: maio/2018 em diante, limitado pela JSDA. Tenta também um BVAR sem fluxo
  na mesma amostra. Se o controle falhar na qualidade da posterior, exporta a limitação
  e deixa vazio o efeito na mesma amostra. A diferença contra o original inclui
  a mudança de amostra e não isola o efeito de incluir o fluxo.

O ESS deve ser pelo menos 50, com pelo menos 50 sorteios distintos. O código não
reduz esse critério para publicar um resultado. Diagnósticos ficam no cache.
Os forecasts publicados são **centros condicionais**, não bandas preditivas.
O condicionamento é conjunto em todo o horizonte: a referência recalculada pode
diferir do forecast anterior salvo, mesmo preservando a posterior original.

## Curvas e cenários

Usa `curvas_bloomberg.csv`, já existente no projeto: FWCV tenor 30Y para UST30,
FWCV tenor 2Y para OIS2, snapshot 30/10/2026. `ARQUIVO_CURVAS` permite substituir
por CSV com as mesmas colunas `variavel,data_snapshot,horizonte_meses,taxa_pct`.
Não é a coluna de forward de um mês. Interpola só entre horizontes exportados;
preserva a aproximação/ajuste de nível para média mensal dos scripts anteriores.
Até o mês do snapshot, usa médias observadas; depois usa apenas a curva.
Os números futuros são taxas implícitas condicionais, não expectativas puras.

Mantém os quatro cenários e horizonte salvos (set/2026 a set/2028).
O UST2Y ausente no caminho salvo de outubro é completado pelo centro UST2Y salvo
do respectivo cenário, e imposto **igualmente em todos os testes e referências**.
Isso define o diferencial futuro de forma consistente com o UST2Y condicionado.
Premissas e suas fontes são exportadas. Juro curto no último mês entre **1,75% e 2,25%**.

## CV pelo procedimento original, por período

Os executores agora carregam `cv_igual_bvar_original.R`. Mantém o procedimento
original: BVAR **reduzido, sem sinais ou narrativas**, seis lags, tendência,
pesos 0,995, Minnesota e COVID recalibrada por treino. Usa 2.500 sorteios,
5.000 iterações COVID e 2.500 de aquecimento, como no script original.

Grade construída com `stretch_tsibble(.init=180, .step=3)`, desde dez/2014.
O horizonte é **12 meses**, conforme pedido, com os horizontes finais disponíveis
nas últimas janelas, assim como o CV original. Não impõe a banda prospectiva BoJ.
UST2, Brent, IP e as exógenas de juros usam caminhos realizados ex post.
O forecast usa `bsvarSIGNs::forecast()` e a mesma correção recursiva do original;
as opções 3/4/5 acrescentam somente seu condicionamento específico.
O suave inclui a covariância acumulada das inovações do satélite, com sorteios
condicionais, em vez de tratar centros de parâmetros como distribuição preditiva.

Opções 1/2: 47 origens na base atual, dez/2014 a jun/2026.
Opções 3–8: 9 origens, jun/2024 a jun/2026, pois a premissa de fluxo médio pós-YCC
só pode ser calculada depois de abr/2024. Não usa média de 2024–26 nas origens antigas.
A estimação do BVAR com fluxo continua limitada ao histórico JSDA desde maio/2018.
O satélite preserva OIS pós-YCC + fluxo defasado; não estima seu efeito pós-YCC
em janelas que ainda não contêm observações desse regime.

Avalia **atividade, CPI, juro curto, inclinação e USD/JPY**, nas unidades do modelo,
com **RMSE, MAE e CRPS**, reunindo horizontes observados 1–12.
Atividade, CPI e câmbio em 100 × log do nível; juros/spread em pontos percentuais.
Não confundir essas métricas com erros de crescimento anual ou com pb.
Benchmarks somente **Naive e Drift**, estimados pelo fpp3, como solicitado.

Períodos definidos pela **data prevista**: 2015–2019, 2020–2021 e 2022–2026,
além do geral. Uma única estimação por modelo/origem gera os escores de todos
os períodos: não há dois CVs estimados para as opções sem fluxo.
Exporta métricas com toda a cobertura disponível e também métricas nas mesmas
origens/datas/horizontes entre os modelos presentes. Períodos sem cobertura de
um modelo permanecem ausentes; não se comparam amostras diferentes silenciosamente.

Resultados novos ficam em `resultados/CV original/`, com checkpoints por origem.
O CV anterior, mensal desde out/2024, fica preservado em `opcao_N/CV/`;
**não é o CV original e não entra na nova comparação**. Seus posteriores com
sinais/narrativas não são reutilizados para o CV reduzido. Os forecasts
prospectivos e seus caches permanecem compatíveis e são reaproveitados.
Para rodar só o CV novo, use `code_run_cv_por_periodo.R`.

## Arquivos principais de saída

- `opcao_N/forecast/forecasts.csv`, `valores finais.csv`, `premissas.csv`.
- `CV original/opcao_N/previsoes e escores.csv`, `metricas por periodo.csv`.
- `comparacao/spread quatro cenarios.png`, `todas variaveis Base.png`.
- `CV original/comparacao/metricas por periodo - mesmas janelas.csv`, gráficos das cinco variáveis.
- `CV original/comparacao/cobertura.csv`: identifica os testes disponíveis e concluídos.

Pacote alvo: bsvarSIGNs **3.0**. Há amostrador local para restringir o coeficiente
da nova exógena nas equações externas; o código exige essa versão em vez de
usar silenciosamente uma interface nativa incompatível.
