# Dois modelos para o spread JGB30Y-5Y

## Versao ativa: mudar somente o forecast

O `code_run_2.R` agora reutiliza **o BVAR original, seus 2.500 sorteios e o forecast
original salvo**, sem estimar nenhum coeficiente. O efeito do fluxo e carregado
do satelite ja estimado e aplicado como sensibilidade incremental. Resultado Base
em set/2028: **186,6 pb -> 212,2 pb**, adicional de **25,6 pb**.

Execute `code_run_2.R`. Os arquivos desta versao ficam em `resultados/so forecast/`.
Detalhes: `LEIA-ME_so_forecast.md`. O controle e o forecast salvo, nao o controle
recalculado de 189,2 pb da comparacao antiga. Os experimentos abaixo foram mantidos
para referencia; o runner que reestima passou a se chamar `code_run_2_reestimar_BVAR.R`.

## Como rodar, a partir da raiz do projeto

```r
# Satelite e efeito de impor sua trajetoria ao BVAR atual:
source('src/chat gpt/JGB fluxo/code_run_1.R', encoding='UTF-8')

# Somente forecast; coeficientes e forecast de referencia preservados:
source('src/chat gpt/JGB fluxo/code_run_2.R', encoding='UTF-8')
```

O primeiro runner estima o satelite. O segundo apenas projeta, com os coeficientes
ja salvos. Nenhum chama o code_run principal ou altera o BVAR principal.

## Experimentos anteriores: satelite e BVAR-X reestimado

## Scripts principais, em secoes curtas

- `00_dados.R`: le os quatro arquivos brutos de mercado e o JSDA preparados pelo
  Claude; calcula medias mensais, diferencas e lags. Nenhum download novo.
- `01_modelo_satelite.R`: estagio 1 MA(2) sem constante; estagio 2 com erro branco,
  constante, OIS pos-YCC e fluxo com lag 2. Forecast, coeficientes, diagnosticos e CV.
- `02_modelo_bvar_fluxo.R`: BVAR com uma exogena adicional, centralizada, restrita
  a equacao do spread. Nao importa o beta da regressao em diferencas.
- `03_forecasts_comparacao.R`: compara o original, o original condicionado ao
  spread do satelite e o BVAR-X com o fluxo estimado. Tambem calcula BVAR-X com x=0
  para separar mudanca de parametros do efeito da trajetoria futura do fluxo.
- `funcoes_restricao.R` e `funcoes_forecast.R`: apoio numerico para manter os
  scripts principais legiveis. Nao sao etapas para executar isoladamente.

## O que foi estimado e executado nesta entrega

O satelite foi reestimado e o runner 1 foi executado ate os forecasts das oito
variaveis e quatro cenarios. A primeira rodada do BVAR-X nao ativou o indice
COVID no prior e foi descartada: seus arquivos estao em
`resultados/descartado sem ajuste COVID/` e NAO devem ser usados.
A copia corrigida confere explicitamente esse indice antes de estimar e passou
um piloto de 10 sorteios, com unidades historicas e narrativas verificadas.
Os arquivos de piloto e seus graficos sao identificados como tal, em
`resultados/comparacao piloto/`; nao sao estimacoes finais.

A rodada corrigida de **500 sorteios** foi concluida: **ESS 131,7**, **234 betas
distintos**, COVID ativo e unidades historicas, sinais e narrativas conferidos.
O coeficiente medio do fluxo e **0,03874 p.p./tri**, isto e, **3,87 pb/tri**;
intervalo posterior de 95% de **-1,11 a 7,22 pb/tri**, probabilidade de beta
positivo de **95,2%**. O intervalo bilateral ainda inclui zero.

Comparacao Base em **setembro de 2028**, com algoritmo de condicionamento comum:

| Caminho | Spread 30-5 |
|---|---:|
| BVAR original, controle recalculado | 189,2 pb |
| BVAR original condicionado ao satelite | 216,7 pb |
| BVAR-X com fluxo projetado | 184,6 pb |
| BVAR-X com fluxo na media historica (x=0) | 166,4 pb |

No BVAR-X, o fluxo futuro acrescenta **18,2 pb** contra x=0. A reestimacao dos
parametros reduz **22,8 pb** contra o controle original; efeito liquido **-4,6 pb**.
Portanto, o resultado do BVAR-X nao confirmou a estimativa previa de 205-215 pb.
Nao se deve simplesmente somar o efeito do fluxo a antiga projecao sem reestimar
o restante do sistema. `impactos no BVAR.csv` e os graficos mostram as oito
variaveis nos quatro cenarios. Sao centros condicionais, nao resultados causais.

O runner 2 esta configurado para **500 sorteios em quatro processos**; pode-se
aumentar `S` para 2.500 para melhorar a precisao. O amostrador so aceita
parametros e rotacoes que satisfazem as restricoes; portanto pode ser demorado.
Ele para se o ESS final ou o numero de betas distintos ficar abaixo de 50.
Nao use os valores do piloto como o resultado final de 205-215 pb sugerido no resumo.

## Amostras, unidades e datas

- Spread e OIS em medias mensais. Spread no satelite em **pb**, no BVAR em **%**.
- Fluxo: compras menos vendas de estrangeiros, **JPY trilhoes/mes**, JSDA.
  Universo superlongo (20/30/40Y e 15Y flutuante); nao JGB30 isolado.
- Estagio 1: **jun/2018 a set/2026**, 100 observacoes. Estagio 2:
  **jul/2018 a set/2026**, 99 observacoes. JSDA comeca em mai/2018;
  seu lag 2 nao existe em jun/2018. Nao inventamos o dado de abril.
- OIS pos-YCC: zero ate mar/2024; variacao do OIS desde abr/2024.
  Impor efeito zero no YCC e uma restricao de especificacao, nao um resultado.
- Curvas FWCV transcritas de 30/10/2026: interpolacao linear entre horizontes
  disponiveis. A taxa mensal futura usa a media dos pontos que delimitam o mes,
  com a mesma ancoragem no spot da versao anterior.
- Outubro/2026 usa OIS e UST30 observados apos o corte de estimacao JSDA.
  E um nowcast com informacao posterior a setembro, nao uma previsao genuina
  feita em setembro. Depois disso, usa curvas a termo, que nao sao expectativas puras.
  O spread parte de setembro: o nowcast de outubro NAO e reancorado no spread
  observado de outubro. O erro desse nowcast sai em arquivo proprio para conferencia.
- Satelite termina em out/2028. O BVAR atual termina em set/2028; os efeitos
  comparativos param em **set/2028**, sem criar um mes extra artificialmente.

## BVAR-X: como o coeficiente e estimado

Usa a base do principal: jan/2000-ago/2026, cinco variaveis domesticas mais tres
do bloco externo, seis lags, tendencia,
rho=0,995, Minnesota e os sinais/narrativas salvos. A calibracao COVID e reutilizada
do principal, para isolar a inclusao do fluxo: os hiperparametros nao sao
reestimados nesta extensao. E uma analise modular, condicional a essa calibracao.
As tres externas sao UST2Y, Brent e IP mundial. Nao recebem feedback do Japao.
O fluxo e uma exogena adicional; nao cria um nono choque estrutural.

`x_t = fluxo_(t-2) - media_do_fluxo_(t-2)` onde o lag e observado. A media e
calculada apenas dentro da amostra do BVAR: **0,3514 tri**, nao fixada em 0,37.
Antes de jul/2018, x=0 e um preenchimento neutro por falta de dados, nao evidencia
de que o fluxo efetivo era zero. Nenhum dado JSDA de set/2026 entra nessa media.

O novo beta tem prior de media zero e variancia de regressor 100, como os outros
termos exogenos. O beta do spread fica livre. Os outros sete sao zero **exato**.
O apoio numerico usa o amostrador de normal matricial e de rotacoes do bsvarSIGNs
3.0, condiciona a distribuicao dos coeficientes as exclusoes e inclui a densidade
das exclusoes nos pesos de importancia. Nao simplesmente zera coeficientes depois
de estima-los. O pacote instalado permanece intacto. A implementacao depende de
funcoes internas da versao 3.0; verificar se mudar a versao do pacote.

O beta do satelite multiplica **Delta spread**, enquanto o do BVAR multiplica
o **nivel do spread**. Mesmo numeros parecidos nao representam o mesmo efeito
dinamico; por isso nao importamos 3,73 pb como beta fixo do BVAR.

`BVAR com fluxo x=0` significa fluxo futuro no valor medio historico estimado,
nao compras liquidas nulas. O efeito direto da exogena so ocorre no spread; as demais
variaveis domesticas recebem efeitos via lags, covariancias e condicionamento.
O bloco externo nao recebe feedback domestico nas matrizes do modelo.
`decomposicao efeito fluxo.csv` separa a mudanca de parametros (x=0 contra o
original) do efeito exclusivo da trajetoria futura do fluxo (fluxo contra x=0).

## Comparacao dos impactos

Todas as variantes usam o mesmo algoritmo de condicionamento gaussiano da
**trajetoria inteira**, as mesmas condicoes salvas dos quatro cenarios e a mesma
banda terminal do juro (1,75%-2,25%). Isso permite comparar as variantes entre si.
O forecast propaga o decaimento da volatilidade COVID por sorteio, como no pacote.
O forecast original salvo usa uma implementacao sequencial com ajustes via IRFs;
nao e numericamente o mesmo controle. Sua tabela original e exportada em separado
como `BVAR salvo referencia.csv`. Nao atribua a inclusao do fluxo a diferenca
entre algoritmos. Efeitos sao calculados contra `Original` refeito na mesma regra.

Por padrao, os centros usam 250 sorteios igualmente espacados da posterior, ou
todos se houver menos. Sao trajetorias condicionais centrais, sem simulacao das
inovacoes futuras e sem bandas preditivas conjuntas. Atividade/CPI aparecem como
variacao em 12 meses nos graficos. Efeitos de taxas/spread em pb; outros niveis
em %; o efeito adicional em inflacao/atividade 12m sai tambem em p.p.

O caminho do satelite e **imposto** ao BVAR: isso e um cenario, nao evidencia de
que o BVAR previu aquele spread. O BVAR-X estima o coeficiente do fluxo na sua
propria equacao; pode gerar resultado diferente do satelite, inclusive menor.

## Diagnosticos e limites economicos

Na reestimacao, o satelite projeta cerca de **219 pb** em out/2028, com faixa
p10-p90 de cerca de **182-254 pb**. A faixa contempla apenas os erros do estagio 2;
nao contempla incerteza do fluxo, parametros ou curvas. Nao e IC completo da tese.

CV recente: 12 origens completas de out/2024 a set/2025, horizonte 12m. RMSE
de cerca de **15,7 pb** para o satelite ex post, **29,2 pb** naive e **25,6 pb** drift.
Usa OIS e fluxo **realizados**. Nao valida a capacidade de prever os insumos do
estagio 1 nem a estrategia operavel em tempo real. O CV do BVAR-X nao foi executado.
Essa e a comparacao principal, em `CV principal ex post.csv`, conforme o objetivo
explicativo pedido. O teste complementar, substituindo o fluxo realizado pelo
projetado no estagio 1 e mantendo UST/OIS realizados, tem RMSE de **31,3 pb**.
Ele aparece em arquivo separado e avalia a geracao de insumos; nao substitui o
criterio ex post. As 12 origens se sobrepoem, e nao equivalem a 12 anos independentes.

Ljung-Box (12 lags): estagio 2 cerca de **0,681**; estagio 1 cerca de **0,0049**.
O MA(2) nao eliminou a autocorrelacao do fluxo. Preservamos a especificacao
pedida e registramos a falha; nao chamamos esse modelo de fluxo de validado.

Compras estrangeiras podem responder a yields mais altos. O beta positivo nao
prova que mais compras causam abertura do spread. Fluxo previsto por UST30 nao
e automaticamente um instrumento exogeno valido. Estes modelos nao identificam
risco fiscal puro nem transformam residuos em premio de prazo observado.

## Fontes metodologicas

- FPP3, regressao com erros ARIMA: https://otexts.com/fpp3/dynamic.html
- bsvarSIGNs, especificacao do prior: https://bsvars.org/bsvarSIGNs/reference/specify_prior_bsvarSIGN.html
- Codigo dos autores (SOE e amostragem): https://github.com/bsvars/bsvarSIGNs/tree/main/src

Os arquivos brutos permanecem em `../spreads JGB modelo satelite/dados`.
Nao foram reextraidos da Bloomberg nem da JSDA nesta etapa.
