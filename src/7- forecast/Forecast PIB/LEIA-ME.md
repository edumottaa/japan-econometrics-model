# Ponte para o PIB: atividade com erros ARIMA

A ponte usa somente o crescimento trimestral do índice coincidente de atividade.
A versão com juro curto foi retirada dos scripts e das saídas ativas. Os arquivos
substituídos e a comparação anterior foram guardados fora do projeto.

## Ordem dos scripts

1. Coleta e limpeza de variáveis complementares existentes.
2. `src/3- feature_engineering/variaveis complementares/1- feature_engineering PIB.R`.
3. `src/6- evaluation/variaveis complementares/0- testes diferenciacao e lags PIB.R`.
4. `src/5- models/variaveis complementares/1- modelos PIB.R` (inclui resíduos).
5. `src/6- evaluation/variaveis complementares/1- cross validation PIB.R`.
6. `src/6- evaluation/variaveis complementares/2- graficos CV PIB.R`.
7. `src/7- forecast/variaveis complementares/codigos/1- forecast PIB.R`.
8. `src/7- forecast/variaveis complementares/codigos/2- graficos forecast PIB.R`.

As funções de seleção ficam em `src/5- models/variaveis complementares/0- funcoes ponte PIB.R`.
O diagnóstico dos resíduos permanece dentro do script do modelo.

## Configuração mantida

Média trimestral da atividade em nível, exigindo três meses completos; depois,
crescimento trimestre contra trimestre em porcentagem. PIB real com ajuste sazonal.
KPSS, ADF e diagnóstico sazonal; selecionar diferenciação antes da comparação de AICc.
Atividade contemporânea e lags acumulados de até dois trimestres; erros ARIMA com
p,q=0:2, busca exaustiva sem aproximação. Preferir menos preditores se delta AICc<2.
Manter a mesma janela de estimação do CV já realizado para preservar seus resultados.

A especificação selecionada usa atividade do trimestre corrente e dos dois anteriores,
com erros ARIMA(0,0,2), sem diferença adicional dos crescimentos. Não é uma relação causal.

O CV da ponte simples e dos benchmarks foi preservado, sem reestimar o BVAR:
46 origens avaliáveis, 340 pares origem-trimestre por modelo, até oito trimestres.
As escolhas foram feitas dentro de cada treino. Preditor futuro vem das previsões salvas
do BVAR reduzido rho=0,995; caminhos externos Treasury/Brent eram ex post. Sem vintages.

## Forecast atualizado em 03/11/2026

Os quatro cenários usam a atividade prevista pelo BVAR estrutural com rho=0,995.
Não foi reestimado o BVAR. As simulações dos erros são centradas na previsão analítica
de `forecast()` antes de construir os níveis, preservando os desvios conjuntos dos sorteios.
Isso garante que a média do crescimento trimestral simulado coincide com o ponto do ARIMA.

A variação em 12 meses é `100 * (PIB_t / PIB_(t-4) - 1)`: comparação com o mesmo
trimestre do ano anterior. Não é variação trimestral anualizada nem o crescimento do ano civil.
O horizonte termina em 2028 T2, último trimestre com três meses completos do BVAR.

Gráficos separados por cenário de petróleo (Base, Benigno, Adverso e Adverso estrutural),
com os mesmos eixos, rótulos no quarto trimestre de cada ano e no último ponto,
e intervalos condicionais de 80% e 95%. Os intervalos não propagam a incerteza da
atividade do BVAR nem dos seus caminhos externos.

Saídas do forecast: `src/Outputs/6- Projeções complementares/PIB/forecast`.
Os arquivos `crescimento_anual_base.png`, `crescimento_anual_benigno.png`,
`crescimento_anual_adverso.png` e `crescimento_anual_adverso_estrutural.png`
mostram a variação em 12 meses. Há também comparação conjunta e gráficos trimestrais.

O aumento da taxa anual no segundo semestre de 2026 depende da atividade prevista e
da substituição de trimestres fracos de 2025 na comparação; é resultado a discutir,
não uma premissa imposta ao PIB. O ganho preditivo da ponte concentrou-se na pandemia;
o ARIMA sem preditores foi melhor em RMSE em 2015-2019 e no período recente.

Fontes metodológicas: https://otexts.com/fpp3/dynamic-regression.html e
https://fable.tidyverts.org/reference/ARIMA.html.


## Atualização dos gráficos
O gráfico base mostra 2019 a 2028 T2. A comparação tem as quatro trajetórias previstas no mesmo painel, com rótulos anuais e no ponto final. Os gráficos separados dos cenários foram retirados. As trajetórias atuais condicionam preços e não identificam choques de oferta isolados.

