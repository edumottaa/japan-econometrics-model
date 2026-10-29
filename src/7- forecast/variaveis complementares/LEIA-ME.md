# Projeção complementar do PIB

Modelo único: crescimento trimestral do PIB em função do crescimento trimestral da atividade, com erros ARIMA. Sem juro, regressão múltipla ou seleção entre especificações.

Executar na ordem das etapas do projeto:

1. `src/1- data_collection/3- data_collection variaveis complementares.R`
2. `src/2- data_cleaning/3- data_cleaning variaveis complementares.R`
3. `src/3- feature_engineering/variaveis complementares/1- feature_engineering PIB.R`
4. `src/4- plot data/4.3 variaveis complementares/1- data visualization PIB.R`
5. `src/5- models/variaveis complementares/1- modelos PIB.R`
6. `src/6- evaluation/variaveis complementares/1- cross validation PIB.R`
7. `src/7- forecast/variaveis complementares/codigos/1- forecast PIB.R`
8. `src/7- forecast/variaveis complementares/codigos/2- graficos forecast PIB.R`

O PIB vem do Cabinet Office: série real com ajuste sazonal em nível. A atividade é o mesmo índice coincidente utilizado no BVAR. Calculamos a média dos três meses do índice em nível e, depois, o crescimento trimestral em porcentagem. As taxas não são anualizadas. Os erros ARIMA usam d = D = 0 porque as variáveis já estão em crescimento.

O CV reutiliza as origens salvas do BVAR reduzido com peso 0,995, a cada três meses. O treinamento do PIB termina no trimestre anterior à origem. A previsão atravessa o trimestre ainda não publicado; as métricas avaliam os trimestres seguintes à origem, até oito trimestres. Janelas sem PIB futuro observado não entram nas métricas.

O teste é condicional ex post aos caminhos externos do BVAR, com a versão atual do PIB e os meses tratados como disponíveis na origem, seguindo a convenção do CV original. Não é um backtest completo de vintages e calendário de divulgação.

O forecast final usa a trajetória média da atividade em cada cenário do BVAR estrutural. `forecast()` e `generate()` são as funções nativas do fpp3. As simulações incluem os erros ARIMA, com parâmetros fixos; os intervalos são condicionais e não incorporam a incerteza da atividade projetada pelo BVAR.

Com previsões mensais até agosto de 2028, só é possível fechar o PIB até 2028 T2. Não calculamos o crescimento de 2028 como ano completo sem os quatro trimestres.

Saídas: visualização em `src/Outputs/1- plot data/variaveis complementares/PIB`, CV em `src/Outputs/2- Evaluation/variaveis complementares/PIB` e modelos/forecast em `src/Outputs/6- Projeções complementares/PIB`.

Fontes: https://www.esri.cao.go.jp/en/sna/sokuhou/sokuhou_top.html e https://otexts.com/fpp3/dynamic-regression.html.
