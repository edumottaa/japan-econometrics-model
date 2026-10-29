# Subsídios, fiscal, comunicação e persistência no BVAR

Pesquisa concluída em 25/10/2026, antes dos novos testes. Projeções de bancos abaixo são exemplos datados de construção de cenários, não uma atualização de consenso nem recomendações de investimento.

## 1. Subsídios no CPI: informação contínua pode ajudar

O CPI core japonês exclui alimentos frescos, mas inclui energia. O BoJ acompanha também medidas sem fatores institucionais, retirando efeitos estimados de subsídios energéticos, educação gratuita, imposto sobre consumo, telefonia e programas de viagem. Isso mostra que existe uma alternativa a uma dummy arbitrária para cada episódio: separar uma contribuição administrativa mensurável da inflação subjacente. O BoJ ressalta, contudo, que nenhuma medida isolada resume toda a inflação subjacente. [BoJ, março/2026, gráfico 3](https://www.boj.or.jp/en/research/wps_rev/rev_2026/data/rev26e05.pdf).

**Minha proposta para este projeto:** construir uma série de efeito direto das medidas, com magnitude e vigência baseadas em dados oficiais. Pode ser uma redução em ienes por kWh/litro ou uma contribuição estimada para o preço agregado. Essa informação pode entrar como regressor externo conhecido, ou servir para decompor o CPI em componente subjacente e efeito administrativo, projetando-os conjuntamente.

Uma variável contínua pode ter mudanças bruscas quando a regra do subsídio muda; suavizar sem fundamento também distorceria os dados. O ganho é representar a magnitude econômica da intervenção, em vez de supor que todos os meses subsidiados têm o mesmo efeito.

**Cuidado de escala:** o BVAR usa `100 × log(nível do CPI)`. Uma contribuição em pontos percentuais para a inflação anual não pode ser subtraída diretamente desse log-nível. Se `w_t` for o efeito no log do nível, a inflação observada em 12 meses contém aproximadamente `w_t - w_(t-12)`. Assim, um subsídio constante pode continuar reduzindo o nível dos preços sem continuar reduzindo a inflação anual. Sua retirada gera outro efeito-base.

O subsídio também sustenta renda real e demanda. Portanto, há dois canais: efeito direto negativo no preço e efeito indireto potencialmente positivo na demanda. Retirar apenas o efeito direto do CPI não significa retirar o estímulo fiscal da economia.

Uma primeira versão pode tratar energia e medidas administrativas mantendo o núcleo atual; não exige montar todo o S2. Se o alvo for substituído por uma medida de inflação subjacente, deve existir uma ponte explícita de volta ao CPI core observado.

## 2. Política fiscal: gasto, déficit e impulso são informações diferentes

O núcleo de sete variáveis não contém gasto público ou arrecadação. A inclinação da curva e o choque de prêmio podem refletir riscos fiscais, mas não medem quanto o governo gastou nem identificam um multiplicador fiscal.

Uma medida usual de impulso é a mudança do saldo primário ajustado ao ciclo, com sinal invertido: expansão do déficit estrutural corresponde a impulso positivo. O déficit observado também varia com o próprio ciclo e os juros pagos; não é uma medida limpa de uma nova decisão de estímulo. A literatura do FMI discute esses problemas de mensuração. [FMI, Structural Balances and All That](https://www.imf.org/en/publications/wp/issues/2016/12/30/structural-balances-and-all-that-which-indicators-to-use-in-assessing-fiscal-policy-15762).

| Informação | Uso proposto | Limite |
|---|---|---|
| Consumo e investimento públicos reais das contas nacionais | Canal de demanda e cenário fiscal trimestral | Frequência trimestral e defasagem de divulgação |
| Execução de obras públicas e desembolsos efetivos | Indicadores mensais de execução | Cobertura parcial; valores nominais exigem tratamento de preços |
| Impulso fiscal e calendário das medidas | Julgamento prospectivo coerente com o orçamento | Anúncio não é execução; depende de hipóteses explícitas |

O próprio BoJ distingue investimento público real, obras executadas nominais e consumo do governo em seu Outlook. Um aumento nominal de obras pode refletir custos maiores. [BoJ, julho/2026, seção Government Spending](https://www.boj.or.jp/en/mopo/outlook/gor2607b.pdf).

Não recomendaria repetir um dado fiscal anual em doze meses e tratá-lo como doze observações independentes, nem usar o valor total de um pacote como gasto imediato. Em uma primeira implementação, o caminho fiscal pode entrar como cenário com incerteza; uma variável fiscal endógena mensal exige uma série consistente e um teste separado.

## 3. Comunicação do BoJ e uma diferença importante na taxa usada

O modelo vê a história do juro curto e suas relações com CPI, atividade, curva, câmbio e externas. Ele não lê comunicados nem recebe uma curva futura japonesa de OIS. As narrativas de identificação usadas em março/2024 e outubro/2025 são hipóteses sobre choques históricos; não incorporam a orientação futura do comitê.

**Conferência na fonte:** a série atual `FM02/STRACLUCON` é a média mensal da overnight efetiva. Setembro/2026 foi 1,043%. O BoJ, porém, fixou o alvo em torno de 1,25%, com a alteração efetiva em 24 de setembro. A média do mês contém vários dias anteriores à alta. Portanto, um forecast acima de 1,043% não necessariamente significa nova alta em relação ao alvo atual de 1,25%. A leitura anterior dos gráficos precisava dessa ressalva. [Definição da série](https://www.stat-search.boj.or.jp/info/nme_Mdframe_en.html), [decisão de setembro/2026](https://www.boj.or.jp/en/mopo/mpmdeci/mpr_2026/k260918a.pdf).

Para incorporar informação prospectiva, há três alternativas, com objetivos diferentes:

1. **Observações e nowcast:** usar a overnight diária já publicada e decisões já tomadas para preparar o primeiro mês ainda incompleto. Isso atualiza o ponto de partida, sem inventar uma trajetória terminal.
2. **Cenário de política:** condicionar a trajetória conjunta a uma curva fundamentada em OIS, pesquisas ou avaliação explícita do BoJ. Um condicionamento suave admite incerteza; um caminho fixo impõe os valores escolhidos. OIS também contém prêmios e convenções que precisam ser tratados.
3. **Informação no modelo:** acrescentar uma medida de expectativas/comunicação, respeitando sua data de divulgação. Índices textuais precisam de método estável e validação histórica; uma pontuação escolhida depois dos acontecimentos não é um teste em tempo real.

O teste de lags abaixo mantém o ponto de partida e as condições anteriores para isolar a mudança estatística. Esses forecasts livres continuam sem uma curva prospectiva de juros japoneses.

## 4. Como instituições constroem seus cenários de juros

As publicações acessíveis mostram quais informações sustentam suas projeções. Elas não permitem afirmar quais algoritmos proprietários ou pesos internos cada banco usa.

| Instituição e publicação | Informação explicitamente utilizada | Lição para o projeto |
|---|---|---|
| ING, Monthly Economic Update, 10/06/2026 | Distingue inflação temporariamente fraca por medidas governamentais de pressões subjacentes; usa salários, iene, votos e falas de dirigentes para avaliar o momento das altas | CPI observado e reação esperada do BoJ precisam ser interpretados prospectivamente |
| MUFG, 17/08/2026 | Combina inflação subjacente, expectativas de mercado, cenário do Fed, câmbio e riscos fiscais; discute datas e ritmo de altas | O cenário de juros usa uma avaliação econômica explícita, com condições e riscos |
| BNP Paribas, 24/09/2026, texto concluído em 18/09 | Separa efeitos temporários de medidas fiscais da dinâmica salários-preços e avalia a interação entre expansão fiscal e aperto monetário | Fiscal pode amortecer o CPI no curto prazo e sustentar demanda e risco inflacionário adiante |

Fontes: [ING, seção Bank of Japan](https://think.ing.com/uploads/reports/Monthly_Economic_update_June_2026_1.pdf), [MUFG](https://mufgresearch.com/rates/japan-economic-financial-weekly-17-august-2026/), [BNP Paribas](https://economic-research.bnpparibas.com/html/en-US/Japan-Growth-under-pressure-9/24/2026,53783).

Como comparação institucional, o SNB publica inflação condicional à taxa de política constante por três anos. Essa hipótese serve para avaliar a necessidade de mudanças futuras; a linha de juros constante não é uma previsão de que o banco manterá a taxa. É um exemplo útil da distinção entre cenário, previsão e decisão de política em uma economia que também teve juros negativos. [Estratégia do SNB](https://www.snb.ch/en/the-snb/mandates-goals/monetary-policy/strategy).

Minha conclusão é que uma trajetória institucional de juros costuma combinar modelos, hipóteses de cenário e julgamento documentado. Não há base para atribuir ao BVAR histórico sozinho a capacidade de absorver medidas futuras ou novas comunicações que nunca entraram em seus dados.

## 5. Mu e delta: vale testar, com reestimação

Na implementação instalada do bsvarSIGNs:

| Parâmetro | O que regula | Experimento |
|---|---|---|
| `lambda` | Dispersão dos coeficientes em torno do prior Minnesota; menor valor aperta | 0,05; 0,10; 0,20 |
| `mu` | Prior da soma dos coeficientes: menor valor favorece somas dos próprios lags próximas de 1 | 1 versus 0,5 |
| `delta` | Prior da observação inicial, associado ao comportamento de longo prazo e às condições iniciais; maior valor enfraquece essa restrição | 1 versus 5 |

O prior da observação inicial não é uma meta de inflação nem uma meta de juros. Um delta muito pequeno pode impor comportamento sem drift ou reverter a médias implícitas, dependendo das relações estimadas; não há fundamento para presumir que apertá-lo sempre melhore o cenário. Mu muito pequeno também impõe restrições fortes sobre raízes unitárias e cointegração. [Giannone, Lenza e Primiceri, seção 2.2](https://eabcn.org/sites/default/files/2023-10/Draft_GLP_V18_0.pdf).

É necessário mudar esses valores **antes de estimar**. Alterá-los apenas no objeto de forecast de um posterior já estimado não reestima os coeficientes nem incorpora informação nova. Mesmo um prior melhor ajustado não substitui dados de subsídios, fiscal e comunicação.

O prior instalado reduz a variância dos coeficientes conforme `1/lag²`: o lag 12 tem variância prévia 1/144 da do lag 1, mantidos os demais fatores. Dobrar o número de lags pode ajudar na dinâmica anual, mas também dobra os coeficientes de defasagens; não garante que a sazonalidade ou um novo regime sejam aprendidos.

## 6. Desenho dos testes definido antes dos resultados

Manter sete variáveis, dados congelados do CV até julho/2026, 47 origens, treino inicial 180, avanço de três meses, H=24, S=2.500 e as mesmas externas realizadas. Forecast com a mesma amostra até agosto/2026 e os mesmos quatro cenários externos da comparação anterior. Sem acrescentar variável fiscal ou trocar o CPI nesta bateria, para isolar lags e priors.

Grade reduzida: referência p6/λ0,20/μ1/δ1; p12 com λ0,20/μ1/δ1 e λ0,10/μ1/δ1; testes de μ0,5 e δ5 isolados e combinados com λ0,10; combinado também com λ0,05 e λ0,20. Reutilizar p6/λ0,10 já concluído como outra referência.

Usar o CV reduzido para examinar a grade. Fazer também CV estrutural p12 com os parâmetros originais, comparável ao p6 estrutural anterior, e validar no estrutural a combinação mais promissora da grade. O critério é desempenho de previsão das cinco domésticas, com atenção ao período recente e às distribuições; não proximidade visual da inflação de 2% ou dos juros de um nível escolhido. A seleção na mesma bateria é exploratória, sem um novo conjunto intocado de teste.
