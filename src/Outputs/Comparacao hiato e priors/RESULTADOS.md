# Comparação dos BVARs — 24/10/2026

## O que foi concluído

- Dois BVARs reduzidos: oito variáveis com hiato do BoJ e sete variáveis com `lambda = 0,10`.
- Dois CVs completos, com 47 origens por modelo, seis lags e 2.500 sorteios por ajuste.
- Dois BVARs estruturais correspondentes, com a identificação de Fed, petróleo, BoJ e prêmio do modelo atual.
- Forecast de todas as variáveis em quatro cenários, de setembro/2026 a agosto/2028. Gráficos individuais com histórico desde 2015 e painéis desde 2019.
- Sensibilidades adicionais do estrutural de sete variáveis com `lambda = 0,05` e `0,30`, somente para o cenário base e sem CV adicional.

Os scripts e saídas estão separados do modelo original. As curvas de Brent e Treasury foram mantidas, inclusive a hipótese de Brent a US$ 111 em novembro/2026.

## CV: resultados principais

RMSE para H1–H24, considerando todas as datas previstas. Atividade, CPI e câmbio estão na escala `100 × log(nível)`; juros e inclinação, em pontos percentuais. Portanto, o erro do CPI nesta tabela não é o erro da inflação em 12 meses.

| Variável | Reduzido com hiato | Reduzido λ = 0,10 | Estrutural anterior, como referência |
|---|---:|---:|---:|
| Atividade | 6,91 | 6,76 | 7,44 |
| CPI core | 1,61 | 1,40 | 1,49 |
| Juro curto | 0,50 | 0,46 | 0,49 |
| Inclinação | 0,18 | 0,19 | 0,18 |
| USD/JPY | 13,18 | 12,20 | 13,02 |

Entre os dois novos reduzidos, λ = 0,10 teve menor RMSE para quatro das cinco variáveis comuns; o modelo com hiato foi um pouco melhor para inclinação. Essa ordenação não prova que apenas o prior causou a diferença: também se alterou o conjunto de variáveis. A referência anterior é estrutural, portanto muda ainda a identificação.

### Período recente: 2022 a julho/2026

- **Inflação em 12 meses:** RMSE de 1,51 p.p. com hiato e 1,37 p.p. com λ = 0,10. O viés médio continuou negativo: −1,17 e −1,07 p.p., respectivamente.
- **Juro curto:** RMSE de 0,51 e 0,41 p.p.; viés de −0,32 e −0,16 p.p. O prior mais apertado reduziu a subestimação observada neste recorte.
- **Atividade em 12 meses:** RMSE de 5,45 e 5,22 p.p., contra 2,35 p.p. do drift. A atividade continua sendo uma fragilidade importante. O índice coincidente não representa a taxa de crescimento do PIB.

As tabelas completas e os gráficos mostram 2015–2019, 2020–2021 e 2022–2026 separadamente, para todas as variáveis. As origens se sobrepõem; não foi feito um teste formal de superioridade preditiva.

## Forecast estrutural: agosto/2028

Médias do cenário base; inflação é a variação do CPI em 12 meses, calculada em cada sorteio antes da média.

| Especificação | Inflação (%) | Juro curto (% a.a.) |
|---|---:|---:|
| Oito variáveis com hiato, λ = 0,20 | 1,37 | 1,26 |
| Sete variáveis, λ = 0,10 | 1,49 | 1,16 |
| Sensibilidade: sete variáveis, λ = 0,05 | 1,57 | 1,18 |
| Sensibilidade: sete variáveis, λ = 0,30 | 1,42 | 1,19 |

Os dois novos estruturais projetam juros acima do nível inicial, embora haja acomodação após o pico em parte das trajetórias médias. O hiato adicional não evitou a queda da inflação média para abaixo de 2%. Os testes de lambda alteraram pouco esse resultado; não indicaram que um ajuste global de shrinkage seja suficiente para produzir a trajetória desejada.

As distribuições são amplas: com λ = 0,10, o intervalo de 95% da inflação em agosto/2028 vai aproximadamente de −0,52% a 3,61%. Uma inflação de 2% está contemplada pela incerteza, mas não corresponde à previsão média. Não foi imposta nenhuma âncora de 2%.

## Por que o hiato não resolveu automaticamente

Adicionar o hiato permite que o modelo estime relações entre pressão de demanda, preços e juros. Isso não impõe uma curva de Phillips positiva nem introduz expectativas de inflação, salários ou uma mudança de regime monetário. Apertar lambda aproxima os coeficientes de seu prior e reduz a flexibilidade; isso também não estabelece uma taxa de inflação de longo prazo.

O próprio BoJ avalia que as pressões de salários e preços podem ser maiores do que o hiato sugere, devido às restrições de oferta de trabalho. No Outlook de julho/2026, sua projeção do hiato é qualitativa, de permanência em terreno positivo, sem trajetória mensal numérica utilizável. Por isso, o hiato futuro ficou endógeno. [BoJ, Outlook de julho/2026, pp. 4 e 15](https://www.boj.or.jp/en/mopo/outlook/gor2607b.pdf).

O hiato empregado é uma **proxy mensal do último hiato trimestral divulgado**, com calendário aproximado de divulgação e sem interpolação com valores futuros. A base usa revisões históricas posteriores, como as demais séries congeladas do CV. Isso limita a leitura dos resultados como avaliação em tempo real. [BoJ: dados e metodologia](https://www.boj.or.jp/en/research/research_data/gap/index.htm).

## Como usar estes resultados

λ = 0,10 é a alternativa mais promissora entre os dois novos formatos no CV, especialmente para juros e CPI. Não adotaria o hiato como correção suficiente do modelo. Também não escolheria λ = 0,05 apenas por deixar a inflação média um pouco maior: essa versão não recebeu um CV próprio.

A análise sugere investigar a persistência e o regime estimados para inflação e atividade, antes de atribuir a queda apenas a priors fracos. Se o objetivo for incorporar uma avaliação prospectiva do BoJ, isso exige uma hipótese econômica explícita, com incerteza, e deve ser apresentado separadamente da previsão livre do modelo.

## Arquivos

- `src/5- models/Comparacao hiato e priors/LEIA-ME.md`: especificações e ordem para rodar.
- `CV/`: métricas, diagnósticos por período e comparação com benchmarks.
- `hiato/` e `priors_fixos/`: quatro cenários, condições, tabelas e gráficos de todas as variáveis.
- `comparacao_forecasts_base.png`: painel das cinco variáveis comuns.
- `sensibilidade/inflacao_e_juros.png`: comparação de lambda 0,05, 0,10 e 0,30.
- `validacao_modelos.csv`: diagnósticos dos ajustes completos e sensibilidades.
