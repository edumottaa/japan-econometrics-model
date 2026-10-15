------------------------------------------------------------------------

editor_options: markdown: wrap: 72 ---

# Modelo integrado \| Case Japão 2026.2

## Especificação do núcleo (A), da camada de ativos (C) e dos satélites

Insumo técnico para o NT. Data de referência: 02/10/2026. Complementa o documento "Modelagem - Opcoes para debate". Onde os dois divergirem, vale este.

------------------------------------------------------------------------

## 0. A pergunta e por que um modelo só

**Pergunta do case:** para onde vai a economia japonesa (atividade, inflação, juro, câmbio, bolsa) nos próximos 6 a 8 trimestres, e como se posicionar para ter retorno com isso.

**Lição do case de US (2026.1):** VAR para atividade, curva de Phillips para inflação e regra de Taylor para juro. Os números não fechavam entre si porque cada modelo: - tratava as variáveis dos outros como dadas, com hipóteses próprias e não coordenadas (a Taylor usava uma inflação que não era a da Phillips, a Phillips usava um hiato que não era o do VAR); - tinha sua própria incerteza, sem nenhuma covariância entre os erros. Isso torna impossível montar um cenário ou uma banda conjunta coerente.

**Solução:** um sistema único, em que a equação de juro é a função de reação, a equação de preço é a Phillips em forma reduzida, e tudo é estimado junto. As trajetórias saem consistentes por construção, e a incerteza também. Os modelos auxiliares (Phillips detalhada, prêmio de prazo) continuam existindo, mas entram **dentro** do sistema via condicionamento (seção 3.7), e não como projeções paralelas.

**O que o desenho A + C entrega:** - projeção em densidade de atividade (com ponte para o PIB), inflação, juro curto, inclinação da curva, câmbio e bolsa; - decomposição histórica: quais choques explicam onde a economia está; - cenários estruturais: o que acontece com tudo se o BoJ for a 1,5%, ou se o prêmio fiscal abrir de novo; - mapa de exposição de cada instrumento a cada choque, e retorno esperado por cenário comparado ao que está no preço.

**O que ele não entrega sozinho:** a trajetória fiscal (entra como julgamento, via cenário), o detalhe de salário e de preço administrado (satélite S2) e o juro neutro (não estimamos; usamos as faixas de BoJ e FMI como cenário).

------------------------------------------------------------------------

## 1. O que mudou em relação ao documento de opções

Revisando a especificação com a pergunta reenquadrada para a economia, cinco ajustes:

1.  **A identificação estava desenhada para decompor o câmbio.** Para responder sobre a economia, faltavam um **choque de oferta doméstico** e um **choque de demanda global**. O Japão é exportador; sem demanda global no modelo, todo ciclo externo vira choque doméstico. Entra a produção industrial mundial (CPB) no bloco externo.
2.  **As restrições de zero no impacto saíram.** O juro curto do modelo (shadow rate ou JGB de 2 anos) é extraído da curva e se move no mesmo mês com qualquer choque. Zero no impacto seria falso. A separação entre choque monetário e choque de prêmio passa a ser feita pela **inclinação da curva**: aperto monetário achata, prêmio de prazo inclina.
3.  **Os choques globais e os domésticos ficam em blocos separados**, com identificação própria em cada um (seção 3.3). Os sinais sobre variáveis japonesas não são impostos nos choques globais; as respostas japonesas a choques globais saem dos dados.
4.  **O prêmio de prazo passa a ser estimado por ACM (regressões em três etapas), e não por um modelo afim via Kalman.** O ACM é mais simples, mais estável e é o padrão do Fed de Nova York. O Kalman fica para o fair value com coeficientes variáveis.
5.  **A implementação da exogeneidade de bloco foi detalhada** (seção 3.3), com o caminho no pacote e o plano B.

O resto se mantém: BVAR com prior hierárquica, restrições de sinal com narrativa, amostra longa, camada de ativos separada do núcleo.

------------------------------------------------------------------------

## 2. Arquitetura

```         
                       ┌──────────────────────────────────────────┐
  S1 Prêmio de prazo   │  NÚCLEO A: BVAR economia pequena e aberta │
  (ACM) ──────────────►│                                          │
                       │  Bloco externo (4)  ──►  Bloco doméstico (6)
  S2 Inflação e        │  US2y, IP mundial,       Atividade, CPI,  │
  salários ──────────► │  Brent, VIX              juro curto,      │
  (condicionamento)    │                          10a-2a, USDJPY,  │
                       │                          TOPIX            │
                       │                                          │
                       │  Saídas: projeções, IRF, FEVD,           │
                       │  decomposição histórica, cenários        │
                       └───────────────┬──────────────────────────┘
                                       │ choques estruturais (por draw)
                                       ▼
                       ┌──────────────────────────────────────────┐
  S3 Fair value do     │  CAMADA C: ativos                        │
  iene (Kalman) ──────►│  uma equação por instrumento,            │
  (diagnóstico)        │  dirigida pelos choques do núcleo        │
                       │  → exposição, retorno por cenário,       │
                       │    comparação com o preço                │
                       └──────────────────────────────────────────┘
```

------------------------------------------------------------------------

## 3. Núcleo A

### 3.1 Variáveis

| \# | Variável | Bloco | Fonte | Transformação | Observação |
|----|----|----|----|----|----|
| 1 | Treasury 2 anos | Externo | FRED | nível (%) | Caminho esperado do Fed |
| 2 | Produção industrial mundial | Externo | CPB World Trade Monitor | log ×100 | Demanda global; mensal desde 1991 |
| 3 | Brent em USD | Externo | FRED | log ×100 | Termos de troca, inflação importada |
| 4 | VIX | Externo | FRED / CBOE | log ×100 | Apetite a risco global |
| 5 | Atividade | Japão | Índice coincidente (Cabinet Office) ou produção industrial (METI) | log ×100 | Ponte para o PIB trimestral (3.6) |
| 6 | CPI core (ex-alimentos frescos), ajustado por imposto | Japão | MIC (e-Stat) | log ×100 | Dummies para medidas administrativas (seção 6.2) |
| 7 | Juro curto | Japão | Shadow rate (LJK/Krippner ou Nakajima) até a saída do ELB; depois a taxa efetiva | nível (%) | Robustez: JGB 2 anos na amostra toda |
| 8 | Inclinação 10a menos 2a | Japão | MoF (curva diária, média do mês) | nível (p.p.) | Alternativa: prêmio de prazo de 10a do S1 |
| 9 | USDJPY | Japão | FRED / BoJ | log ×100 | Alta = iene mais fraco |
| 10 | TOPIX | Japão | JPX | log ×100 | Ajuda a separar choque monetário de choque de risco |

**Alternativa para a atividade:** a JCER publica estimativa mensal do PIB. Se o histórico cobrir a amostra, ela é melhor que o índice coincidente, porque já está na métrica de PIB. Checar a extensão da série.

**Fica fora do núcleo, de propósito:** salários (série mensal ruidosa e com quebras de amostra; entra no S2), fiscal (julgamento via cenário), fluxos de portfólio (candidatos a 11ª variável, ver seção 8).

### 3.2 Estimação

- **Frequência e amostra:** mensal, de jan/2000 até o último dado. Robustez com a subamostra pós-2013.
- **Especificação:** VAR em nível (log ×100), com 12 defasagens. Em nível com prior bayesiana, não é preciso decidir sobre raiz unitária e cointegração (Sims, Stock e Watson, 1990), e as relações de longo prazo ficam preservadas.
- **Prior:** Minnesota hierárquica (Giannone, Lenza e Primiceri, 2015), com os hiperparâmetros estimados pelos dados:
  - λ: aperto geral do shrinkage;
  - ψ: escalas de variância por variável;
  - μ: soma de coeficientes (evita que a parte determinística explique demais o histórico);
  - δ: raiz unitária única (dummy initial observation).
- **COVID:** escalonamento de volatilidade de março a maio de 2020 e decaimento depois (Lenza e Primiceri, 2022). Os hiperparâmetros são estimados.
- **Conta de graus de liberdade:** cerca de 320 meses. O bloco doméstico tem cerca de 125 coeficientes por equação. Por MQO, seria sobreajuste; com o shrinkage, funciona. Esse é o motivo de ser bayesiano, e não "fronteira" por si só.

### 3.3 Exogeneidade de bloco (o "X" do modelo)

```         
Bloco externo   x_t = c_x + Σ_j A_xx,j x_{t-j}                         + u_x,t
Bloco doméstico z_t = c_z + Σ_j A_zx,j x_{t-j} + Σ_j A_zz,j z_{t-j}   + u_z,t

com A_xz,j = 0 para todo j (o Japão não afeta EUA e mundo, nem com defasagem)
e u_t = B ε_t, com B bloco-triangular: choques externos atingem o Japão no mesmo mês,
choques japoneses não atingem o bloco externo.
```

Com essa estrutura, a verossimilhança se fatora em dois blocos (Zha, 1999). Na prática: - **Caminho 1:** o `bsvarSIGNs` tem um argumento `foreign` para modelos de economia pequena e aberta. Confirmar na vignette se ele impõe exatamente essa estrutura. - **Caminho 2 (plano B, equivalente):** estimar o bloco externo sozinho (4 variáveis) e depois o bloco doméstico com `x_t` contemporâneo e defasado como regressores exógenos (argumento `exogenous`). Na projeção, simula-se primeiro o bloco externo e depois o doméstico condicional a cada draw.

### 3.4 Identificação

Sinais impostos no impacto e no mês seguinte, salvo indicação de horizonte. Vazio = livre.

**Bloco externo (4 choques, identificação completa por sinais):**

| Choque             | US2y | IP mundial     | Brent | VIX |
|--------------------|------|----------------|-------|-----|
| Demanda global     | \+   | \+             | \+    | −   |
| Oferta de petróleo |      | − (h = 1 a 6)  | \+    |     |
| Política do Fed    | \+   | ≤ (h = 3 a 12) | −     | \+  |
| Risk-off global    | −    |                | −     | \+  |

Como eles se separam: demanda e Fed pelo sinal do VIX e do Brent; Fed e risk-off pelo sinal do juro americano; petróleo e demanda pelo sinal da atividade mundial. Opcional: identificar o Fed pela proxy de Bauer e Swanson no lugar dos sinais.

**Bloco doméstico (5 choques identificados, 1 residual):**

| Choque | Atividade | CPI | Juro curto | 10a−2a | USDJPY | TOPIX |
|----|----|----|----|----|----|----|
| Demanda doméstica | \+ | \+ | ≥ |  |  |  |
| Oferta doméstica | \+ | − |  |  |  |  |
| Monetário BoJ | ≤ (h = 3 a 12) | ≤ (h = 3 a 12) | \+ | ≤ | **livre** | − |
| Prêmio fiscal / de prazo |  |  |  | \+ | \+ |  |
| Fluxo / UIP do iene |  | ≥ (h = 3 a 12) |  | ≤ | \+ | \+ |

Lógica de separação: - demanda e oferta, pelo sinal relativo de atividade e preço; - monetário e demanda, porque a demanda leva atividade e juro para cima e o aperto monetário derruba a atividade; - monetário e prêmio fiscal, pela inclinação: aperto achata, prêmio inclina; - prêmio fiscal e fluxo: ambos depreciam o iene, mas só o prêmio inclina a curva; - fluxo e afrouxamento do BoJ: ambos depreciam o iene e sobem a bolsa, mas afrouxamento inclina (bull steepening) e fluxo não.

**O câmbio fica livre no choque monetário.** Se uma alta do BoJ aprecia ou não o iene é resultado, não hipótese.

**Restrições narrativas** (Antolín-Díaz e Rubio-Ramírez, 2018). Os eventos são candidatos; **datas e direções a confirmar com fonte primária antes de impor:**

| Bloco | Evento | Mês | Restrição |
|----|----|----|----|
| Externo | Desmonte do carry | ago/2024 | Choque de risk-off \> 0 |
| Externo | Tarifas americanas | abr/2025 | Choque de risk-off \> 0 |
| Doméstico | Fim do juro negativo e do YCC | mar/2024 | Choque monetário BoJ \> 0 |
| Doméstico | Intervenções do MoF | set e out/2022; abr, mai e jul/2024 | Choque de fluxo \< 0 |
| Doméstico | Takaichi eleita para a presidência do LDP | out/2025 | Choque de prêmio fiscal \> 0 |

**Checagens obrigatórias da identificação:** - taxa de aceitação de rotações (se for muito baixa, as restrições estão brigando entre si); - comparar as IRFs da posterior com as IRFs geradas só pela prior, com os dados "desligados". Onde forem parecidas, o resultado é da prior, não dos dados (crítica de Baumeister e Hamilton, 2015); - estabilidade das IRFs principais na subamostra pós-2013.

### 3.5 Saídas

| Saída | Para que serve no deck |
|----|----|
| Projeção incondicional em densidade, 24 meses | Fan charts de atividade, CPI, juro, 10a−2a, USDJPY e TOPIX |
| Ponte para o PIB (3.6) | Projeção de PIB trimestral consistente com o resto |
| Decomposição histórica | "Por que a economia está onde está": slide central da conjuntura |
| FEVD | Que choques importam para cada variável em cada horizonte |
| IRFs | Transmissão: o que uma alta do BoJ faz com atividade, preço, iene e bolsa |
| Cenários estruturais | Base para a tese: ver 3.7 |
| Modelo contra mercado | Distribuição do modelo para o juro do BoJ contra a OIS; para o USDJPY contra o forward |

### 3.6 Ponte para o PIB

Regressão trimestral do crescimento do PIB contra a média trimestral da atividade mensal (e defasagens), estimada desde 2000. A projeção mensal do núcleo, agregada por trimestre, alimenta a ponte. Rodar em cada draw para carregar a incerteza. Alternativa mais elegante (BVAR de frequência mista) fica para uma v2.

### 3.7 Cenários e incorporação de julgamento (o que resolve o problema de 2026.1)

Três ferramentas, em ordem de uso:

1.  **Condicionamento rígido** (Waggoner e Zha, 1999): impor a trajetória de uma ou mais variáveis (por exemplo, Fed pelos futuros, Brent pela curva futura, BoJ pela OIS), e o modelo escolhe a combinação de choques mais provável para entregá-la. Gera um **baseline consistente com o mercado**.
2.  **Cenário estrutural**(Antolín-Díaz, Petrella e Rubio-Ramírez, 2021): impor a trajetória, mas só por meio de choques específicos. Exemplo: "BoJ a 1,5% em 2027 via choque monetário" contra "BoJ a 1,5% porque a demanda doméstica esquentou". O juro chega ao mesmo lugar; atividade, iene e bolsa não.
3.  **Condicionamento suave / entropic tilting** (Robertson, Tallman e Whiteman, 2005; Krüger, Clark e Ravazzolo, 2017): ajustar a densidade projetada para bater com um momento vindo do satélite. Exemplo: se o S2 indica que a média do CPI em 2027 é X por causa do fim dos subsídios, a densidade conjunta é reponderada para ter essa média, e **todas as outras variáveis se ajustam de forma coerente**.

Regra: **toda informação de fora do núcleo entra por uma dessas três portas.** Nunca como projeção paralela. É isso que impede os números de não baterem.

**Cenários sugeridos para a parcial:**

| Cenário | Como se impõe |
|----|----|
| Base de mercado | Condicionamento rígido: Fed, Brent e BoJ nas curvas de mercado |
| Normalização completa | Cenário estrutural: BoJ ao topo da faixa de neutro do FMI (2,2% nominal) via choque monetário |
| Normalização interrompida | Cenário estrutural: demanda doméstica negativa e BoJ parado em 1,0% |
| Prêmio fiscal | Cenário estrutural: choque de prêmio fiscal com magnitude do episódio de out/2025 |
| Desmonte de carry | Cenário estrutural: risk-off global com magnitude de ago/2024 |

### 3.8 Avaliação de projeção

- Pseudo out-of-sample recursivo desde 2015, reestimando a cada mês, com dado final. Não há vintages em tempo real fáceis para o Japão; mencionar a limitação.
- **Benchmarks:** passeio aleatório (câmbio, juro, bolsa), AR (atividade, CPI) e as projeções do BoJ no Outlook Report (CPI e PIB).
- **Métricas:** RMSE, log score e CRPS, mais o teste de direção de Pesaran e Timmermann. Para comparar, usar Diebold-Mariano ou Giacomini-White.
- **Expectativa:** ganho em atividade e CPI; empate com o passeio aleatório em câmbio e bolsa no ponto. Isso é normal e não invalida o modelo, cujo valor nesses ativos está em cenário e decomposição.

------------------------------------------------------------------------

## 4. Camada C: dos choques aos instrumentos

### 4.1 Equação por instrumento

Para cada instrumento i fora do núcleo:

```         
r_i,t = a_i + Σ_{j=1..q} φ_i,j r_i,t-j + Σ_{j=1..q} δ_i,j' y_{t-j} + λ_i' ε_t + e_i,t
```

- `r_i,t`: retorno mensal em excesso ao caixa (log), na moeda-base escolhida (seção 8);
- `y_t`: as 10 variáveis do núcleo;
- `ε_t`: os choques estruturais do núcleo (ortonormais, por draw);
- `λ_ik`: resposta no impacto do instrumento i a 1 desvio-padrão do choque k;
- `q` pequeno (1 a 3).

O instrumento responde ao núcleo, mas o núcleo não responde ao instrumento (mesma lógica de bloco da seção 3.3). Assim, escalar para 15 ou 20 instrumentos não infla o VAR.

**Estimação:** para cada draw da posterior do núcleo, recuperar os `ε_t` e estimar a equação. A incerteza do choque estimado (regressor gerado) é carregada automaticamente.

**Versão simplificada, para começar:** `φ = δ = 0`, ou seja, só os betas aos choques. Serve para a primeira rodada e para os membros mais juniores.

TOPIX e USDJPY já estão no núcleo; as respostas deles saem direto de A.

### 4.2 Universo inicial

| Classe | Modelagem (índice) | Execução | Hipótese principal |
|----|----|----|----|
| Juros | Retorno do JGB 10a (via duration e variação de yield); 10s30s | Futuro de JGB (OSE); swap | Prêmio fiscal, BoJ |
| Bancos | TOPIX Banks | ETF de bancos (1615) | Juro positivo e curva inclinada |
| Exportadoras | TOPIX-17 automóveis e transporte; eletrônicos | ETFs TOPIX-17 | Iene fraco, demanda global |
| Doméstico | TOPIX-17 varejo | ETF TOPIX-17 | Salário real positivo |
| Imobiliário | TSE REIT Index | ETFs de J-REIT | Sensível a juro |
| Seguradoras | TOPIX seguros | Ações / ETF setorial | Curva longa, hedge cambial |
| Bolsa em USD, com e sem hedge | MSCI Japan em USD e hedged | EWJ, HEWJ, DXJ | A diferença entre os dois é uma posição em iene |
| Ouro em iene | Ouro (LBMA) convertido para JPY | Futuro OSE ou ETF local | Prêmio fiscal e iene fraco |

Petróleo e GNL não entram como instrumento: o Japão é tomador de preço, então eles entram como choque no bloco externo.

**Atenção a quebra de beta:** a relação de bancos com juro provavelmente mudou com o fim do juro negativo (o juro negativo comprimia a margem, e o juro positivo a recompõe). Testar a estabilidade de λ com dummy pós-mar/2024 em bancos, seguradoras e J-REIT antes de usar a matriz para escolher posição.

### 4.3 Critério de seleção

Para cada cenário s e instrumento i:

1.  **Retorno esperado em excesso ao que está no preço:** `E[r_i | s]` menos o carry (forward no câmbio, carry e rolldown na curva, dividendo menos custo de financiamento na bolsa).
2.  **Pureza:** fração da variância do instrumento explicada pelo choque sobre o qual temos view (FEVD da equação 4.1). Instrumento com exposição alta ao choque certo, mas dominado por outros choques, é ruim.
3.  **Retorno por unidade de risco:** (1) dividido pela volatilidade condicional no cenário.
4.  **Assimetria:** o percentil 5 do retorno no cenário adverso.

O resultado é uma tabela instrumento × cenário que alimenta a escolha da fase 2.

------------------------------------------------------------------------

## 5. Satélites

### S1 \| Prêmio de prazo do JGB (ACM)

- **Método:** Adrian, Crump e Moench (2013). Primeiro, componentes principais da curva zero-cupom; segundo, VAR(1) dos fatores; terceiro, regressão dos excessos de retorno por vencimento nos fatores defasados e nas inovações. Daí saem os preços de risco, a curva neutra ao risco e o prêmio = taxa observada menos taxa neutra ao risco.
- **Dados:** curva zero-cupom mensal de 1 a 30 anos. O MoF publica a curva par diária; converter para zero-cupom com ajuste de Nelson-Siegel-Svensson mês a mês, ou usar a curva zero do Bloomberg, se houver acesso.
- **Uso:** variável 8 do núcleo (no lugar da inclinação, se ficar estável) e slide de decomposição do 10 anos do bloco de política monetária.
- **Risco:** no período de juro zero, a curva neutra ao risco pode implicar juro esperado negativo por muito tempo, distorcendo o prêmio. Checar contra pesquisas de expectativa. **Plano B:** inclinação 10a−2a.

### S2 \| Inflação, salários e preços administrados

- **Para que serve:** o CPI japonês está poluído por medidas de governo (subsídio de energia, corte do imposto sobre gasolina, tarifa de celular, educação). O núcleo não enxerga isso. O S2 produz a trajetória "limpa" e a de preços administrados, que entram no núcleo por entropic tilting (3.7).

- **Especificação:**

  ```         
  π_t = α π^e_t + (1 − α) π_{t−1} + κ hiato_t + γ Δp^imp_t + Σ dummies administrativas + e_t
  ```

  - π: core-core (ex-alimentos frescos e energia) ajustado por imposto, trimestral anualizado, e serviços separadamente;
  - π\^e: expectativa de inflação das firmas no Tankan (disponível desde 2014; amostra curta, avaliar);
  - hiato: do BoJ, com o do Cabinet Office como robustez;
  - Δp\^imp: preço de importação em iene (canal do câmbio).

- **Bloco de salários:** shunto (base-up) → salário programado no Monthly Labour Survey (defasagem de 3 a 6 meses) → inflação de serviços. É simples, e é o canal que o bloco 2 da conjuntura precisa quantificar.

- **Saída:** caminho para core-core, serviços e administrados até 2027, com as medidas de governo explicitadas.

### S3 \| Fair value do iene com coeficientes variáveis (Kalman)

- **Modelo de espaço de estados:**

  ```         
  Δs_t = β_t' Δx_t + e_t           (medida)
  β_t  = β_{t−1} + η_t             (estado: passeio aleatório)
  ```

  - s: log USDJPY;
  - x: diferencial de juro de 2 anos (EUA menos Japão), inclinação ou prêmio de prazo do JGB, log VIX e, se disponível, fluxo de portfólio de residentes;
  - em diferenças, para evitar regressão espúria.

- **Estimação:** filtro e suavizador de Kalman, com as variâncias de η estimadas por máxima verossimilhança.

- **Pergunta que responde:** o descolamento desde meados de 2025 foi queda do β do diferencial ou aumento do β do prêmio fiscal ou de fluxo? É diagnóstico para a conjuntura e para checar a decomposição do núcleo. Não é modelo de projeção.

### Juro neutro: não estimar

Modelos do tipo Laubach e Williams para o Japão produzem bandas largas demais para serem úteis em 6 semanas. Usar as faixas publicadas (FMI: 1,1% a 2,2% nominal; BoJ) como limites dos cenários de normalização.

------------------------------------------------------------------------

## 6. Dados

### 6.1 Fontes

| Série | Fonte | Frequência | Link |
|----|----|----|----|
| UST 2a, Brent, VIX, USDJPY | FRED | Diária/mensal | <https://fred.stlouisfed.org/> |
| Produção industrial mundial | CPB World Trade Monitor | Mensal, desde 1991 | <https://www.cpb.nl/en/world-trade-monitor> |
| Índice coincidente | Cabinet Office | Mensal | e-Stat / site do Cabinet Office |
| PIB mensal (alternativa) | JCER | Mensal | <https://www.jcer.or.jp/english> |
| CPI e componentes | MIC | Mensal | <https://www.e-stat.go.jp/> |
| Medidas de núcleo, Tankan, preços de importação, juros | BoJ | Mensal/trimestral | <https://www.stat-search.boj.or.jp/> |
| Curva de JGB | MoF | Diária, desde 1974 | <https://www.mof.go.jp/english/policy/jgbs/reference/interest_rate/index.htm> |
| Intervenções | MoF | Diária | <https://www.mof.go.jp/english/policy/international_policy/reference/feio/index.html> |
| Shadow rate | LJK (Krippner) / Nakajima | Mensal | <https://www.ljkmfa.com/> ; <https://sites.google.com/site/jnakajimaweb/yield> |
| TOPIX e índices setoriais | JPX | Diária | <https://www.jpx.co.jp/english/markets/> |
| Salários | MHLW (Monthly Labour Survey); Rengo (shunto) | Mensal/anual | Sites oficiais |

### 6.2 Armadilhas

- Imposto sobre consumo: abr/2014 e out/2019. Usar CPI ajustado ou dummies.
- Medidas administrativas: tarifa de celular (2021), educação gratuita (2019 e 2020), Go To Travel (2020), subsídios de energia (2023 a 2026), imposto sobre gasolina.
- Rebase do CPI para base 2025: checar publicação e encadear.
- Shadow rate: as estimativas variam com o modelo. Rodar com as duas versões e com o JGB de 2 anos.
- Emenda shadow rate → taxa efetiva: verificar se não há salto artificial no ponto de troca.
- Monthly Labour Survey: quebras por rotação de amostra. Usar a série de mesma amostra quando houver.

------------------------------------------------------------------------

## 7. Implementação

**Linguagem: R.** Pacotes: - `bsvarSIGNs`: núcleo com sinais, zeros e narrativa; tem prior hierárquica (λ, ψ, μ, δ), ajuste de COVID, `exogenous` e `foreign` (economia pequena e aberta); - `bsvars`: projeção. Verificar o suporte a condicionamento; se não houver, implementar Waggoner e Zha sobre os draws (cerca de 50 linhas); - `KFAS`: S3; - ACM: código próprio (há implementações públicas de referência em MATLAB e Python para traduzir).

**Sequência de execução:**

| Etapa | Entrega | Critério para seguir |
|----|----|----|
| 1\. Dados | Base mensal limpa + dicionário (série, fonte, código, transformação) | Tudo reproduzível a partir do script |
| 2\. Forma reduzida | Núcleo sem identificação + avaliação de projeção | Não perde do AR em atividade e CPI |
| 3\. Satélites S1 e S2 | Prêmio de prazo; caminho de inflação limpa | S1 estável; S2 coerente com o bloco 3 |
| 4\. Identificação | IRFs, FEVD, decomposição histórica | Aceitação razoável; IRFs diferentes das da prior |
| 5\. Camada C | Matriz de exposição; teste de quebra de beta | Betas estáveis ou quebra modelada |
| 6\. Cenários | Tabela cenário × variável × instrumento | Consistência com a narrativa da conjuntura |

Nada de identificação antes de a forma reduzida passar pela etapa 2.

------------------------------------------------------------------------

## 8. Decisões em aberto para o grupo

1.  Atividade: índice coincidente, produção industrial ou PIB mensal da JCER?
2.  Variável 8: inclinação 10a−2a ou prêmio de prazo do S1?
3.  Uma 11ª variável de fluxo (MoF semanal agregado no mês ou compra estrangeira de ações na JPX)? Ajuda a identificar o choque de fluxo, mas custa graus de liberdade.
4.  Moeda-base dos retornos da camada C: JPY (visão local), USD sem hedge ou USD com hedge? Isso muda o ranking dos instrumentos.
5.  Fed por sinais ou pela proxy de Bauer e Swanson?
6.  Os eventos narrativos: quem documenta cada um com fonte primária?
7.  Os cinco cenários da seção 3.7 cobrem as teses que a conjuntura está apontando?

------------------------------------------------------------------------

## 9. Referências

- Adrian, T., Crump, R. e Moench, E. (2013). Pricing the term structure with linear regressions. *Journal of Financial Economics*.
- Antolín-Díaz, J. e Rubio-Ramírez, J. (2018). Narrative sign restrictions for SVARs. *American Economic Review*.
- Antolín-Díaz, J., Petrella, I. e Rubio-Ramírez, J. (2021). Structural scenario analysis with SVARs. *Journal of Monetary Economics*.
- Bauer, M. e Swanson, E. (2023). A reassessment of monetary policy surprises and high-frequency identification. *NBER Macroeconomics Annual*.
- Baumeister, C. e Hamilton, J. (2015). Sign restrictions, structural vector autoregressions, and useful prior information. *Econometrica*.
- Giannone, D., Lenza, M. e Primiceri, G. (2015). Prior selection for vector autoregressions. *Review of Economics and Statistics*.
- Krüger, F., Clark, T. e Ravazzolo, F. (2017). Using entropic tilting to combine BVAR forecasts with external nowcasts. *Journal of Business & Economic Statistics*.
- Lenza, M. e Primiceri, G. (2022). How to estimate a VAR after March 2020. *Journal of Applied Econometrics*.
- Robertson, J., Tallman, E. e Whiteman, C. (2005). Forecasting using relative entropy. *Journal of Money, Credit and Banking*.
- Sims, C., Stock, J. e Watson, M. (1990). Inference in linear time series models with some unit roots. *Econometrica*.
- Waggoner, D. e Zha, T. (1999). Conditional forecasts in dynamic multivariate models. *Review of Economics and Statistics*.
- Zha, T. (1999). Block recursion and structural vector autoregressions. *Journal of Econometrics*.
- Wang, X. e Woźniak, T. bsvarSIGNs: Bayesian SVARs with sign, zero, and narrative restrictions. Pacote R (CRAN).
