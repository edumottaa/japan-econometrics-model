from pathlib import Path
import hashlib
out=Path(r'C:\Users\dudup\Documents\Codex\2026-10-14\ex\outputs')
report='''# Diagnóstico do forecast do BVAR

## Conclusão

O resultado extremo foi reproduzido e atribuído a dois erros na rotina de previsão usada pelo bsvarSIGNs 3.0 instalado. A divisão de treino/teste e a organização dos sorteios para o fable não são a causa da explosão. Os parâmetros estimados do BVAR foram preservados em todos os testes da correção.

## Base e reprodução

- 319 observações mensais, de janeiro de 2000 a julho de 2026; dez variáveis, sem valores ausentes ou não finitos.
- Treino: janeiro de 2000 a julho de 2024 (295 observações).
- Teste: agosto de 2024 a julho de 2026 (24 observações).
- Mesmas dez variáveis e transformações do código; CPI em 100 × log(índice).
- BVAR com 12 defasagens, hiperparâmetros padrão fixos e 200 sorteios, seed 123.
- Versões instaladas: bsvarSIGNs 3.0; bsvars 4.0; fabletools 0.8.0; distributional 0.8.1.

A reprodução deu RMSE 254,3506, MAE 116,3761 e CRPS 51,5281: os valores arredondados da imagem do usuário. Os benchmarks também reproduziram a tabela mostrada.

## Os dois defeitos

### 1. Estado inicial com um mês de atraso

O método R de previsão do bsvarSIGNs usa a última coluna de X como estado inicial. Essa coluna contém os regressores da última observação estimada, começando em junho de 2024. Para prever agosto, o primeiro bloco deve começar em julho de 2024, o último mês observado no treino.

No CPI, a rotina começa com 468,02777 (junho), em vez de 468,49052 (julho).

### 2. As defasagens de ordem 2 a 12 não avançam

No código C++ consultado, a atualização sem exógenas é:

```cpp
Xt = join_cols(forecasts.slice(s).col(h), Xt.subvec(N, K - 1));
```

O primeiro bloco é substituído pela previsão nova, enquanto os blocos mais antigos são mantidos na posição inicial. Para um VAR(2), o estado deveria passar de [julho, junho] para [agosto previsto, julho]; a rotina mantém o bloco antigo, em vez de inserir julho.

Isso deixa de reproduzir a dinâmica do VAR estimado. A estabilidade da matriz companheira do modelo completo não protege contra essa atualização incorreta.

Fonte oficial consultada: [forecast_bsvarSIGNs.cpp](https://github.com/bsvars/bsvarSIGNs/blob/main/src/forecast_bsvarSIGNs.cpp), particularmente a atualização de Xt; o método R instalado foi inspecionado com getS3method().

## Testes que isolam a causa

1. **Reprodução exata do caso real:** métricas iguais às da imagem após arredondamento.
2. **Sem choques futuros:** foi usada covariância quase zero e o estado inicial foi corrigido. A saída nativa coincidiu com uma recursão de defasagens congeladas, com diferença máxima de cerca de 0,0000071. Isso confirma a atualização identificada no fonte.
3. **Modelo artificial estável:** os coeficientes foram fixados em um AR(2) com 1,5 na primeira defasagem e −0,6 na segunda. O módulo das raízes da matriz companheira é 0,7746. No horizonte 24, a recursão correta deu 0,8233; a rotina nativa deu aproximadamente −1.567.415. Logo, o defeito aparece mesmo sem incerteza dos coeficientes e com um modelo estável.
4. **Mesma posterior do BVAR real, com recursão corrigida:** os 200 sorteios de coeficientes e covariâncias foram reutilizados. As trajetórias passaram a usar o último mês observado e todas as defasagens foram avançadas a cada mês. Choques normais correlacionados continuaram sendo simulados, usando a covariância de cada sorteio.
5. **Três sementes para os choques futuros:** RMSE de 1,9057, 1,8287 e 1,9412; MAE de 1,7005, 1,6359 e 1,7191; CRPS de 1,1342, 1,0708 e 1,1836. A ordem de grandeza corrigida se mantém; há variação de Monte Carlo com apenas 200 trajetórias.
6. **Cópia proposta:** foi executada com a posterior previamente salva. Sua matriz de previsões coincidiu exatamente com o teste independente (diferença máxima zero). Não foi necessário reestimar o BVAR para corrigir a previsão.

A troca simples para a rotina nativa do bsvars 4.0 também foi testada com os mesmos parâmetros. Ela corrigiu o estado inicial, mas manteve a explosão das defasagens; portanto, não foi adotada como solução.

## Comparação após a correção

Resultados com seed 123 para os choques futuros:

| Modelo | RMSE | MAE | CRPS |
|---|---:|---:|---:|
| VAR | 0,611 | 0,517 | 0,415 |
| AR/ARIMA | 0,915 | 0,814 | 0,513 |
| BVAR corrigido | 1,906 | 1,700 | 1,134 |
| Drift | 2,428 | 2,167 | 1,658 |
| Naïve | 2,892 | 2,574 | 2,068 |

Corrigir o algoritmo não torna o BVAR vencedor: ele permanece atrás do VAR e do AR neste holdout. Seu desempenho deve ser avaliado como resultado legítimo desse período e dessa especificação. Nenhuma prior, defasagem ou variável foi ajustada para melhorar artificialmente o ranking.

## Proposta para aprovação

A cópia `Proposta_holdout_BVAR_corrigido.R` preserva a estimação existente e substitui apenas a chamada de previsão por um bloco explícito de simulação bayesiana. Não cria funções próprias nem instala ou modifica pacotes.

Para cada sorteio dos parâmetros:

1. Começa com os últimos 12 meses do treino.
2. Calcula a média condicional com os coeficientes daquele sorteio.
3. Acrescenta um choque com a covariância daquele mesmo sorteio.
4. Retira o mês mais antigo e inclui o mês previsto.
5. Repete até o horizonte 24.

Assim, são mantidas as incertezas dos parâmetros e dos choques futuros. A solução se refere à especificação atual: constante, sem regressores exógenos e sem ajuste de volatilidade de COVID ativado.

O script principal `Modelo var reduzido.R` não foi alterado nesta investigação. A CV antiga também não foi corrigida no principal; o mesmo reparo será necessário nela antes de voltar a interpretar suas métricas de BVAR.
'''
(out/'Diagnostico_BVAR.md').write_text(report,encoding='utf-8')
main=Path(r'C:\Users\dudup\OneDrive\NT Macro\Modelo var reduzido.R')
expected=Path(r'C:\Users\dudup\OneDrive\NT Macro\work\psi_diagnostic\main_hash_before_final.txt').read_text().strip()
actual=hashlib.sha256(main.read_bytes()).hexdigest()
print('Principal preservado: '+str(expected==actual))
for name in ['Diagnostico_BVAR.md','Proposta_holdout_BVAR_corrigido.R','Comparacao_holdout_corrigido.csv','Diagnostico_previsao_BVAR.png']:
 p=out/name
 print(str(p)+' | '+str(p.stat().st_size)+' bytes')
