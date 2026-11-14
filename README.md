# Japan Econometrics Model

Projeto de análise e projeção da economia japonesa em R, com BVAR estrutural e regressões com erros ARIMA.

## Organização

| Pasta ou arquivo | Conteúdo | Git |
| --- | --- | --- |
| `src/1- data_collection/` | Coleta das séries | Versionar |
| `src/2- data_cleaning/` | Limpeza e organização dos dados | Versionar |
| `src/3- feature_engineering/` | Transformações das variáveis | Versionar |
| `src/4- plot data/` | Scripts dos gráficos descritivos | Versionar |
| `src/5- models/` | Estimação dos modelos | Versionar |
| `src/6- evaluation/` | Validação e diagnósticos | Versionar |
| `src/7- forecast/` | Cenários, projeções e gráficos | Versionar |
| `docs/` | Documentação e relatórios selecionados | Revisar e versionar |
| `code_run.R` | Execução do modelo principal | Versionar |
| `data/` | Dados brutos, processados e caches | Local; ignorado |
| `articles/` | Material de referência | Local; ignorado |
| `src/Outputs/` | Gráficos, tabelas e modelos gerados | Local; ignorado |
| `tmp/` | Arquivos de trabalho, renderizações e dependências | Local; ignorado |

As versões antigas e os experimentos permanecem nas pastas existentes. Os modelos de PIB, S2 e BVAR reduzido continuam em `src`. A organização para o Git não altera os caminhos usados pelos scripts.

## Modelo principal

BVAR estrutural com oito variáveis, inclinação JGB30Y–5Y, seis defasagens, tendência, ponderação exponencial de 0,995 e cenários por choques de oferta. A entrada de execução é `code_run.R`; os parâmetros efetivos devem ser conferidos no script chamado por ele.

Os três testes de pesos com tendência têm executores próprios em:

- `src/7- forecast/bvar estrutural/sensibilidade pesos com tendencia/rho_0990/0- rodar.R`
- `src/7- forecast/bvar estrutural/sensibilidade pesos com tendencia/rho_0980/0- rodar.R`
- `src/7- forecast/bvar estrutural/sensibilidade pesos com tendencia/rho_0970/0- rodar.R`

O S2 explicativo trimestral está em `src/5- models/s2 models/s2 explicativo trimestral.R`. Os gráficos e diagnósticos são salvos em `src/Outputs/S2/explicativo trimestral/`. Essa primeira especificação ainda tem limitações nos diagnósticos e não identifica efeitos causais.

## Credenciais

As chaves de FRED e e-Stat ficam no arquivo local `.Renviron`, que não entra no Git. O formato está em `.Renviron.example`. Os scripts de coleta leem esse arquivo sem expor os valores.

## Antes do commit

1. Confira as alterações de código e os documentos que deseja manter.
2. Confira também as exclusões: arquivos gerados podem aparecer como removidos do Git, embora continuem na pasta local.
3. Se quiser guardar um gráfico ou uma tabela no repositório, copie somente a versão selecionada para uma subpasta de `docs/`.
4. Faça o commit pela interface ou pelo terminal quando terminar a revisão.

O `.gitignore` evita incluir novos dados, caches e temporários; arquivos já versionados precisam ser retirados do índice do Git uma vez para essa regra passar a valer. Isso não apaga os arquivos do computador. A remoção do índice também não elimina cópias que já estejam no histórico de commits.
