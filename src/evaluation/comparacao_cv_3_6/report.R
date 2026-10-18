args <- commandArgs(trailingOnly=TRUE)
source("C:/Users/dudup/OneDrive/japan-econometrics-model/src/evaluation/comparacao_cv_3_6/common.R")
run_dir <- if(length(args)) args[1] else readLines(file.path(root,"src/evaluation/comparacao_cv_3_6/latest_run.txt"))[1]
r <- readRDS(file.path(run_dir,"results.rds")); cfg <- readRDS(file.path(run_dir,"config.rds"))
stopifnot(nrow(r$completion)==20,all(r$completion$complete))
fmt <- function(x) ifelse(is.na(x),"-",formatC(x,digits=3,format="f"))
md_table <- function(x) {
 x <- as.data.frame(x); x[] <- lapply(x,function(z) if(is.numeric(z)) fmt(z) else as.character(z))
 c(paste0("| ",paste(names(x),collapse=" | ")," |"),paste0("| ",paste(rep("---",ncol(x)),collapse=" | ")," |"),
 apply(x,1,function(z) paste0("| ",paste(z,collapse=" | ")," |")),"")
}
labels <- c(ar="AR",naive="Naive",drift="Drift",var="VAR domestico",BVAR_normal_p3="BVAR normal (3)",BVAR_normal_p6="BVAR normal (6)",
 BVAR_condicionado_p3="BVAR condicionado (3)",BVAR_condicionado_p6="BVAR condicionado (6)",bsvarSIGNs_foreign_p3="bsvarSIGNs foreign (3)",bsvarSIGNs_foreign_p6="bsvarSIGNs foreign (6)")
metric_table <- function(v) r$overall |> filter(variable==v) |> mutate(Modelo=labels[model],`Ganho RMSE vs AR (%)`=100*RMSE_skill) |>
 select(Modelo,RMSE,MAE,CRPS,`Ganho RMSE vs AR (%)`)
lines <- c("# Comparacao temporal de seis BVARs: 3 e 6 defasagens", "",
 paste0("Gerado em ",format(Sys.time(),"%d/%m/%Y %H:%M %Z"),". Pasta da execucao: `",basename(run_dir),"`."),"",
 "## 1. O que foi testado", "",
 "Foram avaliadas seis configuracoes, sem alterar os scripts principais: BVAR normal, BVAR condicionado e bsvarSIGNs com bloco externo (`foreign`), cada um com 3 e 6 defasagens. CPI core e atividade foram avaliados separadamente. Os benchmarks foram AR, naive, drift e VAR domestico.","",
 "O BVAR normal e o condicionado usam exatamente a mesma estimacao em cada origem/lag. O que muda e a previsao: o condicionado recebe caminhos futuros de quatro variaveis externas previstos por um VAR estimado apenas com o treino. Isso permite medir o efeito do condicionamento sem confundi-lo com outra estimacao.","",
 "O bsvarSIGNs e uma especificacao diferente: impoe exogeneidade de bloco e usa hiperparametros fixos. Uma diferenca de resultado em relacao ao BVAR pode refletir tanto as restricoes quanto os priors; este experimento nao isola o efeito puro do argumento foreign.","",
 "| Configuracao | Hiperparametros | Sorteios por origem |", "| --- | --- | --- |",
 "| BVAR normal e condicionado | lambda, SOC e SUR estimados; psi automatico no treino e depois fixo | 5.000 iteracoes, descarte de 2.500; 2.500 retidas |",
 "| bsvarSIGNs foreign | lambda=0,2; mu=1; delta=1; psi automatico no treino e fixo; nenhum hiperparametro estimado | 2.500 sorteios e reamostragem de importancia do pacote |","",
 "Sem identificacao economica adicional por sinais, sem variavel COVID e sem escolha de configuracao baseada no resultado de uma janela isolada.","",
 "## 2. Protocolo comum", "",
 "- Base congelada: janeiro de 2000 a julho de 2026, 319 meses, dez variaveis, sem lacunas internas.",
 "- Treino inicial: 180 meses. Janela expansiva, avancando tres meses por origem.",
 "- 47 origens: dezembro de 2014 a junho de 2026. Horizonte de previsao: 12 meses.",
 "- 538 pares origem/horizonte observados para cada alvo e modelo. As ultimas origens possuem menos de 12 realizacoes disponiveis; todos usam os mesmos pares.",
 "- Todas as series externas futuras usadas no condicionado sao previsoes, nunca realizacoes futuras: ip_mundial, brent, vix e ust2y, previstas pelo VAR externo com selecao por AICc em cada treino.",
 "- Atividade e o indice coincidente do Cabinet Office; nao e PIB. Os alvos foram avaliados nas transformacoes ja utilizadas pelo projeto, sem converter para inflacao ou crescimento: indices em 100*log.",
 "- As sementes foram fixadas por origem. A base, a configuracao, as previsoes resumidas, os diagnosticos e os avisos foram salvos.","",
 "O sistema tem dez variaveis. Com tres lags, ha 31 coeficientes por equacao incluindo a constante; com seis, 61. As 121 entradas mencionadas anteriormente correspondiam aos regressores de uma equacao com 12 lags, nao a 121 series.","",
 "A avaliacao e pseudo fora da amostra: utiliza uma fotografia atual da base, que pode conter revisoes historicas. Nao e um backtest com os dados exatamente disponiveis em cada data no passado.","",
 "## 3. Resultados do CPI core", "", md_table(metric_table("cpi_core")),
 "## 4. Resultados da atividade", "", md_table(metric_table("atividade")),
 "RMSE, MAE e CRPS: menor e melhor. Ganho positivo no RMSE significa reducao do erro em relacao ao AR. O RMSE penaliza mais os erros grandes; o CRPS avalia a distribuicao preditiva inteira.","",
 "![RMSE geral](figures/rmse_geral.png)","", "![RMSE por horizonte relativo ao AR](figures/rmse_horizontes.png)","",
 "## 5. Diagnosticos da estimacao", "", md_table(r$quality),
 "No BVAR, foi calculado o ESS de cada hiperparametro e registrada a taxa de aceitacao. ESS minimo abaixo de 50 recebeu o aviso low_hyper_ess; cinco ou menos valores distintos ou valores invalidos receberam diagnostic_failure. Esses limiares sao filtros praticos deste teste, nao provas formais de convergencia. Nao foram rodadas cadeias multiplas nem calculado R-hat.","",
 "No bsvarSIGNs, foram registrados os pesos antes da reamostragem, o ESS de importancia, o maior peso e o numero de conjuntos distintos de coeficientes. ESS ou conjuntos distintos abaixo de 50 receberam diagnostic_failure. Tambem foi verificado se os coeficientes domesticos nas equacoes externas e o impacto contemporaneo de choques domesticos nas variaveis externas eram zero, com tolerancia de 1e-8.","",
 "Uma boa metrica acompanhada de concentracao extrema dos pesos nao valida o modelo. As metricas gerais incluem todas as previsoes, com os alertas preservados. O arquivo common_good.csv mostra uma comparacao adicional apenas nos pares aprovados para todos os dez modelos, mantendo a amostra comum; uma amostra muito pequena tambem limita essa leitura.","",
 "## 6. Intervalos preditivos", "", md_table(r$overall |> mutate(Modelo=labels[model]) |> select(variable,Modelo,coverage68,coverage90,width90)),
 "Cobertura68 e cobertura90 sao as proporcoes observadas dentro dos intervalos centrais de 68% e 90%. Intervalos maiores nao sao automaticamente melhores: cobertura e largura devem ser lidas juntas.","",
 "O condicionado fixa os caminhos externos nas medias previstas pelo VAR. Assim, a incerteza sobre o proprio caminho externo nao e propagada. Isso importa especialmente para os intervalos e para o CRPS.","",
 "## 7. Resultados por periodo", "", md_table(r$by_period |> mutate(Modelo=labels[model]) |> select(variable,period,Modelo,cases,RMSE,MAE,CRPS) |> arrange(variable,period,RMSE)),
 "A divisao usa a data prevista: 2015-2019, 2020-2021 e 2022-2026. Serve para localizar a origem dos erros; nenhum periodo foi excluido do ranking principal. A mesma realizacao pode aparecer em mais de uma origem/horizonte, como e usual nesse CV.","",
 "## 8. Frequencia de vitoria por origem", "", md_table(r$origin_wins |> mutate(Modelo=labels[model]) |> select(variable,Modelo,origin_win_rmse,origin_win_crps)),
 "Essas proporcoes comparam o modelo ao AR dentro de cada origem. Diferem de vencer no RMSE agregado: poucos erros muito grandes podem alterar o ranking geral.","",
 "## 9. Cuidados ao comparar com o CV anterior", "",
 "A rotina instalada de previsao nao condicionada do BVAR usa a covariancia diretamente ao gerar o ruido e adiciona esse ruido ao caminho sem propagar os choques nos horizontes seguintes. Nesta bateria foi usada a recursao padrao do VAR: choque com fator de Cholesky de Sigma e atualizacao das defasagens a cada mes. O bsvarSIGNs usa a mesma rotina recursiva. A previsao condicionada permanece no metodo nativo do BVAR.","",
 "A conversao dos coeficientes do BVAR foi conferida contra sua previsao deterministica, com diferenca maxima de 8,2e-11. Uma simulacao AR(1) confirmou as variancias teoricas nos dois primeiros horizontes. O CRPS empirico foi conferido contra sua definicao por distancias entre todos os pares de amostras.","",
 "O CRPS desta bateria e calculado diretamente sobre as 2.500 amostras para os BVARs e analiticamente para benchmarks gaussianos. A versao instalada do fabletools recorre a uma aproximacao por quantis para essas amostras. Portanto, diferencas de CRPS em relacao a tabelas antigas nao podem ser atribuidas apenas aos lags.","",
 "As medias preditivas tambem tem variacao Monte Carlo. A comparacao interna desta bateria e padronizada; comparar numeros antigos exige manter base, origens, horizonte, priors, sementes e rotina de previsao iguais.","",
 "## 10. Como interpretar e escolher", "",
 "O vencedor preditivo deve ser definido por alvo e metrica, considerando os diagnosticos. O condicionado pode continuar util para cenarios economicos mesmo se nao superar o AR em previsoes usuais, mas nesse caso nao se deve apresenta-lo como vencedor preditivo.","",
 "Escolher entre seis configuracoes usando este CV e uma etapa de selecao. Uma diferenca pequena nao prova superioridade populacional: os erros se sobrepoem entre origens e horizontes. Nao foram feitos testes de significancia nem reservado aqui um segundo conjunto intocado para confirmar a escolha.","",
 "## 11. Arquivos e reproducao", "",
 "- config.json / config.rds: protocolo, versoes e identificacao da base.",
 "- data_frozen.rds: base congelada usada em todas as estimacoes.",
 "- windows/: checkpoint separado para cada familia, lag e origem, inclusive eventuais erros.",
 "- forecast_scores.csv: realizacao, media, erro, quantis, CRPS e qualidade de cada previsao.",
 "- overall.csv, by_horizon.csv, by_origin.csv e by_period.csv: comparacoes agregadas.",
 "- diagnostics.csv, quality.csv e issues.csv: qualidade, tempo, avisos e erros.",
 "- completion.csv: conferencia das 47 origens e 538 casos por alvo/modelo.",
 "- results.rds: todas as tabelas em um objeto R.","",
 "Scripts em src/evaluation/comparacao_cv_3_6: prepare.R congela a base e calcula benchmarks/caminhos; worker.R executa uma familia/lag, salvando cada janela; aggregate.R agrega; report.R gera este documento. O worker ignora checkpoints existentes para permitir retomar uma execucao interrompida. Para outra configuracao, use outra pasta de execucao; nao misture checkpoints com parametros diferentes.","",
 "Exemplo de retomada de uma familia em uma sessao R independente, a partir da raiz do projeto:","",
 "```r",paste0("system2(file.path(R.home('bin'), 'Rscript.exe'), c('--vanilla', shQuote('src/evaluation/comparacao_cv_3_6/worker.R'), shQuote('",gsub("\\\\","/",run_dir),"'), 'native', '3', '1', '1'))"),"```","",
 "Os testes foram executados em processos R independentes, com concorrencia limitada pela memoria do computador. Os arquivos dos modelos principais foram preservados.")
writeLines(enc2utf8(lines),file.path(run_dir,"relatorio.md"),useBytes=TRUE)
cat(file.path(run_dir,"relatorio.md"),"\n")