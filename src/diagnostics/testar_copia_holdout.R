.libPaths(c("C:/Users/dudup/AppData/Local/R/win-library/4.6", .libPaths()))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(fpp3))
suppressPackageStartupMessages(library(bsvarSIGNs))
work <- "C:/Users/dudup/OneDrive/NT Macro/work/psi_diagnostic"
out <- "C:/Users/dudup/Documents/Codex/2026-10-14/ex/outputs"
d <- readRDS(file.path(work,"holdout_diagnostic_data.rds"))
base <- as_tibble(rbind(d$train,d$test)) |> mutate(date=d$dates) |> as_tsibble(index=date)
env <- new.env(parent=globalenv())
env$data_bal <- base
expressions <- parse(file.path(out,"Proposta_holdout_BVAR_corrigido.R"))
for(e in expressions) {
 if(is.call(e) && identical(e[[1]],as.name("<-")) && identical(e[[2]],as.name("post_holdout"))) {
  env$post_holdout <- readRDS(file.path(work,"holdout_original_post.rds"))
  cat("Reutilizados os mesmos 200 sorteios ja estimados.\n")
 } else eval(e,envir=env)
}
cat("RESULTADO DA COPIA COMPLETA\n");print(env$holdout_results)
write.csv(env$holdout_results,file.path(out,"Comparacao_holdout_corrigido.csv"),row.names=FALSE)
expected <- readRDS(file.path(work,"holdout_corrected_diagnostic.rds"))$forecasts[["123"]]
cat("DIFERENCA MAXIMA ENTRE COPIA E TESTE INDEPENDENTE\n");print(max(abs(env$previsoes-expected)))
cat("POSTERIOR ORIGINAL PRESERVADA\n");print(identical(env$post_holdout$posterior,readRDS(file.path(work,"holdout_original_post.rds"))$posterior))
orig <- readRDS(file.path(work,"holdout_original_diagnostic.rds"))
comparison <- bind_rows(
 tibble(date=tail(d$dates,d$h),actual=d$test[,"cpi_core"],forecast=rowMeans(orig$samples),method="Rotina nativa"),
 tibble(date=tail(d$dates,d$h),actual=d$test[,"cpi_core"],forecast=rowMeans(env$sorteios_cpi),method="Recursao corrigida")
) |> mutate(date=as.Date(date),method=factor(method,levels=c("Rotina nativa","Recursao corrigida")))
p <- ggplot(comparison,aes(date)) +
 geom_line(aes(y=actual,colour="Observado"),linewidth=.8) +
 geom_line(aes(y=forecast,colour="Previsao BVAR"),linewidth=.9) +
 facet_wrap(~method,scales="free_y",ncol=1) +
 scale_colour_manual(values=c("Observado"="#333333","Previsao BVAR"="#1F77B4")) +
 theme_minimal(base_size=12) + theme(legend.position="top",legend.title=element_blank()) +
 labs(title="O erro estava na recursao da previsao",subtitle="Mesmos 200 sorteios dos parametros; teste de ago/2024 a jul/2026",x=NULL,y="CPI: 100 x log(indice)",caption="Paineis com escalas diferentes. A correcao preserva o BVAR estimado.")
ggsave(file.path(out,"Diagnostico_previsao_BVAR.png"),p,width=9,height=7,dpi=160)
cat("DONE\n")
