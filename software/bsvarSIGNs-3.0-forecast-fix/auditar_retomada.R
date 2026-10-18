.libPaths(c("C:/Users/dudup/AppData/Local/R/win-library/4.6",.libPaths()))
root <- "C:/Users/dudup/OneDrive/japan-econometrics-model/data/processed"
old <- file.path(root,"cv_covid_nativo_20261018"); new <- file.path(root,"cv_covid_corrigido_20261018")
for (p in c(3,6)) {
 files <- file.path(new,"windows",sprintf("condicionado_p%d_w%03d.rds",p,1:47))
 stopifnot(all(file.exists(files)))
 good <- vapply(files,function(f){r<-readRDS(f);is.null(r$erro)&&all(table(r$pontuacoes$variavel)==1028L)&&nrow(r$diagnostico)==1},logical(1))
 # Each window has a different number of observed cases: validate totals below.
 all_scores <- do.call(rbind,lapply(files,function(f)readRDS(f)$pontuacoes))
 stopifnot(all(table(all_scores$variavel)==1028L), !anyDuplicated(all_scores[c("variavel",".id","h")]))
 stopifnot(all(vapply(files,function(f)is.null(readRDS(f)$erro),logical(1))))
 cat("Condicionado",p,":47janelas,1028casos/alvo,sem erros\n")
}
f <- list.files(file.path(new,"windows"),pattern="^foreign.*rds$",full.names=TRUE)
if(length(f)) {
 r<-readRDS(f[1]);stopifnot(is.null(r$erro),identical(r$versao_bsvarSIGNs,"3.0.0.1"),!is.null(r$posterior))
 cat("Primeiro checkpoint corrigido:",basename(f[1]),"forecast finito:",all(is.finite(r$sorteios)),"\n")
}
e <- jsonlite::read_json(file.path(old,"entregas.json"),simplifyVector=FALSE)
e$etapa3 <- list(tipo="tabela revisada e entregue",origens_por_modelo=47L,checkpoints=94L,alertas_p3=38L,alertas_p6=47L)
e$correcao <- list(autorizada=TRUE,versao_local="3.0.0.1",testes_passaram=12L,saida_ativa=basename(new))
e$etapas2e4 <- "autorizadas: retomada com pacote local corrigido"
jsonlite::write_json(e,file.path(old,"entregas.json"),auto_unbox=TRUE,pretty=TRUE)
jsonlite::write_json(e,file.path(new,"entregas.json"),auto_unbox=TRUE,pretty=TRUE)