args <- commandArgs(trailingOnly = TRUE)
source("C:/Users/dudup/OneDrive/japan-econometrics-model/src/evaluation/comparacao_cv_3_6/common.R")
run_dir <- if (length(args)) args[1] else readLines(file.path(root,"src/evaluation/comparacao_cv_3_6/latest_run.txt"),warn=FALSE)[1]
files <- list.files(file.path(run_dir,"windows"),pattern="[.]rds$",full.names=TRUE)
windows <- setNames(lapply(files,readRDS),basename(files))
config <- readRDS(file.path(run_dir,"config.rds"))
scores <- bind_rows(readRDS(file.path(run_dir,"benchmarks.rds")),unname(lapply(windows,`[[`,"scores")))
diagnostics <- bind_rows(unname(lapply(windows,`[[`,"diagnostic")))
issues <- imap_dfr(windows,function(x,nm) tibble(file=nm,origin=as.character(x$origin),seconds=x$elapsed_seconds,
  stage=x$stage,error=if(is.null(x$error)) NA_character_ else x$error,warnings=paste(x$warnings,collapse=" | ")))
expected <- c("ar","naive","drift","var","BVAR_normal_p3","BVAR_normal_p6",
 "BVAR_condicionado_p3","BVAR_condicionado_p6","bsvarSIGNs_foreign_p3","bsvarSIGNs_foreign_p6")
expected_cases <- sum(pmin(config$h,config$n-config$origins))
stopifnot(!anyDuplicated(scores[c("model","fold","variable","h")]),all(is.finite(scores$crps)),all(scores$crps>= -1e-8))
actual_check <- scores |> group_by(fold,variable,h) |> summarise(distinct_actual=n_distinct(actual),.groups="drop")
stopifnot(all(actual_check$distinct_actual==1))
metrics <- function(x,...) x |> group_by(...) |> summarise(cases=n(),origins=n_distinct(fold),
 RMSE=sqrt(mean(squared_error)),MAE=mean(absolute_error),CRPS=mean(crps),
 coverage68=mean(actual>=q16 & actual<=q84),coverage90=mean(actual>=q05 & actual<=q95),
 width90=mean(q95-q05),.groups="drop")
overall <- metrics(scores,variable,model) |> left_join(
 metrics(filter(scores,model=="ar"),variable) |> select(variable,AR_RMSE=RMSE,AR_MAE=MAE,AR_CRPS=CRPS),by="variable") |>
 mutate(RMSE_skill=1-RMSE/AR_RMSE,MAE_skill=1-MAE/AR_MAE,CRPS_skill=1-CRPS/AR_CRPS) |> arrange(variable,RMSE)
by_horizon <- metrics(scores,variable,model,h)
by_origin <- metrics(scores,variable,model,fold,origin)
by_period <- scores |> mutate(period=case_when(date<as.Date("2020-01-01")~"2015-2019",
 date<as.Date("2022-01-01")~"2020-2021",TRUE~"2022-2026")) |> metrics(variable,model,period)
paired <- scores |> left_join(filter(scores,model=="ar") |>
 select(fold,variable,h,ar_se=squared_error,ar_ae=absolute_error,ar_crps=crps),by=c("fold","variable","h")) |>
 group_by(variable,model) |> summarise(case_win_squared=mean(squared_error<ar_se),case_win_absolute=mean(absolute_error<ar_ae),
 case_win_crps=mean(crps<ar_crps),.groups="drop")
origin_wins <- by_origin |> left_join(filter(by_origin,model=="ar") |>
 select(variable,fold,AR_RMSE=RMSE,AR_CRPS=CRPS),by=c("variable","fold")) |>
 group_by(variable,model) |> summarise(origin_win_rmse=mean(RMSE<AR_RMSE),origin_win_crps=mean(CRPS<AR_CRPS),.groups="drop")
completion <- tidyr::expand_grid(variable=alvos,model=expected) |>
 left_join(overall |> select(variable,model,cases,origins),by=c("variable","model")) |>
 mutate(expected_cases=expected_cases,expected_origins=length(config$origins),complete=coalesce(cases==expected_cases & origins==length(config$origins),FALSE))
quality <- diagnostics |> count(family,p,quality,name="windows")
# Comparacao adicional em origens aprovadas para todos os modelos ja calculados: mesma amostra.
good_cases <- scores |> group_by(fold,variable,h) |>
 summarise(n_models=n_distinct(model),all_good=all(quality!="diagnostic_failure" & quality!="execution_failure"),.groups="drop") |>
 filter(n_models==length(expected),all_good)
common_good <- scores |> semi_join(good_cases,by=c("fold","variable","h")) |> metrics(variable,model)
outputs <- list(scores=scores,overall=overall,by_horizon=by_horizon,by_origin=by_origin,by_period=by_period,
 paired=paired,origin_wins=origin_wins,completion=completion,diagnostics=diagnostics,quality=quality,issues=issues,common_good=common_good)
saveRDS(outputs,file.path(run_dir,"results.rds"))
for(nm in setdiff(names(outputs),"scores")) write_csv(outputs[[nm]],file.path(run_dir,paste0(nm,".csv")))
write_csv(scores,file.path(run_dir,"forecast_scores.csv"))
labels <- c(ar="AR",naive="Naive",drift="Drift",var="VAR domestico",BVAR_normal_p3="BVAR normal: 3",BVAR_normal_p6="BVAR normal: 6",
 BVAR_condicionado_p3="BVAR condicionado: 3",BVAR_condicionado_p6="BVAR condicionado: 6",bsvarSIGNs_foreign_p3="bsvarSIGNs foreign: 3",bsvarSIGNs_foreign_p6="bsvarSIGNs foreign: 6")
plot_data <- overall |> mutate(model_label=factor(labels[model],levels=rev(unname(labels))),variable=recode(variable,cpi_core="CPI core",atividade="Atividade"))
p1 <- ggplot(plot_data,aes(RMSE,model_label,fill=RMSE_skill>0))+geom_col(width=.7)+facet_wrap(~variable,scales="free_x")+
 scale_fill_manual(values=c("TRUE"="#287D8E","FALSE"="#B4BCC5"),guide="none")+labs(x="RMSE (menor e melhor)",y=NULL,
 title="Validacao temporal: 47 origens, horizontes de 1 a 12 meses",subtitle="Azul indica RMSE abaixo do AR; o diagnostico de cada modelo deve ser consultado")+
 theme_minimal(base_size=12)+theme(panel.grid.major.y=element_blank())
ggsave(file.path(run_dir,"figures/rmse_geral.png"),p1,width=12,height=6,dpi=160)
p2 <- by_horizon |> left_join(filter(by_horizon,model=="ar") |> select(variable,h,AR_RMSE=RMSE),by=c("variable","h")) |>
 mutate(relative=RMSE/AR_RMSE,model=labels[model],variable=recode(variable,cpi_core="CPI core",atividade="Atividade")) |>
 filter(!model%in%c("AR","Naive","Drift","VAR domestico")) |>
 ggplot(aes(h,relative,color=model))+geom_hline(yintercept=1,color="grey50",linetype=2)+geom_line(linewidth=.8)+
 facet_wrap(~variable,scales="free_y")+scale_x_continuous(breaks=c(1,3,6,9,12))+
 labs(x="Horizonte (meses)",y="RMSE / RMSE do AR",color=NULL,title="Desempenho por horizonte",subtitle="Abaixo de 1: erro menor que o AR")+
 theme_minimal(base_size=12)+theme(legend.position="bottom")+guides(color=guide_legend(nrow=2))
ggsave(file.path(run_dir,"figures/rmse_horizontes.png"),p2,width=12,height=6,dpi=160)
print(select(overall,variable,model,cases,RMSE,MAE,CRPS,RMSE_skill));print(completion);print(quality)