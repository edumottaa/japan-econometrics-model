args<-commandArgs(trailingOnly=TRUE)
source("C:/Users/dudup/OneDrive/japan-econometrics-model/src/evaluation/avaliacao_plano/common.R")
out<-args[1];old_dir<-file.path(root,"data/processed/cv_comparacao_20261017_193604")
files<-list.files(file.path(out,"windows"),pattern="foreign_p6_w[0-9]+[.]rds$",full.names=TRUE)
stopifnot(length(files)==47)
w<-lapply(files,function(f){z<-readRDS(f);list(diagnostic=z$diagnostic,scores=z$scores12,error=z$error,seconds=z$elapsed_seconds)})
stopifnot(all(vapply(w,function(z)is.null(z$error),logical(1))))
sc<-bind_rows(lapply(w,`[[`,"scores")) |> filter(variable%in%c("cpi_core","atividade"))
stopifnot(all((sc |> count(variable))$n==538))
di<-bind_rows(lapply(w,`[[`,"diagnostic"));stopifnot(!any(di$quality%in%c("execution_failure","diagnostic_failure")))
old<-readRDS(file.path(old_dir,"results.rds"));new<-sc |> group_by(variable,model) |> summarise(RMSE=sqrt(mean(squared_error)),MAE=mean(absolute_error),CRPS=mean(crps),.groups="drop")
comparison<-bind_rows(old$overall |> select(variable,model,RMSE,MAE,CRPS),new)
write_csv(comparison,file.path(out,"comparison_h12.csv"));write_csv(di,file.path(out,"diagnostics.csv"))
md<-function(x){x<-as.data.frame(x);x[]<-lapply(x,function(z)if(is.numeric(z))formatC(z,digits=3,format="f") else as.character(z));c(paste0("| ",paste(names(x),collapse=" | ")," |"),paste0("| ",paste(rep("---",ncol(x)),collapse=" | ")," |"),apply(x,1,function(z)paste0("| ",paste(z,collapse=" | ")," |")),"")}
orig<-old$diagnostics |> filter(family=="bsvarSIGNs",p==6)
summary<-bind_rows(orig |> summarise(version="Original: 2500 candidatos",min_ESS=min(ess_importance),median_ESS=median(ess_importance),max_weight=max(max_weight_share),failed=sum(quality=="diagnostic_failure")),
 di |> summarise(version="Estavel: 25000 candidatos, 2500 retidos",min_ESS=min(ess_importance),median_ESS=median(ess_importance),max_weight=max(max_weight_share),failed=sum(quality=="diagnostic_failure")))
write_csv(summary,file.path(out,"importance_summary.csv"))
config<-list(source_run=old_dir,p=6,seed_base=123,origins=47,proposals=25000,retained=2500,lambda=.2,mu=1,delta=1,psi="automatico no treino e fixo",hyper_estimated=FALSE,structural_sign_normalization=FALSE,forecast_horizons=c(12,24))
jsonlite::write_json(config,file.path(out,"config.json"),pretty=TRUE,auto_unbox=TRUE)
p<-bind_rows(orig |> mutate(version="Original"),di |> mutate(version="Estavel")) |>
 ggplot(aes(fold,ess_importance,color=version))+geom_line()+geom_point(size=1)+geom_hline(yintercept=50,linetype=2)+scale_y_log10()+
 labs(x="Origem do CV",y="ESS de importancia (escala log)",color=NULL,title="Mais candidatos antes da reponderacao",subtitle="Mesmos dados, seis lags, foreign e hiperparametros fixos")+theme_minimal(base_size=12)
ggsave(file.path(out,"ess_comparison.png"),p,width=10,height=5,dpi=160)
lines<-c("# Investiga\u00e7\u00e3o da concentra\u00e7\u00e3o no bsvarSIGNs", "",
 "As 47 origens da vers\u00e3o ajustada foram conclu\u00eddas. Esta \u00e9 uma corre\u00e7\u00e3o computacional da estima\u00e7\u00e3o reduzida: o sistema, os priors e as restri\u00e7\u00f5es foreign foram preservados. A avalia\u00e7\u00e3o ampliada das seis vari\u00e1veis e at\u00e9 24 meses est\u00e1 em uma etapa separada.","",
 "## O que causou o alerta", "",
 "O foreign imp\u00f5e que os seis indicadores dom\u00e9sticos n\u00e3o entrem nas quatro equa\u00e7\u00f5es externas. Com seis lags, s\u00e3o 144 coeficientes fixados em zero. O pacote gera propostas e usa pesos de import\u00e2ncia para transformar essas propostas na distribui\u00e7\u00e3o restrita. Algumas propostas recebem peso muito maior; numa amostra finita, uma delas pode dominar.","",
 "Na janela original de junho de 2021, o maior peso foi 22,2% e o ESS foi 18,2. Os hiperpar\u00e2metros n\u00e3o estavam sendo estimados. N\u00e3o houve valores inv\u00e1lidos ou viola\u00e7\u00e3o dos zeros. Portanto, o problema observado foi concentra\u00e7\u00e3o de pesos em uma amostra de import\u00e2ncia, e n\u00e3o uma cadeia de hiperpar\u00e2metros travada.","",
 "A f\u00f3rmula dos pesos foi reconstru\u00edda diretamente da densidade dos coeficientes restritos a zero e conferida com os pesos retornados pelo pacote. Nas duas janelas piloto, o erro m\u00e1ximo entre log pesos foi inferior a 2e-11. Isso apoia o mecanismo descrito, sem precisar atribuir o epis\u00f3dio a um bug num\u00e9rico do foreign.","",
 "O n\u00famero de lags influencia a dimens\u00e3o das restri\u00e7\u00f5es: com tr\u00eas lags s\u00e3o 72 zeros; com seis, 144. Isso ajuda a explicar a dificuldade maior da amostragem com seis lags, mas n\u00e3o significa que seis meses sejam economicamente incorretos nem prova que os hiperpar\u00e2metros sejam irrelevantes para os pesos. Seus valores fixados afetam a posterior; n\u00e3o foi necess\u00e1rio alter\u00e1-los nesta solu\u00e7\u00e3o.","",
 "## A solu\u00e7\u00e3o simples", "",
 "1. Usar orienta\u00e7\u00e3o livre dos choques na etapa reduzida, passando sign_structural=matrix(NA_real_,10,10). N\u00e3o h\u00e1 restri\u00e7\u00f5es econ\u00f4micas de sinais nesta etapa.",
 "2. Gerar 25.000 propostas e manter 2.500 sorteios reponderados para as previs\u00f5es, a mesma quantidade final da bateria original.","",
 "A especifica\u00e7\u00e3o anterior herdava a normaliza\u00e7\u00e3o padr\u00e3o de diagonal positiva da matriz estrutural. O algoritmo tratava isso por rejei\u00e7\u00e3o. Como o sinal de cada choque \u00e9 arbitr\u00e1rio nesta etapa, essa normaliza\u00e7\u00e3o consumia muitas propostas sem acrescentar informa\u00e7\u00e3o \u00e0 previs\u00e3o reduzida. Retir\u00e1-la acelerou a simula\u00e7\u00e3o e tornou vi\u00e1vel ampliar o conjunto de propostas.","",
 "A orienta\u00e7\u00e3o livre n\u00e3o \u00e9, por si s\u00f3, uma cura matem\u00e1tica da concentra\u00e7\u00e3o: ela permite gerar mais propostas rapidamente. A melhora de estabilidade \u00e9 avaliada pelo ESS e pelo maior peso. As sequ\u00eancias aleat\u00f3rias mudam, mesmo com a mesma semente, porque a rotina consome n\u00fameros aleat\u00f3rios de forma diferente. N\u00e3o foram escolhidas sementes at\u00e9 aparecer uma melhora.","",
 "O modelo probabil\u00edstico reduzido permanece o mesmo: a normaliza\u00e7\u00e3o apenas escolhe sinais dos choques, que n\u00e3o alteram A ou Sigma. Na identifica\u00e7\u00e3o econ\u00f4mica futura, os sinais desejados devem ser explicitamente especificados; esta otimiza\u00e7\u00e3o de forecasting n\u00e3o substitui a identifica\u00e7\u00e3o estrutural.","",
 "## Diagn\u00f3sticos das 47 origens", "",md(summary),
 "![Compara\u00e7\u00e3o dos ESS](ess_comparison.png)","",
 "ESS de import\u00e2ncia mede a concentra\u00e7\u00e3o dos pesos. N\u00e3o \u00e9 o mesmo ESS de uma cadeia MCMC de hiperpar\u00e2metros, nem prova converg\u00eancia por si s\u00f3. O crit\u00e9rio de falha manteve o limite de 50; valores abaixo de 500 na vers\u00e3o ampliada s\u00e3o registrados como aviso adicional.","",
 "## Compara\u00e7\u00e3o preditiva em 12 meses", "",md(comparison |> arrange(variable,RMSE)),
 "As diferen\u00e7as entre a vers\u00e3o original e a est\u00e1vel refletem maior precis\u00e3o Monte Carlo e uma sequ\u00eancia aleat\u00f3ria diferente, mantendo os mesmos priors e restri\u00e7\u00f5es. N\u00e3o se deve anunciar que foi descoberto um novo modelo econ\u00f4mico ou que os hiperpar\u00e2metros foram otimizados para vencer o benchmark.","",
 "## C\u00f3digo e evid\u00eancias", "",
 "A fun\u00e7\u00e3o simples est\u00e1 em src/evaluation/investigacao_foreign/estimar_foreign_estavel.R. Os pilotos usaram as origens 26 e 27, primeiro com 2.500 propostas nas mesmas sementes para controle, depois com 25.000. Os arquivos de investiga\u00e7\u00e3o preservam pesos, covari\u00e2ncias e previs\u00f5es. O CV preserva pesos, diagn\u00f3sticos e amostras mensais das seis vari\u00e1veis dom\u00e9sticas para 12 e 24 meses.","",
 "Fontes prim\u00e1rias: [restri\u00e7\u00e3o e c\u00e1lculo do peso](https://raw.githubusercontent.com/bsvars/bsvarSIGNs/main/src/sample_SOE.cpp), [amostragem e repondera\u00e7\u00e3o](https://raw.githubusercontent.com/bsvars/bsvarSIGNs/main/src/bsvars_sign.cpp), [normaliza\u00e7\u00e3o e sele\u00e7\u00e3o de rota\u00e7\u00f5es](https://github.com/bsvars/bsvarSIGNs/blob/main/src/sample_Q.cpp). A f\u00f3rmula foi verificada tamb\u00e9m contra os resultados da instala\u00e7\u00e3o local bsvarSIGNs 3.0.")
writeLines(enc2utf8(lines),file.path(out,"relatorio_etapa2.md"),useBytes=TRUE)
write_status(file.path(out,"status/validated.json"),state="validated",completed=47,min_ess=min(di$ess_importance),max_weight=max(di$max_weight_share))
print(summary);print(new)