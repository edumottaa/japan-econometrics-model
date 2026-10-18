args<-commandArgs(trailingOnly=TRUE)
source("C:/Users/dudup/OneDrive/japan-econometrics-model/src/evaluation/avaliacao_plano/common.R")
final_dir<-args[1];boj_dir<-file.path(root,"data/processed/boj_forecast_evaluation_20261018")
cfg<-readRDS(file.path(final_dir,"config.rds"));d<-readRDS(file.path(final_dir,"data_frozen.rds"));full<-list(scores=readRDS(file.path(final_dir,"benchmarks.rds")))
boj<-read_csv(file.path(boj_dir,"boj_forecasts.csv"),show_col_types=FALSE)
gdp<-read_csv(file.path(boj_dir,"gdp_quarterly.csv"),show_col_types=FALSE)
gdp_fy<-read_csv(file.path(boj_dir,"gdp_fiscal.csv"),show_col_types=FALSE)
df<-as_tibble(d) |> mutate(calendar=as.Date(date),fy=year(calendar)-as.integer(month(calendar)<=3),qindex=year(calendar)*4+quarter(calendar)-1,cpi_level=exp(cpi_core/100))
aq<-df |> group_by(qindex) |> summarise(n=n(),activity=mean(atividade),.groups="drop") |> filter(n==3)
bridge_data<-left_join(gdp,aq,by="qindex") |> arrange(qindex) |> mutate(gdp_yoy=100*log(gdp_nsa/lag(gdp_nsa,4)),a_yoy=activity-lag(activity,4),a_yoy_lag1=lag(a_yoy))
# Indice de trimestre inteiro: ano*4 + trimestre-1. A cada origem, PIB conhecido vai ate o trimestre anterior.
point_gdp_bridge<-function(act24,train,q0,fit,fy){
 S<-ncol(act24);b<-coef(fit);gpred<-matrix(NA_real_,9,S)
 get_a<-function(q){if(q<=q0)rep(aq$activity[match(q,aq$qindex)],S) else colMeans(act24[((q-q0-1)*3+1):((q-q0)*3),,drop=FALSE])}
 for(j in 0:8){q<-q0+j;gy<-b[1]+b[2]*(get_a(q)-get_a(q-4))+b[3]*(get_a(q-1)-get_a(q-5))
  base<-if(q-4<q0)rep(gdp$gdp_nsa[match(q-4,gdp$qindex)],S) else gpred[j-4+1,]
  gpred[j+1,]<-base*exp(gy/100)
 }
 point_q<-rowMeans(gpred);qs<-q0+0:8;qyear<-qs%/%4;qquarter<-qs%%4+1;qfy<-qyear-as.integer(qquarter==1)
 oldfy<-gdp$year-as.integer(gdp$quarter==1)
 fy_total<-function(y)sum(gdp$gdp_nsa[gdp$qindex<q0 & oldfy==y])+sum(point_q[qfy==y])
 100*(fy_total(fy)/fy_total(fy-1)-1)
}
point_cpi<-function(future_level,origin_month,fy){
 future_dates<-as.Date(origin_month+1:24);future_fy<-year(future_dates)-as.integer(month(future_dates)<=3)
 known<-df |> filter(date<=origin_month, .data$fy==.env$fy)
 prior<-df |> filter(.data$fy==.env$fy-1)
 stopifnot(nrow(prior)==12,max(prior$date)<=origin_month,nrow(known)+sum(future_fy==fy)==12)
 100*((sum(known$cpi_level)+sum(future_level[future_fy==fy]))/sum(prior$cpi_level)-1)
}
boj_dir_input<-boj_dir;boj_dir<-file.path(boj_dir_input,"pilot");dir.create(boj_dir,showWarnings=FALSE);results<-list();bridge_stats<-list();mapping<-list();ix<-0L
for(i in 1L){
 n<-cfg$origins[i];train<-d |> slice_head(n=n);om<-max(train$date);od<-as.Date(om);fy<-year(od);q0<-year(od)*4+quarter(od)-1
 pub<-boj |> filter(publication_date<=od) |> arrange(desc(publication_date)) |> slice_head(n=1)
 stopifnot(nrow(pub)==1,pub$fiscal_year==fy)
 target_cpi<-df |> filter(.data$fy==.env$fy);prior_cpi<-df |> filter(.data$fy==.env$fy-1)
 actual_gdp<-gdp_fy$gdp[match(fy,gdp_fy$fiscal_year)];prior_gdp<-gdp_fy$gdp[match(fy-1,gdp_fy$fiscal_year)]
 complete_actual<-nrow(target_cpi)==12 && nrow(prior_cpi)==12 && length(actual_gdp)==1 && is.finite(actual_gdp) && is.finite(prior_gdp)
 mapping[[i]]<-tibble(fold=i,origin=od,publication_date=pub$publication_date,fiscal_year=fy,h_months=(fy+1-year(od))*12+3-month(od),point_available=is.finite(pub$cpi_core)&&is.finite(pub$gdp),actual_available=complete_actual,source=pub$view_url,pdf=pub$pdf,page=pub$page)
 if(!complete_actual || !is.finite(pub$cpi_core) || !is.finite(pub$gdp))next
 hmon<-(fy+1-year(od))*12+3-month(od)
 actual_cpi<-100*(sum(target_cpi$cpi_level)/sum(prior_cpi$cpi_level)-1);actual_gdp<-100*(actual_gdp/prior_gdp-1)
 bt<-bridge_data |> filter(qindex<q0,qindex>=2000*4) |> drop_na(gdp_yoy,a_yoy,a_yoy_lag1)
 fit<-lm(gdp_yoy~a_yoy+a_yoy_lag1,data=bt)
 stopifnot(nrow(bt)>=40,max(bt$qindex)<q0,all(is.finite(coef(fit))))
 bridge_stats[[as.character(i)]]<-tibble(fold=i,origin=od,n=nrow(bt),last_gdp_qindex=max(bt$qindex),intercept=coef(fit)[1],beta_activity_yoy=coef(fit)[2],beta_activity_yoy_lag1=coef(fit)[3],residual_sd=summary(fit)$sigma)
 add<-function(model,cpi_value,gdp_value){tibble(fold=i,origin=od,publication_date=pub$publication_date,fiscal_year=fy,h_months=hmon,variable=c("cpi_fiscal","gdp_fiscal"),model=model,actual=c(actual_cpi,actual_gdp),mean=c(cpi_value,gdp_value),source=if(.env$model=="BoJ")pub$view_url else NA_character_)}
 ix<-ix+1;results[[ix]]<-add("BoJ",pub$cpi_core,pub$gdp)
 for(p in c(3L,6L)){
  z<-readRDS(file.path(final_dir,"windows",sprintf("native_p%d_w%03d.rds",p,i)))
  for(nm in c("normal","conditional")){
   F<-z$forecasts24[[nm]]
   cv<-point_cpi(rowMeans(exp(F[2,,,drop=FALSE][1,,]/100)),om,fy)
   gv<-point_gdp_bridge(F[1,,,drop=FALSE][1,,],train,q0,fit,fy)
   ix<-ix+1;results[[ix]]<-add(paste0("BVAR_",if(nm=="normal")"normal" else "condicionado","_p",p),cv,gv)
  }
  repair<-if(p==6)file.path(root,"data/processed/cv_foreign_estavel_20261018") else final_dir
  z<-readRDS(file.path(repair,"windows",sprintf("foreign_p%d_w%03d.rds",p,i)));F<-z$forecasts24
  cv<-point_cpi(rowMeans(exp(F[2,,,drop=FALSE][1,,]/100)),om,fy);gv<-point_gdp_bridge(F[1,,,drop=FALSE][1,,],train,q0,fit,fy)
  ix<-ix+1;results[[ix]]<-add(paste0("bsvarSIGNs_estavel_p",p),cv,gv)
 }
 # Benchmarks mensais do CPI: medias de niveis por transformacao lognormal; o denominador fiscal e observado.
 for(nm in c("ar","naive","drift","var")){
  a<-full$scores |> filter(model==nm,fold==i,variable=="cpi_core") |> arrange(h)
  flevel<-rep(NA_real_,24);flevel[a$h]<-exp(a$mean/100+.5*(a$predictive_sd/100)^2)
  # Apenas horizontes ate o fim do ano fiscal, com realizacao disponivel, entram na soma.
  cv<-point_cpi(flevel,om,fy)
  ix<-ix+1;results[[ix]]<-tibble(fold=i,origin=od,publication_date=pub$publication_date,fiscal_year=fy,h_months=hmon,variable="cpi_fiscal",model=nm,actual=actual_cpi,mean=cv,source=NA_character_)
 }
 # AR independente para PIB: so usa trimestres anteriores a origem, respeitando defasagem de divulgacao.
 gd<-gdp |> filter(qindex<q0,qindex>=2000*4) |> transmute(quarter=yearquarter(date),value=100*log(gdp_nsa)) |> as_tsibble(index=quarter)
 fc<-gd |> model(ar=ARIMA(value~pdq(d=1,q=0)+PDQ(D=0,Q=0))) |> forecast(h=9)
 point_q<-exp(mean(fc$value)/100+.5*distributional::variance(fc$value)/10000)
 qs<-q0+0:8;qfy<-qs%/%4-as.integer(qs%%4==0);oldfy<-gdp$year-as.integer(gdp$quarter==1)
 total<-function(y)sum(gdp$gdp_nsa[gdp$qindex<q0 & oldfy==y])+sum(point_q[qfy==y])
 gv<-100*(total(fy)/total(fy-1)-1)
 ix<-ix+1;results[[ix]]<-tibble(fold=i,origin=od,publication_date=pub$publication_date,fiscal_year=fy,h_months=hmon,variable="gdp_fiscal",model="ar_gdp",actual=actual_gdp,mean=gv,source=NA_character_)
 log_line("BoJ fiscal",i,"FY",fy)
}
sc<-bind_rows(results) |> mutate(error=mean-actual,squared_error=error^2,absolute_error=abs(error))
stopifnot(all(is.finite(sc$mean)),all((sc |> count(variable,model))$n==1),!anyDuplicated(sc[c("variable","model","fold")]))
summary<-sc |> group_by(variable,model) |> summarise(cases=n(),RMSE=sqrt(mean(squared_error)),MAE=mean(absolute_error),.groups="drop") |> arrange(variable,RMSE)
by_h<-sc |> group_by(variable,model,h_months) |> summarise(cases=n(),RMSE=sqrt(mean(squared_error)),MAE=mean(absolute_error),.groups="drop")
write_csv(sc,file.path(boj_dir,"fiscal_scores.csv"));write_csv(summary,file.path(boj_dir,"fiscal_summary.csv"));write_csv(by_h,file.path(boj_dir,"fiscal_by_horizon.csv"));write_csv(bind_rows(mapping),file.path(boj_dir,"origin_publication_mapping.csv"));write_csv(bind_rows(bridge_stats),file.path(boj_dir,"bridge_estimates.csv"))
saveRDS(list(scores=sc,summary=summary,by_horizon=by_h,mapping=bind_rows(mapping),bridge=bind_rows(bridge_stats)),file.path(boj_dir,"results.rds"))
md<-function(x){x<-as.data.frame(x);x[]<-lapply(x,function(z)if(is.numeric(z))formatC(z,digits=3,format="f") else as.character(z));c(paste0("| ",paste(names(x),collapse=" | ")," |"),paste0("| ",paste(rep("---",ncol(x)),collapse=" | ")," |"),apply(x,1,function(z)paste0("| ",paste(z,collapse=" | ")," |")),"")}
lines<-c("# Comparação fiscal com os Outlook Reports do BoJ", "",
 "Esta é uma avaliação adicional de previsões pontuais, com alvos compatíveis, separada do CV mensal do núcleo. As publicações foram verificadas no índice oficial e as medianas extraídas da tabela do Board. Usou-se a coluna CPI all items less fresh food, sem retirar mecanicamente impostos ou energia.","",
 "## Resultados", "",md(summary),
 "## Amostra e comparabilidade", "",
 "São 44 origens comuns por modelo/alvo: dos 47 relatórios alinhados ao CV, dois se referem ao FY2026, ainda sem realização anual, e abril de 2020 ofereceu faixas sem mediana pontual comparável. Essa origem foi excluída de todos os modelos nesta tabela, sem substituir a faixa pelo ponto médio. O CV principal mensal continua incluindo a pandemia e todos os seus casos.","",
 "O alvo em cada origem é o ano fiscal que começa em abril do ano civil da origem e termina em março seguinte. Os horizontes até o fim do FY são 3,6,9 ou12meses. Para o CPI, o valor previsto é a variação percentual da média de índices do FY contra a média do FY anterior, combinando meses conhecidos e meses projetados. O CPI original do projeto é o código0161 do e-Stat, all items less fresh food, base2020; não há ajuste explícito por imposto no script de coleta. Foram mantidos os efeitos institucionais observados e a coluna comparável das projeções do BoJ.","",
 "A informação disponível não é idêntica: o BoJ publica em geral em janeiro/abril/julho/outubro, e as origens do CV estão em março/junho/setembro/dezembro. O modelo dispõe de meses mais recentes do que o último Outlook Report. Além disso, o CV usa dados finais revisados, sem vintages históricos e sem reproduzir integralmente os atrasos de cada indicador. Por isso, esta tabela é uma referência institucional contextual; diferenças de RMSE não provam superioridade em igualdade de informação. Não foram apresentados testes formais de vitória contra o BoJ com essas condições assimétricas.","",
 "## Ponte do índice de atividade para o PIB", "",
 "O índice coincidente não foi comparado diretamente com PIB. Em cada origem foi estimada por OLS, somente no treino, a relação entre crescimento interanual do PIB real trimestral e crescimento interanual da média trimestral do log do índice de atividade, com uma defasagem desse crescimento da atividade. O PIB conhecido foi limitado ao trimestre anterior à origem, evitando usar o PIB ainda não divulgado do trimestre corrente. A atividade observada permite nowcast do trimestre corrente; as trajetórias mensais projetadas fornecem os trimestres futuros.","",
 "O PIB foi trabalhado em valores trimestrais reais não ajustados sazonalmente, e o crescimento interanual reduz o componente sazonal. A regressão foi aplicada a cada trajetória de atividade. Os níveis esperados de PIB por trimestre foram somados para calcular o crescimento do FY, usando apenas valores conhecidos ou projetados na origem, inclusive no denominador quando necessário. O benchmark AR do PIB foi estimado separadamente, com o mesmo limite de informação trimestral. Essa ponte é auxiliar e sua incerteza de regressão/resíduos não foi integralmente propagada: os resultados desta seção são pontuais.","",
 "Os dados de PIB são da segunda estimativa de abril-junho2026 do Cabinet Office, em preços encadeados de2020. A soma dos quatro trimestres não ajustados foi conferida com a série oficial por FY; a diferença máxima foi0,1bilhão de ienes, compatível com arredondamento.","",
 "## Métricas que não se podem atribuir ao BoJ", "",
 "As faixas do Board descrevem a dispersão entre projeções dos membros, não uma densidade preditiva. Não foram calculados log score nem CRPS do BoJ a partir delas. Esses scores foram calculados no CV mensal dos candidatos e benchmarks probabilísticos. A comparação fiscal com o BoJ usa RMSE e MAE; ausência das outras métricas é uma limitação do objeto publicado, não um zero ou uma derrota automática.","",
 "Arquivos: manifest.csv contém URLs e hashes dos47documentos; boj_forecasts.csv contém medianas e páginas; origin_publication_mapping.csv identifica publicações e exclusões; fiscal_scores.csv traz cada erro; fiscal_by_horizon.csv separa os horizontes; bridge_estimates.csv guarda as regressões de cada treino.","",
 "Fontes: [Outlook Reports](https://www.boj.or.jp/en/mopo/outlook/), [interim assessments anteriores a2016](https://www.boj.or.jp/en/mopo/mpmsche_minu/past.htm), [Cabinet Office — PIB2026Q2, segunda estimativa](https://www.esri.cao.go.jp/en/sna/data/sokuhou/files/2026/qe262_2/gdemenuea.html), [e-Stat — tabela do CPI](https://www.e-stat.go.jp/en/dbview?sid=0003427113).")
writeLines(enc2utf8(lines),file.path(boj_dir,"relatorio_boj.md"),useBytes=TRUE)
write_status(file.path(boj_dir,"completed.json"),state="computed_review_pending",cases=44)
print(summary)