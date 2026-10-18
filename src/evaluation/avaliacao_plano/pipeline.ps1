$taskRoot='C:\Users\dudup\OneDrive\japan-econometrics-model'
$taskRepair=Join-Path $taskRoot 'data\processed\cv_foreign_estavel_20261018'
$taskFinal=Join-Path $taskRoot 'data\processed\cv_plano_20261018'
$taskR='C:\Program Files\R\R-4.6.1\bin\x64\Rscript.exe'
Set-Location -LiteralPath $taskRoot
while(Get-Process -Id 55100 -ErrorAction SilentlyContinue){Start-Sleep -Seconds 10}
& $taskR --vanilla 'src/evaluation/investigacao_foreign/validate_cv.R' $taskRepair *> (Join-Path $taskRepair 'logs\validation.log')
if($LASTEXITCODE -ne 0){@{state='stage2_validation_failed';updated=(Get-Date -Format o)}|ConvertTo-Json|Set-Content -LiteralPath (Join-Path $taskRoot 'data\processed\forecasting_pipeline_state.json');exit 1}
foreach($taskDir in @($taskFinal,(Join-Path $taskFinal 'logs'))){[System.IO.Directory]::CreateDirectory($taskDir)|Out-Null}
& $taskR --vanilla 'src/evaluation/avaliacao_plano/prepare.R' $taskFinal *> (Join-Path $taskFinal 'logs\prepare.log')
if($LASTEXITCODE -ne 0){@{state='stage3_prepare_failed';updated=(Get-Date -Format o)}|ConvertTo-Json|Set-Content -LiteralPath (Join-Path $taskRoot 'data\processed\forecasting_pipeline_state.json');exit 1}
$taskJobs=@(@{family='native';lag=3},@{family='native';lag=6},@{family='foreign';lag=3})
$taskIds=@()
foreach($taskJob in $taskJobs){
 $taskName="$($taskJob.family)_$($taskJob.lag)";$taskScript=Join-Path $taskRoot "src\evaluation\avaliacao_plano\worker_$($taskJob.family).R"
 $taskLaunched=Start-Process $taskR -ArgumentList @('--vanilla',('"'+$taskScript+'"'),('"'+$taskFinal+'"'),$taskJob.lag) -WorkingDirectory $taskRoot -WindowStyle Hidden -PassThru -RedirectStandardOutput (Join-Path $taskFinal "logs\$taskName.out.log") -RedirectStandardError (Join-Path $taskFinal "logs\$taskName.err.log")
 $taskIds+=$taskLaunched.Id
}
@{state='stage3_running';pids=$taskIds;updated=(Get-Date -Format o);repair=$taskRepair;final=$taskFinal}|ConvertTo-Json|Set-Content -LiteralPath (Join-Path $taskRoot 'data\processed\forecasting_pipeline_state.json')
while(@($taskIds|Where-Object {Get-Process -Id $_ -ErrorAction SilentlyContinue}).Count -gt 0){Start-Sleep -Seconds 15}
& $taskR --vanilla 'src/evaluation/avaliacao_plano/aggregate.R' $taskFinal *> (Join-Path $taskFinal 'logs\aggregate.log')
$taskAggExit=$LASTEXITCODE
if($taskAggExit -eq 0){& $taskR --vanilla 'src/evaluation/avaliacao_plano/report.R' $taskFinal *> (Join-Path $taskFinal 'logs\report.log');$taskReportExit=$LASTEXITCODE}else{$taskReportExit=-1}
@{state='stage3_computed_review_pending';aggregate_exit=$taskAggExit;report_exit=$taskReportExit;updated=(Get-Date -Format o);final=$taskFinal}|ConvertTo-Json|Set-Content -LiteralPath (Join-Path $taskRoot 'data\processed\forecasting_pipeline_state.json')