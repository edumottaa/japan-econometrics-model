$taskRoot='C:\Users\dudup\OneDrive\japan-econometrics-model'
$taskRun=Join-Path $taskRoot 'data\processed\cv_comparacao_20261017_193604'
$taskActiveIds=[System.Collections.Generic.List[int]]::new()
@(31100,53764,32676,12360) | ForEach-Object {$taskActiveIds.Add($_)}
$taskQueue=[System.Collections.Generic.Queue[object]]::new()
@(@{lag=6;shard=3},@{lag=6;shard=4},@{lag=3;shard=3},@{lag=3;shard=4}) | ForEach-Object {$taskQueue.Enqueue($_)}
$env:OMP_NUM_THREADS='1';$env:OPENBLAS_NUM_THREADS='1';$env:MKL_NUM_THREADS='1'
while($taskQueue.Count -gt 0 -or $taskActiveIds.Count -gt 0){
 foreach($taskActiveId in @($taskActiveIds.ToArray())){if(-not(Get-Process -Id $taskActiveId -ErrorAction SilentlyContinue)){[void]$taskActiveIds.Remove($taskActiveId)}}
 $taskFreeMB=(Get-CimInstance Win32_OperatingSystem).FreePhysicalMemory/1024
 if($taskQueue.Count -gt 0 -and $taskActiveIds.Count -lt 8 -and $taskFreeMB -gt 1600){
  $taskNext=$taskQueue.Dequeue();$taskName="signs_$($taskNext.lag)_$($taskNext.shard)"
  $taskArgs=@('--vanilla',('"'+(Join-Path $taskRoot 'src\evaluation\comparacao_cv_3_6\worker.R')+'"'),('"'+$taskRun+'"'),'signs',$taskNext.lag,$taskNext.shard,4)
  $taskLaunched=Start-Process -FilePath 'C:\Program Files\R\R-4.6.1\bin\x64\Rscript.exe' -ArgumentList $taskArgs -WorkingDirectory $taskRoot -WindowStyle Hidden -PassThru -RedirectStandardOutput (Join-Path $taskRun "logs\$taskName.out.log") -RedirectStandardError (Join-Path $taskRun "logs\$taskName.err.log")
  $taskActiveIds.Add($taskLaunched.Id)
  "$(Get-Date -Format o) launched $taskName PID $($taskLaunched.Id); free MB $([int]$taskFreeMB)" | Add-Content -LiteralPath (Join-Path $taskRun 'logs\scheduler.log')
 }
 Start-Sleep -Seconds 15
}
while(@(Get-CimInstance Win32_Process -Filter "Name = 'Rscript.exe'" | Where-Object {$_.CommandLine -like '*comparacao_cv_3_6*worker.R*' -and $_.CommandLine -like '*native*'}).Count -gt 0){Start-Sleep -Seconds 15}
Set-Location -LiteralPath $taskRoot
& 'C:\Program Files\R\R-4.6.1\bin\x64\Rscript.exe' --vanilla 'src/evaluation/comparacao_cv_3_6/aggregate.R' $taskRun *> (Join-Path $taskRun 'logs\aggregate.log')
$taskAggregateExit=$LASTEXITCODE
if($taskAggregateExit -eq 0){
 & 'C:\Program Files\R\R-4.6.1\bin\x64\Rscript.exe' --vanilla 'src/evaluation/comparacao_cv_3_6/report.R' $taskRun *> (Join-Path $taskRun 'logs\report.log')
 $taskReportExit=$LASTEXITCODE
}else{$taskReportExit=-1}
@{finished=(Get-Date -Format o);aggregate_exit=$taskAggregateExit;report_exit=$taskReportExit} | ConvertTo-Json | Set-Content -LiteralPath (Join-Path $taskRun 'status\batch_finished.json') -Encoding UTF8