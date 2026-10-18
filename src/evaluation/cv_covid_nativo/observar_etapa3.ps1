$ErrorActionPreference = 'Stop'
$cvRoot = 'C:\Users\dudup\OneDrive\japan-econometrics-model'
$cvOut = Join-Path $cvRoot 'data\processed\cv_covid_nativo_20261018'
function State($value) {$value.updated=Get-Date -Format o;$value|ConvertTo-Json -Depth 5|Set-Content -LiteralPath (Join-Path $cvOut 'status\pipeline.json')}
$mutex = New-Object System.Threading.Mutex($false,'Local\JapanCvCondObserver20261018')
if(-not $mutex.WaitOne(0)){throw 'A etapa3 ja esta sendo acompanhada.'}
try {
  $active = Get-Process -Id 47064 -ErrorAction SilentlyContinue
  if($active -and ($active.ProcessName -ne 'Rscript' -or $active.StartTime.ToString('yyyy-MM-dd HH:mm') -ne '2026-10-18 11:46')){throw 'PID com identidade inesperada; nao interferir.'}
  State @{state='etapa3_aguardando_6lags';etapa=3;pid=$PID;workers=@(47064);bsvarsigns='aguarda resposta do usuario'}
  if($active){$active.WaitForExit()}
  foreach($lag in @(3,6)){foreach ($i in 1..47){$f=Join-Path $cvOut ('windows\condicionado_p{0}_w{1:D3}.rds' -f $lag,$i);if(-not(Test-Path -LiteralPath $f)){throw "Checkpoint faltante: $f"}}}
  $r='C:\Program Files\R\R-4.6.1\bin\x64\Rscript.exe'
  $table=Join-Path $cvRoot 'src\evaluation\cv_covid_nativo\tabelas.R'
  $p=Start-Process $r -ArgumentList @('--vanilla',('"'+$table+'"'),('"'+$cvOut+'"'),3) -WorkingDirectory $cvRoot -WindowStyle Hidden -PassThru -Wait -RedirectStandardOutput (Join-Path $cvOut 'logs\etapa3_tabelas.out.log') -RedirectStandardError (Join-Path $cvOut 'logs\etapa3_tabelas.err.log')
  if($p.ExitCode -ne 0){throw 'Falha na tabela; checkpoints preservados.'}
  State @{state='etapa_condicionada_calculada_revisar';etapa=3;pid=$PID;bsvarsigns='aguarda resposta do usuario'}
} catch {State @{state='falha';etapa=3;pid=$PID;error=$_.Exception.Message};throw} finally {$mutex.ReleaseMutex();$mutex.Dispose()}