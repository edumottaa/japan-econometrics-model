$ErrorActionPreference = 'Stop'
$cvRoot = 'C:\Users\dudup\OneDrive\japan-econometrics-model'
$cvOut = Join-Path $cvRoot 'data\processed\cv_covid_nativo_20261018'
$cvScripts = Join-Path $cvRoot 'src\evaluation\cv_covid_nativo'
$cvR = 'C:\Program Files\R\R-4.6.1\bin\x64\Rscript.exe'
$env:OMP_NUM_THREADS = '1'; $env:OPENBLAS_NUM_THREADS = '1'
Set-Location -LiteralPath $cvRoot
function Save-CvState($value) {
  $value.updated = Get-Date -Format o
  $value | ConvertTo-Json -Depth 5 | Set-Content -LiteralPath (Join-Path $cvOut 'status\pipeline.json')
}
$cvMutex = New-Object System.Threading.Mutex($false, 'Local\JapanCvCovidNativo20261018')
if (-not $cvMutex.WaitOne(0)) { throw 'Este pipeline ja esta executando.' }
try {
  Save-CvState @{state='preparando';pid=$PID}
  if (-not (Test-Path -LiteralPath (Join-Path $cvOut 'benchmarks.rds'))) {
    $cvPreparation = Start-Process -FilePath $cvR -ArgumentList @('--vanilla', ('"'+(Join-Path $cvScripts 'preparar.R')+'"'), ('"'+$cvOut+'"')) -WorkingDirectory $cvRoot -WindowStyle Hidden -PassThru -Wait -RedirectStandardOutput (Join-Path $cvOut 'logs\preparar.out.log') -RedirectStandardError (Join-Path $cvOut 'logs\preparar.err.log')
    if ($cvPreparation.ExitCode -ne 0) { throw 'Falha na preparacao; conferir logs.' }
  }
  foreach ($cvStage in 1..5) {
    $cvLags = if ($cvStage -le 3) { @(3,6) } else { @(12) }
    $cvProcesses = @()
    foreach ($cvLag in $cvLags) {
      $cvName = "etapa$($cvStage)_p$($cvLag)"
      # Cada processo le uma copia imutavel, mesmo se o arquivo de trabalho for corrigido.
      $cvSnapshotDir = Join-Path $cvOut 'scripts_executados'
      [IO.Directory]::CreateDirectory($cvSnapshotDir) | Out-Null
      $cvWorkerSnapshot = Join-Path $cvSnapshotDir ($cvName + '_' + (Get-Date -Format 'yyyyMMdd_HHmmss_fff') + '.R')
      Copy-Item -LiteralPath (Join-Path $cvScripts 'cv.R') -Destination $cvWorkerSnapshot
      $cvProcess = Start-Process -FilePath $cvR -ArgumentList @('--vanilla', ('"'+$cvWorkerSnapshot+'"'), ('"'+$cvOut+'"'), $cvStage, $cvLag) -WorkingDirectory $cvRoot -WindowStyle Hidden -PassThru -RedirectStandardOutput (Join-Path $cvOut "logs\$cvName.out.log") -RedirectStandardError (Join-Path $cvOut "logs\$cvName.err.log")
      $cvProcesses += $cvProcess
    }
    Save-CvState @{state='etapa_rodando';etapa=$cvStage;pid=$PID;workers=@($cvProcesses | ForEach-Object {$_.Id})}
    foreach ($cvProcess in $cvProcesses) {
      $cvProcess.WaitForExit()
      if ($cvProcess.ExitCode -ne 0) { throw "Worker encerrado com erro na etapa $cvStage; conferir logs." }
    }
    $cvTable = Start-Process -FilePath $cvR -ArgumentList @('--vanilla', ('"'+(Join-Path $cvScripts 'tabelas.R')+'"'), ('"'+$cvOut+'"'), $cvStage) -WorkingDirectory $cvRoot -WindowStyle Hidden -PassThru -Wait -RedirectStandardOutput (Join-Path $cvOut "logs\etapa$($cvStage)_tabelas.out.log") -RedirectStandardError (Join-Path $cvOut "logs\etapa$($cvStage)_tabelas.err.log")
    if ($cvTable.ExitCode -ne 0) { throw "Falha na tabela da etapa $cvStage; checkpoints preservados." }
    Save-CvState @{state='etapa_calculada_revisar';etapa=$cvStage;pid=$PID}
  }
  Save-CvState @{state='todas_calculadas_revisar';etapa=5;pid=$PID}
} catch {
  Save-CvState @{state='falha';etapa=$cvStage;pid=$PID;error=$_.Exception.Message}
  throw
} finally {
  $cvMutex.ReleaseMutex(); $cvMutex.Dispose()
}