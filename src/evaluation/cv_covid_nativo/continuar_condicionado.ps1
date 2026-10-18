param([int]$Etapa = 3)
$ErrorActionPreference = 'Stop'
$cvRoot = 'C:\Users\dudup\OneDrive\japan-econometrics-model'
$cvOut = Join-Path $cvRoot 'data\processed\cv_covid_nativo_20261018'
$cvScripts = Join-Path $cvRoot 'src\evaluation\cv_covid_nativo'
$cvR = 'C:\Program Files\R\R-4.6.1\bin\x64\Rscript.exe'
$cvLags = if ($Etapa -eq 3) { @(3,6) } elseif ($Etapa -eq 5) { @(12) } else { throw 'Etapa nao condicionada.' }
$env:OMP_NUM_THREADS = '1'; $env:OPENBLAS_NUM_THREADS = '1'
Set-Location -LiteralPath $cvRoot
function Save-State($value) { $value.updated = Get-Date -Format o; $value | ConvertTo-Json -Depth 5 | Set-Content -LiteralPath (Join-Path $cvOut 'status\pipeline.json') }
$cvJobs = @()
try {
  foreach ($cvLag in $cvLags) {
    $cvName = "etapa$($Etapa)_p$($cvLag)"
    $cvSnapshotDir = Join-Path $cvOut 'scripts_executados'
    [IO.Directory]::CreateDirectory($cvSnapshotDir) | Out-Null
    $cvSnapshot = Join-Path $cvSnapshotDir ($cvName+'_'+(Get-Date -Format 'yyyyMMdd_HHmmss_fff')+'.R')
    Copy-Item -LiteralPath (Join-Path $cvScripts 'cv.R') -Destination $cvSnapshot
    $cvJobs += Start-Process -FilePath $cvR -ArgumentList @('--vanilla', ('"'+$cvSnapshot+'"'), ('"'+$cvOut+'"'), $Etapa, $cvLag) -WorkingDirectory $cvRoot -WindowStyle Hidden -PassThru -RedirectStandardOutput (Join-Path $cvOut "logs\$cvName.out.log") -RedirectStandardError (Join-Path $cvOut "logs\$cvName.err.log")
    $cvHandle = $cvJobs[-1].Handle # Reter o handle para ExitCode nao voltar NULL no PowerShell5.
  }
  Save-State @{state='condicionado_rodando';etapa=$Etapa;pid=$PID;workers=@($cvJobs | ForEach-Object {$_.Id});bsvarsigns='aguarda decisao: erro forecast3.0 comprovado'}
  foreach ($cvJob in $cvJobs) { $cvJob.WaitForExit(); if ($cvJob.ExitCode -ne 0) { throw "Erro no processo condicionado $($cvJob.Id)" } }
  $cvTable = Start-Process -FilePath $cvR -ArgumentList @('--vanilla', ('"'+(Join-Path $cvScripts 'tabelas.R')+'"'), ('"'+$cvOut+'"'), $Etapa) -WorkingDirectory $cvRoot -WindowStyle Hidden -PassThru -Wait -RedirectStandardOutput (Join-Path $cvOut "logs\etapa$($Etapa)_tabelas.out.log") -RedirectStandardError (Join-Path $cvOut "logs\etapa$($Etapa)_tabelas.err.log")
  if ($cvTable.ExitCode -ne 0) { throw 'Falha no calculo posterior da tabela; checkpoints preservados.' }
  Save-State @{state='etapa_condicionada_calculada_revisar';etapa=$Etapa;pid=$PID;bsvarsigns='aguarda decisao sobre forecast'}
} catch { Save-State @{state='falha';etapa=$Etapa;pid=$PID;error=$_.Exception.Message}; throw }
