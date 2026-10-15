from pathlib import Path
from datetime import datetime
import difflib

path = Path(r'C:\Users\dudup\OneDrive\NT Macro\Modelo var reduzido.R')
raw = path.read_bytes()
bom = raw.startswith(b'\xef\xbb\xbf')
newline = '\r\n' if b'\r\n' in raw else '\n'
original = raw.decode('utf-8-sig').replace('\r\n', '\n')
text = original

start = text.index('aacc_var <- cv_var |>')
end = text.index('# CV teste for BVAR:', start)
text = text[:start] + '''# Guardar as metricas dos modelos univariados
acc_fable <- cv_fable |>
  fabletools::forecast(h = 12) |>
  accuracy(data_bal, measures = list(RMSE = RMSE, MAE = MAE, CRPS = CRPS))

# Prever o VAR e selecionar a distribuicao do CPI para calcular o CRPS
previsoes_var <- cv_var |>
  fabletools::forecast(h = 12) |>
  filter(date <= max(data_bal$date))

param_var_cv <- distributional::parameters(previsoes_var$.distribution)

# CPI e a segunda variavel em VAR(vars(atividade, cpi_core, juro_curto))
previsoes_var_cpi <- tibble(
  .id = previsoes_var$.id,
  .model = previsoes_var$.model,
  date = previsoes_var$date,
  cpi_core = distributional::dist_normal(
    mu = map_dbl(param_var_cv$mu, 2),
    sigma = sqrt(map_dbl(param_var_cv$sigma, ~ .x[2, 2]))
  )
) |>
  as_tsibble(key = c(.id, .model), index = date) |>
  as_fable(response = "cpi_core", distribution = "cpi_core")

acc_var <- previsoes_var_cpi |>
  accuracy(data_bal, measures = list(RMSE = RMSE, MAE = MAE, CRPS = CRPS))


''' + text[end:]

replacements = [
    ('  acc_var |> filter(.response == "cpi_core"),', '  acc_var,'),
    ('\nselect(.model, RMSE, MAE, CRPS) |>', '\ncv_results |>\n  select(.model, RMSE, MAE, CRPS) |>'),
    ('\ncc_fable <- prev_fable |>', '\nacc_fable <- prev_fable |>'),
]
for old, new in replacements:
    if text.count(old) != 1:
        raise RuntimeError('Trecho alterado ou ambiguo: ' + old)
    text = text.replace(old, new, 1)

old = '''prev_var <- fit_var |>
  fabletools::forecast(h = h)'''
new = old + '''

# Selecionar a distribuicao do CPI para calcular o CRPS do VAR
param_var <- distributional::parameters(prev_var$.distribution)

# CPI e a segunda variavel na formula do VAR
prev_var_cpi <- tibble(
  .model = prev_var$.model,
  date = prev_var$date,
  cpi_core = distributional::dist_normal(
    mu = map_dbl(param_var$mu, 2),
    sigma = sqrt(map_dbl(param_var$sigma, ~ .x[2, 2]))
  )
) |>
  as_tsibble(key = .model, index = date) |>
  as_fable(response = "cpi_core", distribution = "cpi_core")'''
if text.count(old) != 1:
    raise RuntimeError('Trecho do holdout alterado ou ambiguo')
text = text.replace(old, new, 1)

work = Path(r'C:\Users\dudup\OneDrive\NT Macro\work\psi_diagnostic')
backup = work / ('modelo_var_reduzido_antes_crps_' + datetime.now().strftime('%Y%m%d_%H%M%S') + '.R')
backup.write_bytes(raw)
if path.read_bytes() != raw:
    raise RuntimeError('Arquivo modificado durante a edicao; nada foi sobrescrito')
updated = text.replace('\n', newline).encode('utf-8')
if bom:
    updated = b'\xef\xbb\xbf' + updated
path.write_bytes(updated)
print('Arquivo alterado: ' + str(path))
print('Backup: ' + str(backup))
print(''.join(difflib.unified_diff(original.splitlines(True), text.splitlines(True), fromfile='antes', tofile='depois')))
