from pathlib import Path
import hashlib
root=Path(r'C:\Users\dudup\OneDrive\NT Macro')
main=root/'Modelo var reduzido.R'
raw=main.read_bytes()
text=raw.decode('utf-8-sig').replace('\r\n','\n')
start=text.index('#Metodo mais simples: Holdout')
holdout=text[start:]
old='''prev_bvar <- bsvarSIGNs::forecast(post_holdout, horizon = h)

# Selecionar os sorteios de CPI
pos_cpi <- match("cpi_core", colnames(dados_treino))
sorteios_cpi <- prev_bvar$forecasts[pos_cpi, , ]'''
new='''# Proposta: corrigir somente a recursao da previsao.
# Preserva todos os sorteios dos coeficientes e das covariancias do BVAR.
n <- ncol(dados_treino)
p <- spec_holdout$p
S <- dim(post_holdout$posterior$A)[3]
previsoes <- array(NA_real_, c(n, h, S))

set.seed(123)
for (s in seq_len(S)) {
  A <- post_holdout$posterior$A[, , s]
  L <- t(chol(post_holdout$posterior$Sigma[, , s]))
  historico <- tail(dados_treino, p)

  for (j in seq_len(h)) {
    # Regressor: os ultimos p meses, do mais recente ao mais antigo, e a constante.
    x <- c(t(historico[p:1, , drop = FALSE]), 1)
    y <- drop(A %*% x + L %*% rnorm(n))
    previsoes[, j, s] <- y

    # Retira o mes mais antigo e inclui o novo mes previsto.
    historico <- rbind(historico[-1, , drop = FALSE], y)
  }
}

# Selecionar os sorteios de CPI
pos_cpi <- match("cpi_core", colnames(dados_treino))
sorteios_cpi <- previsoes[pos_cpi, , ]'''
if holdout.count(old)!=1:
 raise RuntimeError('O bloco de forecast mudou; nenhuma alteracao aplicada.')
holdout=holdout.replace(old,new,1)
header='''# PROPOSTA PARA REVISAO - nao altera o script principal.
# Requer data_bal ja carregada. Este arquivo estima cada modelo uma unica vez.
# Correcao: estado inicial inclui o ultimo mes do treino; todas as defasagens avancam.
library(tidyverse)
library(fpp3)
library(bsvarSIGNs)

'''
out=Path(r'C:\Users\dudup\Documents\Codex\2026-10-14\ex\outputs')
out.mkdir(parents=True,exist_ok=True)
proposal=out/'Proposta_holdout_BVAR_corrigido.R'
proposal.write_text(header+holdout,encoding='utf-8')
(root/'work/psi_diagnostic/main_hash_before_final.txt').write_text(hashlib.sha256(raw).hexdigest(),encoding='ascii')
print('Copia para revisao: '+str(proposal))
print('Principal nao alterado. SHA256: '+hashlib.sha256(main.read_bytes()).hexdigest())
p=root/'work/psi_diagnostic/verify_forecast_bug.R'
s=p.read_text(encoding='utf-8-sig').replace('map_dbl(pr$mu,2)','purrr::map_dbl(pr$mu,2)').replace('map_dbl(pr$sigma,~.x[2,2])','purrr::map_dbl(pr$sigma,~.x[2,2])')
p.write_text(s,encoding='utf-8')
