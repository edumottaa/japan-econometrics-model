# JGB30Y | Teste 1: fatores diários e gráficos.
library(fpp3)
library(readr)
library(sandwich)
library(lmtest)

pasta <- "src/JGB30Y"
stopifnot(dir.exists(pasta))
saida <- file.path(pasta, "teste 1", "resultados")

diario <- read_csv(file.path(pasta, "resultados", "mercado_diario.csv"), show_col_types = FALSE) |>
  mutate(date = ymd(date), us_date = ymd(us_date)) |>
  filter(complete.cases(d_jgb30_bp, d_jgb2_bp, d_ust30_bp))

# Uma regressão explicativa 
# JGB2/JGB30: curva de vencimento constante do MoF, nao titulos por ISIN.
# Investigar depois cotacoes de papeis especificos e trocas do benchmark.
# Equacao: d_JGB30[t] = alfa + beta2*d_JGB2[t] + betaUS*d_UST30[t] + erro[t].
# As variacoes d_ sao diarias e estao em pontos-base.
ajuste_d <- lm(d_jgb30_bp ~ d_jgb2_bp + d_ust30_bp, data = diario)
vcov_hac <- NeweyWest(ajuste_d, lag = 5, prewhite = FALSE, adjust = TRUE)
infer_d <- coeftest(ajuste_d, vcov. = vcov_hac)
b_global <- coef(ajuste_d)[-1]
wald_global <- as.numeric(t(b_global) %*% solve(vcov_hac[-1, -1]) %*% b_global)
p_global_hac <- pchisq(wald_global, df = length(b_global), lower.tail = FALSE)
# Amostra ate 30/10/2026 (n = 4115): R2 = 0.1511; p global HAC = 2.31e-92.
# p global: teste conjunto H0 de beta2 = betaUS = 0 (Wald, qui-quadrado com 2 gl).
coef_d <- tibble(termo = rownames(infer_d), estimativa = infer_d[, 1],
                 erro_padrao_hac = infer_d[, 2], p_valor = infer_d[, 4]) |>
  mutate(ic95_inf = estimativa - 1.96 * erro_padrao_hac,
         ic95_sup = estimativa + 1.96 * erro_padrao_hac)
write_csv(coef_d, file.path(saida, "teste1_coeficientes_hac.csv"))

diagnosticos_d <- tibble(
  n = nrow(diario), r2 = summary(ajuste_d)$r.squared,
  r2_ajustado = summary(ajuste_d)$adj.r.squared,
  p_global_hac = p_global_hac,
  desvio_residuo_bp = sd(resid(ajuste_d)),
  ljung_box_p_lag5 = Box.test(resid(ajuste_d), lag = 5,
                              type = "Ljung-Box")$p.value,
  breusch_pagan_p = bptest(ajuste_d)$p.value,
  ultima_data_jp = as.character(max(diario$date)),
  max_idade_us_dias = max(diario$idade_us_dias)
)
write_csv(diagnosticos_d, file.path(saida, "teste1_diagnosticos.csv"))

# Parcelas contábeis das variações de 21 pregões, com coeficientes da amostra inteira.
# A parte não explicada inclui intercepto e resíduo
# Grafico: d21_JGB30 = beta2*d21_JGB2 + betaUS*d21_UST30 + nao_explicado_21d.
# nao_explicado_21d = 21*alfa + soma dos erros da regressao nos 21 pregoes.
b <- coef(ajuste_d)
contribuicoes <- diario |>
  mutate(`JGB2` = b["d_jgb2_bp"] * d_jgb2_bp,
         `UST30` = b["d_ust30_bp"] * d_ust30_bp,
         `Não explicado` = d_jgb30_bp - `JGB2` - `UST30`) |>
  mutate(across(c(`JGB2`, `UST30`, `Não explicado`, d_jgb30_bp),
                ~ cumsum(.) - lag(cumsum(.), 21))) |>
  filter(date >= ymd("2021-01-01")) |>
  select(date, `JGB2`, `UST30`, `Não explicado`, d_jgb30_bp)
stopifnot(max(abs(with(contribuicoes, JGB2 + UST30 + `Não explicado` -
                       d_jgb30_bp)), na.rm = TRUE) < 1e-8)
write_csv(contribuicoes, file.path(saida, "teste1_contribuicoes_21d.csv"))
areas <- contribuicoes |>
  pivot_longer(c(`JGB2`, `UST30`, `Não explicado`),
               names_to = "componente", values_to = "contribuicao")
grafico_areas <- ggplot(areas, aes(date, contribuicao, fill = componente)) +
  geom_area(position = "stack", alpha = 0.8) +
  geom_line(data = contribuicoes, aes(date, d_jgb30_bp),
            inherit.aes = FALSE, linewidth = 0.45, colour = "black") +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.25) +
  scale_x_date(limits = range(contribuicoes$date), expand = c(0, 0),
               breaks = c(seq(ymd("2021-01-01"), ymd("2026-01-01"),
                              by = "1 year"), max(contribuicoes$date)),
               date_labels = "%m/%Y") +
  labs(title = "JGB30: variação observada em 21 pregões",
       subtitle = "Decomposição descritiva dentro da amostra; dados até 30/10/2026",
       x = NULL, y = "pontos-base", fill = "Componente") + theme_minimal()
ggsave(file.path(saida, "teste1_contribuicoes_21d.png"), grafico_areas,
       width = 12, height = 6, dpi = 160, bg = "white")

print(coef_d)

