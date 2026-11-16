# Comparacao: BVAR original x suave atual (Kalman) x tilting do plano x rigido
.libPaths(c("C:/Users/dudup/AppData/Local/R/win-library/4.6", .libPaths()))

library(fpp3)
library(tidyverse)

raiz <- "C:/Users/dudup/OneDrive/japan-econometrics-model"
pasta <- file.path(raiz, "src/chat gpt/condicionamento do plano inicial/resultados")



# 1. Juntar os forecasts e o historico
suave_atual <- read_csv(file.path(raiz, "src/Outputs/Satelite fluxo suave/resultados/opcao_3/forecast/forecasts.csv"),
                        show_col_types = FALSE) |>
  filter(modelo == "3: Satelite suave") |>
  transmute(date, cenario, modelo = "Suave atual (Kalman)", variavel, media = valor)
satelite <- read_csv(file.path(raiz, "src/Outputs/Satelite fluxo suave/resultados/opcao_3/forecast/premissas.csv"),
                     show_col_types = FALSE) |>
  transmute(date, cenario, modelo = "Alvo do satelite", variavel = "inclinacao", media = alvo_spread_pb / 100)
historico <- readRDS(file.path(raiz, "data/processed/bvar_producao_tendencia_jgb30_5_20261112/dados.rds")) |>
  as_tibble() |>
  transmute(date = as.Date(date), juro_curto, inclinacao, usdjpy = exp(usdjpy / 100), cpi_core = exp(cpi_core / 100)) |>
  pivot_longer(-date, names_to = "variavel", values_to = "media")

forecasts <- bind_rows(read_csv(file.path(pasta, "forecast tilting.csv"), show_col_types = FALSE),
                       read_csv(file.path(pasta, "forecast rigido.csv"), show_col_types = FALSE),
                       suave_atual, satelite) |>
  filter(modelo != "Tilting: so set/2028", variavel %in% c("juro_curto", "inclinacao", "usdjpy", "cpi_core"))

# Inflacao em 12 meses e spread em pb
transformar <- function(x) x |>
  mutate(media = case_when(variavel == "cpi_core" ~ 100 * (media / lag(media, 12) - 1),
                           variavel == "inclinacao" ~ 100 * media, TRUE ~ media))
tabela <- forecasts |>
  bind_rows(historico |> filter(date < min(forecasts$date)) |> cross_join(distinct(forecasts, cenario, modelo))) |>
  group_by(cenario, modelo, variavel) |>
  arrange(date) |>
  transformar() |>
  ungroup() |>
  filter(date >= min(forecasts$date))
historico <- historico |> group_by(variavel) |> arrange(date) |> transformar() |> ungroup()



# 2. Tabela em set/2028
final <- tabela |>
  filter(date == max(date)) |>
  select(cenario, modelo, variavel, media) |>
  pivot_wider(names_from = variavel, values_from = media) |>
  select(cenario, modelo, spread_pb = inclinacao, juro_curto, inflacao_12m = cpi_core, usdjpy) |>
  arrange(cenario, spread_pb)
write_csv(final, file.path(pasta, "comparacao set2028.csv"))
print(final |> mutate(across(where(is.double), ~ round(.x, 2))), n = Inf)



# 3. Grafico do cenario Base
nomes <- c(inclinacao = "JGB30Y - JGB5Y | pb", juro_curto = "Juro curto | % a.a.",
           cpi_core = "Inflacao core | 12 meses (%)", usdjpy = "USD/JPY")
ordem <- c("BVAR original", "Suave atual (Kalman)", "Tilting: todos os meses", "Rigido", "Alvo do satelite")
base <- tabela |> filter(cenario == "Base") |> mutate(modelo = factor(modelo, ordem)) |> arrange(modelo)
rotulos <- base |> group_by(modelo, variavel) |> slice_max(date, n = 1) |> filter(modelo != "Alvo do satelite") |>
  mutate(vjust = case_when(modelo == "Tilting: todos os meses" ~ -.5, modelo == "Rigido" ~ 1.5, TRUE ~ .5))
g <- ggplot(base, aes(date, media, colour = modelo, linetype = modelo, linewidth = modelo)) +
  geom_line(data = filter(historico, date >= as.Date("2023-01-01")), colour = "grey20", linewidth = .7, linetype = "solid") +
  geom_line() +
  geom_text(data = rotulos, aes(label = round(media, 2), vjust = vjust), hjust = -.1, size = 3, show.legend = FALSE) +
  scale_colour_manual(values = c("grey50", "#2166AC", "#E69F00", "#B2182B", "black")) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "22", "dotted")) +
  scale_linewidth_manual(values = c(.9, .9, 1.8, .9, .9)) +
  facet_wrap(vars(variavel), scales = "free_y", labeller = as_labeller(nomes)) +
  scale_x_date(date_labels = "%Y", expand = expansion(mult = c(.02, .15))) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Condicionamento pelo satelite: plano inicial x versao atual | Base",
       subtitle = "Tilting: media do spread igual ao satelite em todos os meses. Rigido: spread fixado no satelite.",
       x = NULL, y = NULL, colour = NULL, linetype = NULL, linewidth = NULL)
ggsave(file.path(pasta, "comparacao base.png"), g, width = 13, height = 8, dpi = 150, bg = "white")
