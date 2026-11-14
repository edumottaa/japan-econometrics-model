Sys.setlocale('LC_ALL', 'Portuguese_Brazil.utf8')
.libPaths(c('C:/Users/dudup/AppData/Local/R/win-library/4.6', .libPaths()))
library(tidyverse)
library(tsibble)

raiz <- 'C:/Users/dudup/OneDrive/japan-econometrics-model'
saida <- file.path(raiz, 'src/Outputs/4- Forecast/sensibilidade pesos/rho_0980')
fonte <- file.path(raiz, 'data/processed/sensibilidade_pesos_sem_tendencia/rho_0980')
alvos <- c('atividade', 'cpi_core', 'juro_curto', 'inclinacao', 'usdjpy')
titulos <- c(atividade = 'Atividade japonesa | variação em 12 meses (%)',
  cpi_core = 'CPI core | inflação em 12 meses (%)', juro_curto = 'Juro curto | % a.a.',
  inclinacao = 'Inclinação JGB 10a menos 2a | p.p.', usdjpy = 'USD/JPY | ienes por dólar')
cores <- c(Base = '#2166AC', Benigno = '#1B9E77', Adverso = '#D58500', 'Adverso estrutural' = '#B2182B')
tema <- theme_minimal(base_size = 13) +
  theme(legend.position = 'bottom', panel.grid.minor = element_blank(),
    strip.text = element_text(face = 'bold', hjust = 0), plot.title = element_text(face = 'bold'),
    plot.caption = element_text(hjust = 0, size = 10), panel.spacing = unit(.55, 'cm'))

# 1. Pontos anuais em cada cenário.
previsao <- readRDS(file.path(saida, 'resumo.rds')) |>
  mutate(date = as.Date(date), painel = factor(variavel, levels = alvos, labels = titulos[alvos]))
rotulos <- previsao |> filter(month(date) == 12L | date == max(date)) |>
  mutate(rotulo = if_else(variavel == 'usdjpy', formatC(media, digits = 1, format = 'f', decimal.mark = ','),
    formatC(media, digits = 2, format = 'f', decimal.mark = ',')))
espacos <- previsao |> group_by(painel) |>
  summarise(espaco = max(diff(range(media)) * .135, .04), .groups = 'drop')
rotulos_cenarios <- rotulos |> left_join(espacos, by = 'painel') |>
  group_by(painel, date) |> group_modify(function(x, chave) {
    x <- arrange(x, media)
    x$rotulo_y <- x$media
    for (i in seq_len(nrow(x))[-1L]) {
      x$rotulo_y[i] <- max(x$media[i], x$rotulo_y[i - 1L] + x$espaco[i])
    }
    x
  }) |> ungroup()

# 2. Os quatro cenários, com o mesmo eixo de tempo nos cinco painéis.
grafico_cenarios <- ggplot(previsao, aes(date, media, colour = cenario)) +
  geom_line(linewidth = .75) + geom_point(data = rotulos_cenarios, size = 1.2) +
  geom_segment(data = rotulos_cenarios, aes(xend = date, yend = rotulo_y), linewidth = .3, show.legend = FALSE) +
  geom_label(data = rotulos_cenarios, aes(y = rotulo_y, label = rotulo), size = 3.25,
    linewidth = 0, label.padding = unit(.08, 'lines'), show.legend = FALSE) +
  facet_wrap(vars(painel), ncol = 1, scales = 'free_y') +
  scale_colour_manual(values = cores, breaks = names(cores)) +
  scale_x_date(breaks = as.Date(c('2026-09-01', '2027-01-01', '2028-01-01', '2028-08-01')),
    date_labels = '%m/%Y', expand = expansion(mult = c(.02, .08))) +
  scale_y_continuous(expand = expansion(mult = c(.10, .16))) + tema +
  labs(title = 'Quatro cenários do BVAR sem tendência',
    subtitle = 'Setembro de 2026 a agosto de 2028 | médias condicionais', x = NULL, y = NULL, colour = NULL,
    caption = 'Os cenários alternativos aplicam a resposta mediana ao choque de oferta para reproduzir cada caminho de Brent.')

# 3. Cenário base, histórico e intervalos preditivos.
dados <- readRDS(file.path(fonte, 'dados.rds')) |> as_tibble() |> arrange(date)
origem <- as.Date(max(dados$date))
historico <- dados |> select(date, all_of(alvos)) |>
  pivot_longer(-date, names_to = 'variavel', values_to = 'valor') |>
  group_by(variavel) |> mutate(valor = case_when(
    variavel %in% c('atividade', 'cpi_core') ~ 100 * expm1((valor - lag(valor, 12L)) / 100),
    variavel == 'usdjpy' ~ exp(valor / 100), TRUE ~ valor)) |> ungroup() |>
  mutate(date = as.Date(date), painel = factor(variavel, levels = alvos, labels = titulos[alvos])) |>
  filter(date >= as.Date('2019-01-01'))
grafico_base <- ggplot(filter(previsao, cenario == 'Base'), aes(date, media)) +
  geom_ribbon(aes(ymin = inferior95, ymax = superior95), fill = cores[['Base']], alpha = .14) +
  geom_ribbon(aes(ymin = inferior80, ymax = superior80), fill = cores[['Base']], alpha = .23) +
  geom_line(data = historico, aes(y = valor), colour = '#252525', linewidth = .6) +
  geom_vline(xintercept = origem, linetype = 'dotted', colour = 'grey45') +
  geom_line(colour = cores[['Base']], linewidth = .75) +
  geom_point(data = filter(rotulos, cenario == 'Base'), colour = cores[['Base']], size = 1.2) +
  geom_label(data = filter(rotulos, cenario == 'Base'), aes(label = rotulo),
    colour = cores[['Base']], linewidth = 0, size = 3.4, vjust = -.3, label.padding = unit(.08, 'lines')) +
  facet_wrap(vars(painel), ncol = 1, scales = 'free_y') +
  scale_x_date(breaks = seq(as.Date('2019-01-01'), as.Date('2028-01-01'), by = 'year'),
    date_labels = '%Y', expand = expansion(mult = c(.01, .055))) +
  scale_y_continuous(expand = expansion(mult = c(.08, .18))) + tema +
  labs(title = 'Cenário base do BVAR sem tendência',
    subtitle = 'Histórico e projeções de setembro de 2026 a agosto de 2028 | 2.500 sorteios', x = NULL, y = NULL,
    caption = 'A linha azul é a média prevista. As faixas mostram os intervalos preditivos condicionais de 80% e 95%.')

# Armazenamento da saída em Outputs.
ggsave(file.path(saida, 'forecast base.png'), grafico_base, width = 11, height = 11.3, dpi = 170, bg = 'white')
ggsave(file.path(saida, 'forecast cenarios.png'), grafico_cenarios, width = 11, height = 11.3, dpi = 170, bg = 'white')
message('GRAFICOS_FORECAST_CONCLUIDOS')

