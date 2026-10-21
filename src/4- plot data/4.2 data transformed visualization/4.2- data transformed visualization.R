# 3. Gráficos das variáveis do modelo

library(tidyverse)

data_modelo |>
  as_tibble() |>
  mutate(date = as.Date(date)) |>
  gather(variavel, valor, -date) |>
  ggplot(aes(x = date, y = valor, colour = variavel)) +
  geom_line() +
  facet_wrap(~variavel, scales = "free") +
  theme(legend.position = "none")


