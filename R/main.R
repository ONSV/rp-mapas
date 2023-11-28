library(tidyverse)
library(sf)
library(readxl)
library(janitor)
library(hms)
library(onsvplot)

# Import ------------------------------------------------------------------

sinistros_naofatais <- read_csv2(
  "http://painelderesultados.infosiga.sp.gov.br/bases/acidentes_naofatais.csv",
  locale = locale(encoding = "latin1")
)

fatais_rp <- st_read("data/fatais_rp.gpkg")

obitos <- read_csv2(
  "http://painelderesultados.infosiga.sp.gov.br/bases/obitos_publico.csv",
  locale = locale(encoding = "latin1")
)

# Arrange -----------------------------------------------------------------

naofatais_rp <- sinistros_naofatais |> 
  clean_names() |> 
  filter(municipio == "RIBEIRAO PRETO") |> 
  mutate(
    tipo_sinistro = case_when(
      tipo_de_acidente_atropelamento_animal == 1 ~ "Atropelamento animal",
      tipo_de_acidente_atropelamento_pedestre == 1 ~ "Atropelamento pedestre",
      tipo_de_acidente_choque == 1 ~ "Choque",
      tipo_de_acidente_colisao == 1 ~ "Colisão",
      tipo_de_acidente_outros_tipos_de_acidente == 1 ~ "Outros",
      TRUE ~ NA
    )
  )

fatais_rp <- fatais_rp |> 
  clean_names()

obitos_rp <- obitos |> 
  clean_names() |> 
  filter(municipio == "RIBEIRAO PRETO")

# Spatial data ------------------------------------------------------------

naofatais_rp_sf <- naofatais_rp |> 
  drop_na(long_geo, lat_geo) |> 
  st_as_sf(coords = c("long_geo", "lat_geo")) |> 
  st_set_crs(value = 4674)

obitos_rp_sf <- obitos_rp |> 
  mutate(
    across(ends_with("geo"), ~if_else(.x == "NAO DISPONIVEL", NA, .x)),
    across(ends_with("geo"), ~str_replace(.x, ",", ".")),
    across(ends_with("geo"), as.numeric)
  ) |> 
  drop_na(long_geo, lat_geo) |> 
  st_as_sf(coords = c("long_geo", "lat_geo")) |> 
  st_set_crs(value = 4674)

st_write(naofatais_rp_sf, "data/naofatais_rp.gpkg", append = FALSE)

st_write(obitos_rp_sf, "data/obitos_rp.gpkg", append = FALSE)

# Gráficos e tabelas ------------------------------------------------------

sinistros_dia_hora <- fatais_rp |> 
  st_drop_geometry() |> 
  as_tibble() |> 
  select(data_do_acidente, hora_do_acidente) |> 
  mutate(tipo = "fatal", hora_do_acidente = parse_hms(hora_do_acidente)) |> 
  bind_rows(
    naofatais_rp |> 
      filter(pessoas_envolvidas_grave > 0) |> 
      select(data_do_acidente, hora_do_acidente) |> 
      mutate(tipo = "grave")
  ) |> 
  mutate(
    hora = hour(hora_do_acidente),
    dia_semana = wday(
      data_do_acidente,
      locale = "pt_BR.UTF-8",
      abbr = FALSE,
      label = TRUE
    )
  )


dia_plot <- 
  sinistros_dia_hora |> 
  count(dia_semana) |>
  mutate(dia_semana = fct_rev(dia_semana)) |> 
  ggplot(aes(x = dia_semana, y = n)) +
  geom_segment(
    aes(yend = n, xend = dia_semana, x = dia_semana, y = 0),
    color = onsv_palette$blue
  ) +
  geom_point(fill = "white", pch = 21, color = onsv_palette$blue, size = 2) +
  coord_flip() +
  theme_minimal(base_size = 9) +
  theme(
    text = element_text(color = "grey20", family = "Helvetica"),
    plot.title.position = "plot",
    panel.grid.major.y = element_blank()
  ) +
  geom_text(
    aes(label = n),
    nudge_y = 10,
    size = 2.5,
    color = "grey20"
  ) +
  labs(
    title = "Quantidade de sinistros graves e fatais por dia da semana",
    subtitle = "Período entre jan-2015 e out-2023 em Ribeirão Preto - SP",
    caption = "Fonte: ONSV, com base em INFOSIGA-SP (2023)",
    x = NULL,
    y = NULL
  )

hora_plot <- sinistros_dia_hora |> 
  count(hora) |> 
  ggplot(aes(x = hora, y = n)) +
  geom_segment(
    aes(y = 0, yend = n, x = hora, xend = hora),
    color = onsv_palette$blue
  ) +
  geom_point(pch = 21, fill = "white", color = onsv_palette$blue, size = 2) +
  geom_text(aes(label = n), size = 2.5, nudge_y = 6, color = "grey20") +
  theme_minimal(base_size = 9) +
  labs(
    title = "Quantidade de sinistros graves e fatais por hora do dia",
    subtitle = "Período entre jan-2015 e out-2023 em Ribeirão Preto - SP",
    caption = "Fonte: ONSV, com base em INFOSIGA-SP (2023)",
    x = "Horário",
    y = NULL
  ) +
  scale_x_continuous(breaks = seq(0, 23, 1)) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    text = element_text(color = "grey20", family = "Helvetica"),
    plot.title.position = "plot"
  )

hora_dia_plot <- sinistros_dia_hora |> 
  count(hora, dia_semana, .drop = FALSE) |>
  mutate(dia_semana = fct_rev(dia_semana)) |> 
  ggplot(aes(x = hora, y = dia_semana, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n), size = 2, color = "grey20") +
  coord_equal() +
  scale_x_continuous(breaks = seq(0, 23, 1)) +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  theme_minimal(base_size = 8) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(color = "grey20", family = "Helvetica"),
    plot.title.position = "plot",
    plot.caption = element_text(hjust = 0.93)
  ) +
  labs(
    x = "Horário",
    y = NULL,
    title = "Quantidade de sinistros graves e fatais por hora do dia e dia da semana",
    subtitle = "Período entre jan-2015 e out-2023 em Ribeirão Preto - SP",
    caption = "Fonte: ONSV, com base em INFOSIGA-SP (2023)"
  ) ## Esse aqui vai ser 6 x 3.5 in

plot_vitimas_idade <- obitos_rp |> 
  count(faixa_etaria) |> 
  filter(faixa_etaria != "NAO DISPONIVEL") |> 
  ggplot(aes(x = faixa_etaria, y = n)) +
  geom_segment(
    aes(x = faixa_etaria, xend = faixa_etaria, y = 0, yend = n),
    color = onsv_palette$blue
  ) +
  geom_point(color = onsv_palette$blue, fill = "white", size = 1.5, pch = 21) +
  geom_text(aes(label = n), size = 2, color = "grey20", nudge_y = 3.5) +
  coord_flip() +
  theme_minimal(base_size = 9) +
  labs(
    x = NULL,
    y = NULL,
    title = "Quantidade de vítimas fatais em sinistros de trânsito por faixa etária",
    subtitle = "Período entre jan-2015 e out-2023 em Ribeirão Preto - SP",
    caption = "Fonte: ONSV, com base em INFOSIGA-SP (2023)"
  ) +
  theme(
    text = element_text(color = "grey20", family = "Helvetica"),
    plot.title.position = "plot",
    panel.grid.major.y = element_blank()
  )

plot_vitimas_modal <- obitos_rp |> 
  count(tipo_do_veiculo_da_vitima) |> 
  mutate(tipo_veic = fct_reorder(tolower(tipo_do_veiculo_da_vitima), n)) |>
  filter(tipo_veic != "nao disponivel") |> 
  ggplot(aes(x = tipo_veic, y = n)) +
  geom_segment(
    aes(
      x = tipo_veic,
      xend = tipo_veic,
      y = 0,
      yend = n
    ),
    color = onsv_palette$blue
  ) +
  geom_point(color = onsv_palette$blue, fill = "white", size = 1.5, pch = 21) +
  geom_text(aes(label = n), size = 2, color = "grey20", nudge_y = 8) +
  coord_flip() +
  theme_minimal(base_size = 9) +
  labs(
    x = NULL,
    y = NULL,
    title = "Quantidade de vítimas fatais em sinistros de trânsito por modo de transporte",
    subtitle = "Período entre jan-2015 e out-2023 em Ribeirão Preto - SP",
    caption = "Fonte: ONSV, com base em INFOSIGA-SP (2023)"
  ) +
  theme(
    text = element_text(color = "grey20", family = "Helvetica"),
    plot.title.position = "plot",
    panel.grid.major.y = element_blank()
  )

combinacoes_idade_modal <- obitos_rp |> 
  rename(tipo_veic = tipo_do_veiculo_da_vitima) |> 
  filter(tipo_veic != "NAO DISPONIVEL", faixa_etaria != "NAO DISPONIVEL") |>
  mutate(
    tipo_veic = tolower(tipo_veic),
    faixa_etaria = if_else(faixa_etaria == "80 ou mais", "80+", faixa_etaria)
  ) |> 
  expand(tipo_veic, faixa_etaria)

count_idade_modal <- obitos_rp |> 
  rename(tipo_veic = tipo_do_veiculo_da_vitima) |> 
  filter(tipo_veic != "NAO DISPONIVEL", faixa_etaria != "NAO DISPONIVEL") |>
  mutate(
    tipo_veic = tolower(tipo_veic),
    faixa_etaria = if_else(faixa_etaria == "80 ou mais", "80+", faixa_etaria)
  ) |> 
  count(tipo_veic, faixa_etaria)

vitimas_idade_modal <- combinacoes_idade_modal |> 
  left_join(count_idade_modal, by = c("tipo_veic", "faixa_etaria")) |> 
  replace_na(list(n = 0))

plot_vitimas_idade_modal <- 
  ggplot(vitimas_idade_modal, aes(x = faixa_etaria, y = tipo_veic, fill = n)) +
  geom_tile() +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  geom_text(aes(label = n), size = 2.5, color = "grey20") +
  theme_minimal(base_size = 9) +
  coord_fixed() +
  theme(
    legend.position = "none",
    text = element_text(color = "grey20", family = "Helvetica"),
    plot.title.position = "plot",
    panel.grid.major = element_blank()
  ) +
  labs(
    title = "Quantidade de vítimas fatais por modo de transporte faixa etária",
    subtitle = "Período entre jan-2015 e out-2023 em Ribeirão Preto - SP",
    caption = "Fonte: ONSV, com base em INFOSIGA-SP (2023)",
    x = "Faixa etária",
    y = NULL
  )


