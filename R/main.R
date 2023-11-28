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

sinistros_dia_hora |> 
  count(dia_semana) |> 
  ggplot(aes(x = dia_semana, y = n)) +
  geom_segment(aes(yend = n, xend = dia_semana, x = dia_semana, y = 0)) +
  geom_point(fill = "white", pch = 21, ) +
  coord_flip() +
  theme_void() +
  geom_text(aes(label = dia_semana), y = 15, nudge_x = 0.15) +
  geom_text(aes(label = n), nudge_y = 10)
