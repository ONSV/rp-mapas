library(tidyverse)
library(sf)
library(hms)

naofatais <- st_read("data/naofatais_rp.gpkg")
fatais <- st_read("data/fatais_rp_territorio.gpkg")
obitos <- st_read("data/obitos_rp.gpkg")
naofatais_horafix <- st_read("data/naofatais_rp_sf_horafix.gpkg")

filter_tipo <- function(data, tipos) {
  if ("data_do_obito" %in% names(data)) {
    data |> 
      filter(tipo_de_acidente == tipos)
  } else {
    data |> 
      filter(tipo_sinistro == tipos, pessoas_envolvidas_grave > 0)
  }
}

graves_tipos <- map(
  c("Colisão", "Atropelamento pedestre", "Choque"),
  filter_tipo,
  data = naofatais
)

fatais_tipos <- map(
  c("COLISAO", "ATROPELAMENTO", "CHOQUE"),
  filter_tipo,
  data = obitos
)

sinistros_tipos <- map2(
  graves_tipos,
  fatais_tipos,
  bind_rows
)

graves_noite <- naofatais_horafix |> 
  mutate(
    hora_do_acidente = parse_hms(hora_do_acidente),
    hora = hour(hora_do_acidente)
  ) |> 
  filter(pessoas_envolvidas_grave > 0, hora >= 22 | hora < 6)

fatais_noite <- obitos |> 
  mutate(
    hora_do_acidente = parse_hms(hora_do_acidente),
    hora = hour(hora_do_acidente)
  ) |> 
  filter(hora >= 22 | hora < 6)

sinistros_noite <- bind_rows(graves_noite, fatais_noite)

map2(
  graves_tipos,
  paste0(
    "data/sinistro_grave_",
    c("colisao", "atropelamento", "choque"),
    ".gpkg"
  ),
  st_write,
  append = FALSE
)

map2(
 fatais_tipos,
  paste0(
    "data/sinistro_fatal_",
    c("colisao", "atropelamento", "choque"),
    ".gpkg"
  ),
  st_write,
  append = FALSE
)

st_write(sinistros_noite, "data/sinistros_noite.gpkg", append = FALSE)
