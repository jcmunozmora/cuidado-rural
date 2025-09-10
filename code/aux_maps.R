## Maps
maps     <- "data/inputs/Colombia_Division"

# ---- Mapa: departamentos (adm1) ----
map1 <- st_read(file.path(maps, "COL_adm1.shp"), quiet = TRUE) |> st_make_valid()

# sf departamental con geometría y código de 2 dígitos
depto_sf <- map1 %>%
  transmute(
    dpto = str_pad(as.character(ID_ESP), 2, pad = "0"),
    NAME_0, NAME_1, geometry
  )

# sf departamental con geometría y código de 2 dígitos
depto_sf <- map1 %>%
  transmute(
    dpto = str_pad(as.character(ID_ESP), 2, pad = "0"),
    NAME_0, NAME_1, geometry
  )

# ---- Mapa: mpios (adm2) ----
map2 <- st_read(file.path(maps, "municipios.shp"), quiet = TRUE) |> st_make_valid()

# sf departamental con geometría y código de 2 dígitos
mpio_sf <- map2 %>%
  transmute(
    codigo_dane = as.numeric(as.character(ID_ESPACIA), 4, pad = "0"),
    NMG, geometry
  )
