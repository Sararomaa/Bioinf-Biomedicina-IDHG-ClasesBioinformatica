# =============================================================================
# 03_fusionar_matriz.R (Versión resuelta)
# =============================================================================

try(setwd("~/Escritorio/BIOMEDICINA/TTEPI/datos/scriptR/proyecto_VZV_demencia"), silent = TRUE)
source(file.path("01_sintaxis", "00_configuracion.R"))

# 1. Cargar el panel de GBD procesado
gbd_panel <- readr::read_csv(file.path(ruta_trabajo, "gbd_panel_todos_los_paises.csv"), show_col_types = FALSE)

# 2. Cargar y procesar covariables de Eurostat desde los CSV locales
ruta_eurostat_local <- file.path(ruta_original, "eurostat", "datod con paises finales")

archivo_educ  <- list.files(ruta_eurostat_local, pattern = "^edat_lfse_03.*\\.csv$", full.names = TRUE, recursive = TRUE)
archivo_gasto <- list.files(ruta_eurostat_local, pattern = "^hlth_sha11_hf.*\\.csv$", full.names = TRUE, recursive = TRUE)

archivo_educ  <- archivo_educ[!grepl("\\.gz$", archivo_educ)][1]
archivo_gasto <- archivo_gasto[!grepl("\\.gz$", archivo_gasto)][1]

educ_raw  <- readr::read_csv(archivo_educ, col_types = readr::cols(.default = "c"), show_col_types = FALSE)
gasto_raw <- readr::read_csv(archivo_gasto, col_types = readr::cols(.default = "c"), show_col_types = FALSE)

# Homogeneizar nombres
if ("TIME_PERIOD" %in% names(educ_raw))  educ_raw  <- dplyr::rename(educ_raw, time = TIME_PERIOD)
if ("OBS_VALUE" %in% names(educ_raw))   educ_raw  <- dplyr::rename(educ_raw, values = OBS_VALUE)
if ("TIME_PERIOD" %in% names(gasto_raw)) gasto_raw <- dplyr::rename(gasto_raw, time = TIME_PERIOD)
if ("OBS_VALUE" %in% names(gasto_raw))  gasto_raw <- dplyr::rename(gasto_raw, values = OBS_VALUE)

# Asignar ISO3
educ <- educ_raw |>
  dplyr::mutate(
    time = as.numeric(time), values = as.numeric(values),
    iso3 = countrycode::countrycode(geo, "eurostat", "iso3c", warn = FALSE)
  ) |>
  dplyr::mutate(iso3 = ifelse(is.na(iso3), countrycode::countrycode(geo, "country.name", "iso3c", warn = FALSE), iso3)) |>
  dplyr::filter(time >= periodo_inicio, time <= periodo_fin, !is.na(values), !is.na(iso3))

gasto <- gasto_raw |>
  dplyr::mutate(
    time = as.numeric(time), values = as.numeric(values),
    iso3 = countrycode::countrycode(geo, "eurostat", "iso3c", warn = FALSE)
  ) |>
  dplyr::mutate(iso3 = ifelse(is.na(iso3), countrycode::countrycode(geo, "country.name", "iso3c", warn = FALSE), iso3)) |>
  dplyr::filter(time >= periodo_inicio, time <= periodo_fin, !is.na(values), !is.na(iso3))

# 3. Filtrar Educación Terciaria (ED5-8) y Edad 25-64
if ("isced11" %in% names(educ)) educ <- dplyr::filter(educ, isced11 == "ED5-8" | is.na(isced11))
if ("sex" %in% names(educ))     educ <- dplyr::filter(educ, sex == "T" | is.na(sex))
if ("unit" %in% names(educ))    educ <- dplyr::filter(educ, unit == "PC" | is.na(unit))

if ("age" %in% names(educ)) {
  educ_main <- dplyr::filter(educ, age %in% c("Y25-64", "Y25-64_T") | is.na(age))
  if (nrow(educ_main) == 0) educ_main <- educ
} else {
  educ_main <- educ
}

# 4. Calcular promedios por país
educ_resumen <- educ_main |>
  dplyr::group_by(iso3, time) |>
  dplyr::summarise(values = mean(values, na.rm = TRUE), .groups = "drop") |>
  dplyr::group_by(iso3) |>
  dplyr::summarise(educ_25_64 = mean(values, na.rm = TRUE), n_educ = dplyr::n(), .groups = "drop")

gasto_resumen <- gasto |>
  dplyr::group_by(iso3, time) |>
  dplyr::summarise(values = mean(values, na.rm = TRUE), .groups = "drop") |>
  dplyr::group_by(iso3) |>
  dplyr::summarise(gasto_pps = mean(values, na.rm = TRUE), n_gasto = dplyr::n(), .groups = "drop")

# Resumen de GBD (media del periodo por país)
gbd_resumen <- gbd_panel |>
  dplyr::filter(year >= periodo_inicio, year <= periodo_fin) |>
  dplyr::group_by(iso3) |>
  dplyr::summarise(
    hz65_esp  = mean(hz65_esp, na.rm = TRUE),
    dem65_esp = mean(dem65_esp, na.rm = TRUE),
    hz_as     = mean(hz_as, na.rm = TRUE),
    dem_as    = mean(dem_as, na.rm = TRUE),
    .groups = "drop"
  )

# 5. Fusionar en la matriz final
matriz_completa <- gbd_resumen |>
  dplyr::left_join(educ_resumen, by = "iso3") |>
  dplyr::left_join(gasto_resumen, by = "iso3") |>
  dplyr::mutate(incluido = iso3 %in% paises_estudio$iso3)

matriz_final <- matriz_completa |> dplyr::filter(incluido)

# 6. Guardar en carpeta de datos de trabajo
readr::write_csv(matriz_completa, file.path(ruta_trabajo, "matriz_completa_todos_los_paises.csv"))
readr::write_csv(matriz_final, file.path(ruta_trabajo, "matriz_final.csv"))

message("------------------------------------------------------------")
message("¡ÉXITO! Matriz de datos de trabajo generada correctamente.")
message("Total de países en la matriz final: ", nrow(matriz_final), " (esperados: 28)")
message("------------------------------------------------------------")

print(matriz_final, n = 30)