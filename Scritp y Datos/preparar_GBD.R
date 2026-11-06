# =============================================================================
# 02_preparar_gbd.R (Apuntando a las carpetas con los países finales)
# =============================================================================

# 1. Establecer directorio de trabajo
try(setwd("~/Escritorio/BIOMEDICINA/TTEPI/datos/scriptR/proyecto_VZV_demencia"), silent = TRUE)

# 2. Cargar la configuración general del proyecto
source(file.path("01_sintaxis", "00_configuracion.R"))
USAR_DATOS_SIMULADOS <- FALSE

# 3. RUTA DIRECTA a tu carpeta de datos de GBD mostrada en la captura
ruta_gbd_final <- file.path(ruta_original, "gbd", "IHME-GBD_2023_DATA-357f4bf6-1", "datos paises finales")

message("Directorio de búsqueda para GBD: ", ruta_gbd_final)

# Localizar los archivos CSV dentro de esa subcarpeta concreta
archivos <- list.files(ruta_gbd_final, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)

if (length(archivos) == 0) {
  stop("No se encontraron CSV en: ", ruta_gbd_final)
}

message("Archivos CSV localizados: ", length(archivos))
gbd_raw <- purrr::map_dfr(archivos, readr::read_csv, show_col_types = FALSE)
message("Registros leídos del GBD: ", nrow(gbd_raw))

# ---- 1. Comprobar columnas obligatorias -------------------------------------
necesarias <- c("measure_name", "location_name", "sex_name", "age_name",
                "cause_name", "metric_name", "year", "val")
faltan <- setdiff(necesarias, names(gbd_raw))

if (length(faltan) > 0) {
  stop("Faltan columnas obligatorias: ", paste(faltan, collapse = ", "))
}

# ---- 2. Filtrar según la metodología de la Ficha ---------------------------
gbd <- gbd_raw |>
  dplyr::filter(
    measure_name == "Incidence", 
    metric_name == "Rate",
    sex_name == "Both",
    cause_name %in% c(causa_hz, causa_demencia),
    age_name %in% c("Age-standardized", pesos_esp2013$age_name),
    year >= periodo_inicio, year <= periodo_fin,
    !is.na(val), val >= 0
  )

# ---- 3. Asignación automática de ISO3 para TODOS los países contenidos -------
gbd <- gbd |>
  dplyr::mutate(iso3 = countrycode::countrycode(location_name, "country.name", "iso3c", warn = FALSE)) |>
  dplyr::filter(!is.na(iso3))

paises_detectados <- dplyr::n_distinct(gbd$iso3)
message("------------------------------------------------------------")
message("Países únicos detectados en la descarga de GBD: ", paises_detectados)
message("Lista ISO3: ", paste(sort(unique(gbd$iso3)), collapse = ", "))
message("------------------------------------------------------------")

# ---- 4. Estandarización directa por edad (ESP 2013 para >= 65 años) ----------
tasa65 <- gbd |>
  dplyr::inner_join(pesos_esp2013, by = "age_name") |>
  dplyr::group_by(iso3, year, cause_name) |>
  dplyr::summarise(
    n_grupos = dplyr::n(),
    tasa = sum(val * peso) / sum(peso), 
    .groups = "drop"
  )

incompletos <- dplyr::filter(tasa65, n_grupos != nrow(pesos_esp2013))
if (nrow(incompletos) > 0) {
  warning(nrow(incompletos), " registros país-año no tienen los 7 grupos de edad completos. Se asigna NA.")
  tasa65 <- dplyr::mutate(tasa65, tasa = ifelse(n_grupos == nrow(pesos_esp2013), tasa, NA))
}

tasa_as <- gbd |>
  dplyr::filter(age_name == "Age-standardized") |>
  dplyr::select(iso3, year, cause_name, tasa = val)

# ---- 5. Generar Panel País-Año en formato ancho ----------------------------
etiqueta <- function(causa, sufijo) ifelse(causa == causa_hz, paste0("hz", sufijo), paste0("dem", sufijo))

panel <- dplyr::bind_rows(
  dplyr::mutate(tasa65,  variable = etiqueta(cause_name, "65_esp")) |> dplyr::select(iso3, year, variable, tasa),
  dplyr::mutate(tasa_as, variable = etiqueta(cause_name, "_as"))     |> dplyr::select(iso3, year, variable, tasa)
) |>
  tidyr::pivot_wider(names_from = variable, values_from = tasa) |>
  dplyr::arrange(iso3, year)

# ---- 6. Guardar panel final --------------------------------------------------
archivo_salida <- file.path(ruta_trabajo, "gbd_panel_todos_los_paises.csv")
readr::write_csv(panel, archivo_salida)

message("Panel final guardado en: ", archivo_salida)
message("Total de filas (país-año): ", nrow(panel))
print(summary(panel))