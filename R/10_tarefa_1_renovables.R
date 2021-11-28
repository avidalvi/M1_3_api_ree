#' ---
#' title: "API REE"
#' subtitle: "Tarea 1: porcentaje de producción de renovables de Galicia"
#' author: "Antonio Vidal"
#' date: "`r format(Sys.time(), '%d/%m/%y - %H:%M:%S')`"
#' output: html_document
#' ---

#' # Objetivo:
#'
#' Tarea 1: porcentaje de producción de renovables de Galicia
#'
#' La información de la API está en:
#' [API REE](https://www.ree.es/es/apidatos)
#'
#+ echo=FALSE

# initializacion -------------------------------------------------------------------------------------------------------

rm(list = ls())
setwd(here::here("R"))

# librerias necesarias
pacman::p_load(lubridate, glue, openxlsx, httr, jsonlite, tidyverse)

set.seed(1234)

knitr::opts_chunk$set(list(warning = FALSE, message = FALSE, echo = FALSE, dpi = 180, fig.width = 6, fig.height = 6))

# parametros -----------------------------------------------------------------------------------------------------------

# momento de ejecucion
fecha <- format(x = Sys.time(), format = "%y%m%d%H%M%S")

nombre_tabla <- "porcentaje_renovables_galicia"
ultimo_anho <- 2021

# funciones ------------------------------------------------------------------------------------------------------------

source(file = "00_utils.R")

obtener_tecnologia <- function(datos_tec){
  tecnoloxia <- datos_tec$attributes$title
  tipo <- datos_tec$attributes$type
  valores <- bind_rows(x = datos_tec$attributes$values)
  valores <- valores %>% 
    mutate(fecha = as.Date(datetime),
           tecnoloxia = tecnoloxia,
           tipo = tipo) %>% 
    select(-datetime)
}

obtener_datos_API_ree_anuales_por_mes <- function(anho = 2020, category = "demanda", widget = "evolucion",
                                                  geo_trunc = "electric_system", geo_limit = "ccaa", geo_ids = "17") {
  cat(file = stderr(), paste(
    Sys.time(),
    glue("- descargando datos {anho} de {category}/{widget} para {geo_limit} {geo_ids}"),
    "\n"
  ))
  host <- "https://apidatos.ree.es"
  path <- glue("/es/datos/{category}/{widget}")
  
  start_date <- ymd_hm(glue("{anho}-01-01T00:00"))
  end_date <- ymd_hm(glue("{anho}-12-31T23:59"))
  time_trunc <- "month"
  
  respuesta <- try(GET(
    url = glue("{host}{path}"),
    query = list(
      start_date = start_date,
      end_date = end_date,
      time_trunc = time_trunc,
      geo_trunc = geo_trunc,
      geo_limit = geo_limit,
      geo_ids = geo_ids
    )
  ))
  
  if ((class(respuesta) != "try-error") && (status_code(respuesta) == 200)) {
    respuesta_transformada <- content(respuesta)
    valores <- map_dfr(.x = respuesta_transformada$included, .f = ~obtener_tecnologia(datos_tec = .x))
  } else {
    cat(file = stderr(), paste("*****", Sys.time(), "- error de respuesta", status_code(respuesta), "***** \n"))
    valores <- NULL
  }
  return(valores)
}

# ejecucion ------------------------------------------------------------------------------------------------------------

cat(file = stderr(), paste(Sys.time(), "- descargando generacion script \n"))

anhos <- 2011:ultimo_anho

generacion_galicia <- map_dfr(.x = anhos, .f = ~ obtener_datos_API_ree_anuales_por_mes(
  anho = .x, category = "generacion", widget = "evolucion-renovable-no-renovable",
  geo_trunc = "electric_system", geo_limit = "ccaa", geo_ids = "17"
))

generacion_absoluta <- map_dfr(.x = anhos, .f = ~ obtener_datos_API_ree_anuales_por_mes(
  anho = .x, category = "generacion", widget = "evolucion-renovable-no-renovable",
  geo_trunc = NULL, geo_limit = NULL, geo_ids = NULL
))

generacion_galicia <- generacion_galicia %>% 
  filter(tipo == "Renovable") %>% 
  select(fecha, value) %>% 
  rename(generacion_galicia = value)

generacion_absoluta <- generacion_absoluta %>% 
  filter(tipo == "Renovable") %>% 
  select(fecha, value) %>% 
  rename(generacion_absoluta = value)

renovable <- left_join(x = generacion_absoluta, y = generacion_galicia, by = "fecha") %>% 
  mutate(porcentaje_galicia = round(generacion_galicia / generacion_absoluta * 100, 2))

dibujar_series(datos_serie = select(renovable, fecha, starts_with("generacion")), 
               columna_fecha = "fecha", titulo = "Generación renovables")


# guardar el resultado --------------------------------------------------------------------------------------------

wb <- createWorkbook()
# escribit todos los datos totales
escribir_hoja(datos_escribir = renovable, workbook = wb, nombre_hoja = glue("{nombre_tabla}"))
# excel general
saveWorkbook(
  wb = wb,
  file = file.path("output", glue(fecha, "_{nombre_tabla}.xlsx")),
  overwrite = TRUE
)

