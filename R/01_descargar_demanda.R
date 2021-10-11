#' ---
#' title: "API REE"
#' subtitle: "Descargar consumo de Galicia"
#' author: "Antonio Vidal"
#' date: "`r format(Sys.time(), '%d/%m/%y - %H:%M:%S')`"
#' output: html_document
#' ---

#' # Objetivo:
#'
#' Descargar los datos de la demanda de Galicia
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

# knitr::opts_chunk(list(warning = FALSE, message = FALSE, echo = FALSE, dpi = 180, fig.width = 6, fig.height = 6))

# parametros -----------------------------------------------------------------------------------------------------------

# momento de ejecucion
fecha <- format(x = Sys.time(), format = "%y%m%d%H%M%S")

nombre_tabla <- "demanda_enerxetica"
ultimo_anho <- 2021

# funciones ------------------------------------------------------------------------------------------------------------

source(file = "00_utils.R")

obtener_demanda <- function(anho = 2020, intervalo = "month", ccaa = "17") {
  host <- "https://apidatos.ree.es"
  path <- "/es/datos/demanda/evolucion"
  
  start_date <- ymd_hm(glue("{anho}-01-01T00:00"))  
  end_date <- ymd_hm(glue("{anho}--12-31T23:59"))
  time_trunc <- intervalo
  geo_trunc <- "electric_system"
  geo_limit <- "ccaa"
  geo_ids <- ccaa
  
  respuesta <- try(GET(url = glue("{host}{path}"), 
                       query = list(start_date = start_date, 
                                    end_date = end_date, 
                                    time_trunc = time_trunc,
                                    geo_trunc = geo_trunc,
                                    geo_limit = geo_limit,
                                    geo_ids = geo_ids)))
  
  
  if ((class(respuesta) != "try-error") && (status_code(respuesta) == 200)) {
    respuesta_transformada <- content(respuesta)
    valores <- bind_rows(x = respuesta_transformada$included[[1]]$attributes$values)
    valores <- valores %>% 
      mutate(fecha = as.Date(datetime)) %>% 
      rename(consumo = value) %>% 
      select(fecha, consumo)
  } else {
    cat(file = stderr(), paste("*****", Sys.time(), "- error de respuesta", status_code(home_page), "***** \n"))
    valores <- NULL
  }
  
  return(valores)
}

# ejecucion ------------------------------------------------------------------------------------------------------------

cat(file = stderr(), paste(Sys.time(), "- descargando demanda script \n"))

anhos <- 2011:ultimo_anho

demanda <- map_dfr(.x = anhos, .f = ~obtener_demanda(anho = .x))

# guardar el resultado --------------------------------------------------------------------------------------------

wb <- createWorkbook()
# escribit todos los datos totales
escribir_hoja(datos_escribir = demanda, workbook = wb, nombre_hoja = glue("{nombre_tabla}"))
# excel general
saveWorkbook(wb = wb, 
             file = file.path("output", glue(fecha, "_{nombre_tabla}.xlsx")),
             overwrite = TRUE)