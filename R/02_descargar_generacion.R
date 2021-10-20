#' ---
#' title: "API REE"
#' subtitle: "Descargar generación en Galicia"
#' author: "Antonio Vidal"
#' date: "`r format(Sys.time(), '%d/%m/%y - %H:%M:%S')`"
#' output: html_document
#' ---

#' # Objetivo:
#'
#' Descargar los datos de la generación en Galicia
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

nombre <- "xeracion_por_tipo"
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
    rename(produccion = value,
           porcentaxe = percentage) %>% 
    select(-datetime)
}

obtener_produccion <- function(anho = 2020, intervalo = "month", ccaa = "17") {
  cat(file = stderr(), paste(Sys.time(), "- descargando año: ", anho, "\n"))
  
  host <- "https://apidatos.ree.es"
  path <- "/es/datos/generacion/estructura-generacion"
  
  start_date <- ymd_hm(glue("{anho}-01-01T00:00"))  
  end_date <- ymd_hm(glue("{anho}-12-31T23:59"))
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
    valores <- map_dfr(.x = respuesta_transformada$included, .f = ~obtener_tecnologia(datos_tec = .x))
  } else {
    cat(file = stderr(), paste("*****", Sys.time(), "- error de respuesta: ", status_code(respuesta), "***** \n"))
    valores <- NULL
  }
  
  return(valores)
}

# ejecucion ------------------------------------------------------------------------------------------------------------

cat(file = stderr(), paste(Sys.time(), "- descargando demanda script \n"))

anhos <- 2011:ultimo_anho

produccion <- map_dfr(.x = anhos, .f = ~obtener_produccion(anho = .x))


# transformacion de datos -----------------------------------------------------------------------------------------

orixe_produccion <- produccion %>% 
  select(-porcentaxe, -tipo) %>% 
  pivot_wider(names_from = tecnoloxia, values_from = produccion)

orixe_porcentaxe <- produccion %>% 
  select(-produccion, -tipo) %>% 
  pivot_wider(names_from = tecnoloxia, values_from = porcentaxe)

orixe_tipo_produccion <- produccion %>%
  group_by(fecha, tipo) %>% 
  summarise(produccion = sum(produccion),
            .groups = "drop") %>% 
  pivot_wider(names_from = tipo, values_from = produccion)

orixe_tipo_porcentaxe <- produccion %>%
  group_by(fecha, tipo) %>% 
  summarise(porcentaxe = sum(porcentaxe),
            .groups = "drop") %>% 
  pivot_wider(names_from = tipo, values_from = porcentaxe)

# guardar el resultado --------------------------------------------------------------------------------------------

wb <- createWorkbook()
# escribit todos los datos totales
escribir_hoja(datos_escribir = produccion, workbook = wb, nombre_hoja = "produccion")
escribir_hoja(datos_escribir = orixe_produccion, workbook = wb, nombre_hoja = "orixe_produccion")
escribir_hoja(datos_escribir = orixe_porcentaxe, workbook = wb, nombre_hoja = "orixe_produccion")
escribir_hoja(datos_escribir = orixe_tipo_produccion, workbook = wb, nombre_hoja = "tipo_produccion")
escribir_hoja(datos_escribir = orixe_tipo_porcentaxe, workbook = wb, nombre_hoja = "tipo_porcentaxe")
# excel general
saveWorkbook(wb = wb, 
             file = file.path("output", glue(fecha, "_{nombre}.xlsx")),
             overwrite = TRUE)