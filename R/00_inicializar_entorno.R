#' ---
#' title: "API REE"
#' subtitle: "Inicializar entorno"
#' author: "Antonio Vidal"
#' date: "`r format(Sys.time(), '%d/%m/%y - %H:%M:%S')`"
#' output: html_document
#' ---

#' # Objetivo:
#'
#' Inicializar el entorno de trabajo para el proyecto
#'
#+ echo=FALSE

# initialization -------------------------------------------------------------------------------------------------------

rm(list = ls())
setwd(here::here("R"))

# crear directorios de trabajo
base_dir <- c("./output/", "./input/")
for (ind_dir in base_dir) {
  if (!dir.exists(ind_dir)) {
    dir.create(ind_dir)
  }
}
rm(base_dir, ind_dir)


# instalar librerias
if ("pacman" %in% rownames(installed.packages()) == FALSE) {
  install.packages("pacman")
}

pacman::p_load(openxlsx, httr, jsonlite, glue, tidyverse, tidyselect)
