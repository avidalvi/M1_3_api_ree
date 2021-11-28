# functions -------------------------------------------------------------------------------------------------------

# wb <- createWorkbook()
# # escribit todos los datos totales
# escribir_hoja(datos_escribir = updated_records, workbook = wb, nombre_hoja = objeto_sf)
# # excel general
# saveWorkbook(wb = wb, file = file.path("migracion", glue(fecha, "_SF_update_{objeto_sf}_{bbdd_SAP}.xlsx")),
#              overwrite = TRUE)

# escribir un excel formateado
escribir_hoja <- function(datos_escribir, workbook, nombre_hoja){
  
  # libraries
  pacman::p_load(tidyverse, openxlsx)
  
  # style negative
  negStyle <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
  # style header
  headStyle <- createStyle(fontColour = "#ffffff", fgFill = "#97002d",
                           halign = "center", valign = "center", textDecoration = "Bold")
  
  addWorksheet(wb = workbook, sheetName = nombre_hoja)
  writeData(wb = workbook, sheet = nombre_hoja, x = datos_escribir, startRow = 1, startCol = 1,
            borders = "all", borderStyle = "thin", withFilter = TRUE, headerStyle = headStyle)
  # conditionalFormatting(wb = workbook, sheet = nombre_hoja,
  #                       cols = 2:ncol(datos_escribir),
  #                       rows = 1:nrow(datos_escribir) + 1,
  #                       rule="<0",
  #                       style = negStyle)
  setColWidths(wb = workbook, sheet = nombre_hoja, cols = 1:ncol(datos_escribir), widths = "auto")
  freezePane(wb = workbook, sheet = nombre_hoja, firstRow = TRUE)
}

# dibuja la distribucion de una variable
pintar_distribucion1 <- function(datos_imagen, variable, titulo, eje_x) {
  
  # libraries
  pacman::p_load(tidyverse, GGally, ggpubr)
  
  # https://stackoverflow.com/questions/58785930/r-find-maximum-of-density-plot
  # encontrar los maximos de la funcion de densidad
  densidad <- density(x = pull(datos_imagen, variable), adjust = 2/3)
  # plot(asd)
  modes <- function(d){
    i <- which(diff(sign(diff(d$y))) < 0) + 1
    data.frame(x = d$x[i], y = d$y[i])
  }
  maximos <- modes(densidad)
  
  minimo <- min(dplyr::select(datos_imagen, any_of(variable)), na.rm = TRUE) - 
    0.2 * min(dplyr::select(datos_imagen, any_of(variable)), na.rm = TRUE)
  maximo <- max(dplyr::select(datos_imagen, any_of(variable)), na.rm = TRUE) + 
    0.2 * min(dplyr::select(datos_imagen, any_of(variable)), na.rm = TRUE)
  rango <- maximo - minimo
  
  imagen1 <- datos_imagen %>% ggplot(mapping = aes_string(x = variable)) + 
    geom_histogram(alpha = 0.5, position = "identity", bins = 100, aes(y = ..density..)) +
    geom_density(alpha = 0.4, fill = "#97002d", adjust = 2/3) + 
    geom_rug(alpha = 0.1) +
    # geom_vline(xintercept = maximos$x) +
    theme_light() +
    # scale_x_continuous(# expand = c(0,0), 
    #   # limits = c(minimo, maximo),  
    #   breaks = seq(round(minimo, digits = -1), maximo, 50)) +
    labs(# title = paste("Distribucción de la producción de", producto),
      # subtitle = paste("Comtrade from", min(importaciones_totales$Year), "to", max(importaciones_totales$Year)),
      x = eje_x,
      y = "Density") +
    theme(legend.position = "none") + 
    theme(strip.text.x = element_text(size = 12, color = "white", face = "bold"),
          strip.background = element_rect(color = "#97002d", fill = "#97002d", size = 1.5, linetype = "solid"))
  
  imagen2 <- datos_imagen %>% ggplot(mapping = aes_string(x = variable)) +
    geom_boxplot() +
    theme_light() +
    # scale_x_continuous(# expand = c(0,0), 
    #   # limits = c(minimo, maximo), 
    #   breaks = seq(round(minimo, digits = -1), maximo, 50)) +
    labs(x = eje_x)
  
  ggarrange(imagen1, imagen2, nrow = 2, common.legend = TRUE, align = "v", heights = c(3, 1)) %>%
    annotate_figure(top = text_grob(titulo, size = 15))
}

# draw a scatter with color
scatter_color <- function(data, var_x, var_y, var_color){
  imagen1 <- reorder_predict %>% ggplot(mapping = aes_string(x = variable_x, y = variable_y, color = variable_color)) +
    geom_point() +
    # geom_smooth(method = loess, formula = "y ~ x") +
    theme_light() +
    labs(title = glue("{variable_x} vs {variable_y}"),
         # subtitle = paste("Comtrade from", min(carrag_total$Year), "to", max(carrag_total$Year)),
         x = variable_x,
         y = variable_y)
  
  ggplotly(imagen1)
}

# esperar un tiempo
esperar <- function(media) {
  # espera para la siguiente llamada
  espera <- rnorm(n = 1, mean = media, sd = sqrt(media))
  espera <- ifelse(espera < 0, sqrt(media), espera)
  cat(file = stderr(), paste(Sys.time(), "- esperando", round(espera, 2), "segundos \n"))
  Sys.sleep(espera)
}

# dibujar series temporales 
dibujar_series <- function(datos_serie, columna_fecha, titulo){
  # librerias necesarias
  pacman::p_load(xts, dygraphs, lubridate, glue, tidyverse)
  serie_ts <- xts(x = select(datos_serie, -{{columna_fecha}}), 
                  order.by = select(datos_serie, {{columna_fecha}}) %>% pull())
  dygraph(data = serie_ts, main = titulo, group = "grupo") %>% dyRangeSelector() %>%
    dyLegend(show = "follow") %>%
    dyHighlight(highlightCircleSize = 4, highlightSeriesBackgroundAlpha = 0.5,
                hideOnMouseOut = TRUE, highlightSeriesOpts = list(strokeWidth = 2))
  
}