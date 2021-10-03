# Extracción de tablas de pdfs

## Objetivo

Obtener las siguientes tablas:

* DEMANDANTES DE EMPLEO SEGÚN COLECTIVO POR COMUNIDADES AUTÓNOMAS situada en la página 21 del documento
* PARO REGISTRADO SEGÚN SEXO Y EDAD POR CC.AA. Y PROVINCIAS  situada en la página 37 del documento
* PARO REGISTRADO SEGÚN SECTOR DE ACTIVIDAD ECONÓMICA POR CC.AA. Y PROVINCIAS  situada en la página 38 del documento



Del documento en pdf publicado por el Ministerio de Trabajo con los datos de los "Demandantes de empleo, paro, contratos y prestaciones de desempleo"

## Proceso realizado

Para extraer las tablas se usa la función `extract_tables`:

```R
area_tabla <- locate_areas(file = fichero, pages = 21)

tabla_extraida <- extract_tables(file = fichero, 
                                 pages = 21, 
                                 output = "data.frame", 
                                 area = area_tabla,
                                 guess = FALSE)

tabla_demandantes <- tabla_extraida[[1]]
```

Para asegurarse que la tabla se extrae correctamente es necesario establecer el área de extracción correspondiente a la tabla. En caso de especificar sólo la página, en este caso se extraerían dos tablas que no estarían correctamente formateadas. 

Para obtener el área de búsqueda, es necesario usar la función `locate_area` que abre una ventana interactiva para seleccionar el área de interés. La tabla extraída está dentro de una lista en formato `data.frame` que es necesario limpiar.

## Requisitos y referncias técnicos

* Viñeta de [tabulizer](https://cran.r-project.org/web/packages/tabulizer/vignettes/tabulizer.html) 

* El paquete `tabulizer` requiere la versión de `Java 1.8` para que la función `extract_tables` funcione correctamente tal y como se describe [aquí](https://githubmemory.com/repo/ropensci/tabulizer/issues). También es necesario establecer la variable de entorno `JAVA_HOME` con el path a la instalación de Java. Es posible sólo sea necesario establecer esta variable y no tener instalada la versión exacta de `Java 1.8`

