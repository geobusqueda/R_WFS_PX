# GEOBUSQUEDA

# EXPLOTACION DE DATOS PROCEDENTES DEL INSTITUTO NACIONAL DE ESTADISTICA PARA ANDALUCIA

# HORA DE INICIO
paste('Inicio del script Test_00.R: ',Sys.time()) # MENSAJE

# FICHERO LOG, DIRECTORIO DE TRABAJO, PAQUETES, LIBERAR GLOBAL_ENVIRONMENT
# PARA QUE DEVUELVA INFORMACION DE COMANDOS Y SUS SALIDAS
options(echo=TRUE)
# SE CREA FICHERO LOG EN EL QUE METER LOS MENSAJES
#sink("/cloud/project/Test_00.log", append=FALSE)

# DIRECTORIO DE TRABAJO
#setwd("/cloud/project")

# LIBERAR ESPACIO DE TRABAJO
rm(list = ls())

# VERIFICAMOS QUE ESTAN PRESENTES LOS PAQUETES NECESARIOS
paste('Verificando paquetes necesarios: ',Sys.time()) # MENSAJE
list.of.packages <- c("dplyr","sf","leaflet","pxR","data.table","htmlwidgets")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# CARGAMOS LOS PAQUETES
paste('Carga de paquetes necesarios: ',Sys.time()) # MENSAJE
lapply(list.of.packages, library, character.only = TRUE)

# CARGA DE DATASETS DEL INE: Población residente por fecha, sexo y edad desde: https://www.ine.es/dynt3/inebase/es/index.htm?padre=517&capsel=525
# Almeria: https://www.ine.es/jaxiT3/files/t/es/px/2857.px?nocab=1
datos_al <- read.px("https://www.ine.es/jaxiT3/files/t/es/px/2857.px?nocab=1")
df_datos_al   <-  as.data.frame(datos_al)
# Cadiz: https://www.ine.es/jaxiT3/files/t/es/px/2864.px?nocab=1
datos_ca <- read.px("https://www.ine.es/jaxiT3/files/t/es/px/2864.px?nocab=1")
df_datos_ca   <-  as.data.frame(datos_ca)
# Cordoba: https://www.ine.es/jaxiT3/files/t/es/px/2901.px?nocab=1
datos_co <- read.px("https://www.ine.es/jaxiT3/files/t/es/px/2901.px?nocab=1")
df_datos_co   <-  as.data.frame(datos_co)
# Granada: https://www.ine.es/jaxiT3/files/t/es/px/2871.px?nocab=1
datos_gr <- read.px("https://www.ine.es/jaxiT3/files/t/es/px/2871.px?nocab=1")
df_datos_gr   <-  as.data.frame(datos_gr)
# Huelva: https://www.ine.es/jaxiT3/files/t/es/px/2874.px?nocab=1
datos_hu <- read.px("https://www.ine.es/jaxiT3/files/t/es/px/2874.px?nocab=1")
df_datos_hu   <-  as.data.frame(datos_hu)
# Jaen: https://www.ine.es/jaxiT3/files/t/es/px/2876.px?nocab=1
datos_ja <- read.px("https://www.ine.es/jaxiT3/files/t/es/px/2876.px?nocab=1")
df_datos_ja   <-  as.data.frame(datos_ja)
# Malaga: https://www.ine.es/jaxiT3/files/t/es/px/2882.px?nocab=1
datos_ma <- read.px("https://www.ine.es/jaxiT3/files/t/es/px/2882.px?nocab=1")
df_datos_ma   <-  as.data.frame(datos_ma)
# Sevilla: https://www.ine.es/jaxiT3/files/t/es/px/2895.px?nocab=1
datos_se <- read.px("https://www.ine.es/jaxiT3/files/t/es/px/2895.px?nocab=1")
df_datos_se   <-  as.data.frame(datos_se)

# UNION DE LOS DATASETS DE CADA PROVINCIA EN UN ÚNICO DATASET
df_datos <- df_datos_al
df_datos <- rbind(df_datos, df_datos_ca)
df_datos <- rbind(df_datos, df_datos_co)
df_datos <- rbind(df_datos, df_datos_gr)
df_datos <- rbind(df_datos, df_datos_hu)
df_datos <- rbind(df_datos, df_datos_ja)
df_datos <- rbind(df_datos, df_datos_ma)
df_datos <- rbind(df_datos, df_datos_se)

# SEPARACION DE LOS DATOS PROVINCIALES Y MUNICIPALES
#df_datos_provin <- df_datos[df_datos$Municipios %in% c("04 Almería", "11 Cádiz"
#                                                       , "14 Córdoba", "18 Granada", "21 Huelva"
#                                                       , "23 Jaén", "29 Málaga", "41 Sevilla"), ]
df_datos_municip <- df_datos[!(df_datos$Municipios %in% c("04 Almería", "11 Cádiz"
                                                       , "14 Córdoba", "18 Granada", "21 Huelva"
                                                       , "23 Jaén", "29 Málaga", "41 Sevilla")), ]

# SELECCION DE DATOS
df_datos_municip_1999 <- subset(df_datos_municip, df_datos_municip$Periodo == 1999)

# TRASPONER LOS DATOS POR SEXO Y TOTALES (data.table)
df_datos_municip_1999t <- dcast(melt(as.data.table(df_datos_municip_1999), id.vars = c("Sexo", "Municipios")), 
                                Municipios + variable ~ Sexo, value.var = "value")
df_datos_municip_1999t <- subset(df_datos_municip_1999t, df_datos_municip_1999t$variable == 'value')

# CREACION DE CAMPO CON CODIGO DE MUNICIPIO Y NOMBRE DE MUNICIPIO
df_datos_municip_1999t$cod_ine <- substr(df_datos_municip_1999t$Municipios, start = 1, stop = 5)
df_datos_municip_1999t$nombre <- substr(df_datos_municip_1999t$Municipios, start = 7, stop = 30)

# SELECCION DE VARIABLES A REPRESENTAR
df_datos_municip_1999t <- select(df_datos_municip_1999t, cod_ine, nombre, Hombres, Mujeres, Total)

# CONVERTIR LAS VARIABLES A INTEGER
df_datos_municip_1999t$Hombres <- as.integer(df_datos_municip_1999t$Hombres)
df_datos_municip_1999t$Mujeres <- as.integer(df_datos_municip_1999t$Mujeres)
df_datos_municip_1999t$Total <- as.integer(df_datos_municip_1999t$Total)

# CARGA DE CAPA VECTORIAL A PARTIR DE SERVICIO WFS DEL INSTITUTO DE ESTADISTICA Y CARTOGRAFIA DE ANDALUCIA
# REFERENCIA: https://gis.stackexchange.com/questions/276522/loading-the-spanish-cadastre-inspire-wfs-into-a-leaflet-in-r

# LECTURA DE LOS DATOS DEL WFS
municipios_sf <- st_read("https://www.juntadeandalucia.es/institutodeestadisticaycartografia/wfs/lineaslimite?service=wfs&request=getcapabilities")
# TRANSFORMACION A WGS84
municipios_sf_wgs84 <- st_transform(municipios_sf,4326)

# RELACION DE DATOS CON CAPA GEOGRAFICA
municipios_sf_wgs84_1999t <- merge(x = municipios_sf_wgs84, y = df_datos_municip_1999t, by.x = "codigo_ine", by.y = "cod_ine", all = FALSE)

# CONVERSION A SPATIAL DATA FRAME
municipios_sp_wgs84_1999t <- as(municipios_sf_wgs84_1999t, "Spatial")

# CARGAR EN LEAFLET
#leaflet() %>%
#  addTiles() %>%
#  addPolygons(data = municipios_sp_wgs84_1999t)

# CONFIGURAR MAPA
paleta_poblacion_hombres <- colorNumeric(palette = "YlOrRd", domain = municipios_sp_wgs84_1999t$Hombres)
paleta_poblacion_mujeres <- colorNumeric(palette = "YlOrRd", domain = municipios_sp_wgs84_1999t$Mujeres)
paleta_poblacion_total <- colorNumeric(palette = "YlOrRd", domain = municipios_sp_wgs84_1999t$Total)
label_total <- sprintf(
  "<strong>%s</strong><br/>Total: %g",
  municipios_sp_wgs84_1999t$nombre, municipios_sp_wgs84_1999t$Total
) %>% lapply(htmltools::HTML)
label_hombres <- sprintf(
  "<strong>%s</strong><br/>Hombres: %g",
  municipios_sp_wgs84_1999t$nombre, municipios_sp_wgs84_1999t$Hombres
) %>% lapply(htmltools::HTML)
label_mujeres <- sprintf(
  "<strong>%s</strong><br/>Mujeres: %g",
  municipios_sp_wgs84_1999t$nombre, municipios_sp_wgs84_1999t$Mujeres
) %>% lapply(htmltools::HTML)
map1 <- leaflet(municipios_sp_wgs84_1999t) %>%
  # Personalizar botones
  addEasyButton(easyButton(
    icon = "fa-globe", title = "Zoom completo",
    onClick = JS("function(btn, map){ map.setView([37.1, -4.7], 7);}"))) %>%
  addEasyButton(easyButton(
    icon = "fa-crosshairs", title = "Mi posición",
    onClick = JS("function(btn, map){ map.locate({setView: true});}"))) %>%
  # Overlay groups
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("YlOrRd", Hombres)(Hombres),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              label = label_hombres,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"), group = "Hombres") %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("YlOrRd", Mujeres)(Mujeres),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              label = label_mujeres,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"), group = "Mujeres") %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("YlOrRd", Total)(Total),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE), 
              label = label_total,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"), group = "Total") %>%
  addLegend(group = "Hombres", position = "bottomright", pal = paleta_poblacion_hombres, values = ~Hombres,
            title = "Población hombres",
            labFormat = labelFormat(prefix = " "),
            opacity = 0.7 ) %>%
  addLegend(group = "Mujeres", position = "bottomright", pal = paleta_poblacion_mujeres, values = ~Mujeres,
            title = "Población mujeres",
            labFormat = labelFormat(prefix = " "),
            opacity = 0.7 ) %>%
  addLegend(group = "Total", position = "bottomright", pal = paleta_poblacion_total, values = ~Total,
            title = "Población total",
            labFormat = labelFormat(prefix = " "),
            opacity = 0.7 ) %>%
  # Layers control
  addLayersControl(
    overlayGroups = c("Hombres","Mujeres","Total"),
    options = layersControlOptions(collapsed = TRUE))
map1 %>% hideGroup("Hombres") %>% hideGroup("Mujeres")

# CONVERTIR MAPA EN WIDGET HTML
saveWidget(map1, file="pob_and_1999.html", selfcontained = FALSE)

# HORA DE FIN
paste('Fin del script Test_00.R: ',Sys.time()) # MENSAJE

#LIBERAR ESPACIO DE TRABAJO
rm(list = ls())

# SE RESTAURA PARA QUE DEVUELVA LOS LOG EN PANTALLA
#sink()

# FIN DEL SCRIP DE CREACION DE DATOS EN CSV Y DATASETS PARA EXPLOTACIONES TABULARES Y GRAFICAS.
