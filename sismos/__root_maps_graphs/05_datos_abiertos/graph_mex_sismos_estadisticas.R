rm(list = ls())

##### VISUALIZACION DE DATOS: SISMOS #####
## Estadisticas en México con sismos A NIVEL MUNICIPIO Y ESTADOS -- DINAMICOS
## Parte 2: Estadisticas de sismos
## Equipo: Jesus Martin & Eva Gonzalez & Humberto Garcia & Alexis Rodas 
##

########## DIRECTORIO ##########
setwd("E:/Econometria__Aplicada/PROYECT")
getwd()

### creamos objeto para utilizarlo después
root <- "E:/Econometria__Aplicada/PROYECT/data"

########## LIBRERIAS ##########

x <- c("readstata13", "readxl", "cowplot", "ggplot2", "stringr", "rgeos", 
       "rgdal", "nloptr" , "sp", "scales", "maptools", "viridisLite", 
       "viridis", "dplyr", "tidyr" , "car" , "raster", "ggmap" , "magrittr" , 
       "gtable" , "MASS" , "RColorBrewer" , "htmltools" , "broom" , "classInt" , 
       "tmap" , "leaflet" , "maps" , "GISTools", "htmlwidgets" , "grid" , 
       "gridExtra" , "foreign" , "choroplethr" , "choroplethrMaps" , "mapview" , 
       "proj4" , "gdalUtils" , "shiny" , "shinydashboard" , "shinySignals" ,
       "gdata" , "readxl" , "plotly")
#install.packages(x)
lapply(x, library, character.only = TRUE)

########## INFORMACION GENERAL SISMOS GENERAL ##########

datos_1 <- read.csv(paste(root, "/BaseDatosOriginal_SSN_sinceros.csv", sep = ""))
datos_2 <- read.csv(paste(root, "/sismos_municipio.csv", sep = ""))
datos_3 <- read.csv(paste(root, "/numero_sismos_year.csv", sep = ""))
datos_4 <- read_excel(paste("data/sismos_municipio_modif2.xlsx"), sheet = "sismos_municipios")
datos_5 <- read_excel(paste("data/sismos_municipio_modif2.xlsx"), sheet = "sismos_estados")
datos_6 <- read_excel(paste("data/sismos_municipio_modif2.xlsx"), sheet = "may5_municipios")
datos_7 <- read_excel(paste("data/sismos_municipio_modif2.xlsx"), sheet = "may5_estados")

########## GRAFICOS ESTADISTICOS SISMOS ##########

accumulate_by <- function(dat, var) {var <- lazyeval::f_eval(var, dat) 
                                     lvls <- plotly:::getLevels(var)
                                     dats <- lapply(seq_along(lvls), function(x) {
                                     cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])})
              dplyr::bind_rows(dats)}
datos_3[order(~year)]
d <- datos_3 %>% accumulate_by(~year)
graph_mex_nsismos <- d %>% plot_ly(x = ~year, y = ~n_sismos , frame = ~frame, type = 'scatter', mode = 'lines' , 
                        fill = 'tozeroy' , fillcolor='#F67280', line = list(color='blue')) %>% 
                        layout(xaxis = list(title = "Años", zeroline = F),
                               yaxis = list (title = "Número de sismos")) %>%
                        animation_opts(frame = 160, transition = 0, redraw = FALSE) %>%
                        animation_slider(hide = T) %>%
                        animation_button(x=1, xanchor = "right", y = 0, yanchor = "bottom")
graph_mex_nsismos
setwd("E:/Econometria__Aplicada/PROYECT/program")
saveWidget(graph_mex_nsismos, file="graph_mex_nsismos.html")
##
m1 <- plot_ly(datos_7, labels = ~Estado, values = ~sismos_may5, type = 'pie',
              textposition = 'inside',
              textinfo = 'label+percent',
              insidetextfont = list(color = '#FFFFFF'),
              hoverinfo = 'text',
              text = ~paste(sismos_may5, 'sismos'), domain = list(x = c(0, 0.4)),
              marker = list(colors = colors,
                            line = list(color = '#FFFFFF', width = 1)), showlegend = FALSE) %>%
  layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

m2 <- plot_ly(datos_6, labels = ~Municipio, values = ~sismos_may5, type = 'pie',
              textposition = 'inside',
              textinfo = 'label+percent',
              insidetextfont = list(color = '#FFFFFF'),
              hoverinfo = 'text',
              text = ~paste(sismos_may5, 'sismos<br>', Estado), domain = list(x = c(0.6, 1)),
              marker = list(colors = colors,
                            line = list(color = '#FFFFFF', width = 1)), showlegend = FALSE) %>%
    layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
graph_mex_estymun_may5 <- subplot(m1,m2)
graph_mex_estymun_may5
setwd("E:/Econometria__Aplicada/PROYECT/program")
saveWidget(graph_mex_estymun_may5 , file="graph_mex_estymun_may5.html")
##
n1 <- plot_ly(datos_5, labels = ~Estado, values = ~sismos_total, type = 'pie',
              textposition = 'inside',
              textinfo = 'label+percent',
              insidetextfont = list(color = '#FFFFFF'),
              hoverinfo = 'text',
              text = ~paste(sismos_total, 'sismos'), domain = list(x = c(0, 0.4)),
              marker = list(colors = colors,
                            line = list(color = '#FFFFFF', width = 1)), showlegend = FALSE) %>%
  layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
n2 <- plot_ly(datos_4, labels = ~Municipios, values = ~total_sismos, type = 'pie',
              textposition = 'inside',
              textinfo = 'label+percent',
              insidetextfont = list(color = '#FFFFFF'),
              hoverinfo = 'text',
              text = ~paste(total_sismos, 'sismos<br>', Estado), domain = list(x = c(0.6, 1)),
              marker = list(colors = colors,
                            line = list(color = '#FFFFFF', width = 1)), showlegend = FALSE) %>%
  layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
graph_mex_estymun_todo <- subplot(n1,n2)
graph_mex_estymun_todo
setwd("E:/Econometria__Aplicada/PROYECT/program")
saveWidget(graph_mex_estymun_todo , file="graph_mex_estymun_todo.html")
