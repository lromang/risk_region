###########
## Luis Manuel Roman Gacia
## Este script contiene las pruebas y procedimientos
## para el cálculo del territorio poblado.
###########
source("./functions.R")
###########


###########Datos
### Datos poblacionales ###
data <- read.csv("../data/dataCenso.csv")
data <- data[,6:8]
### Datos shapefiles
# Rural
data.rur  <- readOGR("./data/locRur", "Localidades rurales_2013")
# Urbano
data.urb  <- readOGR("./data/locUr",  "Localidades urbanas_2013")
### Mapa
map       <- get_map(location = "Mexico", zoom = 5, maptype = "roadmap")
map.plot  <- ggmap(map)
###########


########### Pruebas ##########
### Prueba Metodología 1 ###
## En esta prueba se genera una partición de celdas de
## aproximadamente 25km² y se calcula el total de
## km² que se deben considerar para cubrir al 98% de la
#población.
### Crear partición
grid        <- 100 #2.52e5  # número de celdas
tes         <- tesselate(grid, map.plot, alpha = .05)  # partición
block       <- blocks(tes[[2]],tes[[3]])  # crea las celdas
test_1      <- in.block(block, data)  # características por celda

### Análisis
# Características de las celdas
# población total y población promedio por celda
poptest_1  <- ldply(test_1, function(t)t<- t[[3]])
sum(poptest_1)
mean(poptest_1)
# area total y area promedio por celda
areatest_1 <- ldply(test_1,function(t)t<- t[[4]])
sum(areatest_1)
mean(areatest_1)
# número total de localidades y localidades promedio por celda
loctest_1 <- ldply(test_1, function(t)t <- nrow(t[[2]]))
sum(loctest_1)
mean(loctest_1)
# Area cubierta excluyendo población menor a 2,500
sum(areatest_1[poptest_1>2500])
# Área necesaria para cubrir el 98% de la población
sum(poptest_1[poptest_1>2500])/sum(data$pob)
###########


###########
### Prueba 2
#En esta prueba se utiliza la partición generada en la
#primera prueba y se eliminan las celdas que intersectan
#polígonos urbanos, una vez hecho esto se
#determina la extensión territorial
### Extraer coordenadas de los shapes urbanos
#coords <- coordExtract(data.urb)
#coords.tot                     <- ldply(coords, function(t)t <- t)
#names(coords.tot)       <- c("long", "lat")
#plot(coords.tot)
## Determinar que celdas intersectan los shapes.
#intersects                     <- in.block.fac(block, coords.tot)
#intersects.tot               <- ldply(intersects, function(t)t <- data.frame(intersects = t[[3]] >0 , area = t[[4]]))
#intersects.tot$pop       <- ldply(test, function(t)t <- t[[3]])
#names(intersects.tot) <- c("intersects", "area", "pop")
#areaRur                        <- sum(subset(intersects.tot, select = area,
#                                                                                       intersects == FALSE &
#                                                                                       pop > 0))
#areaRur
###########
# Crear celdas para google maps
# res <- resRelevant(test_1)
# write(JSblock, "./data/js_coords.json")
