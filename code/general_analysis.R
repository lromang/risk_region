########################################
## Libraries
########################################
## Manejo de JSON
library(jsonlite)
library(rjson)
library(RJSONIO)
## Manejo de URLs
library(RCurl)
## Manejo de arreglos
library(plyr)
library(dplyr)
library(tidyr)
## Manejo de cadenas de caracteres
library(stringr)
## Manejo de data frames
library(data.table)
## Predicción
library(caret)
## Geoespacial
library(geosphere)
library(maps)
library(maptools)
library(spatstat)
library(rgeos)
library(rgdal)
## Gráficas
library(ggplot2)
## Otros
library(ggmap)
library(deldir)
library(rje)
library(sp)
library(SDMTools)
library(PBSmapping)
library(sp)
library(prevR)
library(foreign)

########################################
## functions
########################################

## From Json coords
from_json_coords <- function(json, cols){
    coords <- llply(json[[2]], function(t)t <- {
        aux <- unlist(t$geometry$coordinates)
        matrix(aux,
               ncol = 2,
               nrow = length(aux),
               byrow = TRUE)})
    data_coords <- c()
    for(i in coords){
        data_coords <- rbind(data_coords, i)
    }

    prop    <- llply(json[[2]], function(t)t <- {
        aux <- unlist(t$properties)[c(cols)]
        matrix(aux,
               ncol  = length(cols),
               nrow  = 1)})
    data_prop <- c()
    for(i in 1:length(prop)){
       card       <- nrow(coords[[i]])
       properties <- prop[[i]]
       ## Extraer entidad
       ## state      <- str_split(properties[1], " ")[[1]][2] %>%
       ## str_replace_all("[[:punct:]]", "")
       ## Extraer municipio
       ## mun        <- str_split(properties[1], " ")[[1]][1] %>%
       ## str_replace_all("[[:punct:]]", "")
       ## properties <- properties[-1]
       ## properties <- c(properties, state, mun)
       ## Formar repeticiones
       aux        <- matrix(rep(properties, card),
                           nrow = card,
                           ncol = length(properties),
                           byrow = TRUE)
       data_prop  <- rbind(data_prop, aux)
    }
    data.frame(cbind(data_coords, data_prop))
}

########################################
## Analysis
########################################

## ---------------------------------
## Stage I (Riesgo)
## ---------------------------------
######
###### Inundaciones
######
## Read in data
flods <- RJSONIO::fromJSON("../datos/vulnerability/flods/flods.json")
## process
data_flods <- from_json_coords(flods, c(2,3,5))
names(data_flods) <- c("lon", "lat", "ent", "vul", "area")
## Save results
write.csv(data_flods, "../datos/output_data/data_flods.csv", row.names = FALSE)
######
###### Laderas
######
## Read in data
ladera <- RJSONIO::fromJSON("../datos/vulnerability/laderas/laderas.json")
## process
data_ladera <- from_json_coords(ladera, c(2,3,4))
names(data_ladera) <- c("lon", "lat", "zone","area", "pop")
## Save results
write.csv(data_ladera, "../datos/output_data/data_laderas.csv", row.names = FALSE)
######
###### Tsunamis
######
## Read in data
tsunami <- RJSONIO::fromJSON("../datos/vulnerability/tsunami/tsunami.json")
## process
data_tsunami <- from_json_coords(tsunami,c(5,2))
names(data_tsunami) <- c("lon", "lat", "area", "peligro")
## Save results
write.csv(data_tsunami, "../datos/output_data/data_tsunamis.csv", row.names = FALSE)
######
###### Volcanes
######
## Read in data
volcanes <- RJSONIO::fromJSON("../datos/vulnerability/volcanes/volcanes.json")
## process
data_volcanes <- from_json_coords(volcanes,c(3,9))
names(data_volcanes) <- c("lon", "lat", "categoria", "actividad")
## Save results
write.csv(data_volcanes, "../datos/output_data/data_volcanes.csv", row.names = FALSE)
######
###### Grietas
######
## Read in data
grietas <- RJSONIO::fromJSON("../datos/vulnerability/grietas/grietas.json")
## process
data_grietas <- from_json_coords(grietas,c(2,4))
names(data_grietas) <- c("lon", "lat", "zona", "area")
## Save results
write.csv(data_grietas, "../datos/output_data/data_grietas.csv", row.names = FALSE)

## ---------------------------------
## Stage II (Marginacion)
## ---------------------------------

## ---------------------------------
## Stage III (Medios Masivos)
## ---------------------------------

## ---------------------------------
## Stage IV (México Conectado)
## ---------------------------------

## ---------------------------------
## Stage V (México Conectividad)
## ---------------------------------

## ---------------------------------
## Stage VI (Link)
## ---------------------------------
######
## Read in data
######
data_flods <- read.csv("../datos/output_data/data_flods.csv", stringsAsFactors = FALSE)

## Municipalities.
files <- list.dirs("../datos/shps2015")
files <- files[!str_detect(files, "metadatos")]
files <- files[!str_detect(files, "catalogos")]
files <- files[-1]

## Read in all shapes
municipalities <- list()
for(i in 1:length(files)){
    state    <- str_split(files[i], "/")[[1]][4]
    mun_name <- paste0(state, "_municipio" )
    municipalities[[i]] <- readOGR(files[i],
                                  mun_name)
}

## Censo
censo <- read.csv("../datos/censo_filter.csv",
                 stringsAsFactors = FALSE)

## ----------------------------
## Assoc vulner locs
## ---------------------------
data_vulner <- data_flods
index_param <- 3

## Censo coords
coords              <- censo[,7:8]
names(coords)       <- c("lon", "lat")
coordinates(coords) <- c("lon", "lat")

## For every vulnerability
censo$flod  <- NA

for(j in 1:length(municipalities)){
    municipality <- municipalities[[j]]
    munic        <- unique(municipality$CVE_MUN)
    for(i in munic){
        mun         <- municipality[municipality$CVE_MUN == i,]
        ## Inside censo
        proj4string(coords) <- proj4string(mun)
        inside              <- !is.na(over(coords,
                                          as(mun, "SpatialPolygons")))
        ## Inside vulner
        vulner_coords              <- data_vulner[,1:2]
        coordinates(vulner_coords) <- c("lon", "lat")
        proj4string(vulner_coords) <- proj4string(mun)
        inside_vulner              <- !is.na(over(vulner_coords,
                                               as(mun, "SpatialPolygons")))
        ## Vulner
        vulner <- plyr::count(data_vulner[inside_vulner, index_param])
        vulner <- vulner$x[which(vulner$freq == max(vulner$freq))]
        ## Etiquetar
        censo$flod[inside] <- vulner
    }
}
censo$flod <- factor(censo$flod, levels = unique(censo$flod),
                    labels = c("BAJA",
                               "MEDIA",
                               "ALTA",
                               "ND"))
## write.csv(censo, "../datos/output_data/cap_data.csv", row.names = FALSE)
names(censo) <- toupper(names(censo))

## graficar
mun <- readOGR("../datos/mex_municipios",
              "pais_mexico")
mun@data$id <- rownames(mun@data)
mun.points  <- fortify(mun, region="id")
mun.df      <- join(mun.points, mun@data, by="id")

mun_plot <- ggplot(mun.df)         +
    aes(long, lat, group = group) +
    geom_polygon()                +
    geom_path(color="white")      +
    coord_equal()                 +
    geom_point(data = censo[inside,],
               aes(x     = LONGITUD,
                   y     = LATITUD),
                   col   = "white",
                   group = "1")     +
    theme(panel.background = element_blank())
mun_plot



## ----------------------
## Marg
## ----------------------
