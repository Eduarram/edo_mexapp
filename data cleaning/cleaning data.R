library(jsonlite)
library(tidyverse)
library(sp)
library(maps)
library(geojsonio)
library(sf)

#read the data


gini_data <- fromJSON("Desigualdad-social-segun-GINI-en-Estado-de-Mexico-2020.json")
education_data <- fromJSON("Poblacion-que-asiste-a-la-escuela--Total-Poblacion-entre-3-y-24-anos.json")
poblacion_total <- read.csv("Distribucion-de-la-poblacion-total-segun-municipio--Clic-en-el-grafico-para-seleccionar.csv")
id_edomex <-education_data$`Municipality ID`

# economic census

gini_data <- gini_data[order(gini_data$Municipality),]
education_data <- education_data[order(education_data$Municipality),]
poblacion_total <- poblacion_total[order(poblacion_total$Municipality),]


# read the total income and salarys

censo_economico <- read.csv("inegi_economic_census_additional_2023-11-23T18_02_13.256Z.csv")
economic_census_Emex <- censo_economico %>% filter(Municipality.ID %in% id_edomex)

salary <- read.csv("salario y horas.csv")

salary_edomex <- salary %>% filter(Municipality.ID %in% id_edomex)

salary_edomex <- salary_edomex[order(salary_edomex$Municipality),]

salary_edomex <- salary_edomex %>%  select(!Municipality.ID)

# read the spatial frame

economic_census_Emex <- economic_census_Emex[order(economic_census_Emex$Municipality),]
datos_geojson <- st_read("gadm41_MEX_2.json")


Edo_Mex <- datos_geojson %>% dplyr::select(NAME_1, geometry) %>% 
  dplyr::filter(NAME_1=='México')

#### gini and population ###

Edo_Mex <- cbind(Municipality=gini_data$Municipality, GINI=gini_data$GINI,population=poblacion_total$Population, 
                 education=education_data$Percentage, 
                 economic_census_Emex[,3:4])

Edo_Mex <- dplyr::left_join(Edo_Mex, salary_edomex, by="Municipality")

#### replace NA with mean 
Edo_Mex[,2:8] <- sapply(Edo_Mex[,2:8], as.numeric)

Edo_Mex <- Edo_Mex %>% mutate(across(where(is.numeric), ~replace_na(., mean(., na.rm=TRUE))))

######################write the data####################################

write.csv(Edo_Mex, "data_edomex.csv")










