library(tidyverse)
library(dplyr)
library(data.table)
library(sf)
library(lubridate)
library(geojsonsf)
library(leaflet)
library(ggplot2)
library(plotly)


# Descarga de datos de Movilidad ####

descarga_google <- fread("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv")


#Primer filtrado de datos
movilidad <- descarga_google%>%
  filter(country_region_code=='AR')%>% #filtro argentina
  filter(sub_region_1%in%c('Buenos Aires', 'Buenos Aires Province'))%>% #filtra region
  mutate_all(~gsub("Partido", "", .)) #limpia el campo y deja solo el nombre del partido


#filtro de municipios
b <- c("Almirante Brown ", "Berazategui ", "Esteban Echeverría ", "Avellaneda ", "Lanús ",
       "Lomas de Zamora " , "La Matanza ", "Ezeiza ", "Florencio Varela ",	
       "General San Martín ", "Hurlingham ", "Ituzaingó ", "José C. Paz ", 
       "Malvinas Argentinas ", "Merlo ", "Moreno ", "Morón ", "Quilmes ", "San Fernando ",
       "San Isidro ", "San Miguel ", "Tigre ", "Tres de Febrero ", "Vicente López ")


#Arma tabla de Movilidad PBA

movbasas <- movilidad%>%
  filter(sub_region_2 %in% c(b))%>%
  group_by(sub_region_2)

movbasas$sub_region_2 <- substr(movbasas$sub_region_2,1,nchar(movbasas$sub_region_2)-1)

movcaba <- movilidad%>%
  filter(sub_region_1=='Buenos Aires')%>%
  filter(iso_3166_2_code=='AR-C')


#Arma tabla de Movilidad CABA

movcaba$sub_region_2 <-"CABA"



# GBA : union de tablas caba y pba

# importante: al trabajar con caba y  algunos partidos de la pba se dicidi trabajar con dos
# dataframes distintos y despues los combinamos

movamba <- movcaba%>%
  full_join(movbasas)%>%
  rename(nam = sub_region_2)%>%
  select(nam, date, 
         retail_and_recreation_percent_change_from_baseline,
         grocery_and_pharmacy_percent_change_from_baseline,
         parks_percent_change_from_baseline,
         transit_stations_percent_change_from_baseline,
         workplaces_percent_change_from_baseline,
         residential_percent_change_from_baseline)%>%
  rename(nam = nam, 
         fecha = date, 
         ocio = retail_and_recreation_percent_change_from_baseline, #traduce variables de movilidad
         supermercados_farmacias = grocery_and_pharmacy_percent_change_from_baseline,
         parques = parks_percent_change_from_baseline,
         transporte = transit_stations_percent_change_from_baseline,
         areas_trabajo = workplaces_percent_change_from_baseline,
         zonas_residenciales = residential_percent_change_from_baseline)%>%
  mutate(across(c(ocio:zonas_residenciales), as.numeric))%>%#funcion de texto a numero
  mutate(fecha=as.Date(fecha))#formato fecha



# Extraccion de casos ####


act_url <- "https://sisa.msal.gov.ar/datos/descargas/covid-19/files/Covid19Casos.zip"

temp <- tempfile()

download.file(act_url, temp)

unzip(temp, "Covid19Casos.csv")

argentina <- fread("Covid19Casos.csv")


#file.remove("Covid19Casos.csv") -> borra el csv descargado


#la descarga tarda un buen tiempo, paciencia!

argentina <- as_tibble(argentina)


#prefiltro
gral <- argentina%>%
  filter(clasificacion_resumen=='Confirmado')%>%
  rename(provincia=residencia_provincia_nombre,
         fecha=fecha_apertura,
         deptoid=residencia_departamento_id)%>%
  filter(provincia %in% c("CABA", "Buenos Aires"))%>%
  dplyr::select(!c(id_evento_caso, residencia_pais_nombre,
                   carga_provincia_nombre, fecha_inicio_sintomas, fecha_internacion,
                   fecha_cui_intensivo, fecha_fallecimiento, fecha_diagnostico, sepi_apertura,
                   carga_provincia_id, residencia_provincia_id,
                   deptoid, ultima_actualizacion))

# vector de filtro 
d <- c("Almirante Brown", "Berazategui", "Esteban Echeverría", "Avellaneda", "Lanús",
       "Lomas de Zamora" , "La Matanza", "Ezeiza", "Florencio Varela",	
       "General San Martín", "Hurlingham", "Ituzaingó", "José C. Paz", 
       "Malvinas Argentinas", "Merlo", "Moreno", "Morón", "Quilmes", "San Fernando",
       "San Isidro", "San Miguel", "Tigre", "Tres de Febrero", "Vicente López")

# tabla de afectacion covid pba
bacovid <- gral%>%
  filter(provincia=='Buenos Aires')%>%
  filter(residencia_departamento_nombre %in% (d))


# tabla de afectacion covid caba

cabacovid <- gral %>%
  filter(provincia=='CABA')

cabacovid$residencia_departamento_nombre <- "CABA"


#armado de base final datos covid

# importante: al trabajar con caba y  algunos partidos de la pba se dicidi trabajar con dos
# dataframes distintos y despues los combinamos

gbacovid <- cabacovid%>%
  full_join(bacovid)%>%
  rename(nam = residencia_departamento_nombre)


#enriquece base generando data procentual de afectación 

datosargentina <- gbacovid %>% 
  group_by(nam) %>%
  summarise(casos = n(),
            muertos = sum(fallecido=='SI'),
            intensivos = sum(cuidado_intensivo =='SI'),
            asistencia_respi = sum(asistencia_respiratoria_mecanica=='SI'),
            letalidad = muertos/casos * 100,
            tasa_resp = asistencia_respi/casos * 100,
            tasa_int = intensivos/casos * 100)


movamba_acumulado <- movamba%>%
  group_by(nam)%>%
  summarise(promedio_ocio = mean(ocio, na.rm = TRUE), 
            promedio_super = mean(supermercados_farmacias, na.rm = TRUE), 
            promedio_parques = mean(parques, na.rm = TRUE),
            promedio_transporte = mean(transporte, na.rm = TRUE),
            promedio_trabajo = mean(areas_trabajo, na.rm = TRUE), 
            promedio_hogar = mean(zonas_residenciales, na.rm = TRUE))


mapagba <- read_sf('githb/data/mapagba.geojson')


datamapa <- mapagba%>%
  left_join(datosargentina, by='nam')%>%
  mutate(casosmil = 1e5*casos/poblacion,
         muertesmil = 1e5*muertos/poblacion)%>%
  left_join(movamba_acumulado, by='nam')%>%
  mutate_if(is.numeric, round, digits=1)




#grafica datos escalados en faceta de plotly####

master <- readRDS('data/master.rds')

# cuadro con datos de afectación diaria enriquecido, suma los reportes de movilidad y poblacion

datageneral <- gbacovid%>%
  mutate(fecha=as.Date(fecha))%>% #formatea fecha
  group_by(fecha, nam)%>% # agrupa por fecha y nombre de departamento
  summarise(casos = n(), #cuenta los casos
            edad_promedio_casos = mean(edad, na.rm = TRUE), #edad promedio de los afectados
            muertos = sum(fallecido=='SI'), #cuenta los fallecidos
            edad_promedio_muertos = mean(edad[fallecido == 'SI'], na.rm = TRUE), #edad promedio de los fallecidxs
            intensivos = sum(cuidado_intensivo =='SI'), # cuenta casos intensivos
            asistencia_respi = sum(asistencia_respiratoria_mecanica=='SI'), # cuenta casos que requirieron asistencia respiratoria mecanica
            letalidad = muertos/casos * 100)%>% #arma letalidad
  left_join(movamba, by=c('nam', 'fecha'))%>% #suma datos de movilidad
  left_join(master, by='nam')%>% #suma datos de poblacion para ponderar afectacion por 100mil habitantes
  mutate(casosmil = 1e5*casos/poblacion, # casos por 100mil habitantes
         muertesmil = 1e5*muertos/poblacion) %>% # fallecidos por 100mil habitantes
  mutate_if(is.numeric, round, digits=1) #redondea variables numericas


# datos covid y movilidad

data_combinada <- datageneral %>%
  select(nam, fecha, casosmil, edad_promedio_casos,
         zonas_residenciales)%>%
  mutate(casosmil_e = scale(casosmil),
         zonas_residenciales_e = scale(zonas_residenciales),
         edad_promedio_e = scale(edad_promedio_casos))%>%
  mutate_if(is.numeric, round, digits=1)