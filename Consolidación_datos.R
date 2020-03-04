rm(list=ls())
library(reshape)
library(dplyr)
library(readxl)
library(tidyverse)
library(rlist)
library(extrafont)
library(animation)
library(scales)
library(highcharter)

# Voy a leer fechas en inglÃ©s
Sys.setlocale("LC_TIME", "English")

# Cóigo para consolidar los pronósticos de agencias y ecopetrol en diferentes momentos del tiempo



# ECP ------------------------------------------------------------------
ruta_ecp <- 'D:/Ecopetrol S.A/PPY - Documentos/Proyección de Precios/Precios Worklines/Corto Plazo/Crudo y Refinados/Pronósticos ECP/'
setwd(ruta_ecp)


#Listo los archivos disponibles en la ruta
archivos <- list.files(ruta_ecp)


# Estructura para importar la InformaciÃ³n
ECP <- data.frame(pronostico = as.Date(character()),
                  date = character(),
                  HSFO = numeric(),
                  ULSD = numeric(),
                  Gasolina = numeric(),
                  Brent = numeric() )


# Comprabamos estructura adecuada de la extracción de la fecha del pronóstico
for (i in archivos){
  #i = archivos[1]
  fecha <- trimws(str_extract(i, "[0-9]*-[0-9]*-[0-9]*"))
  #print(fecha)
  print(as.Date(fecha, format = "%d-%m-%Y"))
}



# Filtros de columnas
filtros <- c("ICE BRENT", "ULS DIESEL USGC Pipeline", "UNL 87 USGC Pipeline", "HSFO USGC", "INDICADOR")

# Creamos el data set 

for (i in archivos) {
  prueba <- read_xlsx(i)
  prueba <- rename(prueba, col1 = 1, col2 = 2, col3 = 3)
  prueba <- prueba %>% filter(col2 %in% filtros)
  columna = ncol(prueba) - 8
  prueba <- prueba[,c(2,4:columna)]
  colnames(prueba) <- prueba[1,]
  prueba <- prueba[c(2:5),]
  prueba <- gather(prueba, colnames(prueba)[2]: tail(colnames(prueba), n=2)[2] , key = "date", value = "Valor" )
  prueba <- rename(prueba, Producto = INDICADOR  )
  prueba <- spread(prueba, Producto, Valor)
  colnames(prueba) <- c("date", "HSFO", "Brent", "ULSD", "Gasolina")
  fecha <- trimws(str_extract(i, "[0-9]*-[0-9]*-[0-9]*"))
  pronostico <- as.Date(fecha, format = "%d-%m-%Y")
  prueba$pronostico <- pronostico
  ECP <- rbind(ECP, prueba)
}

ECP <- mutate(ECP, HSFO2 =  `HSFO`  - `Brent`, 
                 ULSD2 = `ULSD`  - `Brent`,
                 Gasolina2 =  `Gasolina`  - `Brent`,
                 Brent2 = `Brent`)

ECP <- ECP %>% select(HSFO2, ULSD2, Gasolina2, Brent2, date, pronostico)

ECP <- ECP %>% rename(HSFO = HSFO2, ULSD = ULSD2, Gasolina =  Gasolina2, Brent = Brent2)

# Platts ------------------------------------------------------------------


# Platts
ruta_plats <- 'D:/Ecopetrol S.A/PPY - Documentos/Proyección³n de Precios/Precios Worklines/Corto Plazo/Crudo y Refinados/PronÃ³sticos PIRA'
#ruta_plats <- "/Users/fabiangarcia/Ecopetrol S.A/PPY - Documents/ProyeccioÌn de Precios/Precios Worklines/Corto Plazo/Crudo y Refinados/PronoÌsticos PIRA"
setwd(ruta_plats)

#Listo los archivos disponibles en la ruta
archivos <- list.files(ruta_plats)

# Filtramos archivos de 2019, primero los de crudo y lo luego los de productos
archivos_2019 <- archivos[str_detect(archivos, "2019|2020")]
archivos_2019_crude <- archivos_2019[str_detect(archivos_2019, "ST Crude")]
archivos_2019_USGC <- archivos_2019[str_detect(archivos_2019, "ST U.S")]

#######################################################
# CRUDO 
#######################################################

# Procesamos el primer archivo de crudos


year <- substr(archivos_2019_crude[1],1,4)
month <- substr(archivos_2019_crude[1],5,6)
day <- substr(archivos_2019_crude[1],7,8)
crudos_platts <- read_excel(archivos_2019_crude[1], skip = 4)
colnames(crudos_platts)[1] = "date"
crudos_platts <- mutate(crudos_platts, pronostico = paste0(year,"-",month,"-",day))
crudos_platts <- select(crudos_platts, -starts_with("..."))
crudos_platts$date <- as.Date(as.numeric(crudos_platts$date), origin = "1899-12-30")
crudos_platts <- crudos_platts %>% filter(!is.na(date))
crudos_platts$pronostico <- as.Date(crudos_platts$pronostico)
crudos_platts <- crudos_platts %>%  mutate_if(is.character,as.numeric)
print(str(crudos_platts))

for (i in 2:length(archivos_2019_crude)) {
  print(archivos_2019_crude[i])
  #i = 11
  year <- substr(archivos_2019_crude[i],1,4)
  month <- substr(archivos_2019_crude[i],5,6)
  day <- substr(archivos_2019_crude[i],7,8)
  temp <- read_excel(archivos_2019_crude[i], skip = 4)
  temp <- temp %>% drop_na("Dated Brent")
  colnames(temp)[1] = "date"
  temp <- mutate(temp, pronostico = paste0(year,"-",month,"-",day))
  temp <- select(temp, -starts_with("..."))
  temp$date <- as.Date(as.numeric(temp$date), origin = "1899-12-30")
  temp <- temp %>% filter(!is.na(date))
  temp$pronostico <- as.Date(temp$pronostico)
  temp <- temp %>%  mutate_if(is.character,as.numeric)
  print(str(temp))
  crudos_platts <- bind_rows(crudos_platts, temp)
}




#######################################################
# Productos 
#######################################################

year <- substr(archivos_2019_USGC[1],1,4)
month <- substr(archivos_2019_USGC[1],5,6)
day <- substr(archivos_2019_USGC[1],7,8)
productos_platts <- read_excel(archivos_2019_USGC[1], skip = 4)
productos_platts <- productos_platts %>% drop_na("USG ULS Diesel")
colnames(productos_platts)[1] = "date"
productos_platts <- rename(productos_platts, date = "...1")
productos_platts <- mutate(productos_platts, pronostico = paste0(year,"-",month,"-",day))
productos_platts <- select(productos_platts, -starts_with("..."))
productos_platts$date <- as.Date(as.numeric(productos_platts$date), origin = "1899-12-30")
productos_platts$pronostico <- as.Date(productos_platts$pronostico)
productos_platts <- productos_platts %>%  mutate_if(is.character,as.numeric)
productos_platts$`USG Unl 93 Gasoline` <- as.numeric( productos_platts$`USG Unl 93 Gasoline`)



for (i in 2:length(archivos_2019_USGC)) {
  print(archivos_2019_USGC[i])
  #i = 2
  year <- substr(archivos_2019_USGC[i],1,4)
  month <- substr(archivos_2019_USGC[i],5,6)
  day <- substr(archivos_2019_USGC[i],7,8)
  temp <- read_excel(archivos_2019_USGC[i], skip = 4)
  temp <- temp %>% drop_na("USG ULS Diesel")
  colnames(temp)[1] = "date"
  temp <- mutate(temp, pronostico = paste0(year,"-",month,"-",day))
  temp <- select(temp, -starts_with("..."))
  temp$date <- as.Date(as.numeric(temp$date), origin = "1899-12-30")
  temp$pronostico <- as.Date(temp$pronostico)
  temp <- temp %>%  mutate_if(is.character,as.numeric)
  print(colnames(temp)) 
  productos_platts <- bind_rows(productos_platts, temp)
}



# le pegamos el pronÃ³stico del Brent a la base de productos

productos_platts <- crudos_platts %>% select("Dated Brent", "pronostico", "date") %>%  inner_join(productos_platts, by = c("date","pronostico"))
#productos_platts <- gather(productos_platts, Producto, Valor, "Dated Brent",  "USG CBOB 87":"USGC Residual FO 3.5% S ($/MT)" )
#write.csv(productos_platts[,c(3,4)], file = "productos_platt.csv")

# Creamos los cracks de los principales productos

platts <- mutate(productos_platts, HSFO =  `USG No 6 Fuel Oil 3.0% S`  - `Dated Brent`, 
                 ULSD = `USG ULS Diesel`  - `Dated Brent`,
                 Gasolina =  `USG Unl 87 Gasoline`  - `Dated Brent`,
                 Brent = `Dated Brent`)

platts <- platts %>% select(pronostico, date, "HSFO":"Brent")

#platts <- platts %>% select(pronostico, date, "HSFO":"Brent")  %>%  gather( Producto, Valor, "HSFO":"Brent" )

save(platts, file = "D:/Ecopetrol S.A/PPY - Documentos/Informes/App/data/platts.Rdata")




# IHS ---------------------------------------------------------------------

#ruta_ihs <- "/Users/fabiangarcia/Ecopetrol S.A/PPY - Documents/ProyeccioÌn de Precios/Precios Worklines/Corto Plazo/Crudo y Refinados/PronoÌsticos IHS"
ruta_ihs <- 'D:/Ecopetrol S.A/PPY - Documentos/Proyección de Precios/Precios Worklines/Corto Plazo/Crudo y Refinados/Pronósticos IHS/'

setwd(ruta_ihs)

#Listo los archivos disponibles en la ruta
archivos <- list.files(ruta_ihs)


# Solo vamos a procesar 2018, y 2019 y enero de 2018 que no tiene el LLS
archivos <- archivos[c(grep("2018", archivos), grep("2019", archivos), grep("2020", archivos)) ]

# Eliminamos January de 2018 porque no tiene el LLS que no nos permite rescatar el crack del producto con el brent.
archivos <- archivos[-6]

# Filtros de columnas
filtros <- c("Dated Brent, FOB North Sea", "Louisiana Light Sweet (LLS), St James", "West Texas Intermediate (WTI), Cushing",
             "Maya, FOB Mexico", "Dubai, FOB", "Regular unleaded gasoline", "Naphtha", "Ultra-low sulfur diesel (ULSD)",
             "No. 2 heating oil", "No. 6 residual fuel oil 1% sulfur", "No. 6 residual fuel oil 3% sulfur", "Jet Fuel")

meses <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")



# IHS
IHS <- data.frame(pronostico = as.Date(character()),
                 date = character(),
                 HSFO = numeric(),
                 ULSD = numeric(),
                 Gasolina = numeric(),
                 Brent = numeric() )

# PatrÃ³n para extraer el texto despuÃ©s del dash hasta el nÃºmero
for (i in archivos){
  #print(str_extract(i, "[^-]*[0-9]"))
  # Quito el primer espacio
  #i = "WM Short-term pricing Ecopetrol Jul19.xls"
  fecha <- trimws(str_extract(i, "[^-]*[0-9]"))
  #print(fecha)
  mes <- word(fecha,1)
  aÃ±o <- word(fecha,2)
  aÃ±o <- substr(aÃ±o,1,4)
  #print(mes)
  #print(aÃ±o)
  fecha <- paste(aÃ±o,mes,"28")
  print(fecha)
  print(as.Date(fecha, format = "%Y %B %d"))
}

#  Prueba

 for (i in archivos){
   #i = archivos[1]
   test <- read_excel(i, sheet = 2)
   test <- rename(test, col1 = 1, col2 = 2, col3 = 3)
   print(i)
   test <- test  %>% filter((col1 %in% filtros) | (col2 %in% filtros) | col2 %in% c("Unit") | col3 %in% c("Unit") ) 
   colnames(test) <- test[1,]
   test <- test[-1,]
   test <- test[,-which(colnames(test)=="NA")]
   
   if (colnames(test)[3] == "Unit") {
     test <- test[,-1] 
   }
   colnames(test)[1] <- "Producto"
   test <- test  %>% filter(Unit %in% c("$/bbl"))
   print(dim(test))
   
   # Extraigo fecha del momento donde se hizo el pronÃ³stico
   fecha <- trimws(str_extract(i,"[^-]*[0-9]"))
   mes <- word(fecha,1)
   aÃ±o <- word(fecha,2)
   aÃ±o <- substr(aÃ±o,1,4)
   fecha <- paste(aÃ±o,mes,"28")
   fecha <- as.Date(fecha, format = "%Y %B %d")
   test <- mutate(test, pronostico = fecha)
  
   
   # Quitamos el $/bbl columna
   test <- select(test,-Unit)
   test2 <- gather(test, colnames(test)[2]: tail(colnames(test), n=2)[1] , key = "date", value = "Valor" )
   test3 <- spread(test2, Producto, Valor)
   test3 <- mutate(test3, `Louisiana Light Sweet (LLS), St James` = as.numeric(`Louisiana Light Sweet (LLS), St James`),
                   `No. 6 residual fuel oil 3% sulfur` = as.numeric(`No. 6 residual fuel oil 3% sulfur`),
                   `Dated Brent, FOB North Sea` = as.numeric(`Dated Brent, FOB North Sea`),
                   `Regular unleaded gasoline` = as.numeric(`Regular unleaded gasoline`),
                   `Ultra-low sulfur diesel (ULSD)` = as.numeric(`Ultra-low sulfur diesel (ULSD)`))
   
   test3 <- mutate(test3, HSFO = test3$`Louisiana Light Sweet (LLS), St James`+ test3$`No. 6 residual fuel oil 3% sulfur` - test3$`Dated Brent, FOB North Sea`,
                   ULSD = test3$`Ultra-low sulfur diesel (ULSD)` + test3$`Louisiana Light Sweet (LLS), St James`- test3$`Dated Brent, FOB North Sea`,
                   Gasolina = test3$`Regular unleaded gasoline` + test3$`Louisiana Light Sweet (LLS), St James` - test3$`Dated Brent, FOB North Sea`,
                   Brent = test3$`Dated Brent, FOB North Sea`)
   test3 <- select(test3, pronostico, date, HSFO:Brent)
   IHS <- rbind(IHS, test3)

 }

save(IHS, file = "D:/Ecopetrol S.A/PPY - Documentos/Informes/App/data/IHS.Rdata")


# Los pronÃ³sticos de 2020 estÃ¡n desde oct de 2018
archivos[grep("Dec.*2017", archivos)]
archivos[grep("Jun.*2017", archivos)]
archivos[grep("Jul.*2017", archivos)]


# Woodmac -----------------------------------------------------------------


ruta_wm <- 'D:/Ecopetrol S.A/PPY - Documentos/ProyecciÃ³n de Precios/Precios Worklines/Corto Plazo/Crudo y Refinados/PronÃ³sticos WM/'
setwd(ruta_wm)
#Listo los archivos disponibles en la ruta
archivos <- list.files(ruta_wm)


brent <- data.frame(fecha = character(),
                       Brent = character(),
                    pronostico = as.Date(character()))

# Extraemos los precios del Brent
for (i in archivos){
  #i = "Short-term pricing Ecopetrol Dec19.xlsx"
  # Extraemos la fecha del pronÃ³stico
  print(i)
  #i = "Short-term pricing Ecopetrol Dec19.xlsx"
  #i = "WM Short-term pricing Ecopetrol May19.xls"
  aÃ±o <- substr(i, nchar(i)-5, nchar(i)-4)
  aÃ±o <- paste0(20,aÃ±o)
  mes <- substr(i, nchar(i)-8, nchar(i)-6)
  pronostico <- as.Date(paste0(aÃ±o,mes,"01"), format = "%Y%b%d")
  print(pronostico)
  if (i == "Short-term pricing Ecopetrol Dec19.xlsx") {
    aÃ±o <- substr(i, nchar(i)-6, nchar(i)-5)
    aÃ±o <- paste0(20,aÃ±o)
    mes <- substr(i, nchar(i)-9, nchar(i)-7)
    pronostico <- as.Date(paste0(aÃ±o,mes,"04"), format = "%Y%b%d")
    
  }
  

  # Extraemos el precio del Brent
  data <- read_excel(i, sheet = "Crude and gas prices")
  Brent <- data[which(data[,2] == "Dated Brent"), c(2:ncol(data))]
  colnames(Brent) <- data[which(data[,2] == "$/bbl")[1], c(2:ncol(data)) ]
  Brent <- as.data.frame(t(Brent))
  Brent <- rownames_to_column(Brent)
  colnames(Brent) <- c("date", "Brent")
  Brent <- Brent[-1,]
  Brent <- mutate(Brent, pronostico = pronostico)
  brent <- rbind(brent,Brent)
  
}

# Extraemos los cracks de los productos

productos <- data.frame(date=character(),
                 Gasolina=character(), 
                 ULSD=character(),
                 HSFO=character(),
                 pronostico = as.Date(character()),
                 stringsAsFactors=FALSE) 

for (i in archivos){
  # Extraemos la fecha del pronÃ³stico
  #i = "Short-term pricing Ecopetrol Feb20.xls"
  aÃ±o <- substr(i, nchar(i)-5, nchar(i)-4)
  aÃ±o <- paste0(20,aÃ±o)
  mes <- substr(i, nchar(i)-8, nchar(i)-6)
  pronostico <- as.Date(paste0(aÃ±o,mes,"01"), format = "%Y%b%d")
  #print(pronostico)
  
  if (i == "Short-term pricing Ecopetrol Dec19.xlsx") {
    aÃ±o <- substr(i, nchar(i)-6, nchar(i)-5)
    aÃ±o <- paste0(20,aÃ±o)
    mes <- substr(i, nchar(i)-9, nchar(i)-7)
    pronostico <- as.Date(paste0(aÃ±o,mes,"04"), format = "%Y%b%d")
    
  }
  
  
  # Extraemos el precio del Brent
  print(i)
  #print(excel_sheets(i))
  data <- read_excel(i, sheet = "Product prices")
  Gasolina <- data[which(data[,2] == "Gasoline conv87")[3], c(2:ncol(data))]
  ULSD <- data[which(data[,2] == "Diesel ULSD")[3], c(2:ncol(data))]
  HSFO <- data[which(data[,2] == "Fuel oil No6 3.0%S")[3], c(2:ncol(data))]
  fecha <- data[which(data[,2] == "$/bbl")[1], c(2:ncol(data))]
  fecha <- trimws(fecha)
  data <- rbind(Gasolina, ULSD, HSFO)
  colnames(data) <- fecha
  
  # Pilas con las columnas repetidas
  #  tibble::enframe(names(data)) %>% count(value) %>% filter(n > 1)
  
  test2 <- gather(data, colnames(data)[2]: tail(colnames(data), n=1) , key = "date", value = "Valor" )
  colnames(test2)[1] <- c("Producto")
  test3 <- spread(test2, Producto, Valor)
  data <- test3
  colnames(data) <- c("date", "ULSD", "HSFO", "Gasolina")
  data <- mutate(data, pronostico = pronostico)
  print(data[1,])
  productos <- rbind(productos,data)
}


WM <- full_join(brent, productos, by = c("date" = "date", "pronostico" = "pronostico"))
WM$Brent <- as.numeric(as.character(WM$Brent))
WM$Gasolina <-as.numeric(as.character(WM$Gasolina))
WM$ULSD <-as.numeric(as.character(WM$ULSD))
WM$HSFO <-as.numeric(as.character(WM$HSFO))





# ConsolidaciÃ³n de Agencias -----------------------------------------------

# WM
# Me quedo con las fechas que tienen 5 caracteres (pronÃ³sticos mensuales)

# Filtro meses
WM2 <- WM %>% filter(nchar(WM$date)==5)

# Formato fechas
WM2$date <- as.Date(as.numeric(WM2$date), origin = "1899-12-30")

# Fitro lo que voy a mostrar. De ene de 2020 en adelante
#WM2 <- WM2 %>% filter(WM2$date > as.Date("2019-12-31") )

# Solo los pronÃ³sticos de 2020 
#WM2 <- WM2 %>% filter(WM2$pronostico > as.Date("2018-12-31") )


save(WM2, file = "D:/Ecopetrol S.A/PPY - Documentos/Informes/App/data/WM.Rdata")




#hchart(WM2, "line", hcaes(x = date, y = ULSD, group = pronostico))
#hchart(WM2, "line", hcaes(x = date, y = HSFO, group = pronostico))
#hchart(WM2, "line", hcaes(x = date, y = Gasolina, group = pronostico))
#hchart(WM2, "line", hcaes(x = date, y = Brent, group = pronostico))




# Platts
# Me quedo con las fechas que tienen 5 caracteres (pronÃ³sticos mensuales)

# Filtro meses
platts2 <- platts %>% filter(nchar(platts$date)==5)


# Fitro lo que voy a mostrar. De ene de 2020 en adelante
#platts2 <- platts2 %>% filter(platts2$date > as.Date("2019-12-31") )

# Solo los pronÃ³sticos de 2020 
#platts2 <- platts2 %>% filter(substr(platts2$pronostico,start = 1, stop = 4) == "2019")


#hchart(platts2, "line", hcaes(x = date, y = ULSD, group = pronostico))
#hchart(platts2, "line", hcaes(x = date, y = HSFO, group = pronostico))
#hchart(platts2, "line", hcaes(x = date, y = Gasolina, group = pronostico))
#hchart(platts2, "line", hcaes(x = date, y = Brent, group = pronostico))




# IHS
# Filtro meses
IHS2 <- IHS %>% filter(nchar(IHS$date)==5)

# Formato fechas
IHS2$date <- as.Date(as.numeric(IHS2$date), origin = "1899-12-30")

# Fitro lo que voy a mostrar. De ene de 2020 en adelante
#IHS2 <- IHS2 %>% filter(IHS2$date > as.Date("2019-12-31") )

# Solo los pronÃ³sticos de 2020 
#IHS2 <- IHS2 %>% filter(substr(IHS2$pronostico,start = 1, stop = 4) == "2019")


#hchart(IHS2, "line", hcaes(x = date, y = ULSD, group = pronostico))
#hchart(IHS2, "line", hcaes(x = date, y = HSFO, group = pronostico))
#hchart(IHS2, "line", hcaes(x = date, y = Gasolina, group = pronostico))
#hchart(IHS2, "line", hcaes(x = date, y = Brent, group = pronostico))




# Data frames que voy a unir. WM2. IHS2, platts2
WM2 <- WM2  %>% mutate("Agencia" = "WM")
IHS2 <- IHS2  %>% mutate("Agencia" = "IHS")
platts2 <- platts2  %>% mutate("Agencia" = "Platts")
ECP2 <- ECP  %>% mutate("Agencia" = "ECP")
ECP2$date <- as.Date(as.numeric(ECP2$date), origin = "1899-12-30")
agencias <- rbind(WM2, IHS2,platts2, ECP2)


save(agencias, file = "D:/Ecopetrol S.A/PPY - Documentos/Informes/App/data/agencias.Rdata")

table(ECP2$pronostico)
# Fechas del Ãºltimo pronÃ³stico
# WM 2020-01-01 
# Platts 2020-01-13
# IHS 2019-12-01 












# Vamos a hacer el GIF con crack del HSFO
HSFO <- mutate(productos_platts, HSFO_C  = `USG No 6 Fuel Oil 3.0% S`  - `Dated Brent` )
temp <- filter(HSFO, pronostico == as.Date("2019-02-01") & date > as.Date("2019-12-31"))


# Formato de los ejes
x_labels <- format(temp$date, "%b")
y_labels <- seq(-35,0,5)

# FunciÃ³n para probar y ver quÃ©.
plotPrecios <- function(x, y, main = "", showlines = FALSE, add = FALSE) {
  if (!add) {
    plot(x, y, ylim = c(-35,0), type="n", bty="n", las=1, main=main, 
         xlab="", ylab="", axes = FALSE)
    axis(1, at = x, labels = x_labels, tck=0, lwd=0.2, padj=-1.2, pos=0)
    axis(2, tck = 0, lwd = 0.2)
    #abline(NULL, NULL, lty=1, col="black", lwd=.1, h=NULL, v=x)
    
  }
  if (showlines) {
    ## Draw lines. ##
    lines(x, y, lwd=2)
    #lines(x1, y2, col=col2, lwd=2)
  }
}




iterador <- unique(HSFO$pronostico)

for (i in 2:length(iterador)) {
  # La primera observaciÃ³n no tiene info de 2020
  print(iterador[i])
  temp <- filter(HSFO, pronostico == (iterador[i]) & date > as.Date("2019-12-31"))
  plot(temp$date, temp$HSFO_C, type = "l", main =iterador[i])
}





saveGIF({
  par(las=1, mar=c(2.5,3.5,1,0))
  add <- FALSE
  for (i in 2:length(iterador)) {
    
    if (i > 2) {
      # Point of reference
      temp <- filter(HSFO, pronostico == (iterador[2]) & date > as.Date("2019-12-31"))
      plotPrecios(temp$date, temp$HSFO_C, main="", add=FALSE, showlines=TRUE)
      add <- TRUE
    }
    
    temp <- filter(HSFO, pronostico == (iterador[i]) & date > as.Date("2019-12-31"))
    plotPrecios(temp$date, temp$HSFO_C, main="", add=add, showlines=TRUE)
    
    # Anotamos la fecha del pronÃ³stico
    text(as.Date("2020-02-01"), -10, iterador[i], cex=2.4, pos=4)
    
  }
  
}, movie.name="vamos2.gif", interval=.3, ani.width=670, ani.height=460)



