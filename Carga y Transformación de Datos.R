
#--------------------------------------------------------------------------------------
#	Preparación Ambiente	
#--------------------------------------------------------------------------------------

# Carga de Librerías
	library(gdata)
	library(data.table)
	library(randomForest)
	library(sampling)
	library(stringr)
	library(bit64)
	library(tidyverse)
	library(datos)
	library(dplyr)
	library(lubridate)
	library(ggplot2)
	library(corrplot)

# Fija la ruta de fuentes de información (RF: Ruta Fuente / RS: Ruta Salida)
		RF <- "D:/Anfaga Docs/Educación/05 MasterViu/4 Modulo 11MBIG - Trabajo Final de Máster/Fuentes de Datos"
		RS <- "D:/Anfaga Docs/Educación/05 MasterViu/4 Modulo 11MBIG - Trabajo Final de Máster/Salidas"

#--------------------------------------------------------------------------------------
#	Creacion de Tabla Maestra de Datos - Periodos
#--------------------------------------------------------------------------------------

# Definir mes de inicio y fin del periodo
	fini <- as.Date(paste0(1,"/",3,"/",2020),"%d/%m/%Y")
	ffin <- as.Date(paste0(31,"/",8,"/",2020),"%d/%m/%Y")

	vIni <- 0
	vFin <- as.integer(ffin - fini)
	vRng <- vFin - vIni + 1

# Crea Paramétrica de Periodos
	Periodos <- vector("integer", vRng)
	for (i in seq_along(Periodos)) {
		Periodos[[i]] <- vFin - i + 1
	}
	Periodos <- data.table(Periodos)
	names(Periodos)[1] <- "DiaNum"
	Periodos <- Periodos[order(DiaNum)]
	rm(vRng, i)

# Crea Base Final de Registros BCovid
	BCovid <- Periodos
	BCovid[, fecha_caso:= fini]
	BCovid[, fecha_caso:= fini + DiaNum]
	
#--------------------------------------------------------------------------------------
#	Carga de Datos	
#--------------------------------------------------------------------------------------

# Fijar la ruta de extracción
	setwd(RF)
	
# 1 Base de Número de Casos de Covid
	Covid <- fread("01 Casos_positivos_de_COVID-19_en_Colombia.csv",sep=",",dec=".",header=TRUE, encoding="UTF-8", fill=TRUE)
	
# 2 Base de Pruebas PCR
	PCR <- fread("02 Pruebas_PCR_procesadas_de_COVID-19_en_Colombia__Departamental_.csv",sep=";",dec=".",header=TRUE, encoding="UTF-8", fill=TRUE)
	
# 3 Base de Número de Casos de Covid Trabajadores de la Salud
	CovidSalud <- fread("03 OBS_SaludLaboral_COVID-19_Trabsalud.csv",header=TRUE)
	
# 4 Número de Reproducción Efectivo Rt
	RT <- fread("04 Numero-de-Reproduccion-Efectivo-Rt.csv",sep=";",dec=",",header=TRUE, encoding="UTF-8", fill=TRUE)
	
# 5 Porcentaje de ocupación de Hospitalización general para la atención del COVID-19
	OcuCamas <- fread("05 Ocupacion-Hospitalizacion-COVID-19-.csv",sep=";",dec=",",header=TRUE, encoding="UTF-8", fill=TRUE)
	
# 6 Porcentaje de uso de Unidades Cuidado Intensivo para la atención del COVID-19
	OcuUCI <- fread("06 OSB_EnfTransm-Uso-UCIS-COVID.csv",sep=";",dec=",",header=TRUE, encoding="UTF-8", fill=TRUE)
	
# 7 Disposición de cadáveres por COVID – 19 en Bogotá D.C.
	OcuCremat <- fread("07 OSB_EnfTransm-DisposicionCadaveres.csv",sep=";",dec=",",header=TRUE, encoding="UTF-8", fill=TRUE)
	
# 8 Tempreatura mínima diaria
	TempMin <- fread("08 Tempreatura mínima diaria.csv",sep=";",dec=",",header=TRUE, encoding="UTF-8", fill=TRUE)
	
# 9 Tempreatura máxima diaria
	TempMax <- fread("09 Tempreatura máxima diaria.csv",sep=";",dec=",",header=TRUE, encoding="UTF-8", fill=TRUE)

# 10 Cantidad de Pasajeros en Transporte Masivo Transmilenio
	TransMasv <- fread("10 Informaci_n_Pasajeros_Transporte_Masivo.csv",sep=";",dec=".",header=TRUE, encoding="UTF-8", fill=TRUE)

# 10.1 Auxiliar Pasajeros Promedio para imputación
	Aux_TransM <- fread("10 Pasajeros Promedio.csv",sep=";",dec=",",header=TRUE, encoding="UTF-8", fill=TRUE)

# 11 Vuelos de Carga, Ambulancia y humanitarios
	Vuelos <- fread("11 Informe_de_carga_a_rea.csv",header=TRUE)
	
# 12 % trafico respecto a marzo 2020
	Trafic <- fread("12 %Ttrafico respecto a marzo 2020.csv",sep=",",dec=".",header=TRUE, encoding="UTF-8", fill=TRUE)
	
# 13 Hurtos
	Hurtos <- fread("13 Hurto_20por_20Modalidades_20_20Policia_20Nacional_202.csv",sep=";",dec=".",header=TRUE, encoding="UTF-8", fill=TRUE)
	
# 14 Lesiones Personales y Lesiones en accidente de tránsito
	Lesiones <- fread("14 Lesiones_20Policia_20Nacional.csv",header=TRUE)#,sep=";",dec=".",header=TRUE, encoding="UTF-8", fill=TRUE)

# 15 Tasa de cambio representativa del mercado (TRM)
	TRM <- fread("15 TCM_Para rango de fechas dado.csv",sep=";",dec=",",header=TRUE, encoding="UTF-8", fill=TRUE)

# 16 Tasa de intervención Banco de la República (Tasa de intervención de política monetaria)
	TIP <- fread("16 TIP_Serie histórica diaria IQY.csv",sep=";",dec=",",header=TRUE, encoding="UTF-8", fill=TRUE)
	

#--------------------------------------------------------------------------------------
#	Transformación y Selección de Variables
#--------------------------------------------------------------------------------------

# 1 Base de Número de Casos de Covid
	# Arreglo de nombres
		names(Covid) <- chartr("ÁÉÍÓÚáéíóú", "AEIOUaeiou", names(Covid))
		names(Covid) <- gsub(" ", "_", names(Covid))
		names(Covid) <- gsub(",", "", names(Covid))
		names(Covid) <- tolower(names(Covid))

	# Seleecion datos Bogota y variables
		Covid <- Covid[ciudad_de_ubicacion=="Bogotá D.C.",]
		Covid <- Covid[, c("fecha_de_notificacion","id_de_caso")]

	# Seleccion Periodo de tiempo
		Covid[,fecha:= as.Date(fecha_de_notificacion,"%Y-%m-%d"),]
		Covid[,DiaNum:= as.integer(fecha - fini) -7 ,]
		Covid <- Covid[DiaNum >= vIni & DiaNum <= vFin,]
	
	# Creacion Variable
		Covid <- dcast(Covid, DiaNum ~., length , value.var="id_de_caso")
		names(Covid)[2] <- "Casos"
		a <- merge(Periodos[,1], Covid, by="DiaNum", all.x=TRUE)
		a[is.na(a)==TRUE] <- 0
	
	# Agrega variable a BCovid
		BCovid <- merge(BCovid, a, by="DiaNum", all.x=TRUE)
	
# 2 Base de Pruebas PCR
	# Arreglo de nombres
		names(PCR) <- chartr("ÁÉÍÓÚáéíóú", "AEIOUaeiou", names(PCR))
		names(PCR) <- gsub(" ", "_", names(PCR))
		names(PCR) <- gsub(",", "", names(PCR))
		names(PCR) <- tolower(names(PCR))

	# Seleecion variables
		PCR <- PCR[, c("fecha","bogota")]

	# Seleccion Periodo de tiempo
		PCR[,fecha:= as.Date(fecha,"%Y-%m-%d"),]
		PCR[,DiaNum:= as.integer(fecha - fini),]
		PCR <- PCR[DiaNum >= vIni & DiaNum <= vFin,]
	
	# Creacion Variable
		PCR <- dcast(PCR, DiaNum ~., sum , value.var="bogota")
		names(PCR)[2] <- "Pruebas_PCR"
		a <- merge(Periodos[,1], PCR, by="DiaNum", all.x=TRUE)
		a[is.na(a)==TRUE] <- 0
	
	# Agrega variable a BCovid
		BCovid <- merge(BCovid, a, by="DiaNum", all.x=TRUE)

# 3 Base de Número de Casos de Covid Trabajadores de la Salud
	# Arreglo de nombres
		names(CovidSalud) <- chartr("ÁÉÍÓÚáéíóú", "AEIOUaeiou", names(CovidSalud))
		names(CovidSalud) <- gsub(" ", "_", names(CovidSalud))
		names(CovidSalud) <- gsub(",", "", names(CovidSalud))
		names(CovidSalud) <- tolower(names(CovidSalud))

	# Seleecion variables
		CovidSalud <- CovidSalud[, c("fecha_de_inicio_de_sintomas","fecha_de_diagnostico","caso")]

	# Seleccion Periodo de tiempo
		CovidSalud[,fecha:= ifelse(fecha_de_inicio_de_sintomas == "Asintomático",fecha_de_diagnostico, fecha_de_inicio_de_sintomas)]
		CovidSalud[,fecha:= as.Date(fecha ,"%d/%m/%Y"),]
		CovidSalud[,DiaNum:= as.integer(fecha - fini),]
		CovidSalud <- CovidSalud[DiaNum >= vIni & DiaNum <= vFin,]
	
	# Creacion Variable
		CovidSalud <- dcast(CovidSalud, DiaNum ~., length, value.var="caso")
		names(CovidSalud)[2] <- "Casos_Salud"
		a <- merge(Periodos[,1], CovidSalud, by="DiaNum", all.x=TRUE)
		a[is.na(a)==TRUE] <- 0
	
	# Agrega variable a BCovid
		BCovid <- merge(BCovid, a, by="DiaNum", all.x=TRUE)
		
# 4 Número de Reproducción Efectivo Rt
	# Arreglo de nombres
		names(RT) <- chartr("ÁÉÍÓÚáéíóú", "AEIOUaeiou", names(RT))
		names(RT) <- gsub(" ", "_", names(RT))
		names(RT) <- gsub(",", "", names(RT))
		names(RT) <- tolower(names(RT))

	# Seleecion datos Bogota y variables
		RT <- RT[, c("fecha_inicio_ventana","mean(r)")]

	# Seleccion Periodo de tiempo
		RT[,fecha:= as.Date(fecha_inicio_ventana,"%d/%m/%Y"),]
		RT[,DiaNum:= as.integer(fecha - fini),]
		RT <- RT[DiaNum >= vIni & DiaNum <= vFin,]
	
	# Creacion Variable
		RT <- dcast(RT, DiaNum ~., sum , value.var="mean(r)")
		names(RT)[2] <- "RT"
		a <- merge(Periodos[,1], RT, by="DiaNum", all.x=TRUE)
		b <- mean(a[DiaNum!=0]$RT)
		a[is.na(a)==TRUE] <- b
		rm(b)
		
	# Agrega variable a BCovid
		BCovid <- merge(BCovid, a, by="DiaNum", all.x=TRUE)
		
# 5 Porcentaje de ocupación de Hospitalización general para la atención del COVID-19
	# Arreglo de nombres
		names(OcuCamas) <- chartr("ÁÉÍÓÚáéíóú", "AEIOUaeiou", names(OcuCamas))
		names(OcuCamas) <- gsub(" ", "_", names(OcuCamas))
		names(OcuCamas) <- gsub(",", "", names(OcuCamas))
		names(OcuCamas) <- tolower(names(OcuCamas))

	# Seleecion datos Bogota y variables
		OcuCamas <- OcuCamas[, c("fecha","ocupacion")]

	# Seleccion Periodo de tiempo
		OcuCamas[,fecha:= as.Date(fecha,"%d/%m/%Y"),]
		OcuCamas[,DiaNum:= as.integer(fecha - fini),]
		OcuCamas <- OcuCamas[DiaNum >= vIni & DiaNum <= vFin,]
	
	# Creacion Variable
		OcuCamas <- dcast(OcuCamas, DiaNum ~., sum , value.var="ocupacion")
		names(OcuCamas)[2] <- "Ocupacion_Camas"
		a <- merge(Periodos[,1], OcuCamas, by="DiaNum", all.x=TRUE)
		a[is.na(a)==TRUE] <- 0
	
	# Agrega variable a BCovid
		BCovid <- merge(BCovid, a, by="DiaNum", all.x=TRUE)

# 6 Porcentaje de uso de Unidades Cuidado Intensivo para la atención del COVID-19
	# Arreglo de nombres
		names(OcuUCI) <- chartr("ÁÉÍÓÚáéíóú", "AEIOUaeiou", names(OcuUCI))
		names(OcuUCI) <- gsub(" ", "_", names(OcuUCI))
		names(OcuUCI) <- gsub(",", "", names(OcuUCI))
		names(OcuUCI) <- tolower(names(OcuUCI))

	# Seleecion datos Bogota y variables
		OcuUCI <- OcuUCI[, c("fecha","ocupacion_uci")]

	# Seleccion Periodo de tiempo
		OcuUCI[,fecha:= as.Date(fecha,"%d/%m/%Y"),]
		OcuUCI[,DiaNum:= as.integer(fecha - fini),]
		OcuUCI <- OcuUCI[DiaNum >= vIni & DiaNum <= vFin,]
	
	# Creacion Variable
		OcuUCI <- dcast(OcuUCI, DiaNum ~., sum , value.var="ocupacion_uci")
		names(OcuUCI)[2] <- "Ocupacion_UCI"
		a <- merge(Periodos[,1], OcuUCI, by="DiaNum", all.x=TRUE)
		b <- mean(a[is.na(Ocupacion_UCI)==FALSE]$Ocupacion_UCI)
		a[DiaNum > 40 & is.na(a)==TRUE] <- b
		a[is.na(a)==TRUE] <- 0
		rm(b)
		
	
	# Agrega variable a BCovid
		BCovid <- merge(BCovid, a, by="DiaNum", all.x=TRUE)
		
# 7 Disposición de cadáveres por COVID – 19 en Bogotá D.C.
	# Arreglo de nombres
		names(OcuCremat) <- chartr("ÁÉÍÓÚáéíóú", "AEIOUaeiou", names(OcuCremat))
		names(OcuCremat) <- gsub(" ", "_", names(OcuCremat))
		names(OcuCremat) <- gsub(",", "", names(OcuCremat))
		names(OcuCremat) <- tolower(names(OcuCremat))

	# Seleecion datos Bogota y variables
		OcuCremat <- OcuCremat[, c("fecha","ocupacion_servicios_crematorios")]

	# Seleccion Periodo de tiempo
		OcuCremat[,fecha:= as.Date(fecha,"%d/%m/%Y"),]
		OcuCremat[,DiaNum:= as.integer(fecha - fini),]
		OcuCremat <- OcuCremat[DiaNum >= vIni & DiaNum <= vFin,]
	
	# Creacion Variable
		OcuCremat <- dcast(OcuCremat, DiaNum ~., sum , value.var="ocupacion_servicios_crematorios")
		names(OcuCremat)[2] <- "Ocupacion_Crematorios"
		a <- merge(Periodos[,1], OcuCremat, by="DiaNum", all.x=TRUE)
		#a[is.na(a)==TRUE] <- 0
	
	# Agrega variable a BCovid
		BCovid <- merge(BCovid, a, by="DiaNum", all.x=TRUE)

# 8 Tempreatura mínima diaria
	# Arreglo de nombres
		names(TempMin) <- chartr("ÁÉÍÓÚáéíóú", "AEIOUaeiou", names(TempMin))
		names(TempMin) <- gsub(" ", "_", names(TempMin))
		names(TempMin) <- gsub(",", "", names(TempMin))
		names(TempMin) <- tolower(names(TempMin))

	# Seleecion datos Bogota y variables
		TempMin <- TempMin[, c("fecha","valor")]
	# Seleccion Periodo de tiempo
		TempMin[,fecha:= as.Date(fecha,"%d/%m/%Y"),]
		TempMin[,DiaNum:= as.integer(fecha - fini),]
		TempMin <- TempMin[DiaNum >= vIni & DiaNum <= vFin,]
	
	# Creacion Variable
		TempMin <- dcast(TempMin, DiaNum ~., sum , value.var="valor")
		names(TempMin)[2] <- "Temp_Min"
		a <- merge(Periodos[,1], TempMin, by="DiaNum", all.x=TRUE)
		#a[is.na(a)==TRUE] <- 0
	
	# Agrega variable a BCovid
		BCovid <- merge(BCovid, a, by="DiaNum", all.x=TRUE)

# 9 Tempreatura máxima diaria
	# Arreglo de nombres
		names(TempMax) <- chartr("ÁÉÍÓÚáéíóú", "AEIOUaeiou", names(TempMax))
		names(TempMax) <- gsub(" ", "_", names(TempMax))
		names(TempMax) <- gsub(",", "", names(TempMax))
		names(TempMax) <- tolower(names(TempMax))

	# Seleecion datos Bogota y variables
		TempMax <- TempMax[, c("fecha","valor")]
	
	# Seleccion Periodo de tiempo
		TempMax[,fecha:= as.Date(fecha,"%d/%m/%Y"),]
		TempMax[,DiaNum:= as.integer(fecha - fini),]
		TempMax <- TempMax[DiaNum >= vIni & DiaNum <= vFin,]
	
	# Creacion Variable
		TempMax <- dcast(TempMax, DiaNum ~., sum , value.var="valor")
		names(TempMax)[2] <- "Temp_Max"
		a <- merge(Periodos[,1], TempMax, by="DiaNum", all.x=TRUE)
		#a[is.na(a)==TRUE] <- 0
	
	# Agrega variable a BCovid
		BCovid <- merge(BCovid, a, by="DiaNum", all.x=TRUE)

# 10 Cantidad de Pasajeros en Transporte Masivo Transmilenio
	# Arreglo de nombres
		names(TransMasv) <- chartr("ÁÉÍÓÚáéíóú", "AEIOUaeiou", names(TransMasv))
		names(TransMasv) <- gsub(" ", "_", names(TransMasv))
		names(TransMasv) <- gsub(",", "", names(TransMasv))
		names(TransMasv) <- tolower(names(TransMasv))

	# Seleecion datos Bogota y variables
		TransMasv <- TransMasv[sistema=="TRANSMILENIO/SITP",]
		TransMasv <- TransMasv[, c("fecha","pasajeros/dia")]

	# Seleccion Periodo de tiempo
		TransMasv[,fecha:= as.Date(fecha,"%d/%m/%Y"),]
		TransMasv[,DiaNum:= as.integer(fecha - fini),]
		TransMasv <- TransMasv[DiaNum >= vIni & DiaNum <= vFin,]
	
	# Creacion Variable
		TransMasv <- dcast(TransMasv, DiaNum ~., sum , value.var="pasajeros/dia")
		names(TransMasv)[2] <- "Pasajeros_TM"
		a <- merge(Periodos[,1], TransMasv, by="DiaNum", all.x=TRUE)
		a <- merge(a, Aux_TransM[,c(3,4)], by="DiaNum", all.x=TRUE)
		a[,PromedioPasajeros:= as.integer(PromedioPasajeros)]
		a[, Pasajeros_TM:= ifelse(is.na(Pasajeros_TM)==TRUE, PromedioPasajeros, Pasajeros_TM)]
		a <- a[,-3]
	
	# Agrega variable a BCovid
		BCovid <- merge(BCovid, a, by="DiaNum", all.x=TRUE)

# 11 Vuelos de Carga, Ambulancia y humanitarios
	# Arreglo de nombres
		names(Vuelos) <- chartr("ÁÉÍÓÚáéíóú", "AEIOUaeiou", names(Vuelos))
		names(Vuelos) <- gsub(" ", "_", names(Vuelos))
		names(Vuelos) <- gsub(",", "", names(Vuelos))
		names(Vuelos) <- tolower(names(Vuelos))

	# Seleecion datos Bogota y variables
		Vuelos <- Vuelos[, c("fecha","total_vuelos")]

	# Seleccion Periodo de tiempo
		Vuelos[,fecha:= as.Date(fecha,"%d/%m/%Y"),]
		Vuelos[,DiaNum:= as.integer(fecha - fini),]
		Vuelos <- Vuelos[DiaNum >= vIni & DiaNum <= vFin,]
	
	# Creacion Variable
		Vuelos <- dcast(Vuelos, DiaNum ~., sum , value.var="total_vuelos")
		names(Vuelos)[2] <- "Vuelos"
		a <- merge(Periodos[,1], Vuelos, by="DiaNum", all.x=TRUE)
		a[is.na(a)==TRUE] <- 0
	
	# Agrega variable a BCovid
		BCovid <- merge(BCovid, a, by="DiaNum", all.x=TRUE)

# 12 % trafico respecto a marzo 2020
	# Arreglo de nombres
		names(Trafic) <- chartr("ÁÉÍÓÚáéíóú", "AEIOUaeiou", names(Trafic))
		names(Trafic) <- gsub(" ", "_", names(Trafic))
		names(Trafic) <- gsub(",", "", names(Trafic))
		names(Trafic) <- tolower(names(Trafic))

	# Seleecion datos Bogota y variables
		Trafic <- Trafic[region_name=="Bogotá"]
		Trafic <- Trafic[, c("day","month","tcp")]

	# Seleccion Periodo de tiempo
		Trafic[,fecha:= as.Date(paste0(day,"/",month,"/",2020),"%d/%m/%Y"),]
		Trafic[,DiaNum:= as.integer(fecha - fini),]
		Trafic <- Trafic[DiaNum >= vIni & DiaNum <= vFin,]
	
	# Creacion Variable
		Trafic <- dcast(Trafic, DiaNum ~., sum , value.var="tcp")
		names(Trafic)[2] <- "Trafic"
		a <- merge(Periodos[,1], Trafic, by="DiaNum", all.x=TRUE)
		a[is.na(a)==TRUE] <- 0
	
	# Agrega variable a BCovid
		BCovid <- merge(BCovid, a, by="DiaNum", all.x=TRUE)
		
# 13 Hurtos
	# Arreglo de nombres
		names(Hurtos) <- chartr("ÁÉÍÓÚáéíóú", "AEIOUaeiou", names(Hurtos))
		names(Hurtos) <- gsub(" ", "_", names(Hurtos))
		names(Hurtos) <- gsub(",", "", names(Hurtos))
		names(Hurtos) <- tolower(names(Hurtos))

	# Seleecion datos Bogota y variables
		Hurtos <- Hurtos[, c("fecha_hecho","cantidad")]

	# Seleccion Periodo de tiempo
		Hurtos[,fecha:= as.Date(fecha_hecho,"%d/%m/%Y"),]
		Hurtos[,DiaNum:= as.integer(fecha - fini),]
		Hurtos <- Hurtos[DiaNum >= vIni & DiaNum <= vFin,]
	
	# Creacion Variable
		Hurtos <- dcast(Hurtos, DiaNum ~., sum , value.var="cantidad")
		names(Hurtos)[2] <- "Hurtos"
		a <- merge(Periodos[,1], Hurtos, by="DiaNum", all.x=TRUE)
		#a[is.na(a)==TRUE] <- 0
	
	# Agrega variable a BCovid
		BCovid <- merge(BCovid, a, by="DiaNum", all.x=TRUE)
		
# 14 Lesiones Personales y Lesiones en accidente de tránsito
	# Arreglo de nombres
		names(Lesiones) <- chartr("ÁÉÍÓÚáéíóú", "AEIOUaeiou", names(Lesiones))
		names(Lesiones) <- gsub(" ", "_", names(Lesiones))
		names(Lesiones) <- gsub(",", "", names(Lesiones))
		names(Lesiones) <- tolower(names(Lesiones))

	# Seleecion datos Bogota y variables
		Lesiones <- Lesiones[, c("fecha_hecho","cantidad")]

	# Seleccion Periodo de tiempo
		Lesiones[,fecha:= as.Date(fecha_hecho,"%d/%m/%Y"),]
		Lesiones[,DiaNum:= as.integer(fecha - fini),]
		Lesiones <- Lesiones[DiaNum >= vIni & DiaNum <= vFin,]
	
	# Creacion Variable
		Lesiones <- dcast(Lesiones, DiaNum ~., sum , value.var="cantidad")
		names(Lesiones)[2] <- "Lesiones_Personales"
		a <- merge(Periodos[,1], Lesiones, by="DiaNum", all.x=TRUE)
		#a[is.na(a)==TRUE] <- 0
	
	# Agrega variable a BCovid
		BCovid <- merge(BCovid, a, by="DiaNum", all.x=TRUE)
		
# 15 Tasa de cambio representativa del mercado (TRM)
	# Arreglo de nombres
		names(TRM) <- chartr("ÁÉÍÓÚáéíóú", "AEIOUaeiou", names(TRM))
		names(TRM) <- gsub(" ", "_", names(TRM))
		names(TRM) <- gsub(",", "", names(TRM))
		names(TRM) <- tolower(names(TRM))

	# Seleecion datos Bogota y variables
		TRM <- TRM[, c("fecha","trm")]

	# Seleccion Periodo de tiempo
		TRM[,fecha:= as.Date(fecha,"%d/%m/%Y"),]
		TRM[,DiaNum:= as.integer(fecha - fini),]
		TRM <- TRM[DiaNum >= vIni & DiaNum <= vFin,]
	
	# Creacion Variable
		TRM <- dcast(TRM, DiaNum ~., sum , value.var="trm")
		names(TRM)[2] <- "TRM"
		a <- merge(Periodos[,1], TRM, by="DiaNum", all.x=TRUE)
		#a[is.na(a)==TRUE] <- 0
	
	# Agrega variable a BCovid
		BCovid <- merge(BCovid, a, by="DiaNum", all.x=TRUE)
		
# 16 Tasa de intervención Banco de la República (Tasa de intervención de política monetaria)
	# Arreglo de nombres
		names(TIP) <- chartr("ÁÉÍÓÚáéíóú", "AEIOUaeiou", names(TIP))
		names(TIP) <- gsub(" ", "_", names(TIP))
		names(TIP) <- gsub(",", "", names(TIP))
		names(TIP) <- tolower(names(TIP))

	# Seleecion datos Bogota y variables
		TIP <- TIP[, c("fecha","tip")]

	# Seleccion Periodo de tiempo
		TIP[,fecha:= as.Date(fecha,"%d/%m/%Y"),]
		TIP[,DiaNum:= as.integer(fecha - fini),]
		TIP <- TIP[DiaNum >= vIni & DiaNum <= vFin,]
	
	# Creacion Variable
		TIP <- dcast(TIP, DiaNum ~., sum , value.var="tip")
		names(TIP)[2] <- "TIP"
		a <- merge(Periodos[,1], TIP, by="DiaNum", all.x=TRUE)
		#a[is.na(a)==TRUE] <- 0
	
	# Agrega variable a BCovid
		BCovid <- merge(BCovid, a, by="DiaNum", all.x=TRUE)
		
#--------------------------------------------------------------------------------------
#	Summary
#--------------------------------------------------------------------------------------
	
	BCovid <- BCovid[,-1]
	summary(BCovid)
	
	BCovid_variables <- BCovid[,-2]
	summary(BCovid_variables)

# trsponer 
	BCovid_variables_Transp <- t(BCovid_variables[,2:ncol(BCovid_variables)])
	colnames(BCovid_variables_Transp) <- t(BCovid_variables[,1])
	
	
#--------------------------------------------------------------------------------------
#	Generción de Base Final
#--------------------------------------------------------------------------------------

# Guardar archivo Final
	setwd(RS)
	fwrite(BCovid, "Base_Covid.csv", sep=",")
	fwrite(BCovid_variables, "Base_Covid_Variables.csv", sep=",", dec=".")
	fwrite(BCovid_variables_Transp, "Base_Covid_Variables_T.csv", sep=",", dec=".")
	
	
		
	
#--------------------------------------------------------------------------------------
#	Generción de Base Final
#--------------------------------------------------------------------------------------	
	
	X <- BCovid[,-1]
	Corr <- cor(X, method = "pearson")
	Corr <- round(Corr, digits = 2)