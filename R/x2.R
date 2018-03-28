#=======================================================================================
# Cargamos los DATOS y los TRANSFORMAMOS
#=======================================================================================
# Cargamos la proyección de PM

p_pgc <- read_excel("INPUT/PGC.xlsx")
colnames(p_pgc) <- c("GEN","CEDI","FLUJO","TAREA",
                     "Semana 1","Semana 2","Semana 3","Semana 4","Semana 5")
#=======================================================================================
# Vamos a ubicar las SEMANAS como VARIABLE en p_pgc
#=======================================================================================
p_pgc <- p_pgc %>% gather("Semana 1","Semana 2","Semana 3","Semana 4","Semana 5",
                          key = "Semana", value = "Unidades" )
#=======================================================================================
# Cargamos el histórico de PCG:
#=======================================================================================
wms_pgc <- read_excel("INPUT/wms_PGC.xlsx")
#=======================================================================================
# Guardamos todas las fechas desde 01/09/2016 hasta 28/02/2018
#=======================================================================================
inicio <- as.Date(min(wms_pgc$FECHA))
fin <- as.Date(max(wms_pgc$FECHA))
all_date <- data.frame(FECHA=seq(inicio, fin, by = "day"))
#=======================================================================================
# Vamos a cruzar todas las fechas con el historico de wms_pgc
#=======================================================================================
llave<-unique(wms_pgc[,c("Dep_Despacha_ID","Flujo_Logistico","Tarea","Agrupacion_ID")])
total<-merge(all_date,llave)
tmp<-unique(wms_pgc[,c("FECHA","Dep_Despacha_ID","Flujo_Logistico","Tarea","Agrupacion_ID","Unidades")])
wms_pgc<-merge(total,tmp
               ,by = c("FECHA","Dep_Despacha_ID","Flujo_Logistico","Tarea","Agrupacion_ID")
               ,all.x = T )
bk<-wms_pgc
wms_pgc$Unidades[is.na(wms_pgc$Unidades)]<-0

table(is.na(wms_pgc$Unidades))
wms_pgc$FECHA<-as.Date(wms_pgc$FECHA)
rm(bk,tmp,inicio,fin,llave,total)

#=======================================================================================
# Cargamos la información del CALENDARIO
#=======================================================================================
dia_ca <- data.frame(read_excel("INPUT/dia_ca.xlsx"))
dia_ca$FECHA<-as.Date(dia_ca$FECHA)

#=======================================================================================
# Vamos a asignarle el dia 1 al lunes y 7 al domingo  
#=======================================================================================
dia_ca$dia_semana <- dia_ca$dia_semana-1
dia_ca$dia_semana = ifelse(dia_ca$dia_semana==0, 7, dia_ca$dia_semana)
#=======================================================================================
# Creamos la variable semana 
#=======================================================================================
semana <- semana.completa(dia_ca$dia_semana)
dia_ca <- cbind(dia_ca,semana)
rm(semana)
#=======================================================================================
# Codificamos las variables categoricas de dia calendario
#=======================================================================================
dia_ca$promocion=as.numeric(factor(dia_ca$promocion,
                                   levels = c("P0","P1","P2","P3"),
                                   labels = c(0,1,2,3)))
dia_ca$temporada=as.numeric(factor(dia_ca$temporada,
                                   levels = c("Festivo","Normal","Prepromoción",
                                              "Promoción"),
                                   labels = c(1,0,3,2)))
#=======================================================================================
# Vamos a agregarle al calendario la variable tipo_dia
# Dia_normal=0, Dia_festivo_lunes=1,Dia_festivo_nolunes=-1
#=======================================================================================
tipo_dia <- rep(0,nrow(dia_ca))
tipo_dia <- ifelse(dia_ca$temporada==1 & dia_ca$dia_semana==1,1,0)
tipo_dia <- ifelse(dia_ca$temporada==1 & dia_ca$dia_semana!=1,-1,tipo_dia)
dia_ca <- cbind(dia_ca,tipo_dia)
rm(tipo_dia)
#=======================================================================================
# Vamos a agregarle al calendario la variable tipo_semana
# Semana_normal=0, Semana_festivo_lunes=1,Semana_festivo_nolunes=-1,semana_santa=-2
#=======================================================================================
sm <- dia_ca %>% group_by(semana) %>% summarise(tipo_semana=sum(tipo_dia))
dia_ca <- merge(dia_ca,sm,by="semana")
rm(sm)
#=======================================================================================
# Vamos a corregir la Semana_mes
#=======================================================================================
dia_ca$semana_mes <- dia_ca$semana_mes+1 # la semana_mes inicia en 1
dia_ca$Semana_mes <- rep(1, nrow(dia_ca))# Inicializamos la nueva Semana_mes

for (i in 2:nrow(dia_ca)){
  dia_ca$Semana_mes[i] <- dia_ca$semana_mes[i-1]
}
# no da porque se pasa de mes, cuando se pase de mes es 1 :)
dia_ca$Semana_mes <- ifelse(dia_ca$dia_mes==1,1,dia_ca$Semana_mes)
# quitamos la semana_mes original pues esta mala y el índice
dia_ca <- dia_ca %>% select(-semana_mes); rm(i)
#=======================================================================================
# Unimos las bases de datos wms_pgc y dia_ca
#=======================================================================================
wms_pgc <- merge(x = wms_pgc,y = dia_ca,by="FECHA", all.x = T)

#=======================================================================================
# Cambiamos los NAS en las Unidades por Ceros 
#=======================================================================================
wms_pgc$Unidades = ifelse(is.na(wms_pgc$Unidades), 0, wms_pgc$Unidades)
sum(is.na(wms_pgc$Unidades))
#=======================================================================================
# Filtramos lo que necesitamos de wms_pgc:
#=======================================================================================
wms_pgc <- wms_pgc %>% select(FECHA,Dep_Despacha_ID,Agrupacion_ID,
                              Flujo_Logistico:Semana_mes)
#=======================================================================================
# Filtramos una de las series: s1 
#=======================================================================================
s1 <- wms_pgc %>% filter(Dep_Despacha_ID==20,Agrupacion_ID=="4001",
                         Flujo_Logistico=="ALMACENAMIENTO", Tarea=="DESPACHOS") %>% 
  select(FECHA,Unidades,semana:tipo_semana)
#=======================================================================================
# Queremos agregar todas las fechas del calendario
# s1 <- merge(s1,dia_ca, all.y = TRUE)
#=======================================================================================
# Queremos cambiar los NA por CEROS:
s1$Unidades = ifelse(is.na(s1$Unidades), 0, s1$Unidades)
#=======================================================================================
# Creamos las variables: suma_unidades y dens_diaria
s1 <- dens.diaria(s1)
#=======================================================================================

