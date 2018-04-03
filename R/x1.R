#=======================================================================================
# cargamos las LIBRERIAS y las FUNCIONES 
#=======================================================================================
#setwd("~/Proyecto_mum_v1/R")
#source("librerias.R")
library(readxl)
library(RODBC)
library(dplyr)
library(tidyr)
library(cluster)
library(NbClust)
library(data.table)
#=======================================================================================
# Encuentra la posición del primer dia uno 
#
#=======================================================================================
encuentra.uno <- function(v){
  i <- 1 # Inicializamos el contador
  while(v[i]!=1){
    i <- i+1
  }
  return(i)
}
#=======================================================================================
## Encuentra la posición del ultimo dia siete ##
#=======================================================================================
encuentra.usiete <- function(v){
  j <- length(v) # Inicializamos el contador
  while(v[j]!=7){
    j <- j-1
  }
  return(j)
}
#=======================================================================================
## Vamos a crear la variable semana completa ##
#=======================================================================================
semana.completa <- function(v){
  x <- encuentra.uno(v)    # Encontramos el 1er uno 
  y <- encuentra.usiete(v) # Encontramos el ultimo siete
  x1 <- v[x:y]             # Recortamos la variable a las semanas completas
  z0 <- 0                  # Inicializamos  z0
  f <- (y-(x-1))/7         # Encontramos el nro de semanas completas
  for (i in 1:f) {
    zi <- rep(i,7)
    z0 <- c(z0, zi)
  }
  return(c(rep(0,(x-1)),z0[-1],rep(f+1,length(v)-y)))
}
#=======================================================================================
## Creación suma semanal y densiadad semanal##
#=======================================================================================
dens.diaria <- function(serie){
  suma.semana <- serie %>% group_by(semana) %>% summarise(suma_unidades=sum(Unidades))
  serie <- merge(x=serie,y=suma.semana) %>%
    arrange((FECHA)) %>% 
    mutate(dens_diaria=(Unidades/suma_unidades))
  rm(suma.semana)
  #serie <- serie %>% select(-suma_unidades)
  return(serie)
}
#=======================================================================================
# Creación de la funcion reordenamiento
#=======================================================================================
reordenamiento <- function(serie){
  dia.densidad <- serie %>% select(semana,dia_semana,dens_diaria)
  # Queremos que los nombres de las variables de día:
  dia <- c("lunes","martes","miercoles","jueves","viernes","sabado","domingo")
  dia_semana <- c(1:7)
  dia.codigo <- as.data.frame(cbind(dia,dia_semana))
  # Ahora lo unimos con la serie:
  dia.semana2 <- merge(dia.densidad,dia.codigo, by="dia_semana") %>%
    arrange(semana) %>% select(semana,dia,dens_diaria)
  # Vamos ahora a usar los días como variables:
  dia.densidad2 <- unique(dia.semana2) %>% 
    spread(key=dia, value = dens_diaria) %>% 
    select(semana,lunes:miercoles,jueves,viernes,sabado,domingo) %>% 
    filter(semana != 0)
  # Guardamos nuestro resultado
  serie <- dia.densidad2 
  rm(dia.densidad,dia,dia_semana, dia.codigo)
  return(serie)
}
#=======================================================================================
# Cargamos la función numerog
#=======================================================================================
numerog <- function(datos){
  sumas.entre <- function(datos){
    set.seed(6)
    wcss = vector()
    for (i in 1:5) {
      wcss[i] = sum(kmeans(datos,i)$withinss)
    }
    return(wcss)
  }
  
  wcss <- sumas.entre(datos)
  
  
  diferencias <- function(sumas){
    diferencia = vector()
    for (i in 1:length(sumas)-1){
      diferencia[i]=(sumas[i]-sumas[i+1])/sumas[1]
    }
    return(diferencia)
  }
  
  
  numero.grupos <- function(diferencias){
    numero <- 0
    i <- 1
    while (diferencias[i] > 0.08) {
      numero=numero+1
      i=i+1
    }
    return(numero+1)
  }
  
  numerog <- numero.grupos(diferencias = diferencias(sumas = wcss))
  return(numerog)
}
#=======================================================================================
# Cargamos la funcion td() para CONSULTA SQL
#=======================================================================================
#td <- function (server = 'SQL Server', uid = 'tclondono', pwd = 'tclondono01',
#                query ){
#  char <- paste("Driver=Teradata;DBCName=", server, ";UID=", uid, "PWD=", pwd);
#  #ch <- odbcDriverConnect(char);    # Crea conexión
#  ch <- odbcConnect(dsn= server, uid = uid, pwd = pwd)
#  data <- sqlQuery(ch, query);      # Ejecuta consulta
#  odbcClose(ch);                    # Cierra conexión
#  rm(char, ch);                     # Remueve variables
#  return(data);                     # Devuelve resultados de la consulta
#}
#query <- paste(readLines("D:/3.Scripts R/Piloto_PGC_FRESCOS/prueba_consulta_SQL.txt"),
#               collapse = " ",warn=FALSE) # el script sin ; y con un enter al final
#query <- gsub("__VAR1__", "20, 85, 146", query) # para cambiar cosas en el query
#data <- as_tibble(td(query=query)) # para que le guarde otros atributos
#=======================================================================================
## Creación densidad comercial ##
#=======================================================================================
dens.comercial <- function(dc){
  # Calculamos la suma de unidades movilizadas en la semana:
  suma.semana <- dc %>% group_by(semana) %>% summarise(suma_unidades=sum(S_Unidades))
  # Unimos la suma de unidades y calculamos la densidad por agrupación comercial:
  dc <- merge(x=dc,y=suma.semana) %>%
    mutate(dens_comercial=(S_Unidades/suma_unidades))
  return(dc)
}
#=======================================================================================
# Cargamos la funcion CENTROS para calcular el promedio de la densidad comercial
#=======================================================================================
centros <- function(filtro_historico){
  # Agrupamos y sumamos las unidades en cada agrupación:
  filtro_historico <- filtro_historico %>% group_by(semana,Agrupacion_ID) %>%
    summarise(S_Unidades=sum(Unidades)) 
  # Calculamos las densidades comerciales:
  filtro_historico <- dens.comercial(filtro_historico)
  # Quitamos las semanas que no movilizaron unidades
  sem <- unique(filtro_historico$semana)
    for (i in sem){
      if (sum(filter(filtro_historico,semana==i)$suma_unidades)==0){
      
      filtro_historico <- filtro_historico %>% filter(semana != i)
    } 
  }
  #
  dc2 <- filtro_historico %>% select(Agrupacion_ID,dens_comercial,semana)
  # Ubicamos las densidades de secos y frescos en dos columnas
  dc2 <- spread(dc2,key=Agrupacion_ID, value = dens_comercial)
  # Creamos los nombres de las columnas y llenamos con ceros en caso de que no existan 
  if(sum(names(dc2)=="4001")==0){dc2$"4001"<-0}
  if(sum(names(dc2)=="4002")==0){dc2$"4002"<-0}
  # Borramos los NAS EN CASO DE QUE EXISTAN
  dc2$"4001" = ifelse(is.na(dc2$"4001"), 0, dc2$"4001")
  dc2$"4002" = ifelse(is.na(dc2$"4001"), 0, dc2$"4002")
  #=====================================================================================
  # Vamos a calcular el promedio con kmedias n=1
  #=====================================================================================
  kmeans = kmeans(x = dc2[2:3], centers = 1, nstart=25) 
  #=====================================================================================
  # Guardamos el centros
  centro <- as.data.frame(kmeans$centers)
  #=====================================================================================
  return(centro)
}
#=======================================================================================
# Esta función calcula la desagregacion comercial basado en el centros
#=======================================================================================
desag.co <- function(filtro_proyeccion,centros){
  desag_co <- filtro_proyeccion %>%
    mutate(frescos=Unidades*centros[1,1],
           secos=Unidades*centros[1,2]) 
  return(desag_co)
}
#=======================================================================================


#=======================================================================================
# FUNCIONES DE LA DESAGREGACION TEMPORAL
#=======================================================================================
cruzar.fechas <- function(consolidado_cedis,mes){
  conso_ce <- consolidado_cedis %>% separate(Semana, into = c("S","semana"), sep = " ")
  # Vamos a ubicar en una columna los agrupamientos
  conso_ce <- conso_ce %>% gather(frescos,secos,key="AGRUPAMIENTO",value="UNIDADES" )
  # Vamos a ordenar las variables para que los primeros campos conformen una serie
  conso_ce <- conso_ce %>% select(-S,-Unidades,-GEN,CEDI,FLUJO,AGRUPAMIENTO,TAREA,
                                  semana,UNIDADES) %>% arrange(CEDI,semana)
  # CRUZAR FECHAS DEL MES
  conso_ce$semana <- as.numeric(conso_ce$semana) # Pasamos semana a numerica
  cruze <- inner_join(x=conso_ce,y=Marzo[1:2], by="semana")
  # Vamos a CODIFICAR EL TIPO DE MERCANCIA
  cruze$AGRUPAMIENTO <- ifelse(cruze$AGRUPAMIENTO=="frescos",4001,4002)
  return(cruze)
} 

#=======================================================================================
densidad.semana <- function(filtro_hi_m, dia_codigo, m){
  dias_semana <- f_hi_m %>% group_by(semana) %>% summarise(n=n())
  # filtramos los dias que coinciden con los dias de la semana
  dias_semana <- dias_semana %>% filter(n==m)
  # Ahora debemos HALLAR las semanas del HISTORICO que empiezan el "4"
  semanas_iguales <- f_hi_m %>% filter(semana %in% dias_semana$semana)
  # vamos a calcular los centros de estas semanas
  semanas_iguales <- dens.diaria(semanas_iguales)
  semanas_iguales <- semanas_iguales %>% select(semana,dia_semana,dens_diaria)
  
  
  # Unimos con los nombres de los dias
  semanas_iguales <- inner_join(semanas_iguales,dia.codigo, by="dia_semana") %>%
    select(semana,dens_diaria,dia)
  
  densidad_semana <- semanas_iguales %>% 
    spread(key = dia, value = dens_diaria)
 
  return(densidad_semana)
}

#=======================================================================================
centro.dia <- function(densidad_semana){
  kmedias <- kmeans(densidad_semana[2:ncol(densidad_semana)], centers=1)
  centro <- as.data.frame(kmedias$centers)
  # Volvemos como codigo la columna dia.semana
  dia.codigo$dia_semana <- as.numeric(dia.codigo$dia_semana)
  # Ordenamos el centro
  centro <- gather(centro, key = dia, value = densidad)
  # Utilizamos sólo los codigos de los dias que pertenecen a la semana
  dia.codigo2 <- dia.codigo %>% filter(dia %in% centro$dia) # esto organiza los dias
  dia.codigo2$dia <- as.character(dia.codigo2$dia)
  # gUARDAMOS el CONSOLIADADO DE LA DESAGREGACION COMERCIAL
  centro_dia <- inner_join(dia.codigo2,centro,by="dia")
  return(centro_dia)
}






