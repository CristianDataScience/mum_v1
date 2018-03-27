#=======================================================================================
# Preparación del MES ANTES de la Desagregacion Temporaral
#=======================================================================================
# Trabajamo Marzo de Piloto con 5 semanaS desde MES_A__PROYECTAR2.sql
#=======================================================================================
Marzo <- read_excel("/home/tclondono/CODIGO/INPUT/Marzo.xlsx")
#=======================================================================================
Marzo$semana <- semana.completa(Marzo$dia_semana)+1 
#=======================================================================================
# Vamos a contar los dias que tiene cada semana de marzo
#=======================================================================================
n_dia_sm <- Marzo %>% group_by(semana) %>% summarise(n=n())
#=======================================================================================
# Codificamos las variables categoricas de MARZO
#=======================================================================================
Marzo$promocion=as.numeric(factor(Marzo$promocion,
                                  levels = c("P0","P1","P2","P3"),
                                  labels = c(0,2,1,3)))
Marzo$temporada=as.numeric(factor(Marzo$temporada,
                                  levels = c("Festivo","Normal","Prepromoción",
                                             "Promoción"),
                                  labels = c(1,0,3,2)))
#=======================================================================================
# Toca arreglar a mano el dia festivo malo: 25 de Marzo es domingo
#=======================================================================================
Marzo$temporada[25] <- 0
#=======================================================================================
# Vamos a agregarle a Marzo la variable tipo_dia
# Dia_normal=0, Dia_festivo_lunes=1,Dia_festivo_nolunes=-1
#=======================================================================================
tipo_dia <- rep(0,nrow(Marzo))
tipo_dia <- ifelse(Marzo$temporada==1 & Marzo$dia_semana==1,1,0)
tipo_dia <- ifelse(Marzo$temporada==1 & Marzo$dia_semana!=1,-1,tipo_dia)
Marzo <- cbind(Marzo,tipo_dia)
rm(tipo_dia)
#=======================================================================================
# Vamos a agregarle a Marzo la variable tipo_semana
# Semana_normal=0, Semana_festivo_lunes=1,Semana_festivo_nolunes=-1
#=======================================================================================
sm <- Marzo %>% group_by(semana) %>% summarise(tipo_semana=sum(tipo_dia))
Marzo <- merge(Marzo,sm,by ="semana")
rm(sm)

#=======================================================================================
# Queremos información sólo de las SEMANAS_de_MARZO
#=======================================================================================
S_M <- unique(Marzo %>% select(semana,semana_ano,mes,tipo_semana))
#=======================================================================================
S_M <- inner_join(n_dia_sm,S_M,by="semana")
