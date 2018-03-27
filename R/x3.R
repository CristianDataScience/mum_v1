#=======================================================================================
# Vamos a reescribir los dias como variables 
# Y las densidades como observaciones para luego calcular KMEDIAS
#=======================================================================================
dia.variable <- as.data.frame(reordenamiento(s1)) 
#=======================================================================================
#=======================================================================================
# Anexar el tipo de semana a la base dia.variable
#=======================================================================================
tsemana <- unique(dia_ca %>% select(semana,tipo_semana)) %>% filter(semana!=0)
dia.variable <- cbind(dia.variable,tsemana[2])
#=======================================================================================
# Vamos a quitarle la última semana en caso de que no este completa 
#=======================================================================================
a <- sum(is.na(dia.variable[nrow(dia.variable),2:8]))
nfilas <-  ifelse(a>0,nrow(dia.variable)-1,nrow(dia.variable))
dia.variable <- dia.variable[1:nfilas,]
rm(a,nfilas,tsemana)
#=======================================================================================
# Vamos a sacar tres tipos de semanas:
# 1 Normales
# 2 Con festivos lunes : estacional
# 3 Con festivos no lunes: no estacional
#=======================================================================================
semanas.normal <- dia.variable %>% filter(tipo_semana==0)  %>% select(semana:domingo)
semanas.esta   <- dia.variable %>% filter(tipo_semana==1)  %>% select(semana:domingo)
semanas.noesta <- dia.variable %>% filter(tipo_semana==-1) %>% select(semana:domingo)
#=======================================================================================
# Queremos saber que semana_del_mes es cada semana
#=======================================================================================

