#=======================================================================================
# Estos filtros se deben automatizar
#=======================================================================================
options(scipen=999)
k=17

comb<-data.table(unique(cruze[c("CEDI","FLUJO","TAREA","AGRUPAMIENTO")]))
indice_semana <- unique(Marzo$semana)

for(k in 1:nrow(comb)){
  dia_densidad <- data.frame() 
f_co<-data.table(merge(cruze,comb[k,],by =c("CEDI","FLUJO","TAREA","AGRUPAMIENTO") ))
f_co<-f_co[order(Fecha)]
f_co<-data.frame(f_co)
#=======================================================================================
# Ahora filtramos la correspondiente serie del historico
#=======================================================================================


f_hi<-data.table(merge(wms_pgc,comb[k,]
                       ,by.x = c("Dep_Despacha_ID","Flujo_Logistico","Tarea","Agrupacion_ID")
                       ,by.y=c("CEDI","FLUJO","TAREA","AGRUPAMIENTO") ))
f_hi<-f_hi[order(FECHA)]
f_hi<-data.frame(f_hi)

f_hi<-data.table(f_hi)

#if (nrow(f_hi_m)==0){next}
f_hi[,maximo:=max(Unidades),by=c("Semana_mes","mes","ano")]
f_hi<-f_hi[maximo>0,]
f_hi<-data.frame(f_hi)
f_hi$maximo<-NULL

if (nrow(f_hi)==0){next}

#=======================================================================================
# Programamos para una serie en particular las DENSIDADES
#=======================================================================================


for (j in indice_semana){
  

  if (j < max(indice_semana)){
    f_hi_m <- f_hi %>% filter(Semana_mes==j, 
                              tipo_semana==S_M$tipo_semana[j], 
                              dia_semana %in% (8-S_M$n[j]):7) 
    f_hi_m <- f_hi_m %>% select(FECHA,Unidades:Semana_mes)
    f_hi_m<-data.table(f_hi_m)
    
    if (nrow(f_hi_m)==0){next}
    f_hi_m[,maximo:=max(Unidades),by=semana]
    f_hi_m<-f_hi_m[maximo>0,]
    f_hi_m<-data.frame(f_hi_m)
        # Calculamos la densidad semanal
  
    densidad_semana <- densidad.semana(f_hi_m, dia.codigo, m=S_M$n[j]) 
        # Calculamos los centroides de estas densidades
    dia_densidad <- rbind(dia_densidad,centro.dia(densidad_semana))
   
    # else{dia_densidad <- rbind(dia_densidad,densidad_semana[0,])}
  } else { 
    # filtro
    f_hi_m <- f_hi %>% filter(tipo_semana==S_M$tipo_semana[j], # semana "0" normal
                              dia_semana %in% (1:S_M$n[j])) # "6" dias )
    if (nrow(f_hi_m)==0){next}
    f_hi_m <- f_hi_m %>% select(FECHA,Unidades:Semana_mes)
    # Necesitamos completar el día que falta y lo tomamos de dia_ca:
    dia_ca5 <- dia_ca %>% filter(mes==4,tipo_semana==-2)
    f_hi_m <- f_hi_m[1:2]
    f_hi_m <- merge(x=f_hi_m, y=dia_ca5, all.y = TRUE)
    f_hi_m$Unidades = ifelse(is.na(f_hi_m$Unidades), 0, f_hi_m$Unidades)
    # Filtramos la unica semana santa del historico
    f_hi_m <- f_hi_m %>% filter(dia_semana %in% (1:S_M$n[j])) # "6" dias )
    
    # Calculamos la densidad semanal
    densidad_semana <- densidad.semana(f_hi_m, dia.codigo, m=S_M$n[j]) 
    # Ahora calculamos las densidades de cada día usando los centros
    dia_densidad <- rbind(dia_densidad,centro.dia(densidad_semana))
    
  } 
}
dia_densidad

if (nrow(f_co)!=nrow(dia_densidad)){next}
if(k==1){ 
    conso_tempo <- cbind(f_co,dia_densidad) 
}else{ 
  t1<-cbind(f_co,dia_densidad) 
  conso_tempo<-rbind(conso_tempo,t1)
  }

}


#=======================================================================================
# Multiplicar unidades por densidad 
#=======================================================================================
conso_tempo$unidades_diarias<-conso_tempo$densidad*conso_tempo$UNIDADES


