#=======================================================================================
# Vamos a calcular la desagregación comercial en los 5 CEDIS
#=======================================================================================
filtro.cedi <- function(proyeccion_pgc,historia_wms){
  
  dis_flujo <- unique(p_pgc$FLUJO)
  dis_tarea <- unique(p_pgc$TAREA)
  cedis <- unique(p_pgc$CEDI)
  #cedis <- c(20,50,85,146,137,138)    
  
  conso <- data.frame() # inicializamos 
  
  for (c in cedis){
    for (j in dis_flujo){
      for (i in dis_tarea){
        f_p <- p_pgc %>% filter(CEDI==c,
                                FLUJO==j,
                                TAREA==i)
        if (nrow(f_p)==0){next}
        f_h <- wms_pgc %>% filter(Dep_Despacha_ID==c,
                                  Flujo_Logistico==j,
                                  Tarea==i)
        # f_h<-data.table(f_h)
        # 
        # 
        # f_h[,maximo:=max(Unidades),by=semana]
        # f_h<-f_h[maximo>0,]
        # f_h<-data.frame(f_h)
        
        if (nrow(f_h)==0){next}
        di <- desag.co(f_p,centros(f_h))
        if (nrow(di)==0){next}
        conso <- rbind(di,conso)
 
        }
    }
  }
  return(conso)
}
#=======================================================================================
# Tenemos entonces la desagregación comercial consolidada
#=======================================================================================
consolidado_cedis <- filtro.cedi(p_pgc,wms_pgc)

