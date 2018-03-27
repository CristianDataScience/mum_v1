densidad.semana <- function(filtro_hi_m, dia_codigo, m){
  
  filtro_hi_m<-f_hi_m
  dia_codigo<-dia.codigo
  m<-S_M$n[j]
  
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

  