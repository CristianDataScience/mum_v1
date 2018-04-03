#=======================================================================================
# TENEMOS EL CONSOLIDADO DE LA DESG.COMERCIAl Y  preparado EL MES
#=======================================================================================
# Vamos a CRUZARLE al consolidado las FECHAS DEL MES a proyectar
#=======================================================================================
cruze <- cruzar.fechas(consolidado_cedis, Marzo)
#=======================================================================================
#  Y  crear la matrix de día codigo
#=======================================================================================
dia <- c("lunes","martes","miercoles","jueves","viernes","sabado","domingo")
dia_semana <- c(1:7)
dia.codigo <- as.data.frame(cbind(dia,dia_semana))
dia.codigo$dia_semana <- as.numeric(dia.codigo$dia_semana)
rm(dia,dia_semana)
#=======================================================================================









