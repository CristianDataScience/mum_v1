#=======================================================================================
# Vamos CREAR LOS CENTROS | las DENSIDADES
#=======================================================================================
# para las semanas NORMALES 
# Ajustamos las  K-Means con 1 cluster
#=======================================================================================
kmeans = kmeans(x = semanas.normal[2:8], centers = 1, nstart=5) # Para semanas normales
#=======================================================================================
# Guardamos los grupos y los centros
centro <- as.data.frame(kmeans$centers)

rm(kmeans)
