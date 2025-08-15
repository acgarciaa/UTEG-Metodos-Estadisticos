####### LEER LOS CONJUNTOS DE DATOS ---------

library(readr)
registros<-read_csv("registries.csv")
head(registros)
clientes<-read_csv("clients.csv")

####### FUNCIONES PARA PROCESAMIENTO ---------


library(dplyr)
#crear una variable que mida el tiempo de la conexión
registros <- registros %>% mutate(time=exit_time-entry_time)

#unir los dos conjuntos de datos por ID del cliente
registros2 <- inner_join(registros,clientes, by="ID")

# filtrar el conjunto de datos tal que solo queden los clientes "Silver" 
registros2_silver <- registros2 %>% filter(category=="Silver")

# seleccionar solo las variables de ID, n_views y n_buy

registros2 %>% select(ID, n_views, n_buy)



####### FUNCIONES PARA ANÁLISIS DESCRIPTIVO ---------

library(GGally)
registros2 %>% select(category, n_views, n_buy) %>% ggpairs()


#######  PARA TALLER GRUPAL --------

# 1. Cree una tabla donde por se cuente cuántos log ins tuve cada cliente 
# y el tiempo promedio de sus log ins.
# 2. Cree una variable que indique "Sí" si el cliente realizó una compra en ese log in,
# caso contrario "No" y añada esa variable en el gráfico de pares
# 3. Realice un resumen ejecutivo de los principales hallazgos de todo el análisis