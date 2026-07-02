#install.packages("titanic")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("corrplot")
library(titanic) 
library(ggplot2)
library(dplyr)
library(corrplot) 


#### Obtención y limpieza de datos --------
#obtener los datos
data=titanic_train

#limpiar datos
#eliminar faltantes en Puerto de Embarque
which(is.na(data$Embarked))
table(data$Embarked) # hay 2 observaciones en blanco
which(data$Embarked=="") # están en la fila 62 y 830
data=data[data$Embarked!="",] #aquí sacamos esas dos filas

#### Estadística descriptiva: variable cualitativa nominal --------

#Tabla de frecuencia: Puerto de Embarque
table(data$Embarked)

#Diagrama de barras: Puerto de Embarque
ggplot(data,mapping = aes(x=Embarked, fill = Embarked))+geom_bar()+
  theme_classic()+theme(legend.position = "none")



#Diagrama de pastel: Puerto de Embarque

data_c <- data.frame(
  category = c("C", "Q", "S"),
  value = table(data$Embarked)
)

# Calculate percentages for labels
data_c$percent <- round(100 * data_c$value / sum(data_c$value), 1)
data_c$label <- paste0(data_c$category, " (", data_c$percent, "%)")

# Create pie chart
ggplot(data_c, aes(x = "", y = value, fill = category)) +
  geom_bar(stat = "identity", width = 1, color = "white") +  # stacked bar
  coord_polar(theta = "y") +                                 # convert to pie
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5), size = 4) + # labels
  scale_fill_brewer(palette = "Set2") +                      # color palette
  theme_void() +                                             # remove axes
  theme(legend.position = "none") 

#### Estadística descriptiva: variable cuantitativa numérica --------

# Construir tabla de frecuencias
histograma <- hist(data$Age, breaks = 10, plot = FALSE)


tabla_frecuencia <- data.frame(
  Intervalo = paste0("[", head(histograma$breaks, -1), ", ", tail(histograma$breaks, -1), ")"),
  Frecuencia = histograma$counts,
  Frecuencia_Relativa = round(histograma$counts / sum(histograma$counts), 3),
  Frecuencia_Acumulada = cumsum(histograma$counts)
)

# Mostrar tabla
print(tabla_frecuencia)



#histograma
ggplot(data, aes(x=Age))+geom_histogram(bins = 10, color = "black", # Outline color
                                        fill = "steelblue")+theme_classic()


#diagrama de cajas
ggplot(data, aes(y=Age, x=Embarked, color=Embarked))+geom_boxplot()+theme_classic()+
  theme(legend.position = "none")


#Correlación dos variables cuantitativas


#setwd("#pegue aqui el directorio donde está la data")
data_iris=read.csv("iris_dataset.csv")


#gráficos de dispersión de setosa
ggplot(data_iris %>% filter(variety=="setosa"), aes(x=sepal.length,y=sepal.width))+
  geom_point()+theme_classic()+labs(title="Variedad: setosa")


ggplot(data_iris %>% filter(variety=="setosa"), aes(x=petal.length,y=sepal.width))+
  geom_point()+theme_classic()+labs(title="Variedad: setosa")

plot(data_iris[,1:4],col=c("red")) 


#coeficientes de correlación
cor(data_iris %>% filter(variety=="setosa") %>% select(!c(variety)))


corrplot(cor(data_iris[,1:4])) 
