library(tidyverse)
library(readr)
library(Amelia)
library(ggplot2)
library(corrplot)
library(GGally)

#
# Clasificación
#


# leemos el fichero de datos
iris <- read.csv("data/iris/iris.dat", comment.char = "@", header = FALSE)

# vamos a mirar las primeras filas
head(iris)

# cambiamos los nombres de los atributos, no los ha leido bien el read.csv
names(iris) <- c("SepalLength", "SepalWidth", "PetalLength", "PetalWidth", "Class")

head(iris)

# miramos cuantas filas tenemos de datos
dim(iris)

# miramos los tipos de los datos
str(iris)

# pasamos a factor la clase del problema
iris$Class <- as.factor(iris$Class)
summary(iris)

# el missmap nos modifica los valores por defecto de plot
# y luego el qqplot no se verá bien, así que los restauramos despues del missmap
par_default <- par(mar = c(0, 0, 0, 0))
# miramos si tenemos valores perdidos
missmap(iris)
par(par_default)

any(is.na(iris))


# calculamos medidas de interes
# las calculamos sin tener en cuenta la clase de salida
medias_iris <- apply(iris[,-5], 2, mean)
medias_iris

sd_iris <- apply(iris[, -5], 2, sd)
sd_iris

minimos_iris <- apply(iris[,-5], 2, min)
minimos_iris

maximos_iris <- apply(iris[,-5], 2, max)
maximos_iris



# graficamos los datos

# vistazo general
ggpairs(iris, aes(colour = Class)) + theme_minimal()


mostrar_grafico_densidad <- function(nombre_variable) {
	ggplot(data = iris, aes_string(x = nombre_variable, fill = "Class")) +
		geom_density(alpha = 0.5) + 
		ggtitle(paste("Densidad de valores de ", nombre_variable, " por cada clase."))
}

mostrar_grafico_densidad("SepalLength")
mostrar_grafico_densidad("SepalWidth")
mostrar_grafico_densidad("PetalLength")
mostrar_grafico_densidad("PetalWidth")

mostrar_boxplot <- function(nombre_variable) {
	ggplot(data = iris, aes_string(x = nombre_variable, fill = "Class")) +
		geom_boxplot() + 
		ggtitle(paste("Boxplot de ", nombre_variable, " por cada clase."))
}

mostrar_boxplot("SepalLength")
mostrar_boxplot("SepalWidth")
mostrar_boxplot("PetalLength")
mostrar_boxplot("PetalWidth")


mostrar_grafico_qq <- function(datos, nombre) {
	qqnorm(datos, main = paste("Normal Q-Q Plot: ", nombre))
	qqline(datos)
}


par(mfrow=c(2,2))
lapply(colnames(iris[,-5]), function(col_name) { mostrar_grafico_qq(iris[,col_name], col_name) } )
par(mfrow=c(1,1))

# test de normalidad sobre las características
apply(iris[, -5], 2, shapiro.test)

# como podemos ver en los p-value, rechazamos la hipotesis nula de que los datos siguen
# una distribución normal a excepción de la segunda característica, SepalWidth ,por muy poco


matriz_correlaciones_iris <- cor(iris[, -5])

corrplot(matriz_correlaciones_iris, method = "ellipse")


