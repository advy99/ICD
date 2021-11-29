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


# miramos si tenemos valores perdidos
missmap(iris)

any(is.na(iris))


# calculamos medidas de interes

medias_iris <- apply(iris[,-5], 2, mean)
medias_iris

sd_iris <- apply(iris[, -5], 2, sd)
sd_iris

minimos_iris <- apply(iris[,-5], 2, min)
minimos_iris

maximos_iris <- apply(iris[,-5], 2, max)
maximos_iris



# graficamos los datos

mostrar_grafico_qq <- function(x) {
	qqnorm(x)
	qqline(x)
}

par(mfrow=c(2,2))
apply(iris[,-5], 2, mostrar_grafico_qq)
par(mfrow=c(1,1))

# vistazo general
ggpairs(iris, aes(colour = Class)) + theme_minimal()
