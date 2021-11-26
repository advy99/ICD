library(tidyverse)
library(readr)


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



