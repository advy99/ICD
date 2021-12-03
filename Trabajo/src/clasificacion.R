library(tidyverse)
library(readr)
library(GGally)
library(corrplot)
library(class)
library(caret)

set.seed(1)

# leemos el fichero de datos
iris <- read.csv("data/iris/iris.dat", comment.char = "@", header = FALSE)

names(iris) <- c("SepalLength", "SepalWidth", "PetalLength", "PetalWidth", "Class")

str(iris)
iris$Class <- as.factor(iris$Class)
str(iris)

# aunque las medidas de iris están en las mismas unidades, y son relativamente 
# similares, aun así vamos a normalizarlas ya que no sabemos a priori si alguna
# tendrá más peso al clasificar con knn, de forma que todas tengan la misma importancia

iris <- iris %>% mutate_if(is.numeric, scale, center = TRUE, scale = TRUE)

summary(iris)

# separamos en training y test (90 / 10)
PORCENTAJE_TEST = 0.2
muestras_train <- sample(nrow(iris), nrow(iris) * (1 - PORCENTAJE_TEST))

# aprovechamos y le quitamos la clase
iris_train <- iris[muestras_train, -5]
iris_test <- iris[-muestras_train, -5]

nrow(iris_train)
nrow(iris_test)

iris_train_etiquetas <- iris[muestras_train, 5]
iris_test_etiquetas <- iris[-muestras_train, 5]



