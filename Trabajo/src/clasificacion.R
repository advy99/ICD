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

train(iris)


