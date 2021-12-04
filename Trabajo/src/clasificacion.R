library(tidyverse)
library(readr)
library(GGally)
library(corrplot)
library(class)
library(caret)
library(ggplot2)
library(scales)
library(philentropy)
library(MASS)


set.seed(0)


calcular_accuracy <- function(valores_reales, valores_predichos) {
	sum(valores_reales == valores_predichos) / length(valores_reales)
}

plot_matriz_confusion <- function(matriz){
	titulo <- paste("Accuracy: ", round(matriz$overall[1] * 100, 2), "% \t",
					 "Kappa: ", round(matriz$overall[2] * 100, 2), "%")
	
	grafico <- ggplot(data = as.data.frame(matriz$table) ,
				   aes(x = Reference, y = Prediction)) +
				   xlab("Valor real") +
				   ylab("Valor predicho") +
				   geom_tile(aes(fill = log(Freq)), colour = "white") +
				   scale_fill_gradient(low = "white", high = "steelblue") +
				   geom_text(aes(x = Reference, y = Prediction, label = Freq)) +
				   theme(legend.position = "none") +
				   ggtitle(titulo)
	
	grafico
}

mostrar_avance_accuracy_k <- function(resultados_caret) {
	ggplot(resultados_caret, aes(x = k, y = Accuracy)) +
		geom_line() +
		geom_point() +
		ggtitle("Accuracy del modelo en función del valor de K escogido")
}


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

# separamos en training y test
PORCENTAJE_TEST = 0.2
muestras_train <- sample(nrow(iris), nrow(iris) * (1 - PORCENTAJE_TEST))

# aprovechamos y le quitamos la clase
iris_train <- iris[muestras_train, -5]
iris_test <- iris[-muestras_train, -5]

nrow(iris_train)
nrow(iris_test)

iris_train_etiquetas <- iris[muestras_train, 5]
iris_test_etiquetas <- iris[-muestras_train, 5]


# no hace falta escalar, ya hemos escalado antes
# buscamos la mejor k entre 1 y 20
modelo_knn <- train(iris_train, iris_train_etiquetas, method = "knn", 
					metric="Accuracy", tuneGrid = data.frame(.k=1:20),
					trControl = trainControl(method = "cv", number = 10))

# miramos el resultado, que nos dirá la mejor k
modelo_knn

mostrar_avance_accuracy_k(modelo_knn$results)

# predecimos con los datos de test
predicciones_test <- predict(modelo_knn, newdata = iris_test)

# miramos accuracy y kappa con la matriz de confusión
matriz_confusion <- confusionMatrix(predicciones_test, iris_test_etiquetas)
matriz_confusion

plot_matriz_confusion(matriz_confusion)





#
# Ejercicio 2: LDA
#


# primero tenemos que comprobar las asunciones previas para poder aplicar LDA

# 1. Las observaciones se han obtenido de forma aleatoria (tendremos que suponer que así ha sido)


# 2. Cada clase sigue una distribución normal

# ya esta en el EDA, pero volvemos a comprobar
apply(iris %>% filter(as.integer(Class) == 1) %>% select_if(is.numeric), 2, shapiro.test)
apply(iris %>% filter(as.integer(Class) == 2) %>% select_if(is.numeric), 2, shapiro.test)
apply(iris %>% filter(as.integer(Class) == 3) %>% select_if(is.numeric), 2, shapiro.test)

# como vemos, a excepción del último predictor, las demás si siguen una normal
# para las tres etiquetas que tenemos

# 3. Misma matriz de covarianza

covariance <- c(var(iris$SepalLength), var(iris$SepalWidth), var(iris$PetalLength), var(iris$PetalWidth))
covariance

bartlett.test(SepalLength ~ Class, iris) # rechazamos hipotesis nula, no sigue la misma covarianza
bartlett.test(SepalWidth ~ Class, iris)
bartlett.test(PetalLength ~ Class, iris) # rechazamos hipotesis nula, no sigue la misma covarianza

# OJO de este no nos podemos fiar, esta variable no sigue una normal
# como vimos en el apartado 2
bartlett.test(PetalWidth ~ Class, iris)

# con LeveneTest se rechaza también H0, así que tampoco tiene la misma covarianza
leveneTest(PetalWidth ~ Class, iris)

# como vemos, debido a que PetalWidth no sigue una normal dentro de cada clase, y
# que no tenemos la misma matriz de covarianza, no deberíamos aplicar LDA, o por 
# lo menos no podemos esperar que nos de muy buenos resultados
# Lo vamos a lanzar aun así.

lda_fit <- train(iris_train, iris_train_etiquetas,
				method = "lda",
				tuneLength = 10,
				trControl = trainControl(method = "cv", number = 10))

lda_fit

#
# QDA
#

# para las asunciones, son las mismas de LDA, pero sin la de la matriz de covarianza
# así que ya lo hemos hecho

qda_fit <- train(iris_train, iris_train_etiquetas,
				 method = "qda",
				 tuneLength = 10,
				 trControl = trainControl(method = "cv", number = 10))

qda_fit


