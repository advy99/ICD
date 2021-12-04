library(tidyverse)
library(readr)
library(GGally)
library(corrplot)
library(class)
library(caret)
library(ggplot2)
library(scales)

set.seed(0)


calcular_accuracy <- function(valores_reales, valores_predichos) {
	sum(valores_reales == valores_predichos) / length(valores_reales)
}

ggplotConfusionMatrix <- function(matriz){
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
					trControl = trainControl(method = "cv"))

# miramos el resultado, que nos dirá la mejor k
modelo_knn

mostrar_avance_accuracy_k(modelo_knn$results)

# predecimos con los datos de test
predicciones_test <- predict(modelo_knn, newdata = iris_test)

# miramos accuracy y kappa
postResample(pred = predicciones_test, obs = iris_test_etiquetas)

table(iris_test_etiquetas, predicciones_test)

matriz_confusion <- confusionMatrix(factor(predicciones_test), factor(iris_test_etiquetas), dnn = c("Prediction", "Reference"))
matriz_confusion

ggplotConfusionMatrix(matriz_confusion)
