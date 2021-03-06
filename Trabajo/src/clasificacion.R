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
library(car)


set.seed(0)


calcular_accuracy <- function(valores_reales, valores_predichos) {
	sum(valores_reales == valores_predichos) / length(valores_reales)
}

plot_matriz_confusion <- function(matriz, titulo = ""){
	subtitulo <- paste("Accuracy: ", round(matriz$overall[1] * 100, 2), "% \t",
					 "Kappa: ", round(matriz$overall[2] * 100, 2), "%")
	
	grafico <- ggplot(data = as.data.frame(matriz$table) ,
				   aes(x = Reference, y = Prediction)) +
				   labs(title = titulo, subtitle = subtitulo) +
				   xlab("Valor real") +
				   ylab("Valor predicho") +
				   geom_tile(aes(fill = log(Freq)), colour = "white") +
				   scale_fill_gradient(low = "white", high = "steelblue") +
				   geom_text(aes(x = Reference, y = Prediction, label = Freq)) +
				   theme(legend.position = "none")
	
	grafico
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

# para ver el avance en test, entrenamos un knn con las distintas k y calculamos su accuracy en test
resultados_test <- lapply(1:20, function(x) {
	# entreno un modelo con el valor de k = x con validación cruzada y predigo con ese modelo
	modelo_ac_test <- train(iris_train, iris_train_etiquetas, method = "knn", 
						metric="Accuracy", tuneGrid = data.frame(.k=x),
						trControl = trainControl(method = "cv", number = 10))
	predict(modelo_ac_test, iris_test)
})
resultados_test <- sapply(resultados_test, function(x) {calcular_accuracy(iris_test_etiquetas, x)})
resultados_test

avance_accuracy <- data.frame(k = 1:20, Accuracy_test = resultados_test, Accuracy_train = modelo_knn$results$Accuracy)

# mostramos avance de accuracy para train y test y guardamos los ficheros
ggplot(avance_accuracy %>% pivot_longer(c(2, 3)), aes(x = k, y = value, color = name)) +
	geom_line() +
	geom_point() +
	ggtitle("Accuracy del modelo en función del valor de K escogido") +
	xlab("Valor de K") +
	ylab("Accuracy")

ggsave("out/iris/avance_accuracy_iris.svg", device = svg, width = 1920, height = 1080, units = "px", dpi = 150)


# predecimos con los datos de test
predicciones_test <- predict(modelo_knn, newdata = iris_test)

# miramos accuracy y kappa con la matriz de confusión
matriz_confusion <- confusionMatrix(predicciones_test, iris_test_etiquetas)
matriz_confusion

plot_matriz_confusion(matriz_confusion, "Modelo K-NN")

ggsave("out/iris/matriz_confusion_knn.svg", device = svg, width = 1920, height = 1080, units = "px", dpi = 150)

iris_plot <- iris
# le cambiamos las etiquetas a las predichas
iris_plot[-muestras_train, "Class"] <- predicciones_test
# marco las que he usado como test
iris_plot["test"] <- 0
iris_plot[-muestras_train, "test"] <- 1
iris_plot$test <- as.factor(iris_plot$test) 
str(iris_plot)

ggplot(iris_plot, aes(x = PetalLength, y = PetalWidth, color = Class, shape = test)) +
	geom_point(size = 5) + 
	labs(title = "Predicciones utilizando KNN")
ggsave("out/iris/predicciones_knn.svg", device = svg, width = 1920, height = 1080, units = "px", dpi = 150)



#
# Ejercicio 2: LDA
#


# primero tenemos que comprobar las asunciones previas para poder aplicar LDA

# 1. Las observaciones se han obtenido de forma aleatoria (tendremos que suponer que así ha sido)


# 2. Cada clase sigue una distribución normal

# comprobamos por separado en cada clase
apply(iris %>% filter(as.integer(Class) == 1) %>% select_if(is.numeric), 2, shapiro.test)
apply(iris %>% filter(as.integer(Class) == 2) %>% select_if(is.numeric), 2, shapiro.test)
apply(iris %>% filter(as.integer(Class) == 3) %>% select_if(is.numeric), 2, shapiro.test)

# como vemos, a excepción del último predictor, no podemos rechazar que siga una distribución normal
# para las tres etiquetas que tenemos

# 3. Misma matriz de covarianza


bartlett.test(SepalLength ~ Class, iris) # rechazamos hipotesis nula, no sigue la misma covarianza
bartlett.test(SepalWidth ~ Class, iris)
bartlett.test(PetalLength ~ Class, iris) # rechazamos hipotesis nula, no sigue la misma covarianza

# OJO de este no nos podemos fiar, esta variable no sigue una normal
# como vimos en el apartado 2
bartlett.test(PetalWidth ~ Class, iris)

# con LeveneTest se rechaza también H0, así que tampoco tiene la misma covarianza
# (con respecto al as.numeric, me da error si no lo uso, aunque ya es numerico, no se por que)
leveneTest(as.numeric(PetalWidth) ~ Class, iris)

# como vemos, debido a que PetalWidth no sigue una normal dentro de cada clase, y
# que no tenemos la misma matriz de covarianza, no deberíamos aplicar LDA, o por 
# lo menos no podemos esperar que nos de muy buenos resultados
# Lo vamos a lanzar aun así.

lda_fit <- train(iris_train, iris_train_etiquetas,
				method = "lda",
				trControl = trainControl(method = "cv", number = 10))

lda_fit

# predecimos con los datos de test
predicciones_test_lda <- predict(lda_fit, newdata = iris_test)

# miramos accuracy y kappa con la matriz de confusión
matriz_confusion <- confusionMatrix(predicciones_test_lda, iris_test_etiquetas)
matriz_confusion

plot_matriz_confusion(matriz_confusion, "Modelo LDA")

ggsave("out/iris/matriz_confusion_lda.svg", device = svg, width = 1920, height = 1080, units = "px", dpi = 150)


iris_plot <- iris
# le cambiamos las etiquetas a las predichas
iris_plot[-muestras_train, "Class"] <- predicciones_test_lda
# marco las que he usado como test
iris_plot["test"] <- 0
iris_plot[-muestras_train, "test"] <- 1
iris_plot$test <- as.factor(iris_plot$test) 
str(iris_plot)

ggplot(iris_plot, aes(x = PetalLength, y = PetalWidth, color = Class, shape = test)) +
	geom_point(size = 5) + 
	labs(title = "Predicciones utilizando LDA")
ggsave("out/iris/predicciones_lda.svg", device = svg, width = 1920, height = 1080, units = "px", dpi = 150)


#
# QDA
#

# para las asunciones, son las mismas de LDA, pero sin la de la matriz de covarianza
# así que ya lo hemos hecho

qda_fit <- train(iris_train, iris_train_etiquetas,
				 method = "qda",
				 trControl = trainControl(method = "cv", number = 10))

qda_fit


# predecimos con los datos de test
predicciones_test_qda <- predict(qda_fit, newdata = iris_test)

# miramos accuracy y kappa con la matriz de confusión
matriz_confusion <- confusionMatrix(predicciones_test_qda, iris_test_etiquetas)
matriz_confusion

plot_matriz_confusion(matriz_confusion, "Modelo QDA")

ggsave("out/iris/matriz_confusion_qda.svg", device = svg, width = 1920, height = 1080, units = "px", dpi = 150)

iris_plot <- iris
# le cambiamos las etiquetas a las predichas
iris_plot[-muestras_train, "Class"] <- predicciones_test_qda
# marco las que he usado como test
iris_plot["test"] <- 0
iris_plot[-muestras_train, "test"] <- 1
iris_plot$test <- as.factor(iris_plot$test) 
str(iris_plot)

which(iris_plot$Class != iris$Class)

ggplot(iris_plot, aes(x = PetalLength, y = PetalWidth, color = Class, shape = test)) +
	geom_point(size = 5) + 
	geom_rect(data = iris_plot[which(iris_plot$Class != iris$Class),], aes(xmin = PetalLength - 0.03, xmax = PetalLength + 0.03, ymin = PetalWidth - 0.05, ymax = PetalWidth + 0.05)) +
	geom_point(data = iris_plot[which(iris_plot$Class != iris$Class),], size = 5) +
	labs(title = "Predicciones utilizando QDA")
ggsave("out/iris/predicciones_qda.svg", device = svg, width = 1920, height = 1080, units = "px", dpi = 150)




#
# comparacion entre los tres modelos
#
nombre <- "data/iris/iris"

run_method_fold <- function(i, x, metodo) {
	# leemos los datos
	file <- paste(x, "-10-", i, "tra.dat", sep="")
	x_tra <- read.csv(file, comment.char="@", header=FALSE)
	file <- paste(x, "-10-", i, "tst.dat", sep="")
	x_tst <- read.csv(file, comment.char="@", header=FALSE)

	names(x_tra) <- c("SepalLength", "SepalWidth", "PetalLength", "PetalWidth", "Class")
	
	names(x_tst) <- c("SepalLength", "SepalWidth", "PetalLength", "PetalWidth", "Class")
	
	# pasamos a factor la clas
	x_tra$Class <- as.factor(x_tra$Class)
	
	x_tst$Class <- as.factor(x_tst$Class)
	
	# separamos los datos de las etiquetas
	x_tra_labels <- x_tra[, 5]
	x_tra <- x_tra[, -5]
	
	x_tst_labels <- x_tst[, 5]
	x_tst <- x_tst[, -5]

	# si es el knn, usamos el mejor k para entrenar	
	if (metodo == "knn") {
		modelo <- train(x_tra, x_tra_labels,
					   method = metodo,
					   tuneGrid = expand.grid(k = modelo_knn$bestTune$k))
	} else {
		modelo <- train(x_tra, x_tra_labels,
					   method = metodo)
	}
	
	# predecimos y devolvemos el accuracy
	yprime_train <- predict(modelo,x_tra)
	yprime_test <- predict(modelo,x_tst)
	# calculamos accuracy
	accuracy_train <- calcular_accuracy(x_tra_labels, yprime_train)
	accuracy_test <- calcular_accuracy(x_tst_labels, yprime_test)
	
	list(train = accuracy_train, test = accuracy_test)
	
}

# metodos a usar
metodos <- list(knn = "knn", lda = "lda", qda = "qda")
# probe a tener una lista de parametros y pasarsela para cada modelo, sabiendo
# el nombre del modelo, pero a la función train de caret no le gusta y no he encontrado 
# forma de pasarles argumentos dependiendo del modelo
# parametros a usar para cada metodo
#parametros_modelos <- list(knn = list(k = modelo_knn$bestTune$k),  # para knn escogemos la mejor k obtenida
#						   lda = list(), 
#						   qda = list())

resultados <- lapply(metodos, function(x) {
	# obtenemos los resultados de un modelo
	resultados_metodo <- sapply(1:10, run_method_fold, nombre, x)
	# sacamos la media de train y test y ese es el resultado
	media_train <- mean(unlist(resultados_metodo["train",]))
	media_test <- mean(unlist(resultados_metodo["test",]))
	list(train = media_train, test = media_test)
})

resultados

# leemos y cambiamos los valores obtenidos en test
tabla_resultados_test <- read.csv("data/clasif_test_alumnos.csv")
tabla_resultados_test

tabla_resultados_test[9, 2] <- resultados$knn$test
tabla_resultados_test[9, 3] <- resultados$lda$test
tabla_resultados_test[9, 4] <- resultados$qda$test

tabla_resultados_test


# lo mismo pero en train
tabla_resultados_train <- read.csv("data/clasif_train_alumnos.csv")
tabla_resultados_train

tabla_resultados_train[9, 2] <- resultados$knn$train
tabla_resultados_train[9, 3] <- resultados$lda$train
tabla_resultados_train[9, 4] <- resultados$qda$train


tabla_resultados_train

# comparamos los tres algoritmos

# sacamos la tabla de resultados
tablatst <- cbind(tabla_resultados_test[,2:dim(tabla_resultados_test)[2]])
colnames(tablatst) <- names(tabla_resultados_test)[2:dim(tabla_resultados_test)[2]]
rownames(tablatst) <- tabla_resultados_test[,1]

# se la pasamos al test de friedman
test_friedman <- friedman.test(as.matrix(tablatst))
test_friedman

# como el test nos devuelve un pvalue muy alto, del 0.70, no rechazamos la hipotesis
# nula, luego NO hay diferencias significativas entre ningun par de algoritmos
# con los conjuntos de datos que hemos utilizado

