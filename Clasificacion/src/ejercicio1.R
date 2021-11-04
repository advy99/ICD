library(tidyverse)
library(philentropy)


# por defecto el conjunto de test es el de train
my_knn <- function(train, train_labels, test = train, k = 1, metric = "euclidean") {
	
	# para cada fila del test, sacamos las distancias con train,
	# miramos las k etiquetas, y obtenemos una clase 
	etiquetas_test <- apply(test, 1, function(elemento_test) {
		# a cada elemento de train
		# le aplico la distancia dada con el elemento de test a evaluar
		distancias <- apply(train, 1, function(elemento_train) {
			distance(as.data.frame(rbind(elemento_train, elemento_test)), method = metric, mute.message = T)
		})
		
		# obtenemos el numero de filas a mirar (miramos k filas)
		indices_etiquetas_a_mirar <- order(distancias)[1:k]
		
		# ponemos esas etiquetas en una tabla, la ordenamos de forma decreciente
		# (primero las etiquetas que más aparecen), y miramos el nombre de la clase
		# que mas aparece, esa es nuestra etiqueta asignada a esa fila de test
		etiqueta <- names(sort(table(train_labels[indices_etiquetas_a_mirar]), decreasing = T))[1]
		
		etiqueta
	})
	
	etiquetas_test
}

# pequeña prueba con iris
iris
iris_train <- iris %>% select(-Species)
iris_train_labels <- iris %>% select(Species)
iris_test <- iris_train[1,]

iris_train <- iris_train[2:nrow(iris_train),]
iris_train_labels <- iris_train_labels[2:nrow(iris_train_labels),]
iris_train

my_knn(iris_train, iris_train_labels, k = 5)

