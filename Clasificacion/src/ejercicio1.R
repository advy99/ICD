library(tidyverse)
library(caret)
library(ggplot2)
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
		# que mas aparece, esa es nuestra etiqueta asignada a esta fila de test
		etiqueta <- names(sort(table(train_labels[indices_etiquetas_a_mirar]), decreasing = T))[1]
		
		etiqueta
	})
	
	etiquetas_test
}

# pequeña prueba con iris,
# solo para comprobar que se ejecuta bien y para debug
iris
iris_train <- iris %>% select(-Species)
iris_train_labels <- iris %>% select(Species)
iris_test <- iris_train[150,]

iris_train <- iris_train[1:149,]
iris_train_labels <- iris_train_labels[1:149,]
iris_train

my_knn(iris_train, iris_train_labels, iris_test, k = 5)

# probamos con el dataset de Breast cancer
wbcd <- read.csv("https://resources.oreilly.com/examples/9781784393908/raw/ac9fe41596dd42fc3877cfa8ed410dd346c43548/Machine%20Learning%20with%20R,%20Second%20Edition_Code/Chapter%2003/wisc_bc_data.csv")

# Examine the structure of the wbcd data frame
str(wbcd)
wbcd
wbcd <- wbcd %>% select(-id)
wbcd <- wbcd %>% mutate(diagnosis = factor(diagnosis, labels = c("Benign", "Malignant")))
table(wbcd$diagnosis)

# normalizamos el dataset
wbcd_n <- wbcd %>% mutate_if(is.numeric, scale, center = TRUE, scale = TRUE)

# Confirm that normalization worked
summary(wbcd_n[,c("radius_mean", "area_mean", "smoothness_mean")])

mwbcd_n <- wbcd_n %>% pivot_longer(cols = -diagnosis)

# Plot boxplots
ggplot(data=mwbcd_n, aes(x=name, y=value)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90))


# Create training and test data (holdout 90%-10%)
shuffle_ds <- sample(dim(wbcd_n)[1])
pct90 <- (dim(wbcd_n)[1] * 90) %/% 100
wbcd_train <- wbcd_n[shuffle_ds[1:pct90], -1]
wbcd_test <- wbcd_n[shuffle_ds[(pct90+1):dim(wbcd_n)[1]], -1]

# Create labels for training and test data
wbcd_train_labels <- wbcd_n[shuffle_ds[1:pct90], 1]
wbcd_test_labels <- wbcd_n[shuffle_ds[(pct90+1):dim(wbcd_n)[1]], 1]

print(dim(wbcd_train)[1])
print(dim(wbcd_test)[1])

# probamos primero el knn de caret
library(caret)

knnModel <- train(x = wbcd[shuffle_ds[1:pct90],-1], y = wbcd[shuffle_ds[1:pct90],1],
				  method = "knn", preProc = c("center", "scale"))
knnModel

knnPred <- predict(knnModel, newdata = wbcd[shuffle_ds[(pct90+1):dim(wbcd)[1]], -1])
knnPred

postResample(pred = knnPred, obs = as.factor(wbcd[shuffle_ds[(pct90+1):dim(wbcd)[1]], 1]))


# voy a probar con distintas familias:
# L_p Minkowski family: euclidean

mis_predicciones_euclidean <- my_knn(wbcd[shuffle_ds[1:pct90],-1], 
								   wbcd[shuffle_ds[1:pct90],1], 
								   wbcd[shuffle_ds[(pct90+1):dim(wbcd)[1]], -1],
								   k = 5,
								   metric = "euclidean")

resultado_euclidean <- postResample(pred = mis_predicciones_euclidean, obs = as.factor(wbcd[shuffle_ds[(pct90+1):dim(wbcd)[1]], 1]))
resultado_euclidean

# L_1 family: sorensen
mis_predicciones_sorensen <- my_knn(wbcd[shuffle_ds[1:pct90],-1], 
									 wbcd[shuffle_ds[1:pct90],1], 
									 wbcd[shuffle_ds[(pct90+1):dim(wbcd)[1]], -1],
									 k = 5,
									 metric = "sorensen")

resultado_sorensen <- postResample(pred = mis_predicciones_sorensen, obs = as.factor(wbcd[shuffle_ds[(pct90+1):dim(wbcd)[1]], 1]))
resultado_sorensen

# Intersection family: Intersection
mis_predicciones_intersection <- my_knn(wbcd[shuffle_ds[1:pct90],-1], 
									 wbcd[shuffle_ds[1:pct90],1], 
									 wbcd[shuffle_ds[(pct90+1):dim(wbcd)[1]], -1],
									 k = 5,
									 metric = "intersection")

resultado_intersection <- postResample(pred = mis_predicciones_intersection, obs = as.factor(wbcd[shuffle_ds[(pct90+1):dim(wbcd)[1]], 1]))
resultado_intersection

datos_comparativa <- rbind(resultado_euclidean, resultado_sorensen, resultado_intersection)
Metrica <- c("Euclidean", "Sorensen", "Intersection")
datos_comparativa <- cbind(datos_comparativa, Metrica)
datos_comparativa <- as.data.frame(datos_comparativa)

# pasamos las valores a numerico, estaban en caracter
datos_comparativa$Accuracy <- as.numeric(datos_comparativa$Accuracy)
datos_comparativa$Kappa <- as.numeric(datos_comparativa$Kappa)

datos_comparativa

# comparamos los resultados:

ggplot(data = datos_comparativa, aes(x = Metrica, y = Accuracy)) +
	geom_bar(stat="identity") +
	scale_y_continuous(breaks=seq(0, 1, 0.1)) +
	theme_minimal()

