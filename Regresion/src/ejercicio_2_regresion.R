#install.packages("ISLR")
#install.packages("MASS")
#install.packages("kknn)

require(ISLR)
require(MASS)
require(kknn)


# Leemos los datos
California <- read.csv("data/california.dat", comment.char = "@", header = FALSE)

# Le asignamos los nombres
names(California) <- c("Longitude", "Latitude", "HousingMedianAge",
					   "TotalRooms", "TotalBedrooms", "Population", "Households",
					   "MedianIncome", "MedianHouseValue")

head(California)

# "entrenamos" el modelo, en realidad solo cargar los datos y predecir los de test
# que en nuestro caso son los de training
fitknn1 <- kknn(MedianHouseValue ~ ., California, California)

# vamos a mirar como son los puntos que ha predecido con respecto a la mejor 
# variable que teniamos
plot(California$MedianHouseValue ~ California$MedianIncome)
points(California$MedianIncome, fitknn1$fitted.values, col = "blue", pch = 20)

yprime = fitknn1$fitted.values
sqrt(sum((California$MedianHouseValue - yprime)^2)/length(yprime)) #RMSE
# se comporta mucho mejor que regresión, tanto lineal como multiple, pasamos
# de 68692.05 con regresión lineal multiple con interacciones y no linealidad
# a 39131.14 de RMSE

summary(California)

# En este dataset no encontramos variables categóricas o discretas, son todas
# variables continuas, así que no habría que eliminar ninguna de estas

# voy a probar a eliminar algunas de las características que podamos considerar
# "repetidas", como por ejemplo el numero de habitaciones y el numero de dormitorios
# ya que están muy ligados entre si, eliminare el numero de dormitorios
fitknn2 <- kknn(MedianHouseValue ~ . - TotalBedrooms, California, California)

yprime = fitknn2$fitted.values
sqrt(sum((California$MedianHouseValue - yprime)^2)/length(yprime)) #RMSE

# hemos empeorado el RMSE, así que deberíamos mantener la variable

# voy a probar a eliminar la variable población, ya que al graficarla era una variable
# dispersa en el espacio y no nos decía nada sobre la variable de salida.
# he decidido eliminarla ya que podría estar introduciendo ruido.
fitknn3 <- kknn(MedianHouseValue ~ . - Population, California, California)

yprime = fitknn3$fitted.values
sqrt(sum((California$MedianHouseValue - yprime)^2)/length(yprime)) #RMSE

# de nuevo ha empeorado el RMSE, así que hay que mantenerla. Tras esto el mejor modelo
# de KNN sería el que mantiene todas las caracteristicas.


# vamos a probar con la formula obtenida del mejor de regersión lineal:
fitknn4 <- kknn(MedianHouseValue ~ . + TotalBedrooms * Households * MedianIncome + I(MedianIncome^2), California, California)

yprime = fitknn4$fitted.values
sqrt(sum((California$MedianHouseValue - yprime)^2)/length(yprime)) #RMSE

# con esta combinación hemos conseguido mejor un poco el RMSE, de 39131.14 a 
# 37784.8, por lo que la introducción de estas interacciones y no linealidad
# también ha funcionado de forma espacial en el conjunto de datos.

plot(California$MedianHouseValue ~ California$MedianIncome)
points(California$MedianIncome, fitknn4$fitted.values, col = "blue", pch = 20)


#
# 5-cv
#

# en nuestro caso, el mejor modelo para LM ha sido el que mejor ha funcionado para
# knn, de ahí que use en ambos la misma formula

#------------- 5-fold cross-validation LM: mejor obtenido
# IMPORTANTE: por la forma en la que creo los proyectos los datos están en la carpeta
# data, mis proyectos siguien esta estructura:

###
#  nombre_proyecto.Rproj
#  src/ # carpeta con el codigo
#   |
#   |- ejercicio_1_regresion.R
#   |- ejercicio_2_regresion.R
#  data/
#   |
#   |- Todos los ficheros de california, etc

nombre <- "data/california"
run_lm_fold <- function(i, x, tt = "test") {
	file <- paste(x, "-5-", i, "tra.dat", sep="")
	x_tra <- read.csv(file, comment.char="@", header=FALSE)
	file <- paste(x, "-5-", i, "tst.dat", sep="")
	x_tst <- read.csv(file, comment.char="@", header=FALSE)
	In <- length(names(x_tra)) - 1
	names(x_tra)[1:In] <- paste ("X", 1:In, sep="")
	names(x_tra)[In+1] <- "Y"
	names(x_tst)[1:In] <- paste ("X", 1:In, sep="")
	names(x_tst)[In+1] <- "Y"
	if (tt == "train") {
		test <- x_tra
	}
	else {
		test <- x_tst
	}
	fitMulti=lm(Y ~ . + X5 * X7 * X8 + I(X8^2),x_tra)
	yprime=predict(fitMulti,test)
	sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}
lmMSEtrain<-mean(sapply(1:5,run_lm_fold,nombre,"train"))
lmMSEtest<-mean(sapply(1:5,run_lm_fold,nombre,"test"))

#------------- 5-fold cross-validation KNN: mejor obtenido
nombre <- "data/california"
run_knn_fold <- function(i, x, tt = "test") {
	file <- paste(x, "-5-", i, "tra.dat", sep="")
	x_tra <- read.csv(file, comment.char="@", header=FALSE)
	file <- paste(x, "-5-", i, "tst.dat", sep="")
	x_tst <- read.csv(file, comment.char="@", header=FALSE)
	In <- length(names(x_tra)) - 1
	names(x_tra)[1:In] <- paste ("X", 1:In, sep="")
	names(x_tra)[In+1] <- "Y"
	names(x_tst)[1:In] <- paste ("X", 1:In, sep="")
	names(x_tst)[In+1] <- "Y"
	if (tt == "train") {
		test <- x_tra
	}
	else {
		test <- x_tst
	}
	fitMulti=kknn(Y ~ . + X5 * X7 * X8 + I(X8^2),x_tra,test)
	yprime=fitMulti$fitted.values
	sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}
knnMSEtrain<-mean(sapply(1:5,run_knn_fold,nombre,"train"))
knnMSEtest<-mean(sapply(1:5,run_knn_fold,nombre,"test"))


lmMSEtrain
lmMSEtest

knnMSEtrain
knnMSEtest

# como vemos con la formula obtenida se comporta mejor knn para el conjunto de datos
# de california


#
# Comparación general entre algoritmos
#
