
require(ISLR)
require(MASS)

install.packages("ISLR")
install.packages("MASS")

# Leemos los datos
California <- read.csv("data/california.dat", comment.char = "@", header = FALSE)

# Le asignamos los nombres
names(California) <- c("Longitude", "Latitude", "HousingMedianAge",
					   "TotalRooms", "TotalBedrooms", "Population", "Households",
					   "MedianIncome", "MedianHouseValue")

head(California)

# funcion para mostrar en graficas los datos
temp <- California
plotY <- function (x,y) {
	plot(temp[,y]~temp[,x], xlab=paste(names(temp)[x]," X",x,sep=""), ylab=names(temp)[y])
}

#
# REGRESION LINEAL SIMPLE
#



# Mostramos todas las variables con respecto a la variable objetivo
par(mfrow=c(3,3)) #Si margin too large => (2,3)
x <- sapply(1:(dim(temp)[2]-1), plotY, dim(temp)[2])
par(mfrow=c(1,1))

#
# Como vemos en los gráficos, a simple vista no se ve ninguna relación con una variable y
# la variable de salida de gran importancia, la única que podemos observar cierta
# relación es con la característica MedianIncome.
#


# Ahora vamos a ver todas las correlaciones, tanto con la matriz de correlacion, 
# como graficandolo con ggpairs
mapa_correlaciones <- cor(California)
mapa_correlaciones

library(GGally)
ggpairs(as.data.frame(mapa_correlaciones))

# Como nos confirma tanto la matriz de correlaciones como podemos ver en la gráfica
# la única correlacion importante con la variable de salida es la de la característica 
# MedianIncome. También podemos ver que existen correlaciones entre otras variables,
# pero ninguna de esas correlaciones incluye la variable de salida,
# como puede ser la correlación entre Households y TotalBedrooms, o entre
# Population y TotalBedrooms

# con esto graficamos la variable que tiene cierta correlación con la de salida
par(mfrow=c(1,1)) #Si margin too large => (2,3)
x <- plotY(8, dim(temp)[2])
par(mfrow=c(1,1))


# tras esto vamos a probar a hacer un modelo de regresión simple
# entre el MedianHouseValue y MedianIncome

fit1 = lm(MedianHouseValue ~ MedianIncome, data = California)
fit1

summary(fit1)

#
# Como podemos ver del resultado del summary, tanto con el F-statistic como 
# con el p-value de la caracteristica MedianIncome, si que existe una relación lineal
# entre MedianIncome y MedianHouseValue. COmo podemos ver también 
# con el (Adjusted) R-squared, esta variable es capaz de explicar alrededor del 47%
# de la variable que queremos predecir.
#


# mostramos la variable escogida y la linea generada por el fit1
par(mfrow=c(1,1))
x <- plotY(8, dim(temp)[2])
abline(fit1,col="red")
confint(fit1)

# MSE y error estandar de los residuos para el fit1
sqrt(sum(fit1$residuals^2)/length(fit1$residuals)) #MSE
sqrt(sum(fit1$residuals^2)/(length(fit1$residuals)-2)) #Error estandard de los residuos


# Tambien he decidido probar con la segunda variable con mayor correlación, 
# Population, aunque esto es pasar de utilizar una variable con correlacion de
# 0.892 (MedianIncome), a 0.325 (Population, en valor absoluto), que es un valor muy bajo
# Mostramos population con respecto a MedianHouseValue
par(mfrow=c(1,1)) #Si margin too large => (2,3)
x <- plotY(6, dim(temp)[2])
par(mfrow=c(1,1))

# Como vemos, aunque podríamos formar una linea, apenas es capaz de diferenciar
# una distincion del MedianHouseValue con respecto a la población.


# Ajustamos el modelo:
fit2 = lm(MedianHouseValue ~ Population, data = California)
fit2

summary(fit2)

# En este caso, también ha sido capaz de encontrar una relacion lineal
# entre poblacion y la variable de salida. Sin embargo el R-squared es de tan
# solo del 0.00055, no llegando ni al 1%.
#

# mostramos la variable escogida y la linea generada por el fit1
par(mfrow=c(1,1))
x <- plotY(6, dim(temp)[2])
abline(fit2,col="red")
confint(fit2)
# como era de esperar, la linea es mucho peor, ajustandose más a los outliers
# que vemos en la gráfica



# MSE y error estandar de los residuos para el fit1
sqrt(sum(fit2$residuals^2)/length(fit2$residuals)) #MSE
sqrt(sum(fit2$residuals^2)/(length(fit2$residuals)-2)) #Error estandard de los residuos

# Como era de esperar, los errores obtenidos con este modelo son mucho más grandes

#
# REGRESION LINEAL MULTIPLE
#

fit3 = lm(MedianHouseValue ~ ., data = California) #TODAS para descendente
summary(fit3)

# Con el fit3 vemos que obtenemos un buen F-statistic, con un p-value < 2.2e-16,
# por lo que al menos una caracteristica tiene relación lineal con la caracteristica 
# a predecir, por lo que nos sirve el modelo. Conseguimos un R-squared de 0.637,
# que como vemos ha mejorado notablemente el valor con el mejor modelo de regresión
# lineal simple (de un 47% a un 63% de explicación)

# Como podemos ver segun el summary, todas las variables tienen un buen p-value
# por lo que podemos suponer que todas tienen cierta relacion lineal con la variable
# de salida
# Tras esto vamos a probar a ir eliminando las caracteristicas con menor p-value
# para ver si podemos obtener un modelo más simple sin bajar el nivel de explicación
# dado por el R-squared.

# la caracteristica con menor p-value era Households
fit4 = lm(MedianHouseValue ~ . - Households, data = California) #TODAS para descendente
summary(fit4)

# Con esto pasamos de un R-squared de 0.637 a uno de 0.6363, manteniendo en
# las demás caracteristicas un p-value < 2e-16, por lo que podemos eliminar esa
# caracteristica sin perder mucha información, como hemos podido observar.

# Tras esto, aunque ya obtenemos con todas las caracteristicas el mismo p-value,
# voy a probar a eliminar una a una todas las caracteristicas que estaban correlacionadas
# con households, ya que al existir esa correlación es probable que estuvieran explicando
# la misma parte del modelo, por lo que se podría simplificar sin perder información.


#interactions
