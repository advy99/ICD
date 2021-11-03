
require(ISLR)
require(MASS)

#install.packages("ISLR")
#install.packages("MASS")

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
fit4 = lm(MedianHouseValue ~ . - Households, data = California)
summary(fit4)

# Con esto pasamos de un R-squared de 0.637 a uno de 0.6363, manteniendo en
# las demás caracteristicas un p-value < 2e-16, por lo que podemos eliminar esa
# caracteristica sin perder mucha información, como hemos podido observar.

# Tras esto, aunque ya obtenemos con todas las caracteristicas el mismo p-value,
# voy a probar a eliminar una a una todas las caracteristicas que estaban correlacionadas
# con households, ya que al existir esa correlación es probable que estuvieran explicando
# la misma parte del modelo, por lo que se podría simplificar sin perder información.

# Households tiene una correlación muy alta con TotalBedrooms y Population,
# por lo que en principio podríamos dejar solo una de estas dos
fit5 = lm(MedianHouseValue ~ . - Households - TotalBedrooms, data = California) 
summary(fit5)

# Con esto pasamos de un R-squared de 0.6363 a uno de 0.6112, manteniendo en
# las demás caracteristicas un p-value < 2e-16, podríamos eliminar esta variable 
# si queremos un modelo más explicable perdiendo tan solo un 2% de información.

# Llegados a este punto podríamos parar, ya que todas las variables cuantan con un buen
# p-value, indicando una relación lineal con la caracteristica de salida,
# e intentar reducir el número de predictores solo nos lleva a bajar el R-squared



#
# INTERACCIONES
#

# antes se ha graficado unicamente el mapa de correlaciones del dataset. También 
# podemos graficar las variables unas respecto a otras:

ggpairs(California)

# como vemos, en algunas variables existe una gran interacción, como por ejemplo
# la ya comentada TotalBedrooms y Household, TotalRooms y TotalBedrooms (como era de esperar),
# entre otras.
# Con esto, a partir del mejor modelo obtenido con regresión lineal multiple
# vamos a intentar mejorarlo con interacciones.

# Escogeré como mejor modelo el que contenia todas las caracteristicas, ya que 
# obteniamos un mejor R-squared y además debido a las interacciones que vamos a
# probar tendría que añadir igualmente las caracteristicas que probe a eliminar
# por el principio de la jerarquia

# como vemos en la gráfica, hay interacciones entre TotalBedrooms, Households
fit6 = lm(MedianHouseValue ~ . + TotalBedrooms * Households, data = California) 
summary(fit6)

# Si graficamos también esta nueva caracteristica con las demás, podemos ver
# que también hay una relación entre esta nueva y Population
ggpairs(cbind(California, California$TotalBedrooms * California$Households))

fit7 = lm(MedianHouseValue ~ . + TotalBedrooms * Households * Population, data = California) 
summary(fit7)

# con este nuevo termino vemos que el p-value es de 0.444, por lo que no existe 
# una dependencia lineal y no aporta mucho, esta interacción no ha funcionado y
# habría que eliminarla

# voy a probar a cambiarlo por MedianIncome, que también parece mostrar una relacion
fit8 = lm(MedianHouseValue ~ . + TotalBedrooms * Households * MedianIncome, data = California) 
summary(fit8)

# en este caso si ha resultado de interes, subiendo el adjusted R-squared a 0.6429
# y con un p-value que nos indica que existe relación lineal. Aunque ahora algunas
# de las caracteristicas base como Household nos indica que habría que eliminarlas
# por el p-value, se han de mantener por el principio de jerarquia.


#
# NO LINEALIDAD
#

# A partir del mejor modelo anterior (fit8), voy a probar a añadir terminos de no linealidad

# para graficar los valores de x al cuadrado con respecto a Y
temp <- California
temp_cuadrado <- California^2
plotY_cuadrado <- function (x,y) {
	plot(temp[,y]~temp_cuadrado[,x], xlab=paste(names(temp)[x]," X",x,sep=""), ylab=names(temp)[y])
}

# vamos a ver las caracteristicas al cuadrado con respecto a MedianHouseValue
par(mfrow=c(3,3)) #Si margin too large => (2,3)
x <- sapply(1:(dim(temp)[2]-1), plotY_cuadrado, dim(temp)[2])
par(mfrow=c(1,1))

# como vemos no cambian de forma y esta transformación no nos aporta mucha información
# así que vamos a probar con algunas de las variables que mejor se ven,
# como el MedianIncome.
fit9 = lm(MedianHouseValue ~ . + TotalBedrooms * Households * MedianIncome + I(MedianIncome^2), data = California) 
summary(fit9)

# con esta prueba hemos mejorado el R-squared en menos de un 1%, aunque el
# p-value si nos indica que el termino no lineal si es significativo
