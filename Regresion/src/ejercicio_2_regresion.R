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