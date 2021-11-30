library(tidyverse)
library(readr)
library(Amelia)
library(ggplot2)
library(corrplot)
library(GGally)

#
# Funciones
#

mostrar_grafico_densidad <- function(datos, nombre_variable) {
	ggplot(data = datos, aes_string(x = nombre_variable, fill = "Class")) +
		geom_density(alpha = 0.5) + 
		ggtitle(paste("Densidad de valores de ", nombre_variable, " por cada clase."))
}

mostrar_boxplot <- function(datos, nombre_variable) {
	ggplot(data = datos, aes_string(x = nombre_variable, fill = "Class")) +
		geom_boxplot() + 
		ggtitle(paste("Boxplot de ", nombre_variable, " por cada clase."))
}

mostrar_grafico_qq <- function(datos, nombre) {
	qqnorm(datos, main = paste("Normal Q-Q Plot: ", nombre))
	qqline(datos)
}

#
# Clasificación
#


# leemos el fichero de datos
iris <- read.csv("data/iris/iris.dat", comment.char = "@", header = FALSE)

# vamos a mirar las primeras filas
head(iris)

# cambiamos los nombres de los atributos, no los ha leido bien el read.csv
names(iris) <- c("SepalLength", "SepalWidth", "PetalLength", "PetalWidth", "Class")

head(iris)

# miramos cuantas filas tenemos de datos
dim(iris)

# miramos los tipos de los datos
str(iris)

# pasamos a factor la clase del problema
iris$Class <- as.factor(iris$Class)
summary(iris)

# el missmap nos modifica los valores por defecto de plot
# y luego el qqplot no se verá bien, así que los restauramos despues del missmap
par_default <- par(mar = c(0, 0, 0, 0))
# miramos si tenemos valores perdidos
missmap(iris)
par(par_default)

any(is.na(iris))


# calculamos medidas de interes
# las calculamos sin tener en cuenta la clase de salida

# utilizamos una lista con las medidas de interes
medidas <- list(medias = mean, 
				desviacion_estandar = sd, 
				minimos = min, 
				maximos = max)

# aplicamos cada medida a iris por columnas
medidas_iris <- lapply(medidas, function(funcion_medida) {
									apply(iris[,-5], 2, funcion_medida)
								} )
medidas_iris


# graficamos los datos

# vistazo general
ggpairs(iris, aes(colour = Class)) + theme_minimal()



mostrar_grafico_densidad(iris, "SepalLength")
mostrar_grafico_densidad(iris, "SepalWidth")
mostrar_grafico_densidad(iris, "PetalLength")
mostrar_grafico_densidad(iris, "PetalWidth")


mostrar_boxplot(iris, "SepalLength")
mostrar_boxplot(iris, "SepalWidth")
mostrar_boxplot(iris, "PetalLength")
mostrar_boxplot(iris, "PetalWidth")





par(mfrow=c(2,2))
lapply(colnames(iris[,-5]), function(col_name) { mostrar_grafico_qq(iris[,col_name], col_name) } )
par(mfrow=c(1,1))

# test de normalidad sobre las características
apply(iris[, -5], 2, shapiro.test)

# como podemos ver en los p-value, rechazamos la hipotesis nula de que los datos siguen
# una distribución normal a excepción de la segunda característica, SepalWidth ,por muy poco


matriz_correlaciones_iris <- cor(iris[, -5])

corrplot(matriz_correlaciones_iris, method = "ellipse")








#
# Regresión
#


# leemos el fichero de datos
baseball <- read.csv("data/baseball/baseball.dat", comment.char = "@", header = FALSE)

# vamos a mirar las primeras filas
head(baseball)

# cambiamos los nombres de los atributos, no los ha leido bien el read.csv
names(baseball) <- c("Batting_average", "On-base_percentage", "Runs", "Hits", 
					 "Doubles", "Triples", "HomeRuns", "Runs_batted_in", "Walks",
					 "Strike-Outs", "Stolen_bases", "Errors", 
					 "Free_agency_eligibility", "Free_agent", 
					 "Arbitration_eligibility", "Arbitration", "Salary")

head(baseball)

# miramos cuantas filas tenemos de datos
dim(baseball)

# miramos los tipos de los datos
str(baseball)

summary(baseball)

# el missmap nos modifica los valores por defecto de plot
# y luego el qqplot no se verá bien, así que los restauramos despues del missmap
par_default <- par(mar = c(0, 0, 0, 0))
# miramos si tenemos valores perdidos
missmap(baseball)
par(par_default)

any(is.na(baseball))


# calculamos medidas de interes
# las calculamos sin tener en cuenta la clase de salida

# utilizamos una lista con las medidas de interes
medidas <- list(medias = mean, 
				desviacion_estandar = sd, 
				minimos = min, 
				maximos = max)

# aplicamos cada medida a baseball por columnas
medidas_baseball <- lapply(medidas, function(funcion_medida) {
	apply(baseball, 2, funcion_medida)
} )
medidas_baseball


# graficamos los datos

# vistazo general
ggpairs(baseball) + theme_minimal()

#
## CUIDADO!!! LAS ESCALAS ESTAN LIBRES PORQUE NO SE HAN ESCALADO LOS DATOS
#
# He decidido no escalarlos de cada a ver los valores reales de los atributos, así
# que hay que mirar cada grafico por separado
ggplot(baseball %>% gather(), aes(x = value)) +
	geom_boxplot() + 
	coord_flip() +
	facet_wrap(~key, scales = "free")


ggplot(baseball %>% gather, aes(x = value)) +
	geom_density(alpha = 0.5, fill = "red") + 
	facet_wrap(~key, scales = "free")



# tenemos 17 variables, hacemos primero 9 y luego las 8 restantes
par(mfrow=c(3,3))
lapply(colnames(baseball[1:9]), function(col_name) { mostrar_grafico_qq(baseball[,col_name], col_name) } )
lapply(colnames(baseball[10:17]), function(col_name) { mostrar_grafico_qq(baseball[,col_name], col_name) } )
par(mfrow=c(1,1))


# test de normalidad sobre las características
resultados_test_normalidad <- apply(baseball, 2, shapiro.test)

# si su p-value es menor que 0.05 rechazamos la hipotesis de que sigue una distribución normal
sigue_distribucion_normal <- lapply(resultados_test_normalidad, function(x) x$p.value > 0.05)

# los que no rechacen supondremos que siguen una normal
sigue_distribucion_normal

# como podemos ver en los p-value, rechazamos la hipotesis nula de que los datos siguen
# una distribución normal a excepción de la segunda característica, SepalWidth ,por muy poco


matriz_correlaciones_baseball <- cor(baseball)

corrplot(matriz_correlaciones_baseball, method = "ellipse")







