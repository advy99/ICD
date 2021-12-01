library(tidyverse)
library(readr)
library(GGally)
library(corrplot)

#
# Funciones
#

calcular_MSE <- function(valores_reales, valores_predecidos) {
	sqrt(sum((valores_predecidos - valores_reales)^2)/length(valores_reales))
}

calcular_RMSE <- function(valores_reales, valores_predecidos) {
	sqrt(sum((valores_predecidos - valores_reales)^2)/(length(valores_reales)-2))
}

# creamos si es necesario los directorios a usar de salida
directorios_necesarios <- list("out", "out/baseball", "out/baseball/ajuste_lm")

lapply(directorios_necesarios, function(directorio) {if (!dir.exists(directorio)) dir.create(directorio) })


# leemos los datos
baseball <- read.csv("data/baseball/baseball.dat", comment.char = "@", header = FALSE)

# le asignamos los nombres de las columnas
names(baseball) <- c("Batting_average", "On-base_percentage", "Runs", "Hits", 
					 "Doubles", "Triples", "HomeRuns", "Runs_batted_in", "Walks",
					 "Strike-Outs", "Stolen_bases", "Errors", 
					 "Free_agency_eligibility", "Free_agent", 
					 "Arbitration_eligibility", "Arbitration", "Salary")


matriz_correlaciones_baseball <- cor(baseball)
# convertimos la matriz a dataframe 
matriz_correlaciones_baseball <- as.data.frame(matriz_correlaciones_baseball)

# nos quedamos con los cinco predictores más correlados
cinco_mas_correladas <- matriz_correlaciones_baseball %>% select(Salary) %>% # nos quedamos solo con la fila de salario
														  top_n(6) %>% # nos quedamos con las 6 mejores
														  arrange(desc(Salary)) %>% # las ordenamos por correlacion
														  slice(-c(1)) # eliminamos la primera fila (el propio salario)
nombres_cinco_mas_correladas <- row.names(cinco_mas_correladas)
nombres_cinco_mas_correladas

# saco las formulas del salario dependiendo de cada predictor
formulas_mas_correlados <- lapply(nombres_cinco_mas_correladas, function(predictor) {
	as.formula(paste("Salary ~ ", predictor))
})

# aplico la regresión lineal simple de cada formula
fit_lm_baseball <- lapply(formulas_mas_correlados, function(formula_lm) {
	lm(formula_lm, data = baseball)
})

# miramos el summary de todos los ajustes
lapply(fit_lm_baseball, summary)

# calculamos el MSE
lapply(fit_lm_baseball, function (fit) {
	nombre_predictor <- fit$terms[[3]]
	calcular_MSE(baseball$Salary, predict(fit, baseball$nombre_predictor))
})

# calculamos el RMSE
lapply(fit_lm_baseball, function (fit) {
	nombre_predictor <- fit$terms[[3]]
	calcular_RMSE(baseball$Salary, predict(fit, baseball$nombre_predictor))
})


# guardamos en disco un grafico con el ajuste obtenido
lapply(fit_lm_baseball, function (fit) {
	nombre_predictor <- fit$terms[[3]]
	svg(paste("out/baseball/ajuste_lm/ajuste_lm_", nombre_predictor, ".svg", sep = ""))
	plot(as.formula(paste("Salary ~", nombre_predictor) ), baseball)
	abline(fit, col="blue")
	dev.off()
	par(mfrow=c(1,1))
})


#
# Regresión lineal multiple
#

# aplicamos construcción del modelo top-down

fit_lm_multiple <- lm(Salary ~ ., data = baseball)

summary(fit_lm_multiple)

# conseguimos un adjusted R-sqared de 0.6865

# eliminamos la menos significativas: las que tienen mayor p-value
fit_lm_multiple <- lm(Salary ~ . - Doubles, data = baseball)

summary(fit_lm_multiple)

# conseguimos un adjusted R-sqared de 0.6865

# eliminamos la menos significativas

fit_lm_multiple <- lm(Salary ~ . - Doubles - Hits, data = baseball)

summary(fit_lm_multiple)
# conseguimos un adjusted R-sqared de 0.6878

# eliminamos la menos significativas

fit_lm_multiple <- lm(Salary ~ . - Doubles - Hits - Batting_average, data = baseball)

summary(fit_lm_multiple)
# conseguimos un adjusted R-sqared de 0.6881

# eliminamos la menos significativas
fit_lm_multiple <- lm(Salary ~ . - Doubles - Hits - Batting_average - Walks, data = baseball)

summary(fit_lm_multiple)

# conseguimos un adjusted R-sqared de 0.6883

# eliminamos la menos significativas
fit_lm_multiple <- lm(Salary ~ . - Doubles - Hits - Batting_average - Walks - `On-base_percentage`, data = baseball)

summary(fit_lm_multiple)

# conseguimos un adjusted R-sqared de 0.6886

# eliminamos la menos significativas
fit_lm_multiple <- lm(Salary ~ . - Doubles - Hits - Batting_average - Walks - `On-base_percentage` - Triples, data = baseball)

summary(fit_lm_multiple)

# conseguimos un adjusted R-sqared de 0.6883, aunque hemos bajado, no es nada significativo
# seguimos eliminando predictores

# eliminamos la menos significativas
fit_lm_multiple <- lm(Salary ~ . - Doubles - Hits - Batting_average - Walks - `On-base_percentage` - Triples - Arbitration, data = baseball)

summary(fit_lm_multiple)

# conseguimos un adjusted R-sqared de 0.6874, aunque hemos bajado, no es nada significativo
# seguimos eliminando predictores

# eliminamos la menos significativas
fit_lm_multiple <- lm(Salary ~ . - Doubles - Hits - Batting_average - Walks - `On-base_percentage` - Triples - Arbitration - Errors, data = baseball)

summary(fit_lm_multiple)

# conseguimos un adjusted R-sqared de 0.6865, aunque hemos bajado, no es nada significativo
# seguimos eliminando predictores

# eliminamos la menos significativas
fit_lm_multiple <- lm(Salary ~ . - Doubles - Hits - Batting_average - Walks - `On-base_percentage` - Triples - Arbitration - Errors - Runs, data = baseball)

summary(fit_lm_multiple)


# conseguimos un adjusted R-sqared de 0.685, aunque hemos bajado, no es nada significativo
# seguimos eliminando predictores


# A partir de este punto eliminamos características con un p-value < 0.05, por lo que
# podríamos considerarlas significativas, aun así seguiré para probar un modelo simple
#

# eliminamos la menos significativas
fit_lm_multiple <- lm(Salary ~ . - Doubles - Hits - Batting_average - Walks - `On-base_percentage` - Triples - Arbitration - Errors - Runs - Free_agent, data = baseball)

summary(fit_lm_multiple)


# conseguimos un adjusted R-sqared de 0.68, aunque hemos bajado, no es nada significativo
# seguimos eliminando predictores


# eliminamos la menos significativas
fit_lm_multiple <- lm(Salary ~ . - Doubles - Hits - Batting_average - Walks - `On-base_percentage` - Triples - Arbitration - Errors - Runs - Free_agent - HomeRuns, data = baseball)

summary(fit_lm_multiple)

# conseguimos un adjusted R-sqared de 0.67, en este punto hemos bajado un 1% de golpe,
# y eliminando una variable considerada significativa, por lo que el modelo final será el anterior
# a este


# eliminamos la menos significativas
fit_lm_multiple <- lm(Salary ~ . - Doubles - Hits - Batting_average - Walks - `On-base_percentage` - Triples - Arbitration - Errors - Runs - Free_agent, data = baseball)

summary(fit_lm_multiple)

calcular_MSE(baseball$Salary, predict(fit_lm_multiple, baseball))
calcular_RMSE(baseball$Salary, predict(fit_lm_multiple, baseball))

# como vemos, hemos bajado de 920 de MSE y 923 de RMSE a 693 y 695 respectivamente,
# y de un adjusted R-squared de 0.4451 a uno de 0.6806, mejoras bastante significativas


# probamos interacciones: voy  a probar entre predictores que estén correlados entre si, y
# cada uno de ellos con el salario
# pruebo interaccion HomeRuns y Runs en las que batea
fit_lm_multiple_interacciones <- lm(Salary ~ . - Doubles - Hits - Batting_average - Walks - 
									  `On-base_percentage` - Triples - Arbitration - Errors - 
									  Runs - Free_agent + HomeRuns*Runs_batted_in, 
									  data = baseball)

summary(fit_lm_multiple_interacciones)

# como vemos, esta interacción no es importante (aunque por muy poco)

# esta interacción la ha considerado importante, aunque no ha mejorado mucho el R-squared


fit_lm_multiple_interacciones <- lm(Salary ~ . - Doubles - Hits - Batting_average - Walks - 
										`On-base_percentage` - Triples - Arbitration - Errors - 
										Runs - Free_agent + Runs*Hits, 
										data = baseball)

summary(fit_lm_multiple_interacciones)

# eliminamos HomeRuns, ya que con esta interacción ha considerado que no es importante
# ya que la información que aporta seguramente la aporte esta nueva interaccion
fit_lm_multiple_interacciones <- lm(Salary ~ . - Doubles - Hits - Batting_average - Walks - 
										`On-base_percentage` - Triples - Arbitration - Errors - 
										Runs - Free_agent - HomeRuns + Runs*Hits, 
									data = baseball)

summary(fit_lm_multiple_interacciones)

# voy a probar tambien entre batting_average y on-base-percentage, ya que 
# creo que puede existir asociación entre el porcentaje de veces que llega a una base
# y la media de veces que una persona batea
fit_lm_multiple_interacciones <- lm(Salary ~ . - Doubles - Hits - Walks - 
										Triples - Arbitration - Errors - 
										Runs - Free_agent - HomeRuns + 
										Runs*Hits + Batting_average*`On-base_percentage`, 
									data = baseball)

summary(fit_lm_multiple_interacciones)

# no ha funcionado, probamos otra combinacion


fit_lm_multiple_interacciones <- lm(Salary ~ . - Doubles - Hits - Batting_average - Walks - 
										`On-base_percentage` - Triples - Arbitration - Errors - 
										Runs - Free_agent - HomeRuns + Runs*Hits, 
									data = baseball)

summary(fit_lm_multiple_interacciones)
