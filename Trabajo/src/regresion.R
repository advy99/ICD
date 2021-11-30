library(tidyverse)
library(readr)


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






