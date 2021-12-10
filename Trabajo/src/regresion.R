library(tidyverse)
library(readr)
library(GGally)
library(corrplot)
library(kknn)
library(readr)

set.seed(2)

#
# Funciones
#

calcular_MSE <- function(valores_reales, valores_predecidos) {
	sum((valores_predecidos - valores_reales)^2)/length(valores_reales)
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
														  top_n(6) %>% # nos quedamos con las 6 mejores (son todas positivas)
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

# como vemos, hemos bajado de 848113.1 de MSE y 923 de RMSE a 480894.9 y 695 respectivamente,
# y de un adjusted R-squared de 0.4451 a uno de 0.6806, mejoras bastante significativas


# probamos interacciones: voy  a probar entre predictores que estén correlados entre si, y
# cada uno de ellos con el salario
# pruebo interaccion Free_agency_eligibility y Runs_batted_in 
fit_lm_multiple_interacciones <- lm(Salary ~ . - Doubles - Hits - Batting_average - Walks - 
									  `On-base_percentage` - Triples - Arbitration - Errors - 
									  Runs - Free_agent + Free_agency_eligibility*Runs_batted_in, 
									  data = baseball)

summary(fit_lm_multiple_interacciones)


# probamos la interaccion entre Runs y Hits, ya que tiene sentido que estas dos
# tengan relacion con el salario
fit_lm_multiple_interacciones <- lm(Salary ~ . - Doubles - Batting_average - Walks - 
										`On-base_percentage` - Triples - Arbitration - Errors - 
										 Free_agent + Free_agency_eligibility*Runs_batted_in + Runs*Hits, 
										data = baseball)
# esta interacción la ha considerado importante, aunque no ha mejorado mucho el R-squared


summary(fit_lm_multiple_interacciones)

# eliminamos HomeRuns, ya que con esta interacción ha considerado que no es importante
# ya que la información que aporta seguramente la aporte esta nueva interaccion
fit_lm_multiple_interacciones <- lm(Salary ~ . - Doubles - Batting_average - Walks - 
										`On-base_percentage` - Triples - Arbitration - Errors - 
										 Free_agent - HomeRuns + Free_agency_eligibility*Runs_batted_in + Runs*Hits, 
										data = baseball)

summary(fit_lm_multiple_interacciones)

# voy a probar tambien entre batting_average y on-base-percentage, ya que 
# creo que puede existir asociación entre el porcentaje de veces que llega a una base
# y la media de veces que una persona batea
fit_lm_multiple_interacciones <- lm(Salary ~ . - Doubles - Walks - 
										Triples - Arbitration - Errors - 
										Free_agent - HomeRuns + Free_agency_eligibility*Runs_batted_in + 
										Runs*Hits + Batting_average*`On-base_percentage`, 
										data = baseball)

summary(fit_lm_multiple_interacciones)


# pruebo tambien Runs_batted_in con Arbitration_eligibility
fit_lm_multiple_interacciones <- lm(Salary ~ . - Doubles - Batting_average - Walks - 
										`On-base_percentage` - Triples - Arbitration - Errors - 
										Free_agent - HomeRuns + Runs*Hits + 
										Free_agency_eligibility * Runs_batted_in +
										Arbitration_eligibility * Runs_batted_in, 
										data = baseball)

summary(fit_lm_multiple_interacciones)

# tambien ha funcionado bien, r^2 pasa de 0.71 a 0.7367

# calculamos el MSE y RMSE con las interacciones
calcular_MSE(baseball$Salary, predict(fit_lm_multiple_interacciones, baseball))
calcular_RMSE(baseball$Salary, predict(fit_lm_multiple_interacciones, baseball))

# hemos bajado de 480894.9 y 695 MSE y RMSE respectivamente a 391673.8 y 627.7039,
# y de un adjusted R-squared de 0.6806 a uno de 0.7367, mejoras bastante significativas


# pasamos a probar no linealidad

# voy a probar con algunos predictores que no funcionaron de forma lineal
fit_lm_multiple_no_linealidad <- lm(Salary ~ . + Doubles - Batting_average - Walks - 
										`On-base_percentage` - Triples - Arbitration - Errors - 
										Free_agent - HomeRuns + Runs*Hits + 
										Free_agency_eligibility * Runs_batted_in +
										Arbitration_eligibility * Runs_batted_in +
										I(Doubles^2), 
										data = baseball)

summary(fit_lm_multiple_no_linealidad)

# tras introducir el termino de no linealidad con doubles este predictor si ha pasado a tener importancia


fit_lm_multiple_no_linealidad <- lm(Salary ~ . + Doubles - Batting_average - Walks - 
										`On-base_percentage` - Triples - Arbitration - Errors - 
										Free_agent - HomeRuns + Runs*Hits + 
										Free_agency_eligibility * Runs_batted_in +
										Arbitration_eligibility * Runs_batted_in +
										I(Doubles^3 + Doubles^2) , 
									data = baseball)

summary(fit_lm_multiple_no_linealidad)

# si en lugar de al cuadrado introducimos la caracteristica al cubo mejoramos
# un poco más el adjusted R-squared hasta llegar al 0.7414

# probamos batting average
fit_lm_multiple_no_linealidad <- lm(Salary ~ . + Doubles + Batting_average - Walks - 
										`On-base_percentage` - Triples - Arbitration - Errors - 
										Free_agent - HomeRuns + Runs*Hits + 
										Free_agency_eligibility * Runs_batted_in +
										Arbitration_eligibility * Runs_batted_in +
										I(Doubles^3 + Doubles^2) +
										I(Batting_average^2), 
									data = baseball)

summary(fit_lm_multiple_no_linealidad)

# como vemos no ha funcionado, así que lo eliminamos
# y como doubles ha funcionado, vamos a probar con los triples

fit_lm_multiple_no_linealidad <- lm(Salary ~ . + Doubles - Batting_average - Walks - 
										`On-base_percentage` + Triples - Arbitration - Errors - 
										Free_agent - HomeRuns + Runs*Hits + 
										Free_agency_eligibility * Runs_batted_in +
										Arbitration_eligibility * Runs_batted_in +
										I(Doubles^3 + Doubles^2) + 
										I(Triples^2), 
									data = baseball)

summary(fit_lm_multiple_no_linealidad)

# como vemos este tampoco ha funcionado

# probamos con Errors
fit_lm_multiple_no_linealidad <- lm(Salary ~ . + Doubles - Batting_average - Walks - 
										`On-base_percentage` - Triples - Arbitration + Errors - 
										Free_agent - HomeRuns + Runs*Hits + 
										Free_agency_eligibility * Runs_batted_in +
										Arbitration_eligibility * Runs_batted_in +
										I(Doubles^3 + Doubles^2) + 
										I(Errors^2), 
									data = baseball)

summary(fit_lm_multiple_no_linealidad)

# si lo considera significativo, subiendo el adjusted R-squared a 0.7499

# probamos con Errors al cubo
fit_lm_multiple_no_linealidad <- lm(Salary ~ . + Doubles - Batting_average - Walks - 
										`On-base_percentage` - Triples - Arbitration + Errors - 
										Free_agent - HomeRuns + Runs*Hits + 
										Free_agency_eligibility * Runs_batted_in +
										Arbitration_eligibility * Runs_batted_in +
										I(Doubles^3 + Doubles^2) + 
										I(Errors^2 + Errors^3), 
									data = baseball)

summary(fit_lm_multiple_no_linealidad)

# mejora un poco, pero nada significativo

# este será nuestro modelo final
calcular_MSE(baseball$Salary, predict(fit_lm_multiple_no_linealidad, baseball))
calcular_RMSE(baseball$Salary, predict(fit_lm_multiple_no_linealidad, baseball))

# hemos bajado de 391673.8 y 627.7039 MSE y RMSE respectivamente a 367189.1 y 607.7675,
# y de un adjusted R-squared de 0.7367 a uno de 0.7501, mejoras bastante significativas


# predecimos las del mejor modelo
predicciones_train <- predict(fit_lm_multiple_no_linealidad, baseball)

ggplot(baseball %>% mutate(Salary_predicho = predicciones_train) %>% pivot_longer(Salary:Salary_predicho), aes(x = Runs_batted_in, y = value, color = name)) +
	geom_point() + 
	labs(title = "Predicciones utilizando el modelo lineal\n múltiple con interacciones y no linealidad",
		 subtitle = "Utilizando característica con mayor correlación con el salario.") +
	ylab("Salary")
ggsave("out/baseball/predicciones_lm_completo.svg", device = svg, width = 1920, height = 1080, units = "px", dpi = 150)


#
# KNN
#

# por defecto kernel = optimal y scale = TRUE
fit_knn <- kknn(Salary ~ ., baseball, baseball)

calcular_MSE(baseball$Salary, fit_knn$fitted.values)
calcular_RMSE(baseball$Salary, fit_knn$fitted.values)

fit_knn_mejor_lm <- fit_knn <- kknn(Salary ~ . + Doubles - Batting_average - Walks - 
										`On-base_percentage` - Triples - Arbitration + Errors - 
										Free_agent - HomeRuns + Runs*Hits + 
										Free_agency_eligibility * Runs_batted_in +
										Arbitration_eligibility * Runs_batted_in +
										I(Doubles^3 + Doubles^2) + 
										I(Errors^2 + Errors^3), 
									baseball, baseball)

calcular_MSE(baseball$Salary, fit_knn_mejor_lm$fitted.values)
calcular_RMSE(baseball$Salary, fit_knn_mejor_lm$fitted.values)

# como vemos la opción con la formula de lm es algo mejor, pero tampoco mucho


ggplot(baseball %>% mutate(Salary_predicho = fit_knn_mejor_lm$fitted.values) %>% pivot_longer(Salary:Salary_predicho), aes(x = Runs_batted_in, y = value, color = name)) +
	geom_point() + 
	labs(title = "Predicciones utilizando KNN",
		 subtitle = "Utilizando característica con mayor correlación con el salario.") +
	ylab("Salary")
ggsave("out/baseball/predicciones_knn.svg", device = svg, width = 1920, height = 1080, units = "px", dpi = 150)



nombre <- "data/baseball/baseball"

run_lm_fold <- function(i, x, tt = "test") {
	file <- paste(x, "-5-", i, "tra.dat", sep="")
	x_tra <- read.csv(file, comment.char="@", header=FALSE)
	file <- paste(x, "-5-", i, "tst.dat", sep="")
	x_tst <- read.csv(file, comment.char="@", header=FALSE)
	names(x_tra) <- c("Batting_average", "On-base_percentage", "Runs", "Hits", 
						 "Doubles", "Triples", "HomeRuns", "Runs_batted_in", "Walks",
						 "Strike-Outs", "Stolen_bases", "Errors", 
						 "Free_agency_eligibility", "Free_agent", 
						 "Arbitration_eligibility", "Arbitration", "Salary")
	
	names(x_tst) <- c("Batting_average", "On-base_percentage", "Runs", "Hits", 
					  "Doubles", "Triples", "HomeRuns", "Runs_batted_in", "Walks",
					  "Strike-Outs", "Stolen_bases", "Errors", 
					  "Free_agency_eligibility", "Free_agent", 
					  "Arbitration_eligibility", "Arbitration", "Salary")
	
	if (tt == "train") {
		test <- x_tra
	}
	else {
		test <- x_tst
	}
	
	fitMulti=lm(Salary ~ . + Doubles - Batting_average - Walks - 
					`On-base_percentage` - Triples - Arbitration + Errors - 
					Free_agent - HomeRuns + Runs*Hits + 
					Free_agency_eligibility * Runs_batted_in +
					Arbitration_eligibility * Runs_batted_in +
					I(Doubles^3 + Doubles^2) + 
					I(Errors^2 + Errors^3), data = x_tra)
	
	yprime=predict(fitMulti,test)
	sum(abs(test$Salary-yprime)^2)/length(yprime) ##MSE
}

lmMSEtrain<-mean(sapply(1:5,run_lm_fold,nombre,"train"))
lmMSEtest<-mean(sapply(1:5,run_lm_fold,nombre,"test"))



run_knn_fold <- function(i, x, tt = "test") {
	file <- paste(x, "-5-", i, "tra.dat", sep="")
	x_tra <- read.csv(file, comment.char="@", header=FALSE)
	file <- paste(x, "-5-", i, "tst.dat", sep="")
	x_tst <- read.csv(file, comment.char="@", header=FALSE)
	In <- length(names(x_tra)) - 1
	names(x_tra) <- c("Batting_average", "On-base_percentage", "Runs", "Hits", 
					  "Doubles", "Triples", "HomeRuns", "Runs_batted_in", "Walks",
					  "Strike-Outs", "Stolen_bases", "Errors", 
					  "Free_agency_eligibility", "Free_agent", 
					  "Arbitration_eligibility", "Arbitration", "Salary")
	
	names(x_tst) <- c("Batting_average", "On-base_percentage", "Runs", "Hits", 
					  "Doubles", "Triples", "HomeRuns", "Runs_batted_in", "Walks",
					  "Strike-Outs", "Stolen_bases", "Errors", 
					  "Free_agency_eligibility", "Free_agent", 
					  "Arbitration_eligibility", "Arbitration", "Salary")
	
	if (tt == "train") {
		test <- x_tra
	}
	else {
		test <- x_tst
	}
	
	fitMulti=kknn(Salary ~ . + Doubles - Batting_average - Walks - 
					`On-base_percentage` - Triples - Arbitration + Errors - 
					Free_agent - HomeRuns + Runs*Hits + 
					Free_agency_eligibility * Runs_batted_in +
					Arbitration_eligibility * Runs_batted_in +
					I(Doubles^3 + Doubles^2) + 
					I(Errors^2 + Errors^3),x_tra, test)
	
	yprime=fitMulti$fitted.values
	sum(abs(test$Salary - yprime)^2)/length(yprime) ##MSE
}

knnMSEtrain <- mean(sapply(1:5,run_knn_fold,nombre,"train"))
knnMSEtest <- mean(sapply(1:5,run_knn_fold,nombre,"test"))


lmMSEtrain
lmMSEtest

knnMSEtrain
knnMSEtest

tabla_resultados_test <- read.csv("data/regr_test_alumnos.csv")
tabla_resultados_test

tabla_resultados_test[5, 2] <- lmMSEtest
tabla_resultados_test[5, 3] <- knnMSEtest
tabla_resultados_test

tabla_resultados_train <- read.csv("data/regr_train_alumnos.csv")
tabla_resultados_train

tabla_resultados_train[5, 2] <- lmMSEtrain
tabla_resultados_train[5, 3] <- knnMSEtrain
tabla_resultados_train

#
# Comparativa LM y KNN con los resultados obtenidos
# 

tablatst <- cbind(tabla_resultados_test[,2:dim(tabla_resultados_test)[2]])
colnames(tablatst) <- names(tabla_resultados_test)[2:dim(tabla_resultados_test)[2]]
rownames(tablatst) <- tabla_resultados_test[,1]

# sacamos las diferencias usando la tabla de test
difs <- (tablatst[,1] - tablatst[,2]) / tablatst[,1]
wilc_1_2 <- cbind(ifelse (difs<0, abs(difs)+0.1, 0+0.1), ifelse (difs>0, abs(difs)+0.1, 0+0.1))
colnames(wilc_1_2) <- c(colnames(tablatst)[1], colnames(tablatst)[2])
head(wilc_1_2)

LMvsKNNtst <- wilcox.test(wilc_1_2[,1], wilc_1_2[,2], alternative = "two.sided", paired=TRUE)
Rmas <- LMvsKNNtst$statistic
pvalue <- LMvsKNNtst$p.value
LMvsKNNtst <- wilcox.test(wilc_1_2[,2], wilc_1_2[,1], alternative = "two.sided", paired=TRUE)
Rmenos <- LMvsKNNtst$statistic
Rmas
Rmenos
pvalue

# como el p-value es tan alto, no podemos decir que haya diferencias significativas
# entre LM y KNN para los conjuntos de datos que hemos usado


#
# Comparativa de los tres algoritmos usando Friedman y Holms
#

test_friedman <- friedman.test(as.matrix(tablatst))
test_friedman

# el test de friedman nos dice que con un nivel de confianza de más del 98%,
# si hay diferencias significativas entre alguno de los tres metodos

tam <- dim(tablatst)
groups <- rep(1:tam[2], each=tam[1])
pairwise.wilcox.test(as.matrix(tablatst), groups, p.adjust = "holm", paired = TRUE)

# con este resultado podemos concluir que hay diferencias significativas entre 
# el tercer y el primer algoritmo (a favor del tercer algoritmo),
# pero no entre el primero y el segundo y el segundo y el terceo (aunque con este último
# si que las hay si el nivel de confianza es del 90% y no del 5%)
# Por lo tanto, el tercer algoritmo (M5) es mejor que el primero, pero con el 
# segundo no tenemos tan claro una mejora
