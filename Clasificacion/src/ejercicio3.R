library(MASS)
library(ISLR)
library(tidyverse)
library(ggplot2)

str(Smarket)
up <- Smarket %>% filter(Direction == "Up")
down <- Smarket %>% filter(Direction == "Down")

# para mostrar todas las qqnorm
plot_qq_norm <- function(values) {
	qqnorm(y = values)
	qqline(y = values)
}

# vemos graficamente si siguen una normal
# up:
par(mfrow=c(2,3))
sapply(up[, 2:6], plot_qq_norm)
par(mfrow=c(1,1))

# down:
par(mfrow=c(2,3))
sapply(down[, 2:6], plot_qq_norm)
par(mfrow=c(1,1))

# vemos como no llegan a seguir una normal por las colas (en especial up), 
# aunque si por los valores centrales

# hacemos test de shapiro-wilk para probar si son normales
sapply(up[,2:6], shapiro.test)
sapply(down[,2:6], shapiro.test)

# rechazamos la hipotesis nula de que sigue una distribución normal en todos
# los Lag, tanto up como down

# miramos que tengan la misma covarianza
covariance <- sapply(Smarket[, 2:6], var)
covariance

ggplot(Smarket %>% pivot_longer(c(Lag1,Lag2, Lag3, Lag4, Lag5)), aes(x=name, y=value)) + geom_boxplot()


# tienen una covarianza muy similar, todas de 1.29, a excepción de Lag5 de 1.31

# Aplicamos LDA
lda.fit <- lda(Direction~Lag1+Lag2+Lag3+Lag4+Lag5, data=Smarket, subset=Year<2005)
lda.fit

# aunque lo apliquemos a todos los Lag, sigue sin funcionar...
# como vimos estos datos estan todos mezclados y no conseguiremos
# un buen resultado
plot(lda.fit, type="both")


# vamos a intentar predecir
Smarket.2005 <- subset(Smarket, Year==2005)
lda.pred <- predict(lda.fit, Smarket.2005)

data.frame(lda.pred)[1:5,]

table(lda.pred$class,Smarket.2005$Direction)
mean(lda.pred$class==Smarket.2005$Direction)

# hemos conseguido subir de una precision del 0.55 a una del 0.58

# con regresion logistica, entre 0.52 y 0.55 de precision
library(caret)

# AVISO: MASS ha pisado el select de dplyr ...
glmFit <- train(Smarket %>% dplyr::select(-c(Direction, Today)), y = Smarket[, "Direction"], method = "glm", 
				preProcess = c("center", "scale"), tuneLength = 10, 
				control=glm.control(maxit=500), trControl = trainControl(method = "cv"))
glmFit


# ambos modelos intentan poner lineas en unos datos que no
# son para nada separables, LDA al menos cambia los ejes con combinaciones lineales
# mejorando un poco el resultado sobre todo en los puntos más 
# alejados del conglomerado central de puntos donde están todos mezclados



# QDA




