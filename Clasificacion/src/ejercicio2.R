library(ggplot2)
library(caret)
library(tidyverse)

# dataset de Breast cancer
wbcd <- read.csv("https://resources.oreilly.com/examples/9781784393908/raw/ac9fe41596dd42fc3877cfa8ed410dd346c43548/Machine%20Learning%20with%20R,%20Second%20Edition_Code/Chapter%2003/wisc_bc_data.csv")

# Eliminamos el ID y cambiamos las labels de diagnosis
str(wbcd)
wbcd
wbcd <- wbcd %>% select(-id)
wbcd <- wbcd %>% mutate(diagnosis = factor(diagnosis, labels = c("Benign", "Malignant")))
table(wbcd$diagnosis)


glm_fit <- train(wbcd %>% select(-c(diagnosis)), y = wbcd[, "diagnosis"], method = "glm", 
				preProcess = c("center", "scale"), tuneLength = 10, 
				control=glm.control(maxit=500), trControl = trainControl(method = "cv"))

glm_fit
