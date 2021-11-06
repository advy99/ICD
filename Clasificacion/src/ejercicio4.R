library(readr)

clasif_train_alumnos <- read_csv("data/clasif_train_alumnos.csv")

clasif_train_alumnos


# comparar lda y qda usando el test de wilcoxon


wilcox.test(clasif_train_alumnos$out_train_lda, clasif_train_alumnos$out_train_qda)

# como vemos obtenemos un 0.46 de p-value, lo que nos indica que no hay diferencias
# significativas entre ambos métodos con los datos utilizados


# comparativa entre knn, lda y qda usando wilcoxon
datos <- as.matrix(clasif_train_alumnos[2:4])


friedman.test(datos)

# el test de friedman también nos dice que no hay diferencias significativas
# entre ninguno de los tres algoritmos

# test de Holm como post-hoc al test de friedman, para buscar cual era el
# algoritmo con diferencias significativas (en este caso no hay, pero
# nos piden hacerlo igualmente)


tam <- dim(datos)
groups <- rep(1:tam[2], each=tam[1])
pairwise.wilcox.test(datos, groups, p.adjust = "holm", paired = TRUE)


# como era de esperar si friedman nos decía que no existian diferencias significativas
# Holm también nos dice que en cada pareja tampoco hay diferencias significativas


