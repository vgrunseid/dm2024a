# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("/Users/vivi/Google Drive/ITBA/mineria") # Establezco el Working Directory

# cargo el dataset
dataset <- fread("./datasets/gridsearch.txt")

dataset[,ganancia_promedio := ganancia_promedio/1000000]
# genero el modelo,  aqui se construye el arbol
# quiero predecir clase_ternaria a partir de el resto de las variables
modelo <- rpart(
        formula = "ganancia_promedio ~ .",
        data = dataset, # los datos donde voy a entrenar
        xval = 0,
        cp = -0.1, # esto significa no limitar la complejidad de los splits
        minsplit = 10, # minima cantidad de registros para que se haga el split
        minbucket = 3, # tamaño minimo de una hoja
        maxdepth = 5
) # profundidad maxima del arbol


# grafico el arbol

pdf("arbolito.pdf")

prp(modelo,
        extra = 101, digits = -5,
        branch = 1, type = 4, varlen = 0, faclen = 0)

    dev.off()
    


# # aplico el modelo a los datos nuevos
# prediccion <- predict(
#         object = modelo,
#         newdata = dapply,
#         type = "prob"
#)
