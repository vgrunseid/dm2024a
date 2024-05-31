# Script para encontrar Visuamente  el data drifting
# focalizado solo en los campos de un buen arbol de deicision
#  para correr en Google Cloud

# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("rpart")
#if (!require("tidyverse")) {
#  install.packages("tidyverse")
#  library("tidyverse")}
#kmes0 <- 202107
#kmes1 <- 202109

future <- c(202109)
final_train <- c(202009)
#final_train <- c(202007, 202006, 202105, 202104,
#                 202103, 202102, 202101, 202012, 202011, 202010, 202009)


#------------------------------------------------------------------------------

graficar_campo <- function(campo) {
  # quito de grafico las colas del 5% de las densidades
  qA <- quantile(dataset[foto_mes %in% final_train, get(campo)],
                 prob = c(0.05, 0.95), na.rm = TRUE
  )
  
  qB <- quantile(dataset[foto_mes %in% future, get(campo)],
                 prob = c(0.05, 0.95), na.rm = TRUE
  )
  
  xxmin <- pmin(qA[[1]], qB[[1]])
  xxmax <- pmax(qA[[2]], qB[[2]])
  
  densidad_A <- density(dataset[foto_mes %in% final_train, get(campo)],
                        kernel = "gaussian", na.rm = TRUE
  )
  
  densidad_B <- density(dataset[foto_mes %in% future, get(campo)],
                        kernel = "gaussian", na.rm = TRUE
  )
  
  plot(densidad_A,
       col = "blue",
       xlim = c(xxmin, xxmax),
       ylim = c(0, pmax(max(densidad_A$y), max(densidad_B$y))),
       main = campo
  )
  
  lines(densidad_B, col = "red", lty = 2)
  
  legend("topright",
         legend = c("training", "test"),
         col = c("blue", "red"), lty = c(1, 2)
  )
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui comienza el programa
setwd("~/buckets/b1/") # Establezco el Working Directory

# cargo el dataset donde voy a entrenar
#dataset <- fread("~/buckets/b1/expw/DT-0001/dataset.csv.gz")
dataset <- fread("~/buckets/b1/expw/DR-0001/dataset.csv.gz")
#dataset <- fread("~/buckets/b1/expw/DR-0002/dataset.csv.gz")
#DT_incorporar_dataset_competencia2024()

dir.create("./exp/", showWarnings = FALSE)
dir.create("./exp/DR3610/", showWarnings = FALSE)
setwd("./exp/DR3610/")

#final_train <- as.vector(dataset %>% distinct(foto_mes))$foto_mes

future <- c(202109)
#final_train <- c(202007, 202006, 202105, 202104,
#                             202103, 202102, 202101, 202012, 202011, 202010, 202009)
final_train <- c(202009)

#dataset <- dataset[foto_mes %in% final_train]

# creo la clase_binaria SI={ BAJA+1, BAJA+2 }    NO={ CONTINUA }
dataset[
  foto_mes %in% final_train,
  clase_binaria := ifelse(clase_ternaria == "CONTINUA", "NEG", "POS")
]

# Entreno el modelo
# utilizo los mejores hiperparametros encontrados
# en una Bayesian Optimizationcon 5-fold Cross Validation
modelo <- rpart(
  formula = "clase_binaria ~ . -clase_ternaria",
  data = dataset[foto_mes %in% final_train], # los datos donde voy a entrenar
  xval = 0,
  cp = -0.67,
  minsplit = 1144,
  minbucket = 539,
  maxdepth = 8
)


campos_modelo <- names(modelo$variable.importance)
campos_buenos <- c(campos_modelo, setdiff(colnames(dataset), campos_modelo))
campos_buenos <- setdiff(
  campos_buenos,
  c("foto_mes", "clase_ternaria", "clase_binaria")
)
campos_buenos <- campos_buenos[campos_buenos %like%
                                 "^(m|Visa_m|Master_m|vm_m)"]



pdf( paste0("densidades_entrenamiento_testing mes09 - DR01.pdf") )

for (campo in campos_buenos) {
  cat(campo, "  ")
  graficar_campo(campo)
}

dev.off()
