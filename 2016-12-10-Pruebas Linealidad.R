# title: Pruebas rotación
#author: "LACV"
#date: "10 de Diciembre de 2016"

require(caret)
require(pander)
require(stats)
require(mclust)

require(car)
require(rgl)
require(RColorBrewer)


require(MASS)
require(svd)
require(Matrix)
require(dclone)
require(MASS)

######################################################################################
# ensure the results are repeatable
set.seed(7)

###########################################################################################
# Establecer el directorio y cargar el dataset
setwd("D:/Usuarios/Lcalvo/Documents/Cambios/Doctorado/CORRIDAS DEL PROGRAMA/Ver Linealidad")

#tDatosOriginal = read.table("2010-10-01-Sigatoka-LaRita-1a1 - X_Y.csv", header = TRUE, sep =';')
#tNombreDatos = "Sigatoka_La_Rita"

tDatosOriginal = read.table("2010-10-01-Sigatoka-28Millas-1a1 - X_Y.csv", header = TRUE, sep =';')
tNombreDatos = "Sigatoka_28_Millas"

#tDatosOriginal = read.table("2010-10-01-Roya-Barva-1a1 - X_Y.csv", header = TRUE, sep =';')
#tNombreDatos = "Roya_Barva"

#tDatosOriginal = read.table("2010-10-01-Roya-Poas-1a1 - X_Y.csv", header = TRUE, sep =';')
#tNombreDatos = "Roya_Poas"

#tDatosOriginal = read.table("2010-10-01-Roya-SanVito-1a1 - X_Y.csv", header = TRUE, sep =';')
#tNombreDatos = "Roya_SanVito"

#tDatosOriginal = read.table("2016-12-04 28_Millas_EmbolseHa-1a1 - X_Y.csv", header = TRUE, sep =';')
#tNombreDatos = "Floracion_EH_28_Millas"

#tDatosOriginal = read.table("2016-12-04 28_Millas_PesoNetoKilo-1a1 - X_Y.csv", header = TRUE, sep =';')
#tNombreDatos = "Floracion_PNK_28_Millas"

#tDatosOriginal = read.table("2016-12-04 28_Millas_RacimosCortados-1a1 - X_Y.csv", header = TRUE, sep =';')
#tNombreDatos = "Floracion_RC_28_Millas"


###########################################################################################
## cantidad de columnas
tNumColumnas = length(colnames(tDatosOriginal))
tNumAtributos = tNumColumnas - 1
print( tNumColumnas)

###########################################################################################
## Se escalan y centran los features - package caret
# The "range" transformation scales the data to be within [0, 1]. If new samples have values larger or
#smaller than those in the training set, values will be outside of this range. c("center", "scale")
tDatosPreProc = preProcess(tDatosOriginal[1:tNumColumnas-1], method = c("range"))
tDatos = predict(tDatosPreProc, tDatosOriginal[1:tNumColumnas-1])
print(length(tDatos))

###########################################################################################
# Agrega al final de los x's una columna de 1's por el vias
tDatos$bias = rep(1,nrow(tDatos))
colnames(tDatos)[tNumColumnas] = paste("x" , toString(tNumColumnas), sep="")

###########################################################################################
# Une los atributos escalados a y  sin escalar.
y = tDatosOriginal$y
tDatos = cbind(tDatos, y)
print(length(tDatos))
head(tDatos)

###########################################################################################
## cantidad de columnas
tNumColumnas = length(colnames(tDatos))
tNumAtributos = tNumColumnas - 1
print( tNumColumnas)

###########################################################################################
## Nombres de las columnas
colnames(tDatos)

###########################################################################################
## Visualizar y
plot(tDatos$y, type="b")

###########################################################################################
## Regresión Lineal
# package stats por function
totVariables = paste0("x", 1:tNumAtributos)
fmla = as.formula(paste("y ~ ", paste(totVariables, collapse= "+")))
lmfit = lm(fmla, tDatos)
par(mfrow=c(2,2))
plot(lmfit)
summary(lmfit)
mse_regresion = mean(lmfit$residuals^2)
print(mse_regresion)

###########################################################################################
## SVD - singular value decomposition
# package svd   and  Matrix  MASS
totColDatos = ncol(tDatos)
A = tDatos[1:totColDatos-1]
tFilasSVD = nrow(A)
tColumnasSVD = ncol(A)
tMatriz = as.matrix(A)
El_svd =  propack.svd(tMatriz, neig = min(tFilasSVD, tColumnasSVD), opts = list())
Eldato = El_svd[1]$d
for(i in 1:tColumnasSVD) print(Eldato[i])
print(nrow(El_svd$v))
print(ncol(El_svd$v))
print(nrow(El_svd$u))
print(ncol(El_svd$u))
print(length(tDatos$y))
w = El_svd$v %*% solve(diag(El_svd$d)) %*% t(El_svd$u) %*%  tDatos$y
print(w)

###########################################################################################
## Se calcula el error con el w obtenido
filDatos = nrow(tDatos)
colDatos = ncol(tDatos)
errorAw = 0
for (i in 1:filDatos) {
  unaFila = tDatos[i,]
  unaFila = as.matrix(unaFila[1:colDatos-1])
  y_Aw = unaFila  %*% w
  diferencia = (y_Aw - tDatos$y[i])^2
  errorAw = errorAw + diferencia
}
mse_Aw = errorAw/filDatos

###########################################################################################
## Comparación de los mse obtenidos
print("mse obtenido en la regresión de mínimos cuadrados: ")
print(mse_regresion)
print("mse obtenido con Aw = y: ")
print(mse_Aw)

###########################################################################################
## Revisión de subespacios
if (tNombreDatos == "Sigatoka_La_Rita") {
  listaDejar = c("x5", "x11","y")
} else
  if (tNombreDatos == "Sigatoka_28_Millas") {
    listaDejar = c("x3", "x11","y")
  } else
    if (tNombreDatos == "Roya_Barva") {
      listaDejar = c("x10", "x11","y")
    } else
      if (tNombreDatos == "Roya_Poas") {
        listaDejar = c("x10", "x11","y")
      } else
        if (tNombreDatos == "Roya_SanVito") {
          listaDejar = c("x10", "x11","y")
        } else
          if (tNombreDatos == "Floracion_EH_28_Millas") {
            listaDejar = c("x1", "x2","y")            
          } else
            if (tNombreDatos == "Floracion_PNK_28_Millas") {
              listaDejar = c("x5", "x10","y")            
            } else
              if (tNombreDatos == "Floracion_RC_28_Millas") {
                listaDejar = c("x3", "x10","y")            
              }

tEtiquetaX = listaDejar[1]
tEtiquetaZ = listaDejar[2]
tEtiquetaY = listaDejar[3]
tDatosReducida =  tDatos[ , which(names(tDatos) %in% listaDejar)]
filDatosR = nrow(tDatosReducida)
colDatosR = ncol(tDatosReducida)
head(tDatosReducida)

tMetodoClasificacion = "cortes"   # "cortes" o "clusters"
if (tMetodoClasificacion == "cortes") {
  #################################################################################
  # Con cortes
  #################################################################################
  numeroCortes = 2
  dimensiones = length(listaDejar) - 1 # No incluir a 'y'
  totalCortes = numeroCortes^dimensiones
  puntoDivision = 1.0 / numeroCortes
  subEspacios = list()
  tDatosReducida$clase = rep(0,nrow(tDatosReducida))
  tNumSubEspacios = totalCortes
  for (i in 1:totalCortes) {
    subEspacios[[i]] = list()
  }
  print(subEspacios)
  
  for (fila in 1:filDatosR) {
    tBrinque = FALSE
    for(cCorte in 1:numeroCortes) {
      if (tDatosReducida[fila, listaDejar[1]] <= (puntoDivision*cCorte)) {
        for (cDim in 1:numeroCortes) {
          if (tDatosReducida[fila, listaDejar[2]] <= (puntoDivision*cDim))  {
            if(tDatosReducida$clase[fila] != 0)
            {
              print("Error")
              print(tDatosReducida$clase[fila])
              print("===")
              xxx
            }
            subEspacios[[cDim+((cCorte-1)*numeroCortes)]] = c( subEspacios[[cDim+((cCorte-1)*numeroCortes)]], fila )
            tDatosReducida$clase[fila] = cDim+((cCorte-1)*numeroCortes)
            tBrinque = TRUE
            break
          }
        }
      } 
      if (tBrinque) {
        break
      }          
    }
  }
  tTotal = 0
  for ( i in 1 : totalCortes) 
  {
    print(i)
    print(length(subEspacios[[i]]))
    tTotal = tTotal + length(subEspacios[[i]])
  }
  if (tTotal != filDatosR) {
    print("Revisar, alguna fila no fue incluida >>>>>>>>>>>>")
  }
} else   # sigue si son "clusters"
{
  #################################################################################
  # Con clusters
  #################################################################################
  tMetodoClustering = "mclust"  # kmeans  ward.D2  mclust
  tNumSubEspacios = 4  # en si, el numero de clusters
  if (tMetodoClustering == "kmeans") {
    grupos = kmeans(tDatosReducida, tNumSubEspacios)
    tDatosReducida = cbind(tDatosReducida, grupos[1])  
  } else
    if (tMetodoClustering == "ward.D2") {
      tDistancia = dist(tDatosReducida, method = "euclidean") # distance matrix
      tFit = hclust(tDistancia, method="ward.D2") 
      grupos = cutree(tFit, k=tNumSubEspacios)
      tDatosReducida = cbind(tDatosReducida, grupos)  
      
    } else
    if (tMetodoClustering == "mclust") {
      tFit = Mclust(tDatosReducida)
      grupos = tFit$classification
      tDatosReducida = cbind(tDatosReducida, grupos)  
    } 
}

##############################################################
colnames(tDatosReducida) = c("x", "y", "z", "clase")
tDatosReducida$clase = factor(tDatosReducida$clase)
head (tDatosReducida)

#########################################################################
# Preparación para graficar
#########################################################################


colores = c("blue", "red", "green", "darkmagenta", "orange", "brown", 
  "cyan", "darkblue", "gold",  "darkorchid3", "darkolivegreen4", "limegreen", 
  "maroon", "navy",  "orchid", "salmon", "sienna", "steelblue",
  "turquoise", "violet", "wheat", "whitesmoke", "yellow", "yellowgreen",
  "darkblue", "darkcyan", "darkgoldenrod", "darkgoldenrod1", "darkgoldenrod2",
  "darkgoldenrod3", "darkgoldenrod4", "darkgray", "darkgreen", "darkgrey",
  "darkkhaki",  "darkolivegreen", "darkolivegreen1", 
  "darkolivegreen2", "darkolivegreen3",  "darkorange",
  "darkorange1", "darkorange2", "darkorange3", "darkorange4", "darkorchid",
  "darkorchid1", "darkorchid2",  "darkorchid4", "darkred",
  "darksalmon", "darkseagreen", "darkseagreen1", "darkseagreen2", "darkseagreen3",
  "darkseagreen4", "darkslateblue", "darkslategray" )

# fit are: "linear", "quadratic", "smooth" and "additive"  
# Se escala y entre 0 y 1 para mejorar la graficación
tDatosPreProc = preProcess(tDatosReducida[3:3], method = c("range"))
tDatosReducida[3] = predict(tDatosPreProc, tDatosReducida[3:3])
head(tDatosReducida)

#########################################################################
# Graficar
#########################################################################
# planes3d and rgl.planes draw planes using the parametrization 
# ax + by + cz + d = 0.
# conocer el máximo de clipPlanes con   par3d("maxClipPlanes")
# smooth es de stats package
tUseSpline = TRUE
tGradoSpline = 5
tGrafinePlanoGeneral = FALSE
tMinPuntosParaGraficar = 5
tCuantasPudoGraficar = 0
open3d()
# plot3d es de rgl package
plot3d(x=tDatosReducida$x, y=tDatosReducida$y, z=tDatosReducida$z,
       type="p", col = "red", size = 3)  # , site=5, lwd=15
par(mfrow=c(2,2))
for (i in 1:tNumSubEspacios) {
  losDatos = tDatosReducida[which(tDatosReducida$clase == i),]  
  largo = nrow(losDatos)  
  ###################
  print(largo)
  ###################
  if (largo >= tMinPuntosParaGraficar ) {
    tCuantasPudoGraficar = tCuantasPudoGraficar + 1
    tFit = lm(losDatos$z ~  losDatos$x + losDatos$y)
    
    plot(tFit)
    summary(tFit)
    mse_regresionP = mean(tFit$residuals^2)
    print(mse_regresionP)
    
    
    tCoefs = coef(tFit)
    a = tCoefs["losDatos$x"]
    b = tCoefs["losDatos$y"]
    c = -1
    d = tCoefs["(Intercept)"]
    grid.lines = 50
    x.pred = seq(min(losDatos$x), max(losDatos$x), length.out = grid.lines)
    y.pred = seq(min(losDatos$y), max(losDatos$y), length.out = grid.lines)
    xy = expand.grid( x = x.pred, y = y.pred)
    tTemp = xy$x * a + xy$y* b + d
    if (tUseSpline == TRUE) {
      tSplineAnt = smooth.spline(tTemp, df=tGradoSpline)
      # Este contiene los grados de libertad (df). Si no se indica con
      # el parámetro (df), se autocalcula con Cross Validation (CV)
      tSpline = as.numeric(unlist(tSplineAnt[2]))
      tTemp = tSpline
    }
    z.pred = matrix(tTemp, nrow = grid.lines, ncol = grid.lines)
    rgl.surface(x=x.pred, y=z.pred, z=y.pred, color = colores[i], alpha = 0.5, 
                coords=1:3, lit = FALSE, size=50, lwd=200)
    Sys.sleep(10)  # Solo para ir viendo la graficación
  } # if
} # for
if ((tGrafinePlanoGeneral == TRUE) |  (tCuantasPudoGraficar < 2)) {
  tFit2 = lm(losDatos$z ~  losDatos$x + losDatos$y)
  tCoefs = coef(tFit2)
  a = tCoefs["losDatos$x"]
  b = tCoefs["losDatos$y"]
  c = -1
  d = tCoefs["(Intercept)"]
  planes3d(a, b, c, d, alpha = 0.5, col = colores[16])
}

#rgl.snapshot(filename = paste(tNombreDatos,"_2016-12-09_cluster" ,".png", sep=""))

#########################################################################
# Guardar la tabla con valores
#########################################################################
#write.table(tDatosReducida, file="2016-12-06-Archivo reducido.csv", sep=";")


#########################################################################
# Fin
#########################################################################
