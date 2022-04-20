# Proyecto de Segundo Parcial de Modelos Estadísticos Aplicados I.
# Christian Salas M., Karla Silva T., John Borbor M.


library(readxl)
library(openxlsx)
library(httr)
library(psych)
library(car)
library(olsrr)
library(MASS)
library(leaps)
library(gvlma)

# PARTE 1. Dataset y Problema de Investigación ----------------------------


github_link <- 'https://github.com/christianjaviersalasmarquez/Proyecto_ModelosEstadisticosAplicadosI/raw/main/RealEstateValuation_Regression_Dataset/Real%20estate%20valuation%20data%20set.xlsx'

temp_file <- tempfile(fileext = ".xlsx")

req <- GET(github_link, write_disk(path = temp_file))

datos_RealEstate <- readxl::read_excel(temp_file)

unlink(temp_file)

# ESTRUCTURA
str(datos_RealEstate)

# SEPARACIÓN DE VARIABLES
x1 <- datos_RealEstate$`X1 transaction date`
x2 <- datos_RealEstate$`X2 house age`
x3 <- datos_RealEstate$`X3 distance to the nearest MRT station`
x4 <- datos_RealEstate$`X4 number of convenience stores`
x5 <- datos_RealEstate$`X5 latitude`
x6 <- datos_RealEstate$`X6 longitude`
y <- datos_RealEstate$`Y house price of unit area`




# PARTE 2. Construcción del Modelo. ---------------------------------------


# gráfico de matriz de covarianzas:

pairs(datos_RealEstate)

# correlaciones entre pares de variables:

cor(datos_RealEstate)



# Primero comenzamos con un análisis general de todas las variables predictoras potenciales

pairs.panels( data.frame(x1,x2,x3,x4,x5,x6) , col='red')

# Podemos ver que algunas de las variables están sesgadas como x3 y x6. Si queremos un buen modelo de regresión, todas las variables deben tener una distribución normal. Las variables deben ser independientes y no correlacionadas. En este caso, vemos que x3 y x6 tiene una alta correlación negativa.

pairs.panels( data.frame(x1,x2,log10(x3),x4,x5,x6) , col='red')

# Para corregir, obtuvimos el log10(x3) y vemos que existe una mejoría drástica para x3.

# SELECCIÓN DE VARIABLES PREDICTORAS
# TÉCNICA DE ELIMINACIÓN PASO A PASO HACIA ATRÁS 

# Procedemos a utilizar eliminación paso a paso hacia atrás, para ello primero consideramos todas las variables predictoras y utilizando la función summary y analizando los códigos de significancia y los R^2, vamos eliminando una a una hasta tener el modelo con las variables de importancia. Además, evitaremos incluir términos de interacción, dado que tenemos suficientes variables.

summary( lm(y ~ x1 + x2 + x3 + x4 + x5 + x6)  )
# R^2 nos dice que tan bien el modelo explica la variación en la data. En el modelo completo, el Multiple R-squared es de  0.5824, es decir el modelo explica el 58% de la variación de los errores del modelo. El Adjusted R-squared de 0.5669 toma en consideración el número de variables que se usó para construir el modelo, mayor cantidad de variables implica mayor R^2 ajustado.
# Entonces, aplicando Backwards elimination, vamos a eliminar x6 del modelo, pues no tiene ningun asterisco, no podemos confiar en ella pues la probabilidad de rechazar ese valor estimado es de 0.79820, lo cual es muy alta.

summary( lm(y ~ x1 + x2 + x3 + x4 + x5 )  )

# Una vez eliminada x6 del modelo, vemos que el Multiple R-squared:  0.5823 es casi idéntico. El ajustado mejoró ligeramente, pero casi una mejoría insignificante. Hemos eliminado x6 que no contribuye casi nada en la fórmula final. Entonces, eliminando la variable, el modelo no empeoró.
# El siguiente candidato potencial para eliminación es x1, pues posee **.

summary( lm(y ~  x2 + x3 + x4 + x5 )  )

# Tanto el Multiple R-squared de  0.5711 como Adjusted R-squared de 0.5669 empeoraron ligeramente, de forma Insignificante. En general, el modelo es tan bueno como antes.
# Los resultados del summary ahora indican que debemos confiar en cada una de las variables resultantes, pues cada una es significante con ***



# Verificando el método paso a paso hacia atrás con la librería leaps

# función step()
step(lm(y ~ 1), data=datos_RealEstate, scope = ~ x1 + x2 + x3 + x4 + x5 + x6 )

# La función leaps() provee un análisis de todos los subconjuntos posibles (selección del mejor subconjunto).
# Criterio Cp de Mallows
leaps(x = cbind(x1, x2, x3, x4, x5, x6), y = y, nbest=2, method="Cp")



# PARTE 3. Evaluación del Modelo ------------------------------------------


# TÉCNICA EMPÍRICA PARA LA EVALUACIÓN DE SUPUESTOS
plot( lm(y ~  x2 + x3 + x4 + x5 ) )

# Aplicando transformaciones a las predictoras skewed 

pairs.panels( data.frame(x1,x2,log10(x3),x4,x5,x6) , col='red')

# log10(x3) funcionó a la perfección en lograr la normalidad de x3.

crPlots( lm(y ~  x2 + x3 + x4 + x5 ) )

# MULTICOLINEALIDAD

vif( lm(y ~ x2 + x3 + x4 + x5 ) )

ols_vif_tol( lm(y ~ x2 + x3 + x4 + x5 ) )

# No existe multicolnealidad. 

# GVLMA EVALUACIÓN GLOBAL DE SUPUESTOS

summary( gvlma(  lm(y ~  x2 + x3 + x4 + x5 ) )  )


summary( gvlma(  lm(y ~  x2 + log10(x3) + x4 + x5 ) )  )

# Box cox

boxcox_modelo <- boxcox(  lm(y ~  x2 + x3 + x4 + x5 )  , lambda=seq(-0.5,1,1/10))

lambda = boxcox_modelo$x[which(boxcox_modelo$y==max(boxcox_modelo$y))]

summary(gvlma(lm( ((y^lambda)-1)/lambda ~ x2 + x3 + x4 + x5 )))

# MEDIDAS DE INFLUENCIA

influence.measures ( lm(y ~  x2 + x3 + x4 + x5 )  )

# Medidas de influencia
# DFFITS, Distancias Cook, elementos de la diagonal de la matriz sombrero H, y otras medidas.


# PARTE 4. Interpretación de Resultados. ----------------------------------

# PRUEBAS DE HIPÓTESIS

anova( lm(y ~ x2 + x3 + x4 + x5 )  )

ols_regress( lm(y ~ x2 + x3 + x4 + x5 ) )


# Hacemos la prueba F del modelo completo vs. reducido:

anova( lm(y ~ x1 + x2 + x3 + x4 + x5 + x6)  , lm(y ~ x2 + x3 + x4 + x5 ))

# (F* = 5.4819, p-value = 0.004474),  
# Rechazamos H_0. 
# Según ANOVA, el modelo final lm(y ~ x2 + x3 + x4 + x5 ) es significativamente diferente al modelo completo lm(y ~ x1 + x2 + x3 + x4 + x5 + x6)
# con un alpha = 0.001 de significancia.

# El test compara la reducción la suma cuadradática de los residuos
# Podemos ver que existe un ligero aumento en la suma cuadrática de los residuos en el modelo final

# El valor p pequeño indica que quitando x1 y x6 del modelo, se logra un mejor ajuste en comparación con el modelo completo.








