############################################################
# Práctica: Selección de modelos de regresión
# Métodos Forward, Backward y Stepwise
# Base de datos: Boston
############################################################

############################################################
# 1. Instalación y carga de paquetes
############################################################

# Instalar paquetes solo si no los tienes instalados
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("MASS")

library(ggplot2)
library(dplyr)
library(MASS)

############################################################
# 2. Carga de la base de datos
############################################################

# La base Boston está incluida en el paquete MASS
data(Boston)

# Guardamos la base con un nombre más sencillo
datos <- Boston

# Visualizar las primeras filas
head(datos)

# Revisar nombres de las variables
names(datos)

# Revisar dimensiones de la base
dim(datos)

# Revisar estructura de la base
str(datos)

############################################################
# 3. Descripción general de la base
############################################################

# Resumen estadístico de todas las variables
summary(datos)

# Revisar si existen datos faltantes
colSums(is.na(datos))

# Variable de respuesta:
# medv: valor medio de las viviendas

# Variables predictoras candidatas:
# crim, zn, indus, chas, nox, rm, age, dis,
# rad, tax, ptratio, black, lstat

############################################################
# 4. Análisis exploratorio de la variable respuesta
############################################################

# Histograma de medv
ggplot(datos, aes(x = medv)) +
  geom_histogram(bins = 30, fill = "#00A651", color = "white") +
  labs(
    title = "Distribución del valor medio de las viviendas",
    x = "medv",
    y = "Frecuencia"
  )

ggplot(datos, aes(y = medv)) +
  geom_boxplot(fill = "#AF5EA1", color = "#192136") +
  labs(
    title = "Diagrama de caja de medv",
    y = "medv"
  )

############################################################
# Preguntas:
# 1. ¿Qué forma tiene la distribución de medv?
#La distribución tiene un solo pico, por lo que es unimodal
# 2. ¿Parece simétrica o sesgada?
#Parece estar un poco sesgada hacia la derecha ya que hay mas valores altos alejados del centro
# 3. ¿Se observan posibles valores atípicos?
#Si, en el diagrama de caja se observan varios valores atipicos en la parte superior
############################################################

############################################################
# 5. Relación entre variables predictoras y medv
############################################################

# Relación entre número de habitaciones y medv
ggplot(datos, aes(x = rm, y = medv)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Relación entre número de habitaciones y medv",
    x = "Número promedio de habitaciones",
    y = "medv"
  )

# Relación entre lstat y medv
ggplot(datos, aes(x = lstat, y = medv)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Relación entre lstat y medv",
    x = "lstat",
    y = "medv"
  )

# Relación entre contaminación y medv
ggplot(datos, aes(x = nox, y = medv)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Relación entre contaminación y medv",
    x = "nox",
    y = "medv"
  )

# Relación entre impuestos y medv
ggplot(datos, aes(x = tax, y = medv)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Relación entre impuestos y medv",
    x = "tax",
    y = "medv"
  )

# Relación entre distancia a centros de empleo y medv
ggplot(datos, aes(x = dis, y = medv)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Relación entre distancia a centros de empleo y medv",
    x = "dis",
    y = "medv"
  )

# Relación entre ptratio y medv
ggplot(datos, aes(x = ptratio, y = medv)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Relación entre ptratio y medv",
    x = "ptratio",
    y = "medv"
  )

############################################################
# Preguntas:
# 1. ¿Qué variables parecen tener relación positiva con medv?
#la variable dis parece tener una relacion positiva con medv, ya que cuando aumenta tambien tiende a aumentar el valor de las viviendas
# 2. ¿Qué variables parecen tener relación negativa con medv?
#las variables lstat, ptratio y tax muestran una relacion negativa con medv, porque al aumentar estas variables el valor de las viviendas suele disminuir
# 3. ¿Todas las relaciones parecen lineales?
#no, algunas relaciones siguen una tendencia lineal, pero otras muestran cierta curvatura y dispersion en los datos
############################################################

############################################################
# 6. Comparación de medv según cercanía al río Charles
############################################################

# La variable chas indica si la zona está cerca del río Charles
# chas = 0: no limita con el río
# chas = 1: sí limita con el río

ggplot(datos, aes(x = factor(chas), y = medv)) +
  geom_boxplot() +
  labs(
    title = "Valor medio de vivienda según cercanía al río Charles",
    x = "chas",
    y = "medv"
  )

############################################################
# Pregunta:
# ¿Las viviendas cercanas al río parecen tener valores distintos?
#Si. Las viviendas cerca del rio parecen tener un valor mas alto que las que no estan cerca del rio
############################################################

############################################################
# 7. Modelos de regresión simples
############################################################

# Modelo con una variable predictora: rm
modelo_rm <- lm(medv ~ rm, data = datos)

summary(modelo_rm)

# Modelo con una variable predictora: lstat
modelo_lstat <- lm(medv ~ lstat, data = datos)

summary(modelo_lstat)

# Modelo con dos variables predictoras
modelo_rm_lstat <- lm(medv ~ rm + lstat, data = datos)

summary(modelo_rm_lstat)

############################################################
# Preguntas:
# 1. ¿El coeficiente de rm es positivo o negativo?
#el coeficiente de rm es positivo por lo que al aumentar el numero de habitaciones tambien aumenta el valor de la vivienda
# 2. ¿El coeficiente de lstat es positivo o negativo?
#el coeficiente de lstat es negativo, por lo que al aumentar lstat el valor de la vivienda disminuye
# 3. ¿Qué modelo tiene mayor R2 ajustado?
#El modelo rm + lstat tiene el mayor R2 ajustado (0.6371), por lo que explica mejor la variacion de medv
############################################################

############################################################
# 8. Modelo vacío y modelo completo
############################################################

# Modelo vacío:
# y = beta_0 + error

modelo_vacio <- lm(medv ~ 1, data = datos)

summary(modelo_vacio)

# Modelo completo:
# Usa todas las variables predictoras disponibles

modelo_completo <- lm(medv ~ ., data = datos)

summary(modelo_completo)

# AIC del modelo vacío
AIC(modelo_vacio)

# AIC del modelo completo
AIC(modelo_completo)

############################################################
# Preguntas:
# 1. ¿Cuántas variables predictoras tiene el modelo completo?
# 2. ¿Todas las variables del modelo completo parecen significativas?
# 3. ¿Qué modelo tiene menor AIC: el vacío o el completo?
############################################################

############################################################
# 9. ¿Cuántos submodelos se pueden construir?
############################################################

# En esta base existen 13 variables predictoras candidatas.
# Si p = 13, el número de submodelos con al menos una variable es:

p <- 13

numero_submodelos <- 2^p - 1

numero_submodelos

############################################################
# Interpretación:
# Con 13 variables predictoras se pueden construir 8191 submodelos.
# Revisarlos manualmente sería poco práctico.
# Por eso usamos métodos automáticos como:
# Forward, Backward y Stepwise.
############################################################

############################################################
# 10. Selección Forward
############################################################

# La selección Forward inicia con el modelo vacío
# y va agregando variables.

modelo_forward <- step(
  modelo_vacio,
  scope = list(
    lower = modelo_vacio,
    upper = modelo_completo
  ),
  direction = "forward"
)

# Resumen del modelo seleccionado por Forward
summary(modelo_forward)

# Fórmula final del modelo Forward
formula(modelo_forward)

# AIC del modelo Forward
AIC(modelo_forward)

############################################################
# Preguntas:
# 1. ¿Qué variables fueron seleccionadas por Forward?
#las variables seleccionadas fueron: lstat, rm, ptratio, dis, nox, chas, black, zn, crim, rad y tax
# 2. ¿Cuál fue el AIC final?
#fue de 3023.726
# 3. ¿El modelo Forward usa todas las variables?
#no. El modelo no usa todas las variables, ya que dejo fuera indus y age
############################################################

############################################################
# 11. Selección Backward
############################################################

# La selección Backward inicia con el modelo completo
# y va eliminando variables.

modelo_backward <- step(
  modelo_completo,
  direction = "backward"
)

# Resumen del modelo seleccionado por Backward
summary(modelo_backward)

# Fórmula final del modelo Backward
formula(modelo_backward)

# AIC del modelo Backward
AIC(modelo_backward)

############################################################
# Preguntas:
# 1. ¿Qué variables eliminó el método Backward?
#el metodo Backward elimino las variables age e indus
# 2. ¿Qué variables permanecieron en el modelo final?
#las variables que quedaron fueron crim, zn, chas, nox, rm, dis, rad, tax, ptratio, black y lstat
# 3. ¿El resultado coincide con Forward?
#si modelo final obtenido con Backward coincide con el obtenido con Forward, ya que ambos seleccionaron las mismas variables y tuvieron el mismo aic
############################################################

############################################################
# 12. Selección Stepwise
############################################################

# La selección Stepwise combina ambos procesos:
# puede agregar o eliminar variables en cada paso.

modelo_stepwise <- step(
  modelo_vacio,
  scope = list(
    lower = modelo_vacio,
    upper = modelo_completo
  ),
  direction = "both"
)

# Resumen del modelo seleccionado por Stepwise
summary(modelo_stepwise)

# Fórmula final del modelo Stepwise
formula(modelo_stepwise)

# AIC del modelo Stepwise
AIC(modelo_stepwise)

############################################################
# Preguntas:
# 1. ¿Qué variables quedaron en el modelo Stepwise?
#las variables que quedaron fueron lstat, rm, ptratio, dis, nox, chas, black, zn, crim, rad y tax
# 2. ¿Coincide con el modelo Forward?
#Si el modelo Stepwise selecciono las mismas variables que el modelo Forward
# 3. ¿Coincide con el modelo Backward?
#si porque acebaron con las mismas variables y con el mismo aic
############################################################

############################################################
# 13. Comparación de modelos mediante AIC
############################################################

comparacion_aic <- data.frame(
  Modelo = c(
    "Vacío",
    "Completo",
    "Forward",
    "Backward",
    "Stepwise"
  ),
  AIC = c(
    AIC(modelo_vacio),
    AIC(modelo_completo),
    AIC(modelo_forward),
    AIC(modelo_backward),
    AIC(modelo_stepwise)
  )
)

comparacion_aic

# Gráfico de comparación de AIC
ggplot(comparacion_aic, aes(x = Modelo, y = AIC)) +
  geom_col() +
  labs(
    title = "Comparación de modelos mediante AIC",
    x = "Modelo",
    y = "AIC"
  )

############################################################
# Preguntas:
# 1. ¿Qué modelo tiene el menor AIC?
#los modelos Forward, Backward y Stepwise tienen el menor aic 3023.726
# 2. ¿Por qué preferimos modelos con menor AIC?
# porque un menor aic indica que el modelo explica bien los datos usando menos variables innecesarias
############################################################

############################################################
# 14. Comparación de modelos mediante R2 ajustado
############################################################

comparacion_r2 <- data.frame(
  Modelo = c(
    "Completo",
    "Forward",
    "Backward",
    "Stepwise"
  ),
  R2_ajustado = c(
    summary(modelo_completo)$adj.r.squared,
    summary(modelo_forward)$adj.r.squared,
    summary(modelo_backward)$adj.r.squared,
    summary(modelo_stepwise)$adj.r.squared
  )
)

comparacion_r2

# Gráfico de comparación de R2 ajustado
ggplot(comparacion_r2, aes(x = Modelo, y = R2_ajustado)) +
  geom_col() +
  labs(
    title = "Comparación de modelos mediante R² ajustado",
    x = "Modelo",
    y = "R² ajustado"
  )

############################################################
# Preguntas:
# 1. ¿Qué modelo tiene mayor R2 ajustado?
#los modelos Forward, Backward y Stepwise tienen el mayor R2 ajustado 0.7348
# 2. ¿Coincide con el modelo de menor AIC?
#Si los modelos que tienen el mayor R2 ajustado tambien son los que tienen el menor aic
############################################################

############################################################
# 15. Selección de un modelo final
############################################################

# Para continuar con el diagnóstico, elegimos el modelo Stepwise.
# También podrías elegir el modelo con menor AIC.

modelo_final <- modelo_stepwise

summary(modelo_final)

############################################################
# 16. Valores ajustados y residuales
############################################################

# Agregamos residuales y valores ajustados a la base de datos

datos$residuales <- residuals(modelo_final)
datos$ajustados <- fitted(modelo_final)

head(datos)

############################################################
# 17. Diagnóstico gráfico del modelo final
############################################################

# Residuales vs valores ajustados
ggplot(datos, aes(x = ajustados, y = residuales)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuales vs valores ajustados",
    x = "Valores ajustados",
    y = "Residuales"
  )

############################################################
# Preguntas:
# 1. ¿Los residuales parecen distribuirse alrededor de cero?
#Si la mayoria de los residuales se encuentran cerca de la linea de cero
# 2. ¿Se observa algún patrón?
#no se mira un patron muy claro, aunque hay algunos puntos alejados y un poco mas de dispersion
############################################################

# Histograma de residuales
ggplot(datos, aes(x = residuales)) +
  geom_histogram(bins = 30, color = "white") +
  labs(
    title = "Histograma de residuales",
    x = "Residuales",
    y = "Frecuencia"
  )

############################################################
# Pregunta:
# ¿Los residuales parecen tener una distribución aproximadamente normal?
#si ya que los residuales parecen tener una distribucion aproximadamente normal, ya que la mayor parte de los datos se concentra alrededor de cero
############################################################

# Gráfico Q-Q de residuales
ggplot(datos, aes(sample = residuales)) +
  stat_qq() +
  stat_qq_line() +
  labs(
    title = "Gráfico Q-Q de los residuales"
  )

############################################################
# Preguntas:
# 1. ¿Los puntos siguen aproximadamente una línea recta?
#si la mayoria de los puntos siguen una linea recta aunque en los extremos se observa una que hay una desviacion
# 2. ¿Qué indicaría una desviación fuerte de la línea?
#indicaria que los residuales no siguen una distribucion normal
############################################################

# Valores reales vs valores ajustados
ggplot(datos, aes(x = medv, y = ajustados)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Valores reales vs valores ajustados",
    x = "Valores reales de medv",
    y = "Valores ajustados"
  )

############################################################
# Preguntas:
# 1. ¿El modelo predice bien los valores de medv?
#si el modelo predice bastante bien los valores de medv ya que la mayoria de los puntos estan cerca de la linea
# 2. ¿Qué significa que los puntos estén cerca de la línea punteada?
#que los valores ajustados son muy parecidos a los valores reales
############################################################

############################################################
# 18. Interpretación de coeficientes del modelo final
############################################################

coef(modelo_final)

summary(modelo_final)

############################################################
# Preguntas:
# 1. ¿Qué variables tienen coeficiente positivo?
#las variables con coeficiente positivo son rm, chas, black, zn y rad
# 2. ¿Qué variables tienen coeficiente negativo?
#las variables con coeficiente negativo son lstat, ptratio, dis, nox, crim y tax
# 3. Elige dos variables del modelo final e interpreta sus coeficientes.
#rm tiene un coeficiente de 3.80, lo que significa que al aumentar una habitacion en promedio, el valor de la vivienda aumenta aproximadamente 3.8 unidades, manteniendo las demas variables constantes
#y lstat: tiene un coeficiente de -0.52, lo que significa que al aumentar una unidad de lstat, el valor de la vivienda disminuye aproximadamente 0.52 unidades, manteniendo las demas variables constantes
############################################################

############################################################
# 19. Predicción con el modelo final
############################################################

# Podemos usar el modelo final para predecir.
# Tomamos las primeras observaciones de la base como ejemplo.

nuevos_datos <- datos[1:5, ]

predicciones <- predict(modelo_final, newdata = nuevos_datos)

predicciones

# Comparar valores reales y predichos
comparacion_pred <- data.frame(
  Real = nuevos_datos$medv,
  Predicho = predicciones
)

comparacion_pred

############################################################
# Pregunta:
# ¿Qué tan cercanos son los valores predichos a los valores reales?
#los valores predichos son relativamente cercanos a los valores reales, aunque en algunos casos hay diferencias notables entre ambos valores por lo que no son perfectas de todo
############################################################

############################################################
# 20. Preguntas finales de reflexión
############################################################

# Responde en tu reporte:
#
# 1. ¿Cuál fue el mejor modelo de acuerdo con AIC?
#los modelos forward, backward y stepwise, ya que tuvieron el menor AIC
#
# 2. ¿Cuál fue el mejor modelo de acuerdo con R2 ajustado?
#los modelos forward, backward y stepwise, ya que tuvieron el mayor R2 ajustado
#
# 3. ¿Qué variables aparecen en el modelo final?
#lstat, rm, ptratio, dis, nox, chas, black, zn, crim, rad y tax
#
# 4. ¿Qué variable parece tener una relación positiva con medv?
#rm parece tener una relacion positiva con medv
#
# 5. ¿Qué variable parece tener una relación negativa con medv?
#lstat parece tener una relacion negativa con medv
#
# 6. ¿Por qué no siempre conviene usar el modelo completo?
#porque puede incluir variables que no aportan informacion importante y hacer el modelo mas complejo
#
# 7. ¿Qué ventaja tiene usar selección Forward, Backward o Stepwise?
#ayudan a encontrar un modelo mas simple y selecesiona las variables mas importantes 
#
# 8. ¿Qué limitación tienen estos métodos automáticos de selección?
#pueden dejar fuera variables utiles
#
# 9. ¿El análisis de residuales sugiere que el modelo final es razonable?
#si los residuales se distribuyen alrededor de cero y no muestran patrones muy marcados
#
# 10. ¿Qué mejorarías en este análisis?
#probaria otros modelos o transformaciones de variables para mejorar las predicciones

############################################################
# Fin de la práctica
############################################################

