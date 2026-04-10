library(tidyverse)
datos <- read_csv2("student-por.csv")
dim(datos)
glimpse(datos)
names(datos)

# -----------------------------------------------------------------------------------------------
#PROCEDIMIENTO PARA VER SI EL DATASET TIENE VALORES PERDIDOS, DUPLICADOS O INCOHERENCIAS BÁSICAS
# -----------------------------------------------------------------------------------------------

colSums(is.na(datos)) 
sum(duplicated(datos)) 

#no se detectan observaciones duplicadas, lo que garantiza la independencia de las muestras

summary(datos)


# ------------------------------------------------------------------------------------------------------------------------------
#RECODIFICACIÓN DE LAS VARIABLES CUALITATIVAS COMO FACTORES PARA QUE PUEDAN SER TRATADAS CORRECTAMENTE EN EL ANÁLISIS Y MODELADO
# ------------------------------------------------------------------------------------------------------------------------------

datos <- datos %>%
  mutate(
    school     = factor(school),
    sex        = factor(sex),
    address    = factor(address),
    famsize    = factor(famsize),
    Pstatus    = factor(Pstatus),
    Mjob       = factor(Mjob),
    Fjob       = factor(Fjob),
    reason     = factor(reason),
    guardian   = factor(guardian),
    schoolsup  = factor(schoolsup),
    famsup     = factor(famsup),
    paid       = factor(paid),
    activities = factor(activities),
    nursery    = factor(nursery),
    higher     = factor(higher),
    internet   = factor(internet),
    romantic   = factor(romantic)
  )

glimpse(datos)

# ------------------------------------------------------------
# CREACIÓN DE LA VARIABLE OBJETIVO: Riesgo de suspenso
# ------------------------------------------------------------

datos <- datos %>%
  mutate(
    riesgo_suspenso = ifelse(G3 < 10, 1, 0)
  )

table(datos$riesgo_suspenso)

prop.table(table(datos$riesgo_suspenso))

# ------------------------------------------------------------
# VISUALIZACIÓN DE LA DISTRIBUCIÓN DE LA VARIABLE OBJETIVO
# ------------------------------------------------------------

library(ggplot2)

# Representamos gráficamente la distribución del riesgo de suspenso
# Este gráfico permite visualizar de forma intuitiva el posible desbalanceo
# entre estudiantes que aprueban y aquellos en riesgo de suspender

ggplot(datos, aes(x = factor(riesgo_suspenso))) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Distribución de riesgo de suspenso",
    x = "Riesgo (0 = No, 1 = Sí)",
    y = "Número de estudiantes")


# ------------------------------------------------------------
# CONCLUSIONES SOBRE LA VARIABLE OBJETIVO
# ------------------------------------------------------------

# La variable objetivo 'riesgo_suspenso' presenta una distribución en la que la mayoría
# de los estudiantes no se encuentran en riesgo de suspender, lo que indica un cierto
# desbalanceo de clases. Este aspecto deberá considerarse en fases posteriores del
# modelado, especialmente en la selección de métricas de evaluación adecuadas.



# ------------------------------------------------------------
# CÁLCULO DE FRECUENCIAS Y PORCENTAJES
# ------------------------------------------------------------

# Calculamos el número de observaciones y el porcentaje de cada clase
# para cuantificar con precisión el grado de desbalanceo del dataset

datos %>%
  count(riesgo_suspenso) %>%
  mutate(porcentaje = n / sum(n) * 100)


table(datos$riesgo_suspenso)
prop.table(table(datos$riesgo_suspenso))


# Analizamos la relación entre la segunda evaluación (G2) y el riesgo de suspenso
# Esta variable es especialmente relevante al ser más cercana a la nota final (G3)
ggplot(datos, aes(x = factor(riesgo_suspenso), y = G2)) +
  geom_boxplot(fill = "lightgreen") +
  labs(
    title = "Relación entre G2 y riesgo de suspenso",
    x = "Riesgo (0 = No, 1 = Sí)",
    y = "Nota G2"
  )

# ------------------------------------------------------------
# ABSENTISMO
# ------------------------------------------------------------

# Evaluamos si existe relación entre el número de ausencias y el riesgo de suspenso
# El objetivo es identificar si un mayor absentismo se asocia con peores resultados académicos
ggplot(datos, aes(x = factor(riesgo_suspenso), y = absences)) +
  geom_boxplot(fill = "salmon") +
  labs(
    title = "Relación entre absentismo y riesgo de suspenso",
    x = "Riesgo (0 = No, 1 = Sí)",
    y = "Número de ausencias"
  )

# ------------------------------------------------------------
# FRACASOS ACADÉMICOS PREVIOS
# ------------------------------------------------------------

# Analizamos el impacto de los suspensos previos (failures) en el riesgo de suspenso actual
# Esta variable puede reflejar patrones persistentes de bajo rendimiento académico
ggplot(datos, aes(x = factor(riesgo_suspenso), y = failures)) +
  geom_boxplot(fill = "orange") +
  labs(
    title = "Relación entre suspensos previos y riesgo de suspenso",
    x = "Riesgo (0 = No, 1 = Sí)",
    y = "Número de suspensos previos"
  )


# ------------------------------------------------------------
# ANÁLISIS DE VARIABLES CUALITATIVAS
# ------------------------------------------------------------

# Analizamos la relación entre el sexo del estudiante y el riesgo de suspenso
# Se utiliza un gráfico de barras proporcional para comparar distribuciones
ggplot(datos, aes(x = sex, fill = factor(riesgo_suspenso))) +
  geom_bar(position = "fill") +
  labs(
    title = "Riesgo de suspenso según sexo",
    x = "Sexo",
    y = "Proporción",
    fill = "Riesgo"
  )

# Analizamos el impacto de tener acceso a internet en el hogar
ggplot(datos, aes(x = internet, fill = factor(riesgo_suspenso))) +
  geom_bar(position = "fill") +
  labs(
    title = "Riesgo de suspenso según acceso a internet",
    x = "Internet",
    y = "Proporción",
    fill = "Riesgo"
  )

# Analizamos el tiempo de estudio semanal
ggplot(datos, aes(x = factor(studytime), fill = factor(riesgo_suspenso))) +
  geom_bar(position = "fill") +
  labs(
    title = "Riesgo de suspenso según tiempo de estudio",
    x = "Tiempo de estudio",
    y = "Proporción",
    fill = "Riesgo"
  )

# Analizamos el apoyo familiar en los estudios
ggplot(datos, aes(x = famsup, fill = factor(riesgo_suspenso))) +
  geom_bar(position = "fill") +
  labs(
    title = "Riesgo de suspenso según apoyo familiar",
    x = "Apoyo familiar",
    y = "Proporción",
    fill = "Riesgo"
  )

# Analizamos si el estudiante desea continuar con estudios superiores
ggplot(datos, aes(x = higher, fill = factor(riesgo_suspenso))) +
  geom_bar(position = "fill") +
  labs(
    title = "Riesgo de suspenso según intención de estudios superiores",
    x = "Educación superior",
    y = "Proporción",
    fill = "Riesgo"
  )


# ------------------------------------------------------------
# CONCLUSIONES DEL ANÁLISIS DE VARIABLES EXPLICATIVAS
# ------------------------------------------------------------

# A partir del análisis exploratorio realizado, se identifican varios patrones relevantes:
# - Las variables G1 y G2 muestran una fuerte capacidad discriminativa, evidenciando diferencias claras
#   entre estudiantes que aprueban y aquellos en riesgo de suspenso.
# - La variable 'failures' también presenta una relación significativa, indicando que el historial académico
#   previo influye notablemente en el rendimiento final.
# - El tiempo de estudio ('studytime') y la intención de continuar estudios superiores ('higher')
#   muestran una relación inversa con el riesgo de suspenso, lo que sugiere que factores motivacionales
#   y de dedicación tienen un impacto relevante.
# - Variables como 'internet' presentan cierto efecto, mientras que otras como 'sex' o 'famsup'
#   no muestran diferencias significativas entre grupos.
# - El absentismo ('absences') no presenta un patrón claro, lo que sugiere que su influencia puede
#   ser más compleja o estar mediada por otros factores.



# ------------------------------------------------------------
# CREACIÓN DE VARIABLES DERIVADAS
# ------------------------------------------------------------

# ------------------------------------------------------------
# MEDIA DE NOTAS PARCIALES
# ------------------------------------------------------------

# Calculamos la media de las notas G1 y G2
# Esta variable resume el rendimiento previo del estudiante antes de la nota final
datos <- datos %>%
  mutate(media_notas = (G1 + G2) / 2)

# ------------------------------------------------------------
# PROGRESO ACADÉMICO
# ------------------------------------------------------------

# Calculamos la evolución del estudiante desde la primera evaluación (G1) hasta la final (G3)
# Permite identificar si el alumno mejora o empeora su rendimiento
datos <- datos %>%
  mutate(progreso = G3 - G1)

# ------------------------------------------------------------
# NIVEL EDUCATIVO FAMILIAR
# ------------------------------------------------------------

# Promediamos el nivel educativo del padre y la madre
# Esta variable puede reflejar el entorno educativo del estudiante
datos <- datos %>%
  mutate(educacion_familiar = (Medu + Fedu) / 2)

# ------------------------------------------------------------
# CONSUMO MEDIO DE ALCOHOL
# ------------------------------------------------------------

# Calculamos la media entre consumo diario (Dalc) y consumo de fin de semana (Walc)
# Permite capturar el nivel general de consumo de alcohol del estudiante
datos <- datos %>%
  mutate(alcohol_medio = (Dalc + Walc) / 2)

# ------------------------------------------------------------
# ABSENTISMO ELEVADO
# ------------------------------------------------------------

# Creamos una variable binaria que identifica estudiantes con alto número de ausencias
# Se define como 1 si supera 10 ausencias, 0 en caso contrario
datos <- datos %>%
  mutate(absentismo_alto = ifelse(absences > 10, 1, 0))


glimpse(datos)


# ------------------------------------------------------------
# ANÁLISIS DE VARIABLES DERIVADAS
# ------------------------------------------------------------

# ------------------------------------------------------------
# MEDIA DE NOTAS PARCIALES
# ------------------------------------------------------------

# Analizamos si la media de notas previas está relacionada con el riesgo de suspenso
# Se espera que valores bajos estén asociados a mayor probabilidad de suspender
ggplot(datos, aes(x = factor(riesgo_suspenso), y = media_notas)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Relación entre media de notas y riesgo de suspenso",
    x = "Riesgo (0 = No, 1 = Sí)",
    y = "Media de G1 y G2"
  )

# ------------------------------------------------------------
# PROGRESO ACADÉMICO
# ------------------------------------------------------------

# Evaluamos si la evolución del estudiante influye en el resultado final
# Valores negativos indican empeoramiento, positivos indican mejora
ggplot(datos, aes(x = factor(riesgo_suspenso), y = progreso)) +
  geom_boxplot(fill = "lightgreen") +
  labs(
    title = "Relación entre progreso académico y riesgo de suspenso",
    x = "Riesgo (0 = No, 1 = Sí)",
    y = "Progreso (G3 - G1)"
  )

# ------------------------------------------------------------
# EDUCACIÓN FAMILIAR
# ------------------------------------------------------------

# Analizamos si el nivel educativo familiar influye en el rendimiento académico
ggplot(datos, aes(x = factor(riesgo_suspenso), y = educacion_familiar)) +
  geom_boxplot(fill = "purple") +
  labs(
    title = "Relación entre educación familiar y riesgo de suspenso",
    x = "Riesgo (0 = No, 1 = Sí)",
    y = "Nivel educativo medio"
  )

# ------------------------------------------------------------
# CONSUMO DE ALCOHOL
# ------------------------------------------------------------

# Evaluamos si un mayor consumo de alcohol se asocia con mayor riesgo de suspenso
ggplot(datos, aes(x = factor(riesgo_suspenso), y = alcohol_medio)) +
  geom_boxplot(fill = "red") +
  labs(
    title = "Relación entre consumo de alcohol y riesgo de suspenso",
    x = "Riesgo (0 = No, 1 = Sí)",
    y = "Consumo medio"
  )

# ------------------------------------------------------------
# ABSENTISMO ALTO
# ------------------------------------------------------------

# Analizamos si los estudiantes con alto absentismo presentan mayor riesgo de suspenso
ggplot(datos, aes(x = factor(absentismo_alto), fill = factor(riesgo_suspenso))) +
  geom_bar(position = "fill") +
  labs(
    title = "Riesgo de suspenso según absentismo elevado",
    x = "Absentismo alto (0 = No, 1 = Sí)",
    y = "Proporción",
    fill = "Riesgo"
  )


# ------------------------------------------------------------
# CONCLUSIONES DEL ANÁLISIS DE VARIABLES DERIVADAS
# ------------------------------------------------------------

# Las variables derivadas creadas aportan información relevante al análisis:
# - La variable 'media_notas' muestra una fuerte capacidad explicativa del riesgo de suspenso,
#   evidenciando una clara separación entre estudiantes que aprueban y suspenden.
# - La variable 'progreso' refleja cierta relación con el rendimiento final, aunque con menor
#   capacidad discriminativa que las notas previas.
# - El nivel educativo familiar ('educacion_familiar') presenta una influencia moderada,
#   sugiriendo un posible efecto del entorno académico en el rendimiento del estudiante.
# - El consumo de alcohol ('alcohol_medio') muestra una ligera asociación con el riesgo de suspenso,
#   aunque su impacto parece limitado.
# - La variable 'absentismo_alto' permite capturar mejor el efecto del absentismo que la variable original,
#   mostrando una mayor proporción de suspensos en estudiantes con elevado número de ausencias.


# ------------------------------------------------------------
# SELECCIÓN DE VARIABLES PARA MODELADO
# ------------------------------------------------------------

# Creamos un dataset específico para el modelado
# Seleccionamos variables relevantes en base al análisis exploratorio previo
datos_modelo <- datos %>%
  select(
    riesgo_suspenso,
    G1, G2,
    media_notas,
    failures,
    studytime,
    higher,
    absences,
    absentismo_alto,
    internet,
    educacion_familiar,
    progreso,
    alcohol_medio
  )

glimpse(datos_modelo)


# ------------------------------------------------------------
# JUSTIFICACIÓN DE LA SELECCIÓN DE VARIABLES
# ------------------------------------------------------------

# Se seleccionan variables en función de su relevancia observada en el análisis exploratorio,
# priorizando aquellas con mayor capacidad explicativa del riesgo de suspenso.
# Se incluyen tanto variables originales como derivadas, con el objetivo de mejorar
# la capacidad predictiva del modelo.


# ------------------------------------------------------------
# DIVISIÓN DEL DATASET EN TRAIN Y TEST
# ------------------------------------------------------------

# Fijamos semilla para reproducibilidad
set.seed(123)

# Dividimos el dataset en 70% entrenamiento y 30% test
indices <- sample(1:nrow(datos_modelo), size = 0.7 * nrow(datos_modelo))

train <- datos_modelo[indices, ]
test  <- datos_modelo[-indices, ]


dim(train)
dim(test)


# ------------------------------------------------------------
# MODELO DE REGRESIÓN LOGÍSTICA
# ------------------------------------------------------------

# Entrenamos un modelo de regresión logística para predecir el riesgo de suspenso
modelo_logit <- glm(riesgo_suspenso ~ ., data = train, family = "binomial")


summary(modelo_logit)


# ------------------------------------------------------------
# PREDICCIONES SOBRE EL CONJUNTO DE TEST
# ------------------------------------------------------------

# Obtenemos probabilidades
probabilidades <- predict(modelo_logit, newdata = test, type = "response")

# Convertimos a clases (umbral 0.5)
predicciones <- ifelse(probabilidades > 0.5, 1, 0)



# ------------------------------------------------------------
# EVALUACIÓN DEL MODELO
# ------------------------------------------------------------

# Matriz de confusión
table(Predicho = predicciones, Real = test$riesgo_suspenso)


# Accuracy
mean(predicciones == test$riesgo_suspenso)



# ------------------------------------------------------------
# SELECCIÓN DE VARIABLES PARA MODELADO (VERSIÓN CORREGIDA)
# ------------------------------------------------------------

# Se excluyen variables que pueden generar fuga de información o redundancia:
# - 'progreso' se elimina porque incorpora G3, variable a partir de la cual se define la variable objetivo
# - 'media_notas' se elimina para evitar colinealidad con G1 y G2
datos_modelo <- datos %>%
  select(
    riesgo_suspenso,
    G1, G2,
    failures,
    studytime,
    higher,
    absences,
    absentismo_alto,
    internet,
    educacion_familiar,
    alcohol_medio
  )


# ------------------------------------------------------------
# DIVISIÓN DEL DATASET EN TRAIN Y TEST
# ------------------------------------------------------------

# Fijamos semilla para asegurar reproducibilidad
set.seed(123)

# Dividimos el dataset en 70% entrenamiento y 30% test
indices <- sample(1:nrow(datos_modelo), size = 0.7 * nrow(datos_modelo))

train <- datos_modelo[indices, ]
test  <- datos_modelo[-indices, ]


# ------------------------------------------------------------
# MODELO DE REGRESIÓN LOGÍSTICA
# ------------------------------------------------------------

# Entrenamos un modelo de regresión logística con variables explicativas seleccionadas
modelo_logit <- glm(riesgo_suspenso ~ ., data = train, family = "binomial")

# Resumen del modelo
summary(modelo_logit)



# ------------------------------------------------------------
# PREDICCIONES Y EVALUACIÓN
# ------------------------------------------------------------

# Obtenemos probabilidades predichas sobre el conjunto de test
probabilidades <- predict(modelo_logit, newdata = test, type = "response")

# Convertimos probabilidades en clases usando un umbral de 0.5
predicciones <- ifelse(probabilidades > 0.5, 1, 0)

# Matriz de confusión
table(Predicho = predicciones, Real = test$riesgo_suspenso)

# Accuracy
mean(predicciones == test$riesgo_suspenso)


# ------------------------------------------------------------
# AJUSTE METODOLÓGICO DEL DATASET DE MODELADO
# ------------------------------------------------------------

# Se revisa la selección de variables para evitar problemas de fuga de información
# y redundancia entre predictores.
# En particular, se excluye la variable 'progreso' al incorporar G3 en su cálculo,
# y se elimina 'media_notas' por estar directamente construida a partir de G1 y G2.


# ------------------------------------------------------------
# CÁLCULO DE MÉTRICAS DE EVALUACIÓN
# ------------------------------------------------------------

# Verdaderos positivos (TP): predice 1 y es 1
TP <- sum(predicciones == 1 & test$riesgo_suspenso == 1)

# Verdaderos negativos (TN): predice 0 y es 0
TN <- sum(predicciones == 0 & test$riesgo_suspenso == 0)

# Falsos positivos (FP): predice 1 y es 0
FP <- sum(predicciones == 1 & test$riesgo_suspenso == 0)

# Falsos negativos (FN): predice 0 y es 1
FN <- sum(predicciones == 0 & test$riesgo_suspenso == 1)

# Precision: de los que predigo como suspenso, cuántos lo son realmente
precision <- TP / (TP + FP)

# Recall: de los que realmente suspenden, cuántos detecto
recall <- TP / (TP + FN)

# F1-score: equilibrio entre precision y recall
F1 <- 2 * (precision * recall) / (precision + recall)

# Mostramos resultados
precision
recall
F1


# ------------------------------------------------------------
# MÉTRICAS DE EVALUACIÓN AVANZADAS
# ------------------------------------------------------------

# Dado que el problema presenta cierto desbalanceo de clases,
# se calculan métricas adicionales como precision, recall y F1-score,
# que permiten evaluar mejor el rendimiento del modelo en la detección
# de estudiantes en riesgo de suspenso.


# ------------------------------------------------------------
# CONCLUSIONES DEL MODELO LOGÍSTICO
# ------------------------------------------------------------

# El modelo presenta una precisión elevada (0.80), lo que indica que la mayoría
# de los estudiantes identificados como en riesgo realmente lo están.
# Sin embargo, el recall (0.65) muestra que no todos los casos de suspenso
# son detectados, lo que sugiere margen de mejora en la sensibilidad del modelo.
# El F1-score (0.72) refleja un equilibrio razonable entre ambas métricas.


# ------------------------------------------------------------
# MODELO ÁRBOL DE DECISIÓN
# ------------------------------------------------------------

install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

# Entrenamos el árbol
modelo_arbol <- rpart(riesgo_suspenso ~ ., data = train, method = "class")

# Visualizamos el árbol
rpart.plot(modelo_arbol)


# ------------------------------------------------------------
# INTERPRETACIÓN DEL ÁRBOL DE DECISIÓN
# ------------------------------------------------------------

# El árbol de decisión identifica a G2 como la variable más relevante
# en la predicción del riesgo de suspenso, estableciendo un umbral claro en G2 < 9.
# A partir de ahí, G1 refuerza la capacidad predictiva del modelo.
# En situaciones intermedias, variables como la educación familiar
# aportan información adicional, lo que sugiere que factores académicos
# y socioeconómicos influyen conjuntamente en el rendimiento del estudiante.


# ------------------------------------------------------------
# PREDICCIONES ÁRBOL DE DECISIÓN
# ------------------------------------------------------------

# Predicciones del árbol
pred_arbol <- predict(modelo_arbol, newdata = test, type = "class")

# Matriz de confusión
table(Predicho = pred_arbol, Real = test$riesgo_suspenso)

# Accuracy
mean(pred_arbol == test$riesgo_suspenso)


# TP, TN, FP, FN
TP <- sum(pred_arbol == 1 & test$riesgo_suspenso == 1)
TN <- sum(pred_arbol == 0 & test$riesgo_suspenso == 0)
FP <- sum(pred_arbol == 1 & test$riesgo_suspenso == 0)
FN <- sum(pred_arbol == 0 & test$riesgo_suspenso == 1)

# Métricas
precision_arbol <- TP / (TP + FP)
recall_arbol <- TP / (TP + FN)
F1_arbol <- 2 * (precision_arbol * recall_arbol) / (precision_arbol + recall_arbol)

precision_arbol
recall_arbol
F1_arbol


# ------------------------------------------------------------
# CONCLUSIONES DEL ÁRBOL DE DECISIÓN
# ------------------------------------------------------------

# El árbol de decisión presenta un rendimiento ligeramente superior al modelo de regresión logística,
# mejorando la accuracy, la precision y el F1-score.
# No obstante, ambos modelos muestran el mismo recall, por lo que detectan una proporción similar
# de estudiantes en riesgo de suspenso.
# En conjunto, el árbol parece ofrecer una mejor capacidad de clasificación sin perder interpretabilidad.


# Instalamos el paquete 
install.packages("randomForest")

# Cargamos la librería
library(randomForest)


# ------------------------------------------------------------
# MODELO RANDOM FOREST
# ------------------------------------------------------------

# Entrenamos un modelo de Random Forest
modelo_rf <- randomForest(
  riesgo_suspenso ~ ., 
  data = train,
  ntree = 100,        # número de árboles
  mtry = 3,           # variables consideradas en cada split
  importance = TRUE   # para ver importancia de variables
)

modelo_rf

# ------------------------------------------------------------
# IMPORTANCIA DE VARIABLES
# ------------------------------------------------------------

importance(modelo_rf)
varImpPlot(modelo_rf)


# ------------------------------------------------------------
# PREDICCIONES RANDOM FOREST
# ------------------------------------------------------------

pred_rf <- predict(modelo_rf, newdata = test)


# Matriz de confusión
table(Predicho = pred_rf, Real = test$riesgo_suspenso)

# Accuracy
mean(pred_rf == test$riesgo_suspenso)


# TP, TN, FP, FN
TP <- sum(pred_rf == 1 & test$riesgo_suspenso == 1)
TN <- sum(pred_rf == 0 & test$riesgo_suspenso == 0)
FP <- sum(pred_rf == 1 & test$riesgo_suspenso == 0)
FN <- sum(pred_rf == 0 & test$riesgo_suspenso == 1)

# Métricas
precision_rf <- TP / (TP + FP)
recall_rf <- TP / (TP + FN)
F1_rf <- 2 * (precision_rf * recall_rf) / (precision_rf + recall_rf)

precision_rf
recall_rf
F1_rf


# ------------------------------------------------------------
# MODELO RANDOM FOREST
# ------------------------------------------------------------

# Se entrena un modelo de Random Forest como mejora del árbol de decisión,
# ya que combina múltiples árboles para reducir la varianza y mejorar
# la capacidad predictiva del modelo.


# Se evalúa el modelo mediante métricas de clasificación para comparar
# su rendimiento con los modelos anteriores.


# ------------------------------------------------------------
# AJUSTE DE LA VARIABLE OBJETIVO PARA MODELOS DE CLASIFICACIÓN
# ------------------------------------------------------------

# Convertimos la variable objetivo a factor para que Random Forest
# interprete correctamente el problema como una tarea de clasificación
train$riesgo_suspenso <- factor(train$riesgo_suspenso)
test$riesgo_suspenso  <- factor(test$riesgo_suspenso)


# ------------------------------------------------------------
# MODELO RANDOM FOREST
# ------------------------------------------------------------

# Entrenamos un modelo de Random Forest de clasificación
modelo_rf <- randomForest(
  riesgo_suspenso ~ ., 
  data = train,
  ntree = 100,
  mtry = 3,
  importance = TRUE
)

modelo_rf

# ------------------------------------------------------------
# PREDICCIONES RANDOM FOREST
# ------------------------------------------------------------

# Obtenemos predicciones del modelo sobre el conjunto de test
pred_rf <- predict(modelo_rf, newdata = test)


# Matriz de confusión
table(Predicho = pred_rf, Real = test$riesgo_suspenso)

# Accuracy
mean(pred_rf == test$riesgo_suspenso)


# ------------------------------------------------------------
# MÉTRICAS RANDOM FOREST
# ------------------------------------------------------------

# TP, TN, FP y FN
TP <- sum(pred_rf == 1 & test$riesgo_suspenso == 1)
TN <- sum(pred_rf == 0 & test$riesgo_suspenso == 0)
FP <- sum(pred_rf == 1 & test$riesgo_suspenso == 0)
FN <- sum(pred_rf == 0 & test$riesgo_suspenso == 1)

# Precision
precision_rf <- TP / (TP + FP)

# Recall
recall_rf <- TP / (TP + FN)

# F1-score
F1_rf <- 2 * (precision_rf * recall_rf) / (precision_rf + recall_rf)

precision_rf
recall_rf
F1_rf

TP <- sum(pred_rf == "1" & test$riesgo_suspenso == "1")
TN <- sum(pred_rf == "0" & test$riesgo_suspenso == "0")
FP <- sum(pred_rf == "1" & test$riesgo_suspenso == "0")
FN <- sum(pred_rf == "0" & test$riesgo_suspenso == "1")


# ------------------------------------------------------------
# CORRECCIÓN DEL RANDOM FOREST
# ------------------------------------------------------------

# Se convierte la variable objetivo a formato factor para asegurar que
# Random Forest trate el problema como clasificación y no como regresión.


# ------------------------------------------------------------
# COMPARACIÓN FINAL DE MODELOS
# ------------------------------------------------------------

# Reunimos las métricas principales de los tres modelos para compararlos
comparacion_modelos <- data.frame(
  Modelo = c("Regresión logística", "Árbol de decisión", "Random Forest"),
  Accuracy = c(
    mean(predicciones == test$riesgo_suspenso),
    mean(pred_arbol == test$riesgo_suspenso),
    mean(pred_rf == test$riesgo_suspenso)
  ),
  Precision = c(
    precision,
    precision_arbol,
    precision_rf
  ),
  Recall = c(
    recall,
    recall_arbol,
    recall_rf
  ),
  F1 = c(
    F1,
    F1_arbol,
    F1_rf
  )
)

comparacion_modelos

# ------------------------------------------------------------
# CONCLUSIÓN FINAL DEL ANÁLISIS DEL DATO
# ------------------------------------------------------------

# La comparación de modelos permite identificar cuál ofrece mejor equilibrio
# entre capacidad predictiva e interpretabilidad.
# En este caso, el árbol de decisión y el Random Forest presentan un rendimiento
# ligeramente superior a la regresión logística, especialmente en precisión y F1-score.
# No obstante, dado que Random Forest no mejora al árbol de decisión y este último
# resulta más interpretable, el árbol se perfila como la alternativa más equilibrada.


# ------------------------------------------------------------
# INTERPRETACIÓN FINAL DE LOS MODELOS
# ------------------------------------------------------------

# Los resultados muestran que el árbol de decisión y el Random Forest
# presentan un rendimiento ligeramente superior a la regresión logística.
# Sin embargo, ambos modelos ofrecen métricas prácticamente idénticas.

# Dado que el árbol de decisión proporciona una mayor interpretabilidad,
# se considera el modelo más adecuado para este problema.

# Además, todos los modelos coinciden en identificar las variables G2 y G1
# como los principales determinantes del riesgo de suspenso, lo que refuerza
# la consistencia del análisis realizado.


library(ggplot2)

# Convertimos a formato largo
library(tidyr)

comparacion_long <- comparacion_modelos %>%
  pivot_longer(cols = c(Accuracy, Precision, Recall, F1),
               names_to = "Metrica",
               values_to = "Valor")

# Gráfico
ggplot(comparacion_long, aes(x = Modelo, y = Valor, fill = Metrica)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Comparación de modelos",
    x = "Modelo",
    y = "Valor"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# CONCLUSIONES COMPARACIÓN DE MODELOS
# ------------------------------------------------------------

# Los tres modelos presentan un rendimiento muy similar en términos de accuracy,
# lo que indica que el problema está bien definido y que las variables utilizadas
# tienen una alta capacidad explicativa.

# El árbol de decisión y el Random Forest muestran una ligera mejora en precisión
# y F1-score respecto a la regresión logística, lo que indica una mejor capacidad
# para identificar correctamente los casos de suspenso.

# Sin embargo, el recall se mantiene prácticamente constante en los tres modelos,
# lo que sugiere que la capacidad de detectar todos los casos reales de suspenso
# es limitada.

# Esto indica que el problema presenta cierto desbalanceo o dificultad intrínseca
# en la identificación de los casos positivos, aspecto que podría mejorarse en
# fases posteriores mediante técnicas específicas.


# Se selecciona el árbol de decisión como modelo final debido a su equilibrio
# entre rendimiento y capacidad interpretativa, lo que permite explicar de forma
# clara el comportamiento del modelo.


predicciones <- ifelse(probabilidades > 0.5, 1, 0)


# ------------------------------------------------------------
# AJUSTE DEL UMBRAL DE DECISIÓN
# ------------------------------------------------------------

# Probamos un umbral más bajo para detectar mejor los casos de suspenso
umbral <- 0.3

predicciones_umbral <- ifelse(probabilidades > umbral, 1, 0)

# Matriz de confusión
table(Predicho = predicciones_umbral, Real = test$riesgo_suspenso)

# Accuracy
mean(predicciones_umbral == test$riesgo_suspenso)



# TP, TN, FP, FN
TP_u <- sum(predicciones_umbral == 1 & test$riesgo_suspenso == 1)
TN_u <- sum(predicciones_umbral == 0 & test$riesgo_suspenso == 0)
FP_u <- sum(predicciones_umbral == 1 & test$riesgo_suspenso == 0)
FN_u <- sum(predicciones_umbral == 0 & test$riesgo_suspenso == 1)

# Precision
precision_u <- TP_u / (TP_u + FP_u)

# Recall
recall_u <- TP_u / (TP_u + FN_u)

# F1
F1_u <- 2 * (precision_u * recall_u) / (precision_u + recall_u)

precision_u
recall_u
F1_u



# ------------------------------------------------------------
# CONCLUSIONES AJUSTE DE UMBRAL
# ------------------------------------------------------------

# Se reduce el umbral de decisión para priorizar la detección de estudiantes
# en riesgo de suspenso, lo que permite aumentar el recall del modelo.

# Este ajuste implica un incremento de falsos positivos, pero es adecuado en
# este contexto, ya que es preferible identificar más casos potenciales de riesgo
# que dejar sin detectar estudiantes que realmente podrían suspender.

# Este enfoque refleja una decisión metodológica basada en el contexto del problema.


install.packages("pROC")
library(pROC)


# ------------------------------------------------------------
# CURVA ROC Y AUC
# ------------------------------------------------------------

roc_obj <- roc(test$riesgo_suspenso, probabilidades)

plot(roc_obj, col = "blue", main = "Curva ROC")

auc(roc_obj)


# La curva ROC permite evaluar el rendimiento del modelo para distintos umbrales.
# El AUC proporciona una medida global de la capacidad discriminativa del modelo,
# independientemente del umbral seleccionado.


umbrales <- seq(0.1, 0.9, by = 0.1)

resultados_umbral <- data.frame()

for (u in umbrales) {
  
  pred <- ifelse(probabilidades > u, 1, 0)
  
  TP <- sum(pred == 1 & test$riesgo_suspenso == 1)
  FP <- sum(pred == 1 & test$riesgo_suspenso == 0)
  FN <- sum(pred == 0 & test$riesgo_suspenso == 1)
  
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  F1 <- 2 * (precision * recall) / (precision + recall)
  
  resultados_umbral <- rbind(resultados_umbral,
                             data.frame(Umbral = u,
                                        Precision = precision,
                                        Recall = recall,
                                        F1 = F1))
}

resultados_umbral

# Se analizan distintos umbrales de decisión para evaluar el impacto en precision,
# recall y F1-score, con el objetivo de seleccionar el umbral óptimo en función
# del problema.


# ------------------------------------------------------------
# CONCLUSIONES ANÁLISIS DE UMBRAL
# ------------------------------------------------------------

# El análisis de distintos umbrales muestra que el valor por defecto (0.5)
# no es el óptimo para este problema.

# Umbrales más bajos permiten aumentar significativamente el recall,
# es decir, la capacidad de detectar estudiantes en riesgo de suspenso.

# En particular, el umbral 0.1 alcanza el mayor F1-score, equilibrando
# adecuadamente precision y recall.

# Desde el punto de vista del problema, resulta preferible maximizar el recall,
# ya que es más importante detectar posibles suspensos que evitar falsos positivos.

# Por tanto, se selecciona un umbral inferior a 0.5 como estrategia óptima
# de decisión del modelo.


predicciones <- ifelse(probabilidades > 0.5, 1, 0)

umbral_final <- 0.2

predicciones_final <- ifelse(probabilidades > umbral_final, 1, 0)


# Matriz de confusión final
table(Predicho = predicciones_final, Real = test$riesgo_suspenso)

# Accuracy
mean(predicciones_final == test$riesgo_suspenso)


# Se ajusta el umbral de decisión a 0.2 con el objetivo de mejorar la
# detección de estudiantes en riesgo, priorizando el recall frente a la accuracy.

# Esta decisión refleja una adaptación del modelo al contexto real del problema,
# donde es más crítico identificar casos de riesgo que minimizar falsos positivos.


# ------------------------------------------------------------
# ANÁLISIS DEL DESBALANCEO EN EL CONJUNTO DE ENTRENAMIENTO
# ------------------------------------------------------------

# Comprobamos la distribución de la variable objetivo en train
# para evaluar si existe desbalanceo de clases
table(train$riesgo_suspenso)
prop.table(table(train$riesgo_suspenso))


# ------------------------------------------------------------
# BALANCEO DE CLASES MEDIANTE OVERSAMPLING
# ------------------------------------------------------------

# Separamos las observaciones de cada clase
train_no <- train[train$riesgo_suspenso == "0", ]
train_si <- train[train$riesgo_suspenso == "1", ]

# Observamos el tamaño de cada grupo
nrow(train_no)
nrow(train_si)

# Fijamos semilla para reproducibilidad
set.seed(123)

# Generamos una muestra adicional de la clase minoritaria con reemplazo
# hasta igualar el tamaño de la clase mayoritaria
train_si_oversampled <- train_si[sample(1:nrow(train_si),
                                        size = nrow(train_no),
                                        replace = TRUE), ]

# Unimos ambas clases para obtener un conjunto balanceado
train_balanceado <- rbind(train_no, train_si_oversampled)

# Mezclamos aleatoriamente las filas para evitar bloques de una sola clase
train_balanceado <- train_balanceado[sample(1:nrow(train_balanceado)), ]

# Comprobamos la nueva distribución
table(train_balanceado$riesgo_suspenso)
prop.table(table(train_balanceado$riesgo_suspenso))


# ------------------------------------------------------------
# REGRESIÓN LOGÍSTICA CON TRAIN BALANCEADO
# ------------------------------------------------------------

# Entrenamos una nueva regresión logística sobre el dataset balanceado
modelo_logit_bal <- glm(riesgo_suspenso ~ ., data = train_balanceado, family = "binomial")

# Resumen del modelo
summary(modelo_logit_bal)


# ------------------------------------------------------------
# PREDICCIONES DEL MODELO BALANCEADO
# ------------------------------------------------------------

# Obtenemos probabilidades sobre el conjunto de test original
prob_bal <- predict(modelo_logit_bal, newdata = test, type = "response")

# Usamos inicialmente el umbral 0.5 para comparar de forma limpia
pred_bal <- ifelse(prob_bal > 0.5, 1, 0)

# Matriz de confusión
table(Predicho = pred_bal, Real = test$riesgo_suspenso)

# Accuracy
mean(pred_bal == test$riesgo_suspenso)


# ------------------------------------------------------------
# MÉTRICAS DEL MODELO BALANCEADO
# ------------------------------------------------------------

# Cálculo de TP, TN, FP y FN
TP_bal <- sum(pred_bal == 1 & test$riesgo_suspenso == "1")
TN_bal <- sum(pred_bal == 0 & test$riesgo_suspenso == "0")
FP_bal <- sum(pred_bal == 1 & test$riesgo_suspenso == "0")
FN_bal <- sum(pred_bal == 0 & test$riesgo_suspenso == "1")

# Precision
precision_bal <- TP_bal / (TP_bal + FP_bal)

# Recall
recall_bal <- TP_bal / (TP_bal + FN_bal)

# F1-score
F1_bal <- 2 * (precision_bal * recall_bal) / (precision_bal + recall_bal)

precision_bal
recall_bal
F1_bal

# ------------------------------------------------------------
# BALANCEO DE CLASES
# ------------------------------------------------------------

# Dado que la clase correspondiente a estudiantes en riesgo de suspenso
# es minoritaria, se aplica una técnica de oversampling sobre el conjunto
# de entrenamiento para equilibrar ambas clases.

# Este procedimiento busca mejorar la capacidad del modelo para detectar
# casos positivos, evitando que el aprendizaje quede sesgado hacia la clase mayoritaria.

# ------------------------------------------------------------
# CONCLUSIONES DEL BALANCEO
# ------------------------------------------------------------

# El balanceo de clases permite evaluar si el modelo mejora su capacidad
# para detectar estudiantes en riesgo de suspenso.

# En particular, el aspecto más relevante a observar será el recall,
# ya que un aumento de esta métrica indicará una mayor sensibilidad
# del modelo ante la clase minoritaria.


# ------------------------------------------------------------
# CONCLUSIONES DEL MODELO BALANCEADO
# ------------------------------------------------------------

# El uso de técnicas de balanceo mediante oversampling ha permitido mejorar
# significativamente la capacidad del modelo para detectar estudiantes en riesgo
# de suspenso.

# En particular, se observa un aumento notable del recall, pasando de valores
# cercanos a 0.65 a valores superiores a 0.90, lo que indica una mejora sustancial
# en la identificación de la clase minoritaria.

# Aunque la precisión disminuye ligeramente, el incremento en recall y en F1-score
# demuestra que el modelo balanceado ofrece un mejor rendimiento global en el
# contexto del problema.

# Este resultado confirma la importancia de tratar el desbalanceo de clases en
# problemas de clasificación, especialmente cuando la detección de la clase
# positiva es prioritaria.



comparacion_final <- data.frame(
  Modelo = c("Logística original", "Logística balanceada"),
  Precision = c(precision, precision_bal),
  Recall = c(recall, recall_bal),
  F1 = c(F1, F1_bal)
)

comparacion_final


# ------------------------------------------------------------
# CONCLUSIONES COMPARACIÓN FINAL DE MODELOS
# ------------------------------------------------------------

# La comparación entre el modelo original y el modelo balanceado muestra
# diferencias significativas en el rendimiento.

# El modelo original presenta una alta precisión, pero un recall muy bajo,
# lo que indica que no es capaz de detectar adecuadamente los casos reales
# de suspenso.

# Por el contrario, el modelo balanceado incrementa notablemente el recall,
# alcanzando valores superiores a 0.90, lo que permite identificar la mayoría
# de los estudiantes en riesgo.

# Aunque la precisión disminuye ligeramente, el aumento del F1-score demuestra
# que el modelo balanceado ofrece un mejor equilibrio global.

# En el contexto del problema, donde es prioritario detectar alumnos en riesgo,
# el modelo balanceado se considera la mejor opción.




































