# ============================================================
# TFG | Predicción del riesgo de suspenso en educación secundaria
# Código final de ingeniería del dato y análisis del dato
# Autor: Manuel Ventosa Rodríguez
# ============================================================

# ------------------------------------------------------------
# 1. Instalación y carga de librerías
# ------------------------------------------------------------


library(tidyverse)
library(caret)
library(pROC)
library(rpart)
library(rpart.plot)
library(randomForest)
library(car)
library(gridExtra)
library(ROSE)
library(ggcorrplot)

set.seed(123)
options(scipen = 999)
theme_set(theme_minimal())

# ------------------------------------------------------------
# 2. Carga e inspección inicial de datos
# ------------------------------------------------------------

datos_por <- read.csv2("student-por.csv")
datos_mat <- read.csv2("student-mat.csv")


# Inspección básica
dim(datos_por)
dim(datos_mat)
colSums(is.na(datos_por))
colSums(is.na(datos_mat))
sum(duplicated(datos_por))
sum(duplicated(datos_mat))

summary(datos_por)
summary(datos_mat)

cat("Valores nulos student-por:", sum(colSums(is.na(datos_por))), "\n")
cat("Valores nulos student-mat:", sum(colSums(is.na(datos_mat))), "\n")


# ------------------------------------------------------------
# 3. Combinación de datasets
# ------------------------------------------------------------

# Columnas comunes para identificar estudiantes en ambos datasets
cols_cruce <- c("school", "sex", "age", "address", "famsize", "Pstatus",
                "Medu", "Fedu", "Mjob", "Fjob", "reason", "nursery", "internet")

# Cruce — solo estudiantes presentes en los dos datasets
datos_combinados <- inner_join(
  datos_por,
  datos_mat %>% select(all_of(cols_cruce), G1, G2, G3, absences, failures) %>%
    rename(G1_mat = G1, G2_mat = G2, G3_mat = G3,
           absences_mat = absences, failures_mat = failures),
  by = cols_cruce
)

# Inspección del resultado
dim(datos_combinados)
colSums(is.na(datos_combinados))


# ------------------------------------------------------------
# 4. Preparación del dato: tipado de variables
# ------------------------------------------------------------

datos <- datos_combinados %>%
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

str(datos)


# ------------------------------------------------------------
# 5. Definición de la variable objetivo
# ------------------------------------------------------------

datos <- datos %>%
  mutate(
    riesgo_suspenso = ifelse(G3 < 10, "Si", "No"),
    riesgo_suspenso = factor(riesgo_suspenso, levels = c("Si", "No"))
  )

table(datos$riesgo_suspenso)
prop.table(table(datos$riesgo_suspenso))

ggplot(datos, aes(x = riesgo_suspenso, fill = riesgo_suspenso)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
  labs(
    title = "Distribución de la variable objetivo",
    x = "Riesgo de suspenso",
    y = "Número de alumnos"
  ) +
  scale_fill_manual(values = c("No" = "#2ecc71", "Si" = "#e74c3c")) +
  theme(legend.position = "none")


# ------------------------------------------------------------
# 6. Análisis exploratorio de variables relevantes
# ------------------------------------------------------------

# Distribución de notas numéricas
p1 <- ggplot(datos, aes(x = G1, fill = riesgo_suspenso)) +
  geom_histogram(binwidth = 1, alpha = 0.7, position = "identity") +
  labs(title = "Distribución G1", x = "Nota G1", y = "Frecuencia") +
  scale_fill_manual(values = c("No" = "#2ecc71", "Si" = "#e74c3c"))

p2 <- ggplot(datos, aes(x = G2, fill = riesgo_suspenso)) +
  geom_histogram(binwidth = 1, alpha = 0.7, position = "identity") +
  labs(title = "Distribución G2", x = "Nota G2", y = "Frecuencia") +
  scale_fill_manual(values = c("No" = "#2ecc71", "Si" = "#e74c3c"))

p3 <- ggplot(datos, aes(x = G3, fill = riesgo_suspenso)) +
  geom_histogram(binwidth = 1, alpha = 0.7, position = "identity") +
  labs(title = "Distribución G3 (objetivo)", x = "Nota G3", y = "Frecuencia") +
  scale_fill_manual(values = c("No" = "#2ecc71", "Si" = "#e74c3c"))

grid.arrange(p1, p2, p3, ncol = 3)

# Boxplots de variables clave vs riesgo
p4 <- ggplot(datos, aes(x = riesgo_suspenso, y = G2, fill = riesgo_suspenso)) +
  geom_boxplot() +
  labs(title = "G2 vs riesgo", x = "Riesgo", y = "Nota G2") +
  scale_fill_manual(values = c("No" = "#2ecc71", "Si" = "#e74c3c")) +
  theme(legend.position = "none")

p5 <- ggplot(datos, aes(x = riesgo_suspenso, y = failures, fill = riesgo_suspenso)) +
  geom_boxplot() +
  labs(title = "Failures vs riesgo", x = "Riesgo", y = "Nº suspensos previos") +
  scale_fill_manual(values = c("No" = "#2ecc71", "Si" = "#e74c3c")) +
  theme(legend.position = "none")

p6 <- ggplot(datos, aes(x = riesgo_suspenso, y = studytime, fill = riesgo_suspenso)) +
  geom_boxplot() +
  labs(title = "Studytime vs riesgo", x = "Riesgo", y = "Tiempo de estudio") +
  scale_fill_manual(values = c("No" = "#2ecc71", "Si" = "#e74c3c")) +
  theme(legend.position = "none")

p7 <- ggplot(datos, aes(x = riesgo_suspenso, y = absences, fill = riesgo_suspenso)) +
  geom_boxplot() +
  labs(title = "Absences vs riesgo", x = "Riesgo", y = "Ausencias") +
  scale_fill_manual(values = c("No" = "#2ecc71", "Si" = "#e74c3c")) +
  theme(legend.position = "none")

grid.arrange(p4, p5, p6, p7, ncol = 2)

# Variables categóricas vs riesgo
p8 <- ggplot(datos, aes(x = internet, fill = riesgo_suspenso)) +
  geom_bar(position = "fill") +
  labs(title = "Internet vs riesgo", x = "Internet", y = "Proporción") +
  scale_fill_manual(values = c("No" = "#2ecc71", "Si" = "#e74c3c"))

p9 <- ggplot(datos, aes(x = higher, fill = riesgo_suspenso)) +
  geom_bar(position = "fill") +
  labs(title = "Quiere estudiar más vs riesgo", x = "Higher", y = "Proporción") +
  scale_fill_manual(values = c("No" = "#2ecc71", "Si" = "#e74c3c"))

p10 <- ggplot(datos, aes(x = sex, fill = riesgo_suspenso)) +
  geom_bar(position = "fill") +
  labs(title = "Sexo vs riesgo", x = "Sexo", y = "Proporción") +
  scale_fill_manual(values = c("No" = "#2ecc71", "Si" = "#e74c3c"))

p11 <- ggplot(datos, aes(x = address, fill = riesgo_suspenso)) +
  geom_bar(position = "fill") +
  labs(title = "Zona residencial vs riesgo", x = "Address", y = "Proporción") +
  scale_fill_manual(values = c("No" = "#2ecc71", "Si" = "#e74c3c"))

grid.arrange(p8, p9, p10, p11, ncol = 2)

# Comparativa notas Portugués vs Matemáticas
p12 <- ggplot(datos, aes(x = G3, y = G3_mat, color = riesgo_suspenso)) +
  geom_point(alpha = 0.7) +
  labs(title = "Nota final: Portugués vs Matemáticas",
       x = "G3 Portugués", y = "G3 Matemáticas") +
  scale_color_manual(values = c("No" = "#2ecc71", "Si" = "#e74c3c"))

p13 <- ggplot(datos, aes(x = riesgo_suspenso, y = G3_mat, fill = riesgo_suspenso)) +
  geom_boxplot() +
  labs(title = "Nota Matemáticas vs riesgo en Portugués",
       x = "Riesgo suspenso Portugués", y = "G3 Matemáticas") +
  scale_fill_manual(values = c("No" = "#2ecc71", "Si" = "#e74c3c")) +
  theme(legend.position = "none")

grid.arrange(p12, p13, ncol = 2)


# ------------------------------------------------------------
# 7. Creación de variables derivadas
# ------------------------------------------------------------

datos <- datos %>%
  mutate(
    # Variables básicas
    media_notas         = (G1 + G2) / 2,
    educacion_familiar  = (Medu + Fedu) / 2,
    alcohol_medio       = (Dalc + Walc) / 2,
    absentismo_alto     = factor(ifelse(absences > 10, "Si", "No")),
    
    # Variables avanzadas nivel 1
    progreso_academico      = G3 - G1,
    # Nota: progreso_academico contiene G3 (variable objetivo). Se mantiene
    # para el análisis exploratorio pero NO se incluye en el modelado
    # predictivo para evitar data leakage.
    rendimiento_relativo    = G3 - G3_mat,
    # Nota: rendimiento_relativo también contiene G3. Misma razón anterior.
    riesgo_doble            = factor(ifelse(G3 < 10 & G3_mat < 10, "Si", "No")),
    apoyo_educativo         = as.numeric(schoolsup == "yes") +
      as.numeric(famsup == "yes") +
      as.numeric(paid == "yes") +
      as.numeric(internet == "yes"),
    perfil_riesgo_social    = goout + alcohol_medio - studytime,
    
    # Variables avanzadas nivel 2
    indice_rendimiento_global   = (G1 + G2 + G1_mat + G2_mat) / 4,
    estabilidad_academica       = abs(G2 - G1) + abs(G2_mat - G1_mat),
    # Nota: estabilidad_academica usa G1, G2, G1_mat y G2_mat. Se incluye en
    # el Escenario A (con notas previas) pero NO en el Escenario B, cuya
    # premisa es prescindir de calificaciones previas.
    interaccion_alcohol_estudio = alcohol_medio * (5 - studytime),
    perfil_familiar_riesgo      = as.numeric(Pstatus == "A") +
      as.numeric(famsup == "no") +
      as.numeric(schoolsup == "no") +
      as.numeric(educacion_familiar < 2)
  )

# Variables excluidas del modelado por data leakage:
# - progreso_academico: usa G3
# - rendimiento_relativo: usa G3 y G3_mat
# - riesgo_doble: usa G3 y G3_mat
# Estas variables se conservan únicamente con fines exploratorios.


# ------------------------------------------------------------
# 8. Visualización de variables derivadas
# ------------------------------------------------------------

p17 <- ggplot(datos, aes(x = riesgo_suspenso, y = media_notas, fill = riesgo_suspenso)) +
  geom_boxplot() +
  labs(title = "Media de notas vs riesgo", x = "Riesgo", y = "Media G1+G2") +
  scale_fill_manual(values = c("No" = "#2ecc71", "Si" = "#e74c3c")) +
  theme(legend.position = "none")

# p18 y p19 se incluyen en el EDA con valor exploratorio.
# progreso_academico y rendimiento_relativo NO entrarán en el modelado
# al contener G3 (data leakage con la variable objetivo).
p18 <- ggplot(datos, aes(x = riesgo_suspenso, y = progreso_academico, fill = riesgo_suspenso)) +
  geom_boxplot() +
  labs(title = "Progreso académico vs riesgo (solo EDA)", x = "Riesgo", y = "G3 - G1") +
  scale_fill_manual(values = c("No" = "#2ecc71", "Si" = "#e74c3c")) +
  theme(legend.position = "none")

p19 <- ggplot(datos, aes(x = riesgo_suspenso, y = rendimiento_relativo, fill = riesgo_suspenso)) +
  geom_boxplot() +
  labs(title = "Rendimiento relativo vs riesgo (solo EDA)", x = "Riesgo", y = "G3 Por - G3 Mat") +
  scale_fill_manual(values = c("No" = "#2ecc71", "Si" = "#e74c3c")) +
  theme(legend.position = "none")

p20 <- ggplot(datos, aes(x = riesgo_suspenso, y = apoyo_educativo, fill = riesgo_suspenso)) +
  geom_boxplot() +
  labs(title = "Apoyo educativo vs riesgo", x = "Riesgo", y = "Índice de apoyo") +
  scale_fill_manual(values = c("No" = "#2ecc71", "Si" = "#e74c3c")) +
  theme(legend.position = "none")

p21 <- ggplot(datos, aes(x = riesgo_suspenso, y = perfil_riesgo_social, fill = riesgo_suspenso)) +
  geom_boxplot() +
  labs(title = "Perfil riesgo social vs riesgo", x = "Riesgo", y = "Índice social") +
  scale_fill_manual(values = c("No" = "#2ecc71", "Si" = "#e74c3c")) +
  theme(legend.position = "none")

p22 <- ggplot(datos, aes(x = riesgo_doble, fill = riesgo_suspenso)) +
  geom_bar(position = "fill") +
  labs(title = "Riesgo doble vs riesgo (solo EDA)",
       x = "Suspende ambas", y = "Proporción") +
  scale_fill_manual(values = c("No" = "#2ecc71", "Si" = "#e74c3c"))

p23 <- ggplot(datos, aes(x = riesgo_suspenso, y = indice_rendimiento_global, fill = riesgo_suspenso)) +
  geom_boxplot() +
  labs(title = "Índice rendimiento global vs riesgo", x = "Riesgo", y = "Índice") +
  scale_fill_manual(values = c("No" = "#2ecc71", "Si" = "#e74c3c")) +
  theme(legend.position = "none")

p24 <- ggplot(datos, aes(x = riesgo_suspenso, y = estabilidad_academica, fill = riesgo_suspenso)) +
  geom_boxplot() +
  labs(title = "Estabilidad académica vs riesgo", x = "Riesgo", y = "Variación entre evaluaciones") +
  scale_fill_manual(values = c("No" = "#2ecc71", "Si" = "#e74c3c")) +
  theme(legend.position = "none")

p25 <- ggplot(datos, aes(x = riesgo_suspenso, y = interaccion_alcohol_estudio, fill = riesgo_suspenso)) +
  geom_boxplot() +
  labs(title = "Interacción alcohol-estudio vs riesgo", x = "Riesgo", y = "Índice") +
  scale_fill_manual(values = c("No" = "#2ecc71", "Si" = "#e74c3c")) +
  theme(legend.position = "none")

p26 <- ggplot(datos, aes(x = riesgo_suspenso, y = perfil_familiar_riesgo, fill = riesgo_suspenso)) +
  geom_boxplot() +
  labs(title = "Perfil familiar de riesgo vs riesgo", x = "Riesgo", y = "Índice") +
  scale_fill_manual(values = c("No" = "#2ecc71", "Si" = "#e74c3c")) +
  theme(legend.position = "none")

grid.arrange(p17, p18, p19, p20, p21, p22, ncol = 3)
grid.arrange(p23, p24, p25, p26, ncol = 2)

# ------------------------------------------------------------
# 9. Matriz de correlación
# ------------------------------------------------------------

variables_numericas <- datos %>%
  select(G1, G2, G3, G1_mat, G2_mat, G3_mat,
         failures, failures_mat, studytime, absences, absences_mat,
         educacion_familiar, alcohol_medio, media_notas,
         progreso_academico, rendimiento_relativo,
         apoyo_educativo, perfil_riesgo_social,
         age, freetime, goout) %>%
  cor()

ggcorrplot(variables_numericas,
           method = "circle",
           type = "lower",
           lab = TRUE,
           lab_size = 2,
           title = "Matriz de correlación entre variables numéricas",
           colors = c("#e74c3c", "white", "#2ecc71"))


# ------------------------------------------------------------
# 10. Selección de variables para modelado
# ------------------------------------------------------------

# Escenario A: con notas previas de ambas asignaturas (19 variables)
# Se excluyen progreso_academico y rendimiento_relativo por contener G3
# (data leakage con la variable objetivo riesgo_suspenso).
datos_modelo_A <- datos %>%
  select(
    riesgo_suspenso,
    G1, G2, G1_mat, G2_mat,
    failures, failures_mat,
    studytime,
    higher,
    absences,
    absentismo_alto,
    educacion_familiar,
    alcohol_medio,
    apoyo_educativo,
    perfil_riesgo_social,
    indice_rendimiento_global,
    estabilidad_academica,
    interaccion_alcohol_estudio,
    perfil_familiar_riesgo
  )

# Escenario B: sin notas previas (14 variables)
# Se excluye estabilidad_academica porque usa G1, G2, G1_mat y G2_mat,
# variables que contradicen la premisa de este escenario.
datos_modelo_B <- datos %>%
  select(
    riesgo_suspenso,
    failures, failures_mat,
    studytime,
    higher,
    absences,
    absentismo_alto,
    educacion_familiar,
    alcohol_medio,
    apoyo_educativo,
    perfil_riesgo_social,
    interaccion_alcohol_estudio,
    perfil_familiar_riesgo
  )

str(datos_modelo_A)
str(datos_modelo_B)

# Escenario B: sin notas previas ni variables derivadas de calificaciones finales
# Se excluyen:
# - G1, G2, G1_mat, G2_mat
# - estabilidad_academica
# - progreso_academico
# - rendimiento_relativo
# - riesgo_doble


# ------------------------------------------------------------
# 11. División en train y test (estratificada)
# ------------------------------------------------------------

# Escenario A
train_index_A <- createDataPartition(datos_modelo_A$riesgo_suspenso, p = 0.7, list = FALSE)
train_A <- datos_modelo_A[train_index_A, ]
test_A  <- datos_modelo_A[-train_index_A, ]

# Escenario B
train_index_B <- createDataPartition(datos_modelo_B$riesgo_suspenso, p = 0.7, list = FALSE)
train_B <- datos_modelo_B[train_index_B, ]
test_B  <- datos_modelo_B[-train_index_B, ]

# Comprobación de proporciones
prop.table(table(train_A$riesgo_suspenso))
prop.table(table(test_A$riesgo_suspenso))
prop.table(table(train_B$riesgo_suspenso))
prop.table(table(test_B$riesgo_suspenso))


# ------------------------------------------------------------
# 12. Balanceo de clases con ROSE
# ------------------------------------------------------------

# Balanceo Escenario A
train_A_balanced <- ROSE(riesgo_suspenso ~ ., data = train_A, seed = 123)$data
prop.table(table(train_A_balanced$riesgo_suspenso))

# Balanceo Escenario B
train_B_balanced <- ROSE(riesgo_suspenso ~ ., data = train_B, seed = 123)$data
prop.table(table(train_B_balanced$riesgo_suspenso))

# Visualización antes vs después del balanceo
par(mfrow = c(1, 2))

barplot(prop.table(table(train_A$riesgo_suspenso)),
        main = "Antes del balanceo (A)",
        col = c("#2ecc71", "#e74c3c"),
        ylim = c(0, 1),
        ylab = "Proporción")

barplot(prop.table(table(train_A_balanced$riesgo_suspenso)),
        main = "Después del balanceo (A)",
        col = c("#2ecc71", "#e74c3c"),
        ylim = c(0, 1),
        ylab = "Proporción")

par(mfrow = c(1, 1))

# Se genera una versión balanceada del conjunto de entrenamiento para el ajuste
# final de los modelos. En la búsqueda de hiperparámetros, el balanceo se aplica
# dentro de cada fold mediante sampling = "rose" en trainControl.


# ------------------------------------------------------------
# 13. Búsqueda de hiperparámetros óptimos
# ------------------------------------------------------------

control_hp <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  sampling = "rose"
)
# Nota: sampling = "rose" dentro de trainControl aplica el balanceo
# en cada fold de validación cruzada, evitando que observaciones
# sintéticas se filtren a los folds de validación.

# --- Árbol de decisión Escenario A ---
grid_arbol <- expand.grid(cp = seq(0.001, 0.05, by = 0.005))

busqueda_arbol_A <- train(
  riesgo_suspenso ~ .,
  data = train_A,
  method = "rpart",
  trControl = control_hp,
  tuneGrid = grid_arbol,
  metric = "ROC"
)

print(busqueda_arbol_A)
plot(busqueda_arbol_A, main = "Selección de hiperparámetro cp - Árbol Escenario A")
cp_optimo_A <- busqueda_arbol_A$bestTune$cp
cat("cp óptimo Escenario A:", cp_optimo_A, "\n")

# --- Random Forest Escenario A ---
grid_rf <- expand.grid(mtry = c(2, 3, 4, 5, 6, 7))

busqueda_rf_A <- train(
  riesgo_suspenso ~ .,
  data = train_A,
  method = "rf",
  trControl = control_hp,
  tuneGrid = grid_rf,
  metric = "ROC",
  ntree = 100
)

print(busqueda_rf_A)
plot(busqueda_rf_A, main = "Selección de hiperparámetro mtry - RF Escenario A")
mtry_optimo_A <- busqueda_rf_A$bestTune$mtry
cat("mtry óptimo Escenario A:", mtry_optimo_A, "\n")

# --- Árbol de decisión Escenario B ---
busqueda_arbol_B <- train(
  riesgo_suspenso ~ .,
  data = train_B,
  method = "rpart",
  trControl = control_hp,
  tuneGrid = grid_arbol,
  metric = "ROC"
)

print(busqueda_arbol_B)
plot(busqueda_arbol_B, main = "Selección de hiperparámetro cp - Árbol Escenario B")
cp_optimo_B <- busqueda_arbol_B$bestTune$cp
cat("cp óptimo Escenario B:", cp_optimo_B, "\n")

# --- Random Forest Escenario B ---
busqueda_rf_B <- train(
  riesgo_suspenso ~ .,
  data = train_B,
  method = "rf",
  trControl = control_hp,
  tuneGrid = grid_rf,
  metric = "ROC",
  ntree = 100
)

print(busqueda_rf_B)
plot(busqueda_rf_B, main = "Selección de hiperparámetro mtry - RF Escenario B")
mtry_optimo_B <- busqueda_rf_B$bestTune$mtry
cat("mtry óptimo Escenario B:", mtry_optimo_B, "\n")


# ------------------------------------------------------------
# 14. Modelos Escenario A (con notas previas)
# ------------------------------------------------------------

# --- Regresión Logística ---

modelo_logit_A <- glm(riesgo_suspenso ~ ., data = train_A_balanced, family = "binomial")
summary(modelo_logit_A)
vif(modelo_logit_A)

# Predicciones umbral 0.5
prob_A <- predict(modelo_logit_A, newdata = test_A, type = "response")
pred_A <- ifelse(prob_A > 0.5, "Si", "No")

confusionMatrix(
  factor(pred_A, levels = c("No", "Si")),
  factor(test_A$riesgo_suspenso, levels = c("No", "Si")),
  positive = "Si"
)

# Umbral ajustado 0.3
pred_A_u <- ifelse(prob_A > 0.3, "Si", "No")

confusionMatrix(
  factor(pred_A_u, levels = c("No", "Si")),
  factor(test_A$riesgo_suspenso, levels = c("No", "Si")),
  positive = "Si"
)

# Justificación visual del umbral
umbrales <- seq(0.1, 0.9, by = 0.05)
resultados_umbral <- data.frame(
  umbral = umbrales,
  recall = sapply(umbrales, function(u) {
    pred_u <- ifelse(prob_A > u, "Si", "No")
    tp <- sum(pred_u == "Si" & test_A$riesgo_suspenso == "Si")
    fn <- sum(pred_u == "No" & test_A$riesgo_suspenso == "Si")
    tp / (tp + fn)
  }),
  precision = sapply(umbrales, function(u) {
    pred_u <- ifelse(prob_A > u, "Si", "No")
    tp <- sum(pred_u == "Si" & test_A$riesgo_suspenso == "Si")
    fp <- sum(pred_u == "Si" & test_A$riesgo_suspenso == "No")
    tp / (tp + fp)
  })
)

ggplot(resultados_umbral, aes(x = umbral)) +
  geom_line(aes(y = recall, color = "Recall"), linewidth = 1) +
  geom_line(aes(y = precision, color = "Precision"), linewidth = 1) +
  geom_vline(xintercept = 0.3, linetype = "dashed", color = "gray40") +
  labs(title = "Precision y Recall según umbral de decisión",
       x = "Umbral", y = "Valor", color = "Métrica") +
  scale_color_manual(values = c("Recall" = "#e74c3c", "Precision" = "#2ecc71"))

# ROC
roc_A <- roc(test_A$riesgo_suspenso, prob_A)
auc(roc_A)


# --- Árbol de decisión ---

modelo_arbol_A <- rpart(riesgo_suspenso ~ ., data = train_A_balanced, method = "class",
                        control = rpart.control(cp = cp_optimo_A))
rpart.plot(modelo_arbol_A)

pred_arbol_A <- predict(modelo_arbol_A, newdata = test_A, type = "class")

confusionMatrix(
  factor(pred_arbol_A, levels = c("No", "Si")),
  factor(test_A$riesgo_suspenso, levels = c("No", "Si")),
  positive = "Si"
)

prob_arbol_A <- predict(modelo_arbol_A, newdata = test_A, type = "prob")[, "Si"]
roc_arbol_A <- roc(test_A$riesgo_suspenso, prob_arbol_A)
auc(roc_arbol_A)


# --- Random Forest ---

modelo_rf_A <- randomForest(riesgo_suspenso ~ ., data = train_A_balanced,
                            ntree = 100, mtry = mtry_optimo_A)

pred_rf_A <- predict(modelo_rf_A, newdata = test_A)

confusionMatrix(
  factor(pred_rf_A, levels = c("No", "Si")),
  factor(test_A$riesgo_suspenso, levels = c("No", "Si")),
  positive = "Si"
)

varImpPlot(modelo_rf_A, main = "Importancia de variables - Random Forest Escenario A")

prob_rf_A <- predict(modelo_rf_A, newdata = test_A, type = "prob")[, "Si"]
roc_rf_A <- roc(test_A$riesgo_suspenso, prob_rf_A)
auc(roc_rf_A)


# ------------------------------------------------------------
# 15. Modelos Escenario B (sin notas previas)
# ------------------------------------------------------------

# --- Regresión Logística ---

modelo_logit_B <- glm(riesgo_suspenso ~ ., data = train_B_balanced, family = "binomial")
summary(modelo_logit_B)

prob_B <- predict(modelo_logit_B, newdata = test_B, type = "response")
pred_B <- ifelse(prob_B > 0.5, "Si", "No")

confusionMatrix(
  factor(pred_B, levels = c("No", "Si")),
  factor(test_B$riesgo_suspenso, levels = c("No", "Si")),
  positive = "Si"
)

roc_B <- roc(test_B$riesgo_suspenso, prob_B)
auc(roc_B)


# --- Árbol de decisión ---

modelo_arbol_B <- rpart(riesgo_suspenso ~ ., data = train_B_balanced, method = "class",
                        control = rpart.control(cp = cp_optimo_B))
rpart.plot(modelo_arbol_B)

pred_arbol_B <- predict(modelo_arbol_B, newdata = test_B, type = "class")

confusionMatrix(
  factor(pred_arbol_B, levels = c("No", "Si")),
  factor(test_B$riesgo_suspenso, levels = c("No", "Si")),
  positive = "Si"
)

prob_arbol_B <- predict(modelo_arbol_B, newdata = test_B, type = "prob")[, "Si"]
roc_arbol_B <- roc(test_B$riesgo_suspenso, prob_arbol_B)
auc(roc_arbol_B)


# --- Random Forest ---

modelo_rf_B <- randomForest(riesgo_suspenso ~ ., data = train_B_balanced,
                            ntree = 100, mtry = mtry_optimo_B)

pred_rf_B <- predict(modelo_rf_B, newdata = test_B)

confusionMatrix(
  factor(pred_rf_B, levels = c("No", "Si")),
  factor(test_B$riesgo_suspenso, levels = c("No", "Si")),
  positive = "Si"
)

varImpPlot(modelo_rf_B, main = "Importancia de variables - Random Forest Escenario B")

prob_rf_B <- predict(modelo_rf_B, newdata = test_B, type = "prob")[, "Si"]
roc_rf_B <- roc(test_B$riesgo_suspenso, prob_rf_B)
auc(roc_rf_B)


# ------------------------------------------------------------
# 16. Comparación final de modelos
# ------------------------------------------------------------

# Las métricas se extraen directamente de confusionMatrix()
# para evitar cálculos manuales redundantes.

cm_logit_A   <- confusionMatrix(factor(pred_A,       levels = c("No","Si")), factor(test_A$riesgo_suspenso, levels = c("No","Si")), positive = "Si")
cm_logit_Au  <- confusionMatrix(factor(pred_A_u,     levels = c("No","Si")), factor(test_A$riesgo_suspenso, levels = c("No","Si")), positive = "Si")
cm_arbol_A   <- confusionMatrix(factor(pred_arbol_A, levels = c("No","Si")), factor(test_A$riesgo_suspenso, levels = c("No","Si")), positive = "Si")
cm_rf_A      <- confusionMatrix(factor(pred_rf_A,    levels = c("No","Si")), factor(test_A$riesgo_suspenso, levels = c("No","Si")), positive = "Si")
cm_logit_B   <- confusionMatrix(factor(pred_B,       levels = c("No","Si")), factor(test_B$riesgo_suspenso, levels = c("No","Si")), positive = "Si")
cm_arbol_B   <- confusionMatrix(factor(pred_arbol_B, levels = c("No","Si")), factor(test_B$riesgo_suspenso, levels = c("No","Si")), positive = "Si")
cm_rf_B      <- confusionMatrix(factor(pred_rf_B,    levels = c("No","Si")), factor(test_B$riesgo_suspenso, levels = c("No","Si")), positive = "Si")

extraer_metricas <- function(cm, roc_obj, nombre, escenario) {
  data.frame(
    Escenario   = escenario,
    Modelo      = nombre,
    Accuracy    = round(cm$overall["Accuracy"], 3),
    Precision   = round(cm$byClass["Precision"], 3),
    Recall      = round(cm$byClass["Recall"], 3),
    F1          = round(cm$byClass["F1"], 3),
    Especificidad = round(cm$byClass["Specificity"], 3),
    AUC         = round(auc(roc_obj), 3)
  )
}

comparacion <- rbind(
  extraer_metricas(cm_logit_A,  roc_A,       "Logística (0.5)", "A"),
  extraer_metricas(cm_logit_Au, roc_A,       "Logística (0.3)", "A"),
  extraer_metricas(cm_arbol_A,  roc_arbol_A, "Árbol",           "A"),
  extraer_metricas(cm_rf_A,     roc_rf_A,    "Random Forest",   "A"),
  extraer_metricas(cm_logit_B,  roc_B,       "Logística",       "B"),
  extraer_metricas(cm_arbol_B,  roc_arbol_B, "Árbol",           "B"),
  extraer_metricas(cm_rf_B,     roc_rf_B,    "Random Forest",   "B")
)

row.names(comparacion) <- NULL
comparacion

# Curva ROC comparativa A vs B
plot(roc_A,       col = "#3498db", lwd = 2, main = "Curva ROC comparativa A vs B")
plot(roc_arbol_A, col = "#e74c3c", lwd = 2, add = TRUE)
plot(roc_rf_A,    col = "#2ecc71", lwd = 2, add = TRUE)
plot(roc_B,       col = "#3498db", lwd = 2, lty = 2, add = TRUE)
plot(roc_arbol_B, col = "#e74c3c", lwd = 2, lty = 2, add = TRUE)
plot(roc_rf_B,    col = "#2ecc71", lwd = 2, lty = 2, add = TRUE)
legend("bottomright",
       legend = c(
         paste("A - Logística AUC:", round(auc(roc_A), 3)),
         paste("A - Árbol AUC:",     round(auc(roc_arbol_A), 3)),
         paste("A - RF AUC:",        round(auc(roc_rf_A), 3)),
         paste("B - Logística AUC:", round(auc(roc_B), 3)),
         paste("B - Árbol AUC:",     round(auc(roc_arbol_B), 3)),
         paste("B - RF AUC:",        round(auc(roc_rf_B), 3))
       ),
       col = c("#3498db", "#e74c3c", "#2ecc71", "#3498db", "#e74c3c", "#2ecc71"),
       lwd = 2,
       lty = c(1, 1, 1, 2, 2, 2))

# Visualización comparativa por métrica
comparacion_long <- comparacion %>%
  pivot_longer(cols = c(Accuracy, Precision, Recall, F1, Especificidad, AUC),
               names_to = "Metrica",
               values_to = "Valor")

ggplot(comparacion_long, aes(x = Modelo, y = Valor, fill = Escenario)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Metrica, scales = "free_y") +
  labs(
    title = "Comparación de modelos por escenario y métrica",
    x = "Modelo",
    y = "Valor"
  ) +
  scale_fill_manual(values = c("A" = "#3498db", "B" = "#e74c3c")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ------------------------------------------------------------
# 17. Validación cruzada (Cross-validation)
# ------------------------------------------------------------

# Se usa sampling = "rose" dentro de trainControl para aplicar
# el balanceo en cada fold, siendo consistente con la sección 13.
control_cv <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = TRUE,
  sampling = "rose"
)

# Escenario A — se usa train_A (sin balancear) para que ROSE
# actúe dentro de cada fold via trainControl
cv_logit_A <- train(
  riesgo_suspenso ~ ., data = train_A,
  method = "glm", family = "binomial",
  trControl = control_cv, metric = "ROC"
)

cv_arbol_A <- train(
  riesgo_suspenso ~ ., data = train_A,
  method = "rpart",
  trControl = control_cv, metric = "ROC"
)

cv_rf_A <- train(
  riesgo_suspenso ~ ., data = train_A,
  method = "rf",
  trControl = control_cv, metric = "ROC",
  ntree = 100
)

# Escenario B
cv_logit_B <- train(
  riesgo_suspenso ~ ., data = train_B,
  method = "glm", family = "binomial",
  trControl = control_cv, metric = "ROC"
)

cv_arbol_B <- train(
  riesgo_suspenso ~ ., data = train_B,
  method = "rpart",
  trControl = control_cv, metric = "ROC"
)

cv_rf_B <- train(
  riesgo_suspenso ~ ., data = train_B,
  method = "rf",
  trControl = control_cv, metric = "ROC",
  ntree = 100
)

# Comparación de resultados CV
resultados_cv <- resamples(list(
  "A - Logística"    = cv_logit_A,
  "A - Árbol"        = cv_arbol_A,
  "A - RandomForest" = cv_rf_A,
  "B - Logística"    = cv_logit_B,
  "B - Árbol"        = cv_arbol_B,
  "B - RandomForest" = cv_rf_B
))

summary(resultados_cv)
dotplot(resultados_cv, main = "Comparación de modelos mediante Cross-Validation")
