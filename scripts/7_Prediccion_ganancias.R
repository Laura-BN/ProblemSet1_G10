#------------------------------------------------------------------------------#
# Predicción de ganancias ----
#------------------------------------------------------------------------------#
options(scipen = 999)
source(file.path(scripts_path, "1_master.R"))
data = readRDS(file.path(stores_path, "geih_2018_VF.rds"))

log_y_total_m
log_y_total_m_ha
#------------------------------------------------------------------------------#
# a. Divida la muestra en dos ----
# una muestra de entrenamiento (70 \%) y una muestra de prueba (30 \%).
# (No olvide establecer una semilla para lograr reproducibilidad.
# En R, por ejemplo, puede usar set.seed(10101), donde 10101 es la semilla).
#------------------------------------------------------------------------------#

set.seed(10101) 

inTrain = caret::createDataPartition(y = data$y_total_m_ha, # the outcome data are needed
                                     p = .70,               # data training
                                     list = FALSE)

training = data %>% filter(row_number() %in% inTrain)
testing  = data %>% filter(!row_number() %in% inTrain)

#------------------------------------------------------------------------------#
# b. Informe y compare el rendimiento predictivo en términos del RMSE ----
# de todas las especificaciones anteriores con al menos cinco (5) 
# especificaciones adicionales que exploren las no linealidades y la complejidad.
#------------------------------------------------------------------------------#

# MODELOS

# ModelO 1 (punto 3):

model1_pt3 <- lm(log_y_total_m ~ age, data = data_pt3)

model2_pt3 <- lm(log_y_total_m ~ age + I(age^2), data = data_pt3)






form_1<- totalHoursWorked ~ log_ingtot + age  + gender 

modelo1a <- lm(form_1,
               data = training)

# Out of sample Performance
predictions <- predict(modelo1a, testing)
score1a<- caret::RMSE(predictions, testing$totalHoursWorked )
score1a

# Model 2:

form_2<- totalHoursWorked ~log_ingtot + age + gender + maxEducLevel+ nmenores

modelo2a <- lm(form_2,
               data = training )

# Out of sample Performance

predictions <- predict(modelo2a, testing)
score2a<- RMSE(predictions, testing$totalHoursWorked )

score2a

# Model 3:

form_3<- totalHoursWorked ~log_ingtot + age + gender + maxEducLevel+ nmenores+ H_Head + Head_Female

modelo3a <- lm(form_3,
               data = training )

# Out of sample Performance

predictions <- predict(modelo3a, testing)
score3a<- RMSE(predictions, testing$totalHoursWorked )

score3a

# Model 4:

form_4<- totalHoursWorked ~log_ingtot +
  poly(age,3,raw=TRUE) +
  gender + poly(age,3,raw=TRUE):gender  +
  maxEducLevel + poly(age,3,raw=TRUE):maxEducLevel  +
  nmenores+ poly(age,3,raw=TRUE):nmenores +
  H_Head +  poly(age,3,raw=TRUE):H_Head +
  Head_Female+  poly(age,3,raw=TRUE):Head_Female 

modelo4a <- lm(form_4,
               data = training )

# Out of sample Performance

predictions <- predict(modelo4a, testing)
score4a<- RMSE(predictions, testing$totalHoursWorked )
score4a

#------------------------------------------------------------------------------#
# b.1 Comparación modelos ----
#------------------------------------------------------------------------------#
#put them in a vector
mse<-c(score1a,score2a,score3a,score4a)

#create a data frame
db<-data.frame(model=factor(c("model1","model2","model3","model4"),ordered=TRUE),
               MSE=mse)

db

# It is clear that as complexity increases, performance improves until a point 
# where too much complexity results in inferior performance.

#------------------------------------------------------------------------------#
# c. Para la especificación con el menor error de predicción ----
# explore aquellas observaciones que parecen \textit{miss the mark} (no predicen 
# bien). Para ello, calcule los errores de predicción en la muestra de prueba y
# examine su distribución. ¿Hay alguna observación en las colas de la 
# distribución del error de predicción? ¿Son estos valores atípicos personas 
# potenciales que la DIAN debería examinar, o son simplemente el producto de un
# modelo audaz?
#------------------------------------------------------------------------------#

# min de db


#------------------------------------------------------------------------------#
# d. LOOCV ----
# Para los dos modelos con el menor error de predicción en la sección anterior, 
# calcule el error de predicción utilizando la validación cruzada de dejar uno 
# afuera (LOOCV). Compare los resultados del error de prueba con los obtenidos
# con el enfoque del conjunto de validación y explore los vínculos potenciales 
# con la estadística de influencia. {\color{red}(Nota: al intentar realizar esta 
# subsección, los cálculos pueden llevar mucho tiempo, según sus habilidades de
# codificación, ¡planifique en consecuencia!)}
#------------------------------------------------------------------------------#

# dos min de db

# We will just change the method of our cross validation approach in the
# function trainControl.

ctrl <- trainControl(
  method = "LOOCV") ## input the method Leave One Out Cross Validation

# model 1 

# contar tiempo en correeeeer

# Start timing
start_time <- Sys.time()

# Get total number of observations for progress tracking
n_obs <- nrow(db)
cat("Starting LOOCV training with", n_obs, "iterations...\n")

# Train model with progress printing
ctrl$verboseIter <- TRUE  # Enable progress printing
modelo1c <- train(form_1,
                  data = db,
                  method = 'lm', 
                  trControl = ctrl)

# Calculate and display timing
end_time <- Sys.time()
training_time <- difftime(end_time, start_time, units = "mins")
cat("\nLOOCV training completed in:", round(training_time, 2), "minutes\n")
cat("Average time per fold:", round(training_time/n_obs, 4), "minutes\n")



# normalito

ctrl$verboseIter <- TRUE  # Enable progress printing
modelo1c <- train(form_1,
                  data = db,
                  method = 'lm', 
                  trControl= ctrl)
modelo1c




head(modelo1c$pred)
score1c<-RMSE(modelo1c$pred$pred, db$totalHoursWorked)

