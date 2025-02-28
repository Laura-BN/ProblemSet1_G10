#------------------------------------------------------------------------------#
# Predicción de ganancias ----
#------------------------------------------------------------------------------#

options(scipen = 999)
source(file.path(scripts_path, "1_master.R"))
data = readRDS(file.path(stores_path, "geih_2018_VF.rds"))
table(data$Ocupacion)
#------------------------------------------------------------------------------#
# a. Divida la muestra en dos ----
# una muestra de entrenamiento (70 \%) y una muestra de prueba (30 \%).
# (No olvide establecer una semilla para lograr reproducibilidad.
# En R, por ejemplo, puede usar set.seed(10101), donde 10101 es la semilla).
#------------------------------------------------------------------------------#

set.seed(10101) 

inTrain = caret::createDataPartition(
                 y = data$log_y_total_m_ha, # the outcome data are needed
                 p = .70,                   # data training
                 list = FALSE)

training = data %>% filter(row_number() %in% inTrain)
testing  = data %>% filter(!row_number() %in% inTrain)

#------------------------------------------------------------------------------#
# b. Informe y compare el rendimiento predictivo en términos del RMSE ----
# de todas las especificaciones anteriores con al menos cinco (5) 
# especificaciones adicionales que exploren las no linealidades y la complejidad.
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# b.1 Modelos ----
#------------------------------------------------------------------------------#

formulas = list(
  
           # Modelo punto 3
           form_1_pt3 = log_y_total_m_ha ~ poly(age, 2, raw = TRUE),
           
           # Modelo punto 4 
           form_2_pt4 = log_y_total_m_ha ~ 
                        Mujer + 
                        poly(age, 2, raw = TRUE) +
                        I(Max_nivel_educacion2) +
                        poly(Experiencia_emp_act, 2, raw = TRUE)                       
                        I(Tamaño_firma) + 
                        Full_time + formal,

           # Modelos adicionales punto 5
           form_3_pt4 = log_y_total_m_ha ~ 
                        Mujer + 
                        poly(age, 2, raw = TRUE) + 
                        I(Max_nivel_educacion2) + 
                        poly(Experiencia_emp_act, 2, raw = TRUE)                       
                        I(sizeFirm_cat) + 
                        Full_time + formal,

              form_4 = log_y_total_m_ha ~ 
                        Mujer + poly(age, 3, raw = TRUE):Mujer + # interacción
                        poly(age, 3, raw = TRUE) + 
                        I(Max_nivel_educacion2) + poly(age, 3, raw = TRUE):I(Max_nivel_educacion2) + # interacción
                        poly(Experiencia_emp_act, 3, raw = TRUE) +
                        I(sizeFirm_cat) + 
                        Full_time + formal,

              form_5 = log_y_total_m_ha ~ 
                        Mujer + poly(age, 4, raw = TRUE):Mujer + # interacción
                        poly(age, 4, raw = TRUE) + 
                        I(Max_nivel_educacion2) + poly(age, 4, raw = T):I(Max_nivel_educacion2) + # interacción
                        poly(Experiencia_emp_act, 4, raw = T) +
                        I(sizeFirm_cat) + 
                        Full_time + formal, 
           
              form_6 = log_y_total_m_ha ~ 
                        Mujer + poly(age, 3, raw = TRUE):Mujer + # interacción
                        poly(age, 3, raw = TRUE) + 
                        I(Max_nivel_educacion2) + poly(age, 3, raw = TRUE):I(Max_nivel_educacion2) + # interacción
                        poly(Experiencia_emp_act, 3, raw = TRUE) +
                        I(sizeFirm_cat) + 
                        Full_time + formal +
                        I(Ocupacion) + I(oficio), # más categorías 
           
              form_7 = log_y_total_m_ha ~ 
                        Mujer + poly(age, 4, raw = TRUE):Mujer + # interacción
                        poly(age, 4, raw = TRUE) + 
                        I(Max_nivel_educacion2) + poly(age, 4, raw = TRUE):I(Max_nivel_educacion2) + # interacción
                        poly(Experiencia_emp_act, 4, raw = TRUE) +
                        I(sizeFirm_cat) + 
                        Full_time + formal + 
                        I(Ocupacion) + I(oficio), # más categorías 

              form_8 = log_y_total_m_ha ~ 
                        Mujer + poly(age, 3, raw = TRUE):Mujer + # interacción
                        poly(age, 3, raw = TRUE) + 
                        I(Max_nivel_educacion2) + poly(age, 3, raw = TRUE):I(Max_nivel_educacion2) + # interacción
                        poly(Experiencia_emp_act, 3, raw = TRUE) +
                        I(sizeFirm_cat) + 
                        Full_time + formal + 
                        I(Ocupacion) + I(oficio) + # más categorías
                        I(Cot_pension) + Jefe_hogar + I(Estrato), # más categorías 
           
              form_9 = log_y_total_m_ha ~ 
                        Mujer + poly(age, 4, raw = TRUE):Mujer + # interacción
                        poly(age, 4, raw = TRUE) + 
                        I(Max_nivel_educacion2) + poly(age, 4, raw = TRUE):I(Max_nivel_educacion2) + # interacción
                        poly(Experiencia_emp_act, 4, raw = TRUE) +
                        I(sizeFirm_cat) + 
                        Full_time + formal + 
                        I(Ocupacion) + I(oficio) + # más categorías 
                        I(Cot_pension) + Jefe_hogar + I(Estrato) # más categorías 

                         )

modelos     = list()
mse_scores = data.frame(Modelo = character(), 
                         MSE = numeric())

table(data$Tamaño_firma) # sizeFirm_cat, sizeFirm

for (i in seq(formulas)) {
  
  modelo       = lm(formulas[[i]], data = training)  # estimación modelo
  modelos[[i]] = modelo  
  
   predictions = predict(modelo, testing)  # predicciones
  
   mse_scores = rbind(mse_scores, 
                       data.frame(
                       Modelo = paste0("Modelo ", i),
                       MSE    = caret::RMSE(predictions, testing$log_y_total_m_ha)))  # cálculo MSE
  
  cat(paste0("\nModelo ", i, " completo. MSE: ", mse_scores[i, 2]))
}

# mse_scores
# modelos_1 = modelos[6:9]

stargazer::stargazer(modelos, type = "text",
                     title = "Resultados de los Modelos",
                     dep.var.labels = "log_y_total_m_ha",
                     float = FALSE)

# It is clear that as complexity increases, performance improves until a point 
# where too much complexity results in inferior performance.
mse_scores
#------------------------------------------------------------------------------#
# c. Para la especificación con el menor error de predicción ----
# explore aquellas observaciones que parecen \textit{miss the mark} (no predicen 
# bien). Para ello, calcule los errores de predicción en la muestra de prueba y
# examine su distribución. ¿Hay alguna observación en las colas de la 
# distribución del error de predicción? ¿Son estos valores atípicos personas 
# potenciales que la DIAN debería examinar, o son simplemente el producto de un
# modelo audaz?
#------------------------------------------------------------------------------#

n_modelo_min_MSE = which.min(mse_scores$MSE)
n_modelo_min_MSE
modelo_min_MSE = modelos[[n_modelo_min_MSE]] 

#------------------------------------------------------------------------------#
# c.1 Cálculo de errores de predicción ----
#------------------------------------------------------------------------------#

hat_log_y_total_m_ha = predict(modelo_min_MSE, testing)    # Predicciones en muestra prueba
testing$errores = testing$log_y_total_m_ha - hat_log_y_total_m_ha  # Errores de predicción

#------------------------------------------------------------------------------#
# c.2 Revisión de la distribución de los errores----
#------------------------------------------------------------------------------#

summary(testing$errores)  

hist(testing$errores, breaks = 30, main = "Distribución de errores de predicción",
     xlab = "Error", col = "white", border = "black")

# Percentiles 5 % y 95 %

upper_perc_e  = quantile(testing$errores, 0.99)
lower_perc_e  = quantile(testing$errores, 0.01)

bp_errores = ggplot(data = testing, 
             mapping = aes(y = errores, x="")) +
             theme_bw() +
             geom_boxplot()  +
             ggtitle("Box-plot errores de predicción en muestra de prueba del modelo con mínimo MSE")+
             ylab("Errores")+
             xlab("") +
  geom_hline(yintercept = upper_perc_e, linetype = "solid", color = "#00EEEE", size = 0.7) +
  geom_hline(yintercept = lower_perc_e, linetype = "solid", color = "#00EEEE", size = 0.7)  

bp_errores

#------------------------------------------------------------------------------#
# c.3 Identificación de observaciones con errores extremos (outliers) ----
#------------------------------------------------------------------------------#

outliers  = testing[testing$errores < lower_perc_e | testing$errores > upper_perc_e, ] 
nrow(outliers)
print(outliers)

vars_factores = c("Grupo_etario", "Estrato", "Tamaño_firma", "Edu_cat", 
                  "Ocupacion_cat", "Jefe_hogar_cat", "Reg_salud_c", 
                  "cotPension")

outliers = dummy_cols(outliers, select_columns = vars_factores)
testing  = dummy_cols(testing,  select_columns = vars_factores)

diff_means_table = data.frame(Variable      = character(), 
                              Mean_testing  = numeric(), 
                              se_test       = numeric(), 
                              Mean_outliers = numeric(), 
                              se_out        = numeric(), 
                              Dif           = numeric(),
                              p_value       = numeric(), 
                              Significance  = character(),
                              stringsAsFactors = FALSE)

variables = c("formal", "informal",  
                 "Mujer", 
                 "Experiencia_emp_act", 
                 "Full_time",
                 "Grupo_etario_Joven", "Grupo_etario_Adulto", "Grupo_etario_Adulto_m",
                 
                 "Estrato_1", "Estrato_2", "Estrato_3", "Estrato_4", "Estrato_5", "Estrato_6", 
                 
                 "Tamaño_firma_Micro", "Tamaño_firma_Pequeña", "Tamaño_firma_Mediana_grande",
                 
                 "Ocupacion_cat_Obreros_empleados", "Ocupacion_cat_Emp_domésticos",
                 "Ocupacion_cat_Cuenta_propia", "Ocupacion_cat_Patron_empleador", 
                 "Ocupacion_cat_Jornalero_peon", "Ocupacion_cat_Otro") 

#------------------------------------------------------------------------------#
# c.4 T-test entre muestra testing y outliers de testing ----
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# c.4.1 variables dummy ----
#------------------------------------------------------------------------------#

for (var in variables) {
  
  n1 = nrow(testing)  # Total de observaciones en testing
  n2 = nrow(outliers) # Total de observaciones en outliers
  
  x1 = sum(testing[[var]]  == 1, na.rm = TRUE)  
  x2 = sum(outliers[[var]] == 1, na.rm = TRUE) 
  
  test = prop.test(x = c(x1, x2), n = c(n1, n2), alternative = "two.sided", correct = T)
  
  significance = ifelse(test$p.value < 0.05, "***", "")
  
  diff_means_table = rbind(diff_means_table, data.frame(
  Variable      = var,
  Mean_testing  = (x1 /n1) * 100, 
  se_test       = sqrt(((x1 /n1) * (1-(x1 /n1)))/n1) * 100,
  Mean_outliers = (x2/n2) * 100, 
  se_out        = sqrt(((x2 /n2) * (1-(x2 /n2)))/n2) * 100,
  Dif           = ((x1 /n1) * 100) - ((x2/n2) * 100),
  p_value       = test$p.value, 
  Significance  = significance
  ))
  
}

diff_means_table

# Nota 1: hay mucha más gente informal en la muestra de outliers que en la muestra
# de testing, más gente adulta mayor, más gente estrato 5 y 6, mucha muchas más 
# gente en microempresas, el doble de cuenta propia y oatron empleador en con-
# traste con la muestra de testing

# Nota 2: menos trabajadores de tiempo completo, menos gente joven, mucha menos gente 
# estrato 2, mucha menos gente en empresas grandes, menos gente obrera/empleada

#------------------------------------------------------------------------------#
# c.4.2 variables continuas (ingreso) ----
#------------------------------------------------------------------------------#

variables_2 = list("y_total_m", "y_total_m_ha")

for (var in variables_2) {
  
  x1 = mean(testing[[var]],  na.rm = TRUE)  
  x2 = mean(outliers[[var]], na.rm = TRUE) 
  
  test = t.test(testing[[var]], outliers[[var]], 
                alternative = "two.sided", conf.level = 0.95)
  
  significance = ifelse(test$p.value < 0.05, "***", "")
  
  diff_means_table = rbind(diff_means_table, data.frame(
  Variable         = var,
  Mean_testing     = x1, 
  se_test          = sd(testing[[var]], na.rm = TRUE) / sqrt(sum(!is.na(testing[[var]]))),
  Mean_outliers    = x2, 
  se_out           = (sd(outliers[[var]], na.rm = TRUE) / sqrt(sum(!is.na(outliers[[var]]))) ),
  Dif              =  x1 - x2,
  p_value          = test$p.value, 
  Significance     = significance
  ))
}

# Nota 3: los ingresos son significativamente mayores en los outliers

#------------------------------------------------------------------------------#
# c.4.2.1 Box plot ingresos testing y outliers ----
#------------------------------------------------------------------------------#

upper_perc_test  = quantile(testing$y_total_m, 0.99)
lower_perc_test  = quantile(testing$y_total_m, 0.01)

b_1 = ggplot(data    = testing, 
      mapping = aes(y = y_total_m/1000000, x="")) +
      theme_bw() +
      geom_boxplot()  +
      ggtitle("Testing")+
      ylab("Ingresos mensuales (millones de pesos)")+
      xlab("") +
      geom_hline(yintercept = upper_perc_test/1000000, linetype="solid", color="#00EEEE",size=0.7) +
      geom_hline(yintercept = lower_perc_test/1000000, linetype="solid", color="#00EEEE",size=0.7)  

upper_perc_out  = quantile(outliers$y_total_m, 0.99)
lower_perc_out  = quantile(outliers$y_total_m, 0.01)

b_2 = ggplot(data = outliers, 
      mapping = aes(y = y_total_m/1000000, x="")) +
      theme_bw() +
      geom_boxplot()  +
      ggtitle("Outliers")+
      ylab("Ingresos mensuales (millones de pesos)")+
      xlab("") +
      geom_hline(yintercept = upper_perc_test/1000000, linetype="solid", color="#00EEEE",size=0.7) +
      geom_hline(yintercept = lower_perc_test/1000000, linetype="solid", color="#00EEEE",size=0.7)  

box_plot_m = grid.arrange(b_1, b_2, ncol = 2, 
                          top = textGrob("Box-plot ingresos mensuales",
                           gp = gpar(fontsize = 14)))

#------------------------------------------------------------------------------#
# c.4.3 Tabla diferencia de medias testing y outliers ----
#------------------------------------------------------------------------------#

diff_means_table
diff_means = xtable(diff_means_table, digits = 1)
print(diff_means, type = "latex", include.rownames = FALSE)

table1 = testing  %>% dplyr::summarise(num_observaciones = n()); table1
table2 = outliers %>% dplyr::summarise(num_observaciones = n()); table2

#stargazer(as.data.frame(outliers[, variables]), type = "text")

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

ctrl = trainControl(method = "LOOCV") # Leave One Out Cross Validation

modelos_LOOCV = list()

mse_scores_LOOCV = data.frame(Modelo = character(), 
                              MSE = numeric())

for (i in seq(formulas)) {
  
  # Contar tiempo de ejecución 
  
  start_time = Sys.time()
  n_obs <- nrow(data)
  cat("Starting LOOCV training with", n_obs, "iterations...\n")
  ctrl$verboseIter = TRUE  # Enable progress printing
  end_time = Sys.time() # Calculate and display timing
  training_time = difftime(end_time, start_time, units = "mins")
  cat("\nLOOCV training completed in:", round(training_time, 2), "minutes\n")
  cat("Average time per fold:", round(training_time/n_obs, 4), "minutes\n")
  
  ctrl$verboseIter = TRUE  # Enable progress printing
  
  # Modelo 
  
  modelo = train(formulas[[i]],
                 data = data,
                 method = 'lm', 
                 trControl= ctrl)
  
  modelos_LOOCV[[i]] = modelo  
  
  predictions = predict(modelo, data)  # predicciones
  
  mse_scores_LOOCV = rbind(mse_scores_LOOCV, 
                           data.frame(
                             Modelo = paste0("Modelo ", i),
                             MSE    = caret::RMSE(predictions, data$log_y_total_m_ha)))  # cálculo MSE
  
  # score_MSE = caret::RMSE(predictions, data$log_y_total_m_ha)
  
  head(modelo$pred)
  #score1c<-RMSE(modelo1c$pred$pred, data$log_y_total_m_ha)
  
  cat(paste0("\nModelo ", i, " completo. MSE: ", mse_scores_LOOCV[i, 2]))
}



rbind(mse_scores, mse_scores_LOOCV)
