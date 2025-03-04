#------------------------------------------------------------------------------#
# Predicción de ganancias ----
#------------------------------------------------------------------------------#

options(scipen = 999)
source(file.path(scripts_path, "1_master.R"))
data = readRDS(file.path(stores_path, "geih_2018_VF.rds"))
table(data$children_6years)
table(data$oficio)

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

table(training$oficio)
table(testing$oficio)
table(data$Tamaño_firma)


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
                        poly(Experiencia_emp_act, 2, raw = TRUE) +                     
                        I(Tamaño_firma) + 
                        Full_time + formal + I(Oficio_cat),

           # Modelos adicionales punto 5
           
               form_3 = log_y_total_m_ha ~ 
                        Mujer + 
                        poly(age, 2, raw = TRUE) + 
                        I(Max_nivel_educacion2) + 
                        poly(Experiencia_emp_act, 2, raw = TRUE) +                    
                        I(sizeFirm_cat) + 
                        Full_time + formal + I(Oficio_cat),

              form_4 = log_y_total_m_ha ~ 
                        Mujer + poly(age, 3, raw = TRUE):Mujer + # interacción
                        poly(age, 3, raw = TRUE) + 
                        I(Max_nivel_educacion2) + poly(age, 3, raw = TRUE):I(Max_nivel_educacion2) + # interacción
                        poly(Experiencia_emp_act, 3, raw = TRUE) +
                        I(sizeFirm_cat) + 
                        Full_time + formal + I(Oficio_cat),

               form_5 = log_y_total_m_ha ~ 
                        Mujer + 
                        poly(age, 2, raw = TRUE) + 
                        I(Max_nivel_educacion2) + 
                        poly(Experiencia_emp_act, 2, raw = TRUE) +                    
                        I(sizeFirm_cat) + 
                        Full_time + formal + I(Oficio_cat) +
                        (children_6years),
           
              form_6 = log_y_total_m_ha ~ 
                        Mujer + poly(age, 3, raw = TRUE):Mujer + # interacción
                        poly(age, 3, raw = TRUE) + 
                        I(Max_nivel_educacion2) + poly(age, 3, raw = TRUE):I(Max_nivel_educacion2) + # interacción
                        poly(Experiencia_emp_act, 3, raw = TRUE) +
                        I(sizeFirm_cat) + 
                        Full_time + formal + I(Oficio_cat) +
                        (children_6years),
           
              form_7 = log_y_total_m_ha ~ 
                        Mujer + poly(age, 4, raw = TRUE):Mujer + # interacción
                        poly(age, 4, raw = TRUE) + 
                        I(Max_nivel_educacion2) + poly(age, 4, raw = TRUE):I(Max_nivel_educacion2) + # interacción
                        poly(Experiencia_emp_act, 4, raw = TRUE) +
                        I(sizeFirm_cat) + 
                        Full_time + formal + I(Oficio_cat) +
                        (children_6years),
           
               form_8 = log_y_total_m_ha ~ 
                        Mujer + poly(age, 3, raw = TRUE):Mujer + # interacción
                        poly(age, 3, raw = TRUE) + 
                        I(Max_nivel_educacion2) + poly(age, 3, raw = TRUE):I(Max_nivel_educacion2) + # interacción
                        poly(Experiencia_emp_act, 3, raw = TRUE) +
                        I(sizeFirm_cat) + 
                        Full_time + formal + I(Oficio_cat) +
                        (children_6years) + (children_6years):Mujer,
           
              form_9 = log_y_total_m_ha ~ 
                        Mujer + poly(age, 4, raw = TRUE):Mujer + # interacción
                        poly(age, 4, raw = TRUE) + 
                        I(Max_nivel_educacion2) + poly(age, 4, raw = TRUE):I(Max_nivel_educacion2) + # interacción
                        poly(Experiencia_emp_act, 4, raw = TRUE) +
                        I(sizeFirm_cat) + 
                        Full_time + formal + I(Oficio_cat) +
                        (children_6years) + (children_6years):Mujer,

           
              form_10 = log_y_total_m_ha ~ 
                        Mujer + poly(age, 3, raw = TRUE):Mujer + # interacción
                        poly(age, 3, raw = TRUE) + 
                        I(Max_nivel_educacion2) + poly(age, 3, raw = TRUE):I(Max_nivel_educacion2) + # interacción
                        poly(Experiencia_emp_act, 3, raw = TRUE) +
                        I(sizeFirm_cat) + 
                        Full_time + formal +
                        I(Ocupacion) + I(Oficio_cat) + # más categorías 
                        (children_6years) + (children_6years):Mujer,
           
              form_11 = log_y_total_m_ha ~ 
                        Mujer + poly(age, 4, raw = TRUE):Mujer + # interacción
                        poly(age, 4, raw = TRUE) + 
                        I(Max_nivel_educacion2) + poly(age, 4, raw = TRUE):I(Max_nivel_educacion2) + # interacción
                        poly(Experiencia_emp_act, 4, raw = TRUE) +
                        I(sizeFirm_cat) + 
                        Full_time + formal + 
                        I(Ocupacion) + I(Oficio_cat) + # más categorías 
                        (children_6years) + (children_6years):Mujer,
           
              form_12 = log_y_total_m_ha ~ 
                        Mujer + poly(age, 3, raw = TRUE):Mujer + # interacción
                        poly(age, 3, raw = TRUE) + 
                        I(Max_nivel_educacion2) + poly(age, 3, raw = TRUE):I(Max_nivel_educacion2) + # interacción
                        poly(Experiencia_emp_act, 3, raw = TRUE) +
                        I(sizeFirm_cat) + 
                        Full_time + formal + 
                        I(Ocupacion) + I(Oficio_cat) + # más categorías
                        I(Cot_pension) + Jefe_hogar + I(Estrato) +  # más categorías 
                        (children_6years) + (children_6years):Mujer,
           
              form_13 = log_y_total_m_ha ~ 
                        Mujer + poly(age, 4, raw = TRUE):Mujer + # interacción
                        poly(age, 4, raw = TRUE) + 
                        I(Max_nivel_educacion2) + poly(age, 4, raw = TRUE):I(Max_nivel_educacion2) + # interacción
                        poly(Experiencia_emp_act, 4, raw = TRUE) +
                        I(sizeFirm_cat) + 
                        Full_time + formal + 
                        I(Ocupacion) + I(Oficio_cat) + # más categorías 
                        I(Cot_pension) + Jefe_hogar + I(Estrato) + # más categorías 
                        (children_6years) + (children_6years):Mujer, 
           
              form_14 = log_y_total_m_ha ~ 
                        Mujer + poly(age, 6, raw = TRUE):Mujer + # interacción
                        poly(age, 6, raw = TRUE) + 
                        I(Max_nivel_educacion2) + poly(age, 6, raw = TRUE):I(Max_nivel_educacion2) + # interacción
                        poly(Experiencia_emp_act, 6, raw = TRUE) +
                        I(sizeFirm_cat) + 
                        Full_time + formal + 
                        I(Ocupacion) + I(Oficio_cat) + # más categorías 
                        I(Cot_pension) + Jefe_hogar + I(Estrato) + # más categorías 
                        (children_6years) + (children_6years):Mujer
           
                         )

modelos     = list()
mse_scores = data.frame(Modelo = character(), 
                         MSE = numeric())


table(data$oficio) # sizeFirm_cat, sizeFirm, Tamaño_firma

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

resultados_modelos = file.path(view_path, "/modelos_p5.txt")
sink(resultados_modelos)

stargazer::stargazer(modelos, type = "text",
                     title = "Resultados de los Modelos",
                     dep.var.labels = "log_y_total_m_ha",
                     float = FALSE)
sink(NULL)

# It is clear that as complexity increases, performance improves until a point 
# where too much complexity results in inferior performance.
mse_scores

mse_scores_table = file.path(view_path, "mse_scores_CV_p5.txt")
sink(mse_scores_table)

mse_scores_1 = xtable(mse_scores, digits = 5)

print(mse_scores_1, type = "latex", include.rownames = FALSE)

sink(NULL)

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
# c.2 Revisión de la distribución de los errores en la muestra de prueba ----
#------------------------------------------------------------------------------#

summary(testing$errores)  

hist_errores = ggplot(testing, aes(x = errores)) +
               geom_histogram(bins = 30, fill = "gray", color = "black", alpha = 0.7) +
               labs(title = "", 
                     x = "Error", 
                     y = "Frecuencia") +
               theme_classic() +
               theme(plot.title = element_text(hjust = 0.5, size = 12))
hist_errores

ggsave(file.path(paste0(view_path, "/hist_errores_p5.png")), 
       plot = hist_errores, width = 10, height = 6, dpi = 300)

# Percentiles 5 % y 95 %

upper_perc_e  = quantile(testing$errores, 0.99)
lower_perc_e  = quantile(testing$errores, 0.01)

bp_errores = ggplot(data = testing, 
             mapping = aes(y = errores, x="")) +
             theme_bw() +
             geom_boxplot()  +
             ggtitle("")+
             ylab("Errores")+
             xlab("") +
             theme_classic() +
             theme(plot.title = element_text(hjust = 0.5, size = 12)) + 
  geom_hline(yintercept = upper_perc_e, linetype = "solid", color = "#00EEEE", size = 0.7) +
  geom_hline(yintercept = lower_perc_e, linetype = "solid", color = "#00EEEE", size = 0.7)  


bp_errores

ggsave(file.path(paste0(view_path, "/bp_errores_p5.png")), 
       plot = bp_errores, width = 12, height = 6, dpi = 300)

hist_bp_errores = grid.arrange(hist_errores, bp_errores, ncol = 2, 
                          top = textGrob("",
                           gp = gpar(fontsize = 11)))
ggsave(file.path(paste0(view_path, "/hist_bp_errores_p5.png")), 
       plot = hist_bp_errores, width = 12, height = 6, dpi = 300)

#------------------------------------------------------------------------------#
# c.3 Rev outliers (errores) del EP en muestra de prueba  ----
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

upper_perc_test  = quantile(testing$y_total_m_ha, 0.99)
lower_perc_test  = quantile(testing$y_total_m_ha, 0.01)

b_1 = ggplot(data    = testing, 
      mapping = aes(y = y_total_m_ha/1000, x="")) +
      theme_bw() +
      geom_boxplot()  +
      ggtitle("Testing")+
      ylab("Ingresos porhora (miles de pesos)")+
      xlab("") +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5, size = 12)) + 
      geom_hline(yintercept = upper_perc_test/1000, linetype="solid", color="#00EEEE",size=0.7) +
      geom_hline(yintercept = lower_perc_test/1000, linetype="solid", color="#00EEEE",size=0.7)  

upper_perc_out  = quantile(outliers$y_total_m_ha, 0.99)
lower_perc_out  = quantile(outliers$y_total_m_ha, 0.01)

b_2 = ggplot(data = outliers, 
      mapping = aes(y = y_total_m_ha/1000, x="")) +
      theme_bw() +
      geom_boxplot()  +
      ggtitle("Puntos atípicos")+
      ylab("Ingresos por hora (miles de pesos)")+
      xlab("") +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5, size = 12)) + 
      geom_hline(yintercept = upper_perc_out/1000, linetype="solid", color="#00EEEE",size=0.7) +
      geom_hline(yintercept = lower_perc_out/1000, linetype="solid", color="#00EEEE",size=0.7)  

box_plot_m = grid.arrange(b_1, b_2, ncol = 2, 
                          top = textGrob("Box-plot ingresos por hora en muestras de prueba y puntos atípicos",
                           gp = gpar(fontsize = 14)))

ggsave(file.path(paste0(view_path, "/bp_testing_outliers_p5.png")), 
       plot = box_plot_m, width = 10, height = 6, dpi = 300)

#------------------------------------------------------------------------------#
# c.4.3 Tabla diferencia de medias testing y outliers ----
#------------------------------------------------------------------------------#

diff_means_table
diff_means = xtable(diff_means_table, digits = 1)
print(diff_means, type = "latex", include.rownames = FALSE)

table1 = testing  %>% dplyr::summarise(num_observaciones = n()); table1
table2 = outliers %>% dplyr::summarise(num_observaciones = n()); table2


diff_means_table_1 = file.path(view_path, "diff_means_test_outl_p5.txt")
sink(diff_means_table_1)

print(diff_means, type = "latex", include.rownames = FALSE)

sink(NULL)

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

# Seleccionar los dos modelos con menor MSE 
n_modelos_min_MSE = order(mse_scores$MSE)[1:2]
n_modelos_min_MSE

formulas_min_MSE = formulas[n_modelos_min_MSE]

#------------------------------------------------------------------------------#

modelos_LOOCV = list()

mse_scores_LOOCV = data.frame(Modelo = character(), 
                               MSE = numeric(), 
                               stringsAsFactors = FALSE)

# formulas_ = formulas[1:2]
# data_ = data
# data = data_
# data = data %>%  slice(1:100)

for (i in seq_along(formulas_min_MSE)) {
  
  ctrl = trainControl(method = "LOOCV") # Leave One Out Cross Validation
  
  # Contar tiempo de ejecución 
  
  start_time = Sys.time()
  n_obs = nrow(data)
  cat("Starting LOOCV training with", n_obs, "iterations...\n", " model", i)
  ctrl$verboseIter = TRUE  # Enable progress printing

  
  # Modelo 
  
  modelo = train(formulas_min_MSE[[i]],
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
  
  # head(modelo$pred)
  #score1c<-RMSE(modelo$pred$pred, data$log_y_total_m_ha)
  
  
  cat(paste0("\nModelo ", i, " completo. MSE: ", mse_scores_LOOCV[i, 2]))
  
  end_time = Sys.time() # Calculate and display timing
  training_time = difftime(end_time, start_time, units = "mins")
  cat("\nLOOCV training completed in:", round(training_time, 2), "minutes\n")
  cat("Average time per fold:", round(training_time/n_obs, 4), "minutes\n")

}

# GUARDAR MODELOS DE LOOCV: 

saveRDS(modelos_LOOCV, file = paste0(view_path, "/modelos_LOOCV.rds"))
# modelos_LOOCV = readRDS("modelos_LOOCV.rds")



# Tablas MSE CV y LOOCV: ajuste 

colnames(mse_scores_LOOCV)[2] = "MSE_LOOCV"

mse_scores_1213 = mse_scores[12:13,]
colnames(mse_scores_1213)[2]       = "MSE_CV"

tabla_VF_MSE = cbind(mse_scores_1213, mse_scores_LOOCV[ , 2]) 
tabla_VF_MSE
colnames(tabla_VF_MSE)[3]       = "MSE_LOOCV"
tabla_VF_MSE

# Guardar

tabla_VF_LOOCV_MSE = file.path(view_path, "mse_scores_CV_LOOCV_p5.txt")
sink(tabla_VF_LOOCV_MSE)
tabla_VF_MSE


tabla_VF_MSE_1 = xtable(tabla_VF_MSE, digits = 5)
print(tabla_VF_MSE_1, type = "latex", include.rownames = FALSE)
sink(NULL)
tabla_VF_MSE


#------------------------------------------------------------------------------#
# Gráfico valores ajustados y observados----
#------------------------------------------------------------------------------#

resultados = data.frame(
  Obsr = seq_along(data$directorio),
  Observado = data$log_y_total_m_ha,  
  Ajustado_CV = predict(modelos[n_modelos_min_MSE[1]], data), 
  Ajustado_LOOCV = predict(modelos_LOOCV[1], data))

head(resultados)
colnames(resultados) = c("Obsr", "Observado", "Ajustado_CV", "Ajustado_LOOCV")

ajus_obs_CV_LOOCV = 
  ggplot(resultados, aes(x = Observado)) +
  geom_density(aes(fill = "Observado"), alpha = 0.5, color = NA) +
  geom_density(aes(x = Ajustado_CV, fill = "Ajustado CV"), alpha = 0.2, color = NA) +
  geom_density(aes(x = Ajustado_LOOCV, fill = "Ajustado LOOCV"), alpha = 0.1, color = NA) +
  
  labs(title = "Densidad de valores observados vs. ajustados: CV y LOOCV (modelo 13)",
       x = "Log ingreso por hora",
       y = "Densidad",
       fill = "Valores") +
  scale_fill_manual(values = c("Observado" = "gray", "Ajustado CV" = "blue", "Ajustado LOOCV" = "#00EEEE")) +
  theme_classic()

ajus_obs_CV_LOOCV

resultados$Error_CV = resultados$Observado - resultados$Ajustado_CV
resultados$Error_LOOCV = resultados$Observado - resultados$Ajustado_LOOCV

erores_CV_LOOCV = 
  ggplot(resultados) +
  geom_density(aes(x = Error_CV, fill = "Error CV"), alpha = 0.2, color = NA) +
  geom_density(aes(x = Error_LOOCV, fill = "Error LOOCV"), alpha = 0.1, color = NA) +
  
  labs(title = "Densidad de Errores de Predicción (modelo 13) CV vs. LOOCV",
       x = "Error de predicción log ingreso por hora",
       y = "Densidad",
       fill = "Errores de predicción") +
  scale_fill_manual(values = c("Error CV" = "blue", "Error LOOCV" = "#00EEEE")) +
  theme_classic()

erores_CV_LOOCV

e_ajus_ob_CV_LOOV = grid.arrange(ajus_obs_CV_LOOCV, erores_CV_LOOCV, ncol = 2, 
             top = textGrob("",
             gp = gpar(fontsize = 14)))


ggsave(file.path(paste0(view_path, "/e_ajus_ob_CV_LOOV_m13_p5.png")), 
       plot = e_ajus_ob_CV_LOOV, width = 15, height = 10, dpi = 300)



