
# Punto 4
rm(list = ls())

# Cargar datos
data = readRDS(file.path(stores_path, "geih_2018_VF.rds"))

# 4.a. unconditional wage gap ------------------------------------------------- //

#Salario por hora en log
uncond_reg = lm(log_y_total_m_ha ~ Mujer, data = data)
stargazer(uncond_reg, type = "text")

# 4.b. conditional wage gap --------------------------------------------------- //

#Revision general de variables explicativas en ecuación Mincer

summary(data[, c( "Mujer", "age", "Max_nivel_educacion2", "cuentaPropia",
                 "Experiencia_emp_act", "Tamaño_firma", "Full_time", 
                 "formal", "Jefe_hogar")])

sapply(data[, c("Mujer", "age", "Max_nivel_educacion2",  "cuentaPropia",
                "Experiencia_emp_act", "Tamaño_firma", "Full_time", 
                "formal", "Jefe_hogar")], class)

#Para revisar posibles bad controls revisamos correlacion de variables 
num_vars <- data %>% 
            select(Mujer, age, Experiencia_emp_act, age, Full_time,
                   formal, Jefe_hogar, cuentaPropia)

cor_matrix <- cor(num_vars, use = "complete.obs")
print(cor_matrix)
        
# Estimacion condicional 
cond_reg <- lm(log_y_total_m_ha ~ Mujer + poly(age, 2, raw = TRUE) + 
                 Max_nivel_educacion2 + poly(Experiencia_emp_act, 2, raw = TRUE) + 
                 Tamaño_firma + Full_time + formal, data = data)


# unconditional wage gap vs conditional wage gap
stargazer(uncond_reg, cond_reg, type = "text", digits=4)


# 4.c. FWL -------------------------------------------------------------------- //

#1) Regresar todas las variable Mujer en las variables de control y tomar residuos
data = data %>% 
     mutate(MujerResidF=lm(Mujer ~ +
                             poly(age, 2, raw = TRUE) + 
                             Max_nivel_educacion2 + 
                             poly(Experiencia_emp_act, 2, raw = TRUE) + 
                             Tamaño_firma + 
                             Full_time + 
                             formal,
                             data)$residuals) 

#2) Regresar log del salario por hora en las variables de control y tomar residuos
data = data %>% 
      mutate(IncomeResidF=lm(log_y_total_m_ha ~ +
                              poly(age, 2, raw = TRUE) + 
                              Max_nivel_educacion2 + 
                              poly(Experiencia_emp_act, 2, raw = TRUE) + 
                              Tamaño_firma + 
                              Full_time + 
                              formal,
                              data)$residuals)

#3) Regresar los residuos del paso 2 en los residuos del paso 1
fwl_reg = lm(IncomeResidF~MujerResidF,data)
stargazer(cond_reg,fwl_reg, type="text",digits=4) 

# 4.d. FWL con bootstrap ---------------------------------------------------- //
boot_stat  <- function(data, index){
  model <- lm(IncomeResidF ~ MujerResidF, data = data, subset = index)  
  return(c(coef(model)[2], summary(model)$r.squared)) # Devuelve coef y R^2
}

#check
round(boot_stat(data, 1:nrow(data)), 4)

#Error estandar variable Mujer
set.seed(321)
boot(data, boot_stat, R = 10000)

# 4.e. Graficar el perfil de edad-salario previsto y estimar las “edades pico” por genero 

# Función para calcular predicciones por género
predictions_gender <- function(data, indices, gender) {
  sample_data <- data[indices, ]
  
  # Ajustar el modelo con la nueva especificación
  model <- lm(log_y_total_m_ha ~ Mujer + poly(age, 2, raw = TRUE) + 
                Max_nivel_educacion2 + poly(Experiencia_emp_act, 2, raw = TRUE) + 
                Tamaño_firma + Full_time + formal, data = sample_data)
  
  # Crear datos para predicción - asignando la media/moda a variables explicativas
  pred_data <- data.frame(
    age = seq(18, 91, by = 1),  # Rango de edad
    Mujer = gender,  
    Max_nivel_educacion2 = names(which.max(table(sample_data$Max_nivel_educacion2))),  # Moda de educación
    Experiencia_emp_act = mean(sample_data$Experiencia_emp_act, na.rm = TRUE),  # Media de experiencia
    Tamaño_firma = names(which.max(table(sample_data$Tamaño_firma))),  # Moda del tamaño de la firma
    Full_time = mean(sample_data$Full_time, na.rm = TRUE),  # Media de tiempo completo
    formal = mean(sample_data$formal, na.rm = TRUE)  # Media de formalidad
  )
  
  # Calcular predicciones
  pred_data$predicted_log_salaries <- predict(model, newdata = pred_data)
  
  # Devolver las predicciones 
  return(pred_data$predicted_log_salaries)
}

# Bootstrap para mujeres
set.seed(321)
boot_women <- boot(data = data, statistic = predictions_gender, R = 10000, gender = 1)
conf_women <- t(sapply(1:length(seq(18, 91, by = 1)), function(i) {
  boot.ci(boot_women, type = "perc", index = i)$percent[4:5]
}))

# Bootstrap para hombres
set.seed(321)
boot_men <- boot(data = data, statistic = predictions_gender, R = 10000, gender = 0)
conf_men <- t(sapply(1:length(seq(18, 91, by = 1)), function(i) {
  boot.ci(boot_men, type = "perc", index = i)$percent[4:5]
}))

# Crear data frames para mujeres y hombres
pred_women <- data.frame(
  age = seq(18, 91, by = 1),
  predicted_log_salaries = boot_women$t0,  # Predicciones en escala logarítmica
  conf_low = conf_women[, 1],
  conf_high = conf_women[, 2],
  Gender = "Mujer"
)

pred_men <- data.frame(
  age = seq(18, 91, by = 1),
  predicted_log_salaries = boot_men$t0,  # Predicciones en escala logarítmica
  conf_low = conf_men[, 1],
  conf_high = conf_men[, 2],
  Gender = "Hombre"
)

# Combinar los data frames
pred_data_gender <- bind_rows(pred_women, pred_men)

# Estimar el "peak age" para cada género usando los coeficientes del modelo
get_peak_age <- function(model) {
  coef_model <- coef(model)
  
  # Calcular la edad pico a partir de los coeficientes (beta1 y beta2)
  # La fórmula es -beta1 / (2 * beta2) 
  peak_age <- -coef_model["poly(age, 2, raw = TRUE)1"] / (2 * coef_model["poly(age, 2, raw = TRUE)2"])
  
  return(peak_age)
}

# Estimar los "peak ages" por genero
peak_age_women <- get_peak_age(lm(log_y_total_m_ha ~ Mujer + poly(age, 2, raw = TRUE) + 
                                    Max_nivel_educacion2 + poly(Experiencia_emp_act, 2, raw = TRUE) + 
                                    Tamaño_firma + Full_time + formal, data = data[data$Mujer == 1, ]))
peak_age_men <- get_peak_age(lm(log_y_total_m_ha ~ Mujer + poly(age, 2, raw = TRUE) + 
                                  Max_nivel_educacion2 + poly(Experiencia_emp_act, 2, raw = TRUE) + 
                                  Tamaño_firma + Full_time + formal, data = data[data$Mujer == 0, ]))

cat("Peak age for women:", peak_age_women, "\n")
cat("Peak age for men:", peak_age_men, "\n")

# Graficar las predicciones por género con las peak age
fig_peak_age = ggplot(pred_data_gender, aes(x = age, y = predicted_log_salaries, color = Gender)) +
  geom_line(size = 1.1) + 
  geom_ribbon(aes(ymin = conf_low, ymax = conf_high, fill = Gender), alpha = 0.15) + 
  geom_vline(xintercept = peak_age_women, linetype = "dashed", color = "gray40", size = 1) +
  geom_vline(xintercept = peak_age_men, linetype = "dashed", color = "black", size = 1) +
  labs(
    x = "Edad", 
    y = "Log(Salario Predicho)",
    color = "Género",
    fill = "Género"
  ) +
  scale_color_manual(values = c("Mujer" = "gray40", "Hombre" = "black")) + 
  scale_fill_manual(values = c("Mujer" = "gray60", "Hombre" = "gray80")) +  
  theme_minimal() +  
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black"), 
    legend.position = "bottom"  
  ) +
  annotate("text", x = peak_age_women, 
           y = max(pred_data_gender$predicted_log_salaries) - 0.37, 
           label = paste(round(peak_age_women, 1)), color = "gray40") + 
  annotate("text", x = peak_age_men, 
           y = max(pred_data_gender$predicted_log_salaries) + 0.1, 
           label = paste(round(peak_age_men, 1)), color = "black") 
  ggsave(file.path(view_path, "wage_peak_age.png"), plot = fig_peak_age)