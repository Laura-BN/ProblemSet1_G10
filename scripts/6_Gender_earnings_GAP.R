
# Punto 4

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
        
# Estimacion condicional 
cond_reg <- lm(log_y_total_m_ha ~ Mujer + poly(age, 2, raw = TRUE) + 
                 Max_nivel_educacion2 + poly(Experiencia_emp_act, 2, raw = TRUE) + 
                 Tamaño_firma + Full_time + formal, data = data)


# unconditional wage gap vs conditional wage gap
stargazer(uncond_reg, cond_reg, type = "text", digits=4)


# 4.b - 1 FWL ------------------------------------------------------------------ //

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

# 4.b - 2 FWL with bootstrap --------------------------------------------------- //
boot_stat  <- function(data, index){
  model <- lm(IncomeResidF ~ MujerResidF, data = data, subset = index)  
  return(c(coef(model)[2], summary(model)$r.squared)) # Devuelve coef y R^2
}

#check
round(boot_stat(data, 1:nrow(data)), 4)

#Error estandar variable Mujer
set.seed(321)
boot(data, boot_stat, R = 10000)

# 4.c. Graficar el perfil de edad-salario previsto y estimar las “edades pico” por genero 

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
  predicted_log_salaries = boot_women$t0, 
  conf_low = conf_women[, 1],
  conf_high = conf_women[, 2],
  Gender = "Mujer"
)

pred_men <- data.frame(
  age = seq(18, 91, by = 1),
  predicted_log_salaries = boot_men$t0,  
  conf_low = conf_men[, 1],
  conf_high = conf_men[, 2],
  Gender = "Hombre"
)

# Combinar los data frames
pred_data_gender <- bind_rows(pred_women, pred_men)

# Función para calcular la edad pico a partir de un modelo
get_peak_age <- function(model) {
  coef_model <- coef(model)
  peak_age <- -coef_model["poly(age, 2, raw = TRUE)1"] / (2 * coef_model["poly(age, 2, raw = TRUE)2"])
  return(peak_age)
}

# Función para calcular la edad pico en cada muestra bootstrap
get_peak_age_bootstrap <- function(data, indices, gender) {
  sample_data <- data[indices, ]
  model <- lm(log_y_total_m_ha ~ Mujer + poly(age, 2, raw = TRUE) + 
                Max_nivel_educacion2 + poly(Experiencia_emp_act, 2, raw = TRUE) + 
                Tamaño_firma + Full_time + formal, data = sample_data[sample_data$Mujer == gender, ])
  return(get_peak_age(model))
}

# Bootstrap para edades pico (mujeres)
set.seed(321)
boot_women_peak <- boot(data = data, statistic = get_peak_age_bootstrap, R = 10000, gender = 1)
peak_age_women <- boot_women_peak$t0  # Edad pico estimada
se_peak_age_women <- sd(boot_women_peak$t)  # Error estándar de la edad pico

# Bootstrap para edades pico (hombres)
set.seed(321)
boot_men_peak <- boot(data = data, statistic = get_peak_age_bootstrap, R = 10000, gender = 0)
peak_age_men <- boot_men_peak$t0  # Edad pico estimada
se_peak_age_men <- sd(boot_men_peak$t)  # Error estándar de la edad pico

# Mostrar resultados de las edades pico y sus errores estándar
cat("Peak age for women:", peak_age_women, "±", se_peak_age_women, "\n")
cat("Peak age for men:", peak_age_men, "±", se_peak_age_men, "\n")

# Simular distribución de la diferencia en edades pico
diff_peak_ages <- boot_women_peak$t - boot_men_peak$t

# Calcular IC al 95% para la diferencia
ci_diff <- quantile(diff_peak_ages, c(0.025, 0.975))

cat("Diferencia en edad pico (Mujeres - Hombres):", mean(diff_peak_ages), "±", sd(diff_peak_ages), "\n")
cat("IC 95% de la diferencia:", ci_diff, "\n")


# Graficar las predicciones por género con las peak age  ----------------------- //
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
           label = paste(round(peak_age_women, 1)), color = "gray40", size = 5, hjust = 1.2) + 
  annotate("text", x = peak_age_men, 
           y = max(pred_data_gender$predicted_log_salaries) + 0.1, 
           label = paste(round(peak_age_men, 1)), color = "black", size = 5, hjust = 1.5)
print(fig_peak_age)

   ggsave(file.path(view_path, "wage_peak_age.png"), plot = fig_peak_age)