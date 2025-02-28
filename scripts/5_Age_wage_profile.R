##########################################################
# Punto 3 - Age-wage profile.
# Autores: Grupo 10
##########################################################

options(scipen = 999)


# Cargar paquetes ----------------------------------

library(pacman)

p_load(rio, # import/export data
       tidyverse, # tidy-data
       caret, # For predictive model assessment
       gridExtra, # arrange plots
       skimr, # summarize data 
       stargazer, #model viz and descriptive statistics
       boot,
       xtable
      )   
 

# Importar base de datos ----------------------------------

rm(data)
data_VF <- readRDS(file.path(stores_path, "geih_2018_VF.rds"))
data_VF <- as_tibble(data_VF)

data_pt3 <- data_VF %>% 
  dplyr::select(directorio, secuencia_p, orden, estrato1, sex, age, ocu,
                oficio, totalHoursWorked, formal, informal, p6426, 
                sizeFirm, regSalud, cotPension, maxEducLevel, relab,
                hoursWorkUsual, y_salary_m_hu, y_ingLab_m_ha, y_total_m_ha, 
                y_total_m, y_ingLab_m, ingtot, ingtotob, ingtotes, y_salary_m, 
                fex_c, ingtot, impa, impaes, isa, isaes, totalHoursWorked,
                y_gananciaIndep_m, y_gananciaNeta_m, y_gananciaNetaAgro_m, dominio,
                clase, cuentaPropia)


# Data preprocesing ----------------------------------

# Filtrar base para personas ocupadas mayores de 18 años y eliminar variables sin observaciones en y_total_m
data_pt3 <- data_pt3 %>% 
            dplyr::filter(age >= 18 & ocu == 1 ) %>%
            dplyr::filter(!is.na(y_total_m) | !is.na(y_total_m_ha))

# Suponemos que las personas que no informan su nivel de educación es bajo
data_pt3 <- data_pt3  %>%
            mutate(maxEducLevel = ifelse(is.na(maxEducLevel) == TRUE, 1 , maxEducLevel))

# Comprobar que todas las personas tienen horas trabajadas positivas 
data_pt3 <- data_pt3 %>% filter(totalHoursWorked>0)

# Explorar los datos
des_vars <- c("y_total_m_ha", "age")

stargazer(as.data.frame(data_pt3[,des_vars]), type="text")

# Transformar la variable de ingreso a logaritmo
data_pt3$logB_y_total_m_ha <- log(data_pt3$y_total_m_ha)


# Visualización de los datos ----------------------------------

a <- ggplot(data_pt3, aes(x = y_total_m)) +
    geom_histogram(bins = 50, fill = "darkblue") +
    labs(x = "Ingreso mensual", y = "N° obs") +
    theme_bw() 

b <- ggplot(data_pt3, aes(x = y_total_m_ha)) +
  geom_histogram(bins = 50, fill = "darkblue") +
  labs(x = "Ingreso por hora", y = "N° obs") +
  theme_bw() 

c <- ggplot(data_pt3, aes(x = age)) +
    geom_histogram(bins = 30, fill = "darkblue") +
    labs(x = "Edad", y = "N° obs") +
    theme_bw() 

grid.arrange(a, b, c, ncol = 3)



# Estimación del modelo base the Age-wage profile ----------------------------

model1_pt3 <- lm(logB_y_total_m_ha ~ age, data = data_pt3)

model2_pt3 <- lm(logB_y_total_m_ha ~ age + I(age^2), data = data_pt3)


# Generar la tabla con los resultados de ambos modelos
stargazer(model1_pt3, model2_pt3, type = "text", 
          title = "Resultados de Modelos de Regresión", 
          dep.var.labels = "Log(ingreso)", 
          covariate.labels = c("Edad", "Edad al cuadrado"))


stargazer(model1_pt3, model2_pt3, type = "latex", 
          title = "Resultados de Modelos de Regresión", 
          dep.var.labels = "Log(ingreso)", 
          covariate.labels = c("Edad", "Edad al cuadrado"))


# Gráfica de la curva age-wage ----------------------------

# Generar los valores estimados para la gráfica
age_range <- seq(min(data_pt3$age), max(data_pt3$age), by = 1)
predicted_log_w <- predict(model2_pt3, newdata = data.frame(age = age_range))

# Graficar
png(file.path(view_path, "age_wage_profile.png"), family = "Times New Roman", width = 800, height = 600)  # Guardar la gráfica en un archivo PNG
plot(age_range, exp(predicted_log_w), type = "l", 
        xlab = "Edad", ylab = "Ingreso por hora trabajada", 
       family = "Times New Roman",
       cex.lab = 2.0,    # Aumenta el tamaño de las etiquetas de los ejes
       cex.axis = 2.0,   # Aumenta el tamaño de los números de los ejes
       cex.main = 2      # Aumenta el tamaño del título
       )
dev.off()  # Cierra el dispositivo gráfico


# Edad pico ----------------------------

# Calcular edad pico
age_peak <- -coef(model2_pt3)[2] / (2 * coef(model2_pt3)[3])
age_peak

# Función para calcular la edad pico con la variable 'w' explícita
age_peak_func <- function(data, indices) {
  d <- data[indices, ]
  model_boot <- lm(d$logB_y_total_m_ha ~ d$age + I(d$age^2), data = d)
  return(-coef(model_boot)[2] / (2 * coef(model_boot)[3]))
}

# Ejecutar el bootstrap
set.seed(10101)  # Fijar semilla para reproducibilidad
bootstrap_results <- boot(data_pt3, age_peak_func, R = 10000)

# Ver el intervalo de confianza del 95%
boot_ci <- boot.ci(bootstrap_results, type = "bca")

# Extraer los resultados del intervalo de confianza
lower <- boot_ci$bca[4]  # Límite inferior
upper <- boot_ci$bca[5]  # Límite superior


# Crear una tabla en LaTeX con xtable
ci_table <- data.frame(
  "Intervalo de Confianza" = c("Límite Inferior", "Límite Superior"),
  "Valor" = c(lower, upper))


# Imprimir la tabla en formato LaTeX
print(xtable(ci_table, caption = "Intervalo de confianza (95%)"), type = "latex")
