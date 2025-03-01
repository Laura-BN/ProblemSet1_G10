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
predicted_log_w <- predict(model2_pt3, newdata = data.frame(age = age_range), se.fit = TRUE)

# Calcular los intervalos de confianza (95%)
#lower_bound <- exp(predicted_log_w$fit - 1.96 * predicted_log_w$se.fit)  # Límite inferior
#upper_bound <- exp(predicted_log_w$fit + 1.96 * predicted_log_w$se.fit)  # Límite superior

# Calcular los límites inferior y superior en términos de 2 desviaciones estándar de la media
lower_bound <- predicted_log_w$fit - 2 * predicted_log_w$se.fit  # Límite inferior (2 desviaciones estándar)
upper_bound <- predicted_log_w$fit + 2 * predicted_log_w$se.fit  # Límite superior (2 desviaciones estándar)

# Convertir a escala original para graficar (porque se usa el logaritmo de los ingresos por hora trabajada)
lower_bound_exp <- exp(lower_bound)  # Exponenciar el límite inferior
upper_bound_exp <- exp(upper_bound)  # Exponenciar el límite superior

# Graficar la predicción
png(file.path(view_path, "age_wage_profile.png"), family = "Times New Roman", width = 800, height = 600)  # Guardar la gráfica en un archivo PNG


# Sombrear el área de los intervalos de confianza
polygon(c(age_range, rev(age_range)), 
        c(lower_bound_exp, rev(upper_bound_exp)), 
        col = rgb(0.6, 0.8, 1, 0.5),  # Azul claro con mayor transparencia
        border = rgb(0, 0, 1, 0.5))  # Borde azul claro de la sombra con algo de transparencia

plot(age_range, exp(predicted_log_w$fit), type = "l", 
     xlab = "Edad", ylab = "Ingreso por hora trabajada", 
     family = "Times New Roman",
     cex.lab = 2.0,    # Aumenta el tamaño de las etiquetas de los ejes
     cex.axis = 2.0,   # Aumenta el tamaño de los números de los ejes
     cex.main = 2.0,     # Aumenta el tamaño del título
     lwd = 2 
    )

# Añadir los intervalos de confianza
lines(age_range, lower_bound, col = "blue", lty = 2)  # Línea para el límite inferior (azul y línea discontinua)
lines(age_range, upper_bound, col = "blue", lty = 2)  # Línea para el límite superior (azul y línea discontinua)




dev.off()  # Cierra el dispositivo gráfico



# Edad pico ----------------------------

# Calcular edad pico
age_peak_final <- -coef(model2_pt3)[2] / (2 * coef(model2_pt3)[3])
age_peak_final

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


# Calcular la distribución bootstrap de la edad pico
age_peak_bootstrap <- bootstrap_results$t

# Crear un data frame con los resultados para el boxplot
df_age_peak <- data.frame(age_peak = age_peak_bootstrap)


# Crear el gráfico boxplot con la edad pico estimada

age_peak_boxplot <- ggplot(df_age_peak, aes(x = "", y = age_peak)) +
                geom_boxplot(fill = "white", color = "black", size=0.7) +
                geom_point(aes(x = 1, y = age_peak), 
                           position = position_jitter(width = 0.1), 
                           color = "gray", 
                           alpha = 0.07) +  # Ajustar la transparencia de los puntos (más bajo es más sutil)
                labs(#title = "",
                     x = "",
                     y = "Edad pico estimada") +
                geom_hline(yintercept = age_peak, linetype = "solid", color = "red", size = 1) +  # Línea continua
                geom_hline(yintercept = lower, linetype = "solid", color = "blue", size = 1) +  # Línea continua
                geom_hline(yintercept = upper, linetype = "solid", color = "blue", size = 1) +  # Línea continua
                # Añadir los valores a la derecha de las líneas
                geom_text(data = data.frame(y = age_peak, label = round(age_peak_final, 2)), 
                          aes(x = 1.5, y = y, label = label), color = "red", size = 8, vjust = -0.5, family = "Times New Roman") +  # Valor de la edad pico
                geom_text(data = data.frame(y = lower, label = round(lower, 2)),
                          aes(x = 1.5, y = y, label = label), color = "blue", size = 8, vjust = -0.5, family = "Times New Roman") +  # Valor límite inferior
                geom_text(data = data.frame(y = upper, label = round(upper, 2)), 
                          aes(x = 1.5, y = y, label = label), color = "blue", size = 8, vjust = -0.5, family = "Times New Roman") +  # Valor límite superior
                theme_minimal() +
                theme(axis.text.x = element_blank(),
                      axis.ticks.x = element_blank(),
                      panel.border = element_rect(color = "grey", fill = NA, size = 1),
                      text = element_text(family = "Times New Roman", size = 25))  # Cambiar la fuente a Times New Roman


# Crear un gráfico de barras con la distribución de las estimaciones por bootstrap

age_peak_bars <- ggplot(df_age_peak, aes(x = age_peak)) +
                geom_histogram(fill = "grey", color = "black", bins = 30, boundary = 0, size = 0.5) +  # Gráfico de barras (histograma)
                labs(
                  #title = "Distribución de la Edad Pico Estimada (Bootstrap)",  # Título del gráfico
                  x = "Edad pico estimada",
                  y = "Frecuencia"
                ) +
                geom_vline(xintercept = age_peak_final, linetype = "solid", color = "red", size = 1) +  # Línea vertical en age_peak_final
                annotate("text", x = 42.3, y = 1040, label = round(age_peak_final, 2),  # Etiqueta con el valor de age_peak_final
                         color = "red", size = 6, vjust = -1, hjust = 0.5, family = "Times New Roman") +  # Etiqueta arriba de la línea
                theme_minimal() +
                theme(
                  text = element_text(family = "Times New Roman", size = 25),  # Fuente y tamaño
                  plot.title = element_text(hjust = 0.5),  # Centra el título
                  axis.title.x = element_text(size = 25),  # Tamaño del título del eje X
                  axis.title.y = element_text(size = 25),  # Tamaño del título del eje Y
                  axis.text = element_text(size = 25),  # Tamaño de los textos de los ejes
                  panel.grid.major = element_blank(),  # Eliminar cuadrícula mayor
                  panel.grid.minor = element_blank(),  # Eliminar cuadrícula menor
                  panel.border = element_blank(),  # Eliminar el borde del panel
                  axis.line = element_line(color = "gray"),  # Cambiar el color de las líneas de los ejes a gris
                  axis.ticks = element_line(color = "gray")  # Cambiar el color de las marcas de los ejes a gris
                )


# Combinar ambos gráficos en uno solo usando grid.arrange
png(file.path(view_path, "age_wage_box-plot.png"), family = "Times New Roman", width = 1200, height = 600)  # Guardar la imagen combinada

grid.arrange(age_peak_bars, age_peak_boxplot, ncol = 2)  # Colocar los gráficos lado a lado (ncol = 2)

dev.off()  # Cierra el dispositivo gráfico




