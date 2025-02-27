#------------------------------------------------------------------------------#
# Descriptivas----
#------------------------------------------------------------------------------#
options(scipen = 999)
#------------------------------------------------------------------------------#
# 0. Correr el master + script bases (completa + sin NAs)----
#------------------------------------------------------------------------------#

source(file.path(scripts_path, "1_master.R"))
source(file.path(scripts_path, "3_Datos_limp_selec.R"))
# Nota: data y data2 base completa y sin NAs

#------------------------------------------------------------------------------#
# 1. Gráficas de valores extremos en ingreso ----
#------------------------------------------------------------------------------#

b_1 = ggplot(data = data, 
      mapping = aes(y = y_total_m/1000000, x="")) +
      theme_bw() +
      geom_boxplot()  +
      ggtitle("")+
      ylab("Ingresos mensuales (millones de pesos)")+
      xlab("") +
      geom_hline(yintercept = upper_perc_m/1000000, linetype="solid", color="#00EEEE",size=0.7) 

b_2 = ggplot(data = data2, 
      mapping = aes(y = y_total_m/1000000, x="")) +
      theme_bw() +
      geom_boxplot()  +
      ggtitle("")+
      ylab("Ingresos mensuales (millones de pesos)")+
      xlab("") +
      geom_hline(yintercept = upper_perc_m/1000000, linetype="solid", color="#00EEEE",size=0.7) 

box_plot_m = grid.arrange(b_1, b_2, ncol = 2, 
             top = textGrob("Box-plot ingresos mensuales (millones de pesos)",
                            gp = gpar(fontsize = 14)))

b_1_h = ggplot(data = data, 
        mapping = aes(y = y_total_m_ha/1000, x="")) +
        theme_bw() +
        geom_boxplot()  +
        ggtitle("")+
        ylab("Ingresos por hora (miles de pesos)")+
        xlab("") +
        geom_hline(yintercept = upper_perc_ha/1000, linetype="solid", color="#00EEEE",size=0.7) 

b_2_h = ggplot(data = data2, 
               mapping = aes(y = y_total_m_ha/1000, x="")) +
        theme_bw() +
        geom_boxplot()  +
        ggtitle("")+
        ylab("Ingresos por hora (miles de pesos)")+
        xlab("") +
        geom_hline(yintercept = upper_perc_ha/1000, linetype="solid", color="#00EEEE",size=0.7) 

box_plot_h = grid.arrange(b_1_h, b_2_h, ncol = 2, 
             top = textGrob("Box-plot ingresos por hora (miles de pesos)",
                            gp = gpar(fontsize = 14)))

# Guardar gráficos 

ggsave(file.path(paste0(view_path, "/box_plot_m.png")), 
       plot = box_plot_m, width = 10, height = 6, dpi = 300)

ggsave(file.path(paste0(view_path, "/box_plot_h.png")), 
       plot = box_plot_h, width = 10, height = 6, dpi = 300)


# ANTES DE DEJAR ESTO VER EL PLOT DE INGRESOS EN NIVEL Y LOGARITMO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#------------------------------------------------------------------------------#
# 2. Comparativa distribución ingresos ambas bases----
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
# 2.1 Split de variables categóricas----
#------------------------------------------------------------------------------#

data = dummy_cols(data, select_columns = c("Grupo_etario", "Estrato", 
                                           "Tamaño_firma", 
                                           "Edu_cat", "Ocupacion_cat", 
                                           "Jefe_hogar_cat", "Reg_salud_c", 
                                           "cotPension"))

data2 = dummy_cols(data2, select_columns = c("Grupo_etario", "Estrato",
                                             "Tamaño_firma", 
                                           "Edu_cat", "Ocupacion_cat", 
                                           "Jefe_hogar_cat", "Reg_salud_c", 
                                           "cotPension"))

#------------------------------------------------------------------------------#
# 2.2 Tabla guardar resultados y variables para comparar----
#------------------------------------------------------------------------------#

diff_means_table = data.frame(Variable = character(), 
                            Mean_data1 = numeric(), 
                            Mean_data2 = numeric(), 
                            Dif = numeric(),
                            p_value = numeric(), 
                            Significance = character(),
                            stringsAsFactors = FALSE)


variables = list("formal", "informal",  
              "Mujer", 
              "Experiencia", 
              "Full_time",
              "Grupo_etario_Joven", "Grupo_etario_Adulto", "Grupo_etario_Adulto_m",
              
              "Estrato_1", "Estrato_2", "Estrato_3", "Estrato_4", "Estrato_5", "Estrato_6", 
              
              "Tamaño_firma_Micro", "Tamaño_firma_Pequeña", "Tamaño_firma_Mediana_grande",
              
              "Ocupacion_cat_Obreros_empleados", "Ocupacion_cat_Emp_domésticos",
              "Ocupacion_cat_Cuenta_propia", "Ocupacion_cat_Patron_empleador", 
              "Ocupacion_cat_Jornalero_peon", "Ocupacion_cat_Otro") 

#------------------------------------------------------------------------------#
# 2.3 T-test ----
#------------------------------------------------------------------------------#

for (var in variables) {

  n1 = nrow(data)  # Total de observaciones en data
  n2 = nrow(data2) # Total de observaciones en data2
  
  x1 = sum(data[[var]] == 1, na.rm = TRUE)  # Mujeres en data
  x2 = sum(data2[[var]] == 1, na.rm = TRUE) # Mujeres en data2
  
  test = prop.test(x = c(x1, x2), n = c(n1, n2), alternative = "two.sided", correct = T)
  
  significance = ifelse(test$p.value < 0.05, "***", "")
  
  diff_means_table = rbind(diff_means_table, data.frame(
    Variable = var,
    Mean_data1 = (x1 /n1)*100, 
    Mean_data2 = (x2/n2)*100, 
    Dif =  ((x1 /n1)*100) - ((x2/n2)*100),
    p_value = test$p.value, 
    Significance = significance
  ))
    
}

# Plot 
plot_dif_p = ggplot(diff_means_table, aes(x = Variable, y = Dif, fill = Significance)) + 
  geom_bar(stat = "identity", color = "black", position = "dodge") +
  labs(
    title = "Diferencia de proporciones de variables entre muestras con y sin NAs en ingreso",
    x = "Variable",
    y = "Diferencia de proporciones (p.p.)",
    fill = "Significancia"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),  
    plot.title = element_text(hjust = 0.5),
    legend.position = "top", 
    panel.grid = element_blank()
  ) +
  scale_fill_manual(values = c("***" = "#00EEEE"))  

plot_dif_p

# Diferencia de medias ingreso (va continua)

variables_2 = list("y_total_m", "y_total_m_ha")

for (var in variables_2) {
  
  x1 = mean(data[[var]], na.rm = TRUE)  
  x2 = mean(data2[[var]], na.rm = TRUE) 
  
  test = t.test(data[[var]], data2[[var]], 
         alternative = "two.sided", conf.level = 0.95)
  
  significance = ifelse(test$p.value < 0.05, "***", "")
  
  diff_means_table = rbind(diff_means_table, data.frame(
    Variable = var,
    Mean_data1 = x1, 
    Mean_data2 = x2, 
    Dif =  x1 - x2,
    p_value = test$p.value, 
    Significance = significance
  ))
}

# Plot 

plot_dif_ing = ggplot(diff_means_table[24:25, ], 
          aes(x = Variable, y = (Dif/Mean_data1)*100, fill = Significance)) + 
  geom_bar(stat = "identity", color = "black", position = "dodge") +
  labs(
    title = "Diferencia de medias % entre muestras con y sin NAs",
    x = "Variable",
    y = "Diferencia de medias (%)",
    fill = "Significancia"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1),  
    plot.title = element_text(hjust = 0.5),
    legend.position = "top", 
    panel.grid = element_blank()
  ) +
  scale_fill_manual(values = c("***" = "#00EEEE"))  

plot_dif_ing

#------------------------------------------------------------------------------#
# 3. Guardar tablas y gráficas diferencia de medias ----
#------------------------------------------------------------------------------#

ggsave(file.path(paste0(view_path, "/plot_dif_p.png")), 
       plot = plot_dif_p, width = 10, height = 6, dpi = 300)

ggsave(file.path(paste0(view_path, "/plot_dif_ing.png")), 
       plot = plot_dif_ing, width = 10, height = 6, dpi = 300)

diff_means = xtable(diff_means_table, digits = 1)
print(diff_means, type = "latex", include.rownames = FALSE)

diff_means_table


table1 = data %>% dplyr::summarise(num_observaciones  = n()); table1
table2 = data2 %>% dplyr::summarise(num_observaciones = n()); table2








#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
# 1.4 Outliers (revisión) ----
#------------------------------------------------------------------------------#

g1 = ggplot(data2, aes(x = y_total_m_ha)) +
  geom_density(aes(y = ..density.. * 100), color = "black", fill = "gray", 
               alpha = 0.5, size = 0.5, adjust = 1.5) + 
  ggtitle("Antes de aplicar Windsorization") +
  theme_minimal() 

t1 = sum(data2$y_total_m_ha > 0)  
sum(data2$y_total_m_ha >= 59000) / t1 * 100
sum(data2$y_total_m_ha < 700) / t1 * 100

# Aplicar winsorización 

upper_perc_ha  = quantile(data2$y_total_m_ha, 0.99)
upper_perc_m = quantile(data2$y_total_m, 0.99)

#------------------------------------------------------------------------------#

data_p = data2 %>% select(y_total_m_ha, y_total_m) %>% 
  dplyr::filter(y_total_m_ha > upper_perc_ha & y_total_m > upper_perc_ha)

summary(data_p$y_total_m_ha,  na.rm = TRUE) # observaciones reemplazadas
summary(data_p$y_total_m, na.rm = TRUE)

#------------------------------------------------------------------------------#

data_ext = data2

sum(data_ext$y_total_m_ha > upper_perc_ha,  na.rm = TRUE) # observaciones reemplazadas
sum(data_ext$y_total_m    > upper_perc_m, na.rm = TRUE)

data_ext$y_total_m_ha =  ifelse(data_ext$y_total_m_ha > upper_perc_ha, upper_perc_ha,
                                data_ext$y_total_m_ha)

data_ext$y_total_m =  ifelse(data_ext$y_total_m > upper_perc_m, upper_perc_m,
                             data_ext$y_total_m)      

g2 = ggplot(data_ext, aes(x = y_total_m_ha)) +
  geom_density(aes(y = ..density.. * 100), color = "black", fill = "gray", 
               alpha = 0.5, size = 0.5, adjust = 1.5)  + 
  ggtitle("Después de aplicar Windsorization") +
  theme_minimal() 

grid.arrange(g1, g2, ncol = 2)

summary(data$y_total_m_ha) # Antes windsor
summary(data_ext$y_total_m_ha)
summary(data$y_total_m)    # Antes windsor
summary(data_ext$y_total_m)
