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

#------------------------------------------------------------------------------#
# 1.1 Gráficas de valores extremos en ingreso ----
#------------------------------------------------------------------------------#
upper_perc_m  = quantile(data2$y_total_m, 0.99)
lower_perc_m  = quantile(data2$y_total_m, 0.01)

upper_perc_m_log = quantile(log(data2$y_total_m), 0.99)
lower_perc_m_log = quantile(log(data2$y_total_m), 0.01)

sum(data2$y_total_m > upper_perc_m_log,  na.rm = TRUE) 
sum(data2$y_total_m < lower_perc_m_log,  na.rm = TRUE)

b_1 = ggplot(data = data, 
      mapping = aes(y = y_total_m/1000000, x="")) +
      theme_bw() +
      geom_boxplot()  +
      ggtitle("")+
      ylab("Ingresos mensuales (millones de pesos)")+
      xlab("") +
      geom_hline(yintercept = upper_perc_m/1000000, linetype="solid", color="#00EEEE",size=0.7) +
      geom_hline(yintercept = lower_perc_m/1000000, linetype="solid", color="#00EEEE",size=0.7)  

b_2 = ggplot(data = data2, 
      mapping = aes(y = log(y_total_m), x="")) +
      theme_bw() +
      geom_boxplot()  +
      ggtitle("")+
      ylab("Ingresos mensuales (log)")+
      xlab("") +
      geom_hline(yintercept = upper_perc_m_log, linetype="solid", color="#00EEEE",size=0.7) +
      geom_hline(yintercept = lower_perc_m_log, linetype="solid", color="#00EEEE",size=0.7) 

box_plot_m = grid.arrange(b_1, b_2, ncol = 2, 
             top = textGrob("Box-plot ingresos mensuales (log y millones de pesos)",
                            gp = gpar(fontsize = 14)))

#------------------------------------------------------------------------------#
# 1.2 Gráficas de valores extremos en ingreso por hora ----
#------------------------------------------------------------------------------#

upper_perc_ha  = quantile(data2$y_total_m_ha, 0.99)
lower_perc_ha  = quantile(data2$y_total_m_ha, 0.01)

upper_perc_ha_log = quantile(log(data2$y_total_m_ha), 0.99)
lower_perc_ha_log = quantile(log(data2$y_total_m_ha), 0.01)

sum(data2$y_total_m_ha > upper_perc_ha,  na.rm = TRUE) # observaciones reemplazadas
sum(data2$y_total_m_ha < lower_perc_ha,  na.rm = TRUE) # observaciones reemplazadas

b_1_h = ggplot(data = data, 
               mapping = aes(y = y_total_m_ha/1000, x="")) +
  theme_bw() +
  geom_boxplot()  +
  ggtitle("")+
  ylab("Ingresos por hora (miles de pesos)")+
  xlab("") +
  geom_hline(yintercept = upper_perc_ha/1000, linetype="solid", color="#00EEEE",size=0.7) +
  geom_hline(yintercept = lower_perc_ha/1000, linetype="solid", color="#00EEEE",size=0.7) 

b_2_h = ggplot(data = data, 
               mapping = aes(y = log(y_total_m_ha), x="")) +
  theme_bw() +
  geom_boxplot()  +
  ggtitle("")+
  ylab("Ingresos mensuales (log)")+
  xlab("") +
  geom_hline(yintercept = upper_perc_ha_log, linetype="solid", color="#00EEEE",size=0.7) +
  geom_hline(yintercept = lower_perc_ha_log, linetype="solid", color="#00EEEE",size=0.7) 

box_plot_h = grid.arrange(b_1_h, b_2_h, ncol = 2, 
             top = textGrob("Box-plot ingresos por hora (log y miles de pesos)",
             gp = gpar(fontsize = 14)))

# Guardar gráficos 

ggsave(file.path(paste0(view_path, "/box_plot_m.png")), 
       plot = box_plot_m, width = 10, height = 6, dpi = 300)

ggsave(file.path(paste0(view_path, "/box_plot_h.png")), 
       plot = box_plot_h, width = 10, height = 6, dpi = 300)

#------------------------------------------------------------------------------#
data_p = data2 %>% select(y_total_m_ha, y_total_m) %>% 
  dplyr::filter(y_total_m_ha <= lower_perc_ha & y_total_m <= lower_perc_m)

summary(data_p$y_total_m_ha,  na.rm = TRUE) # observaciones reemplazadas
summary(data_p$y_total_m, na.rm = TRUE)
#------------------------------------------------------------------------------#


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
                            se_1 = numeric(), 
                            Mean_data2 = numeric(), 
                            se_2 = numeric(), 
                            Dif = numeric(),
                            p_value = numeric(), 
                            Significance = character(),
                            stringsAsFactors = FALSE)


variables = list("formal", "informal",  
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
    se_1 = sqrt(((x1 /n1)*(1-(x1 /n1)))/n1) * 100,
    Mean_data2 = (x2/n2)*100, 
    se_2 = sqrt(((x2 /n2)*(1-(x2 /n2)))/n2) * 100,
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
    se_1 = sd(data[[var]], na.rm = TRUE) / sqrt(sum(!is.na(data[[var]]))),
    Mean_data2 = x2, 
    se_2 = ( sd(data2[[var]], na.rm = TRUE) / sqrt(sum(!is.na(data2[[var]]))) ),
    Dif =  x1 - x2,
    p_value = test$p.value, 
    Significance = significance
  ))
}

#------------------------------------------------------------------------------#
# 3. Guardar tablas y gráficas diferencia de medias ----
#------------------------------------------------------------------------------#

ggsave(file.path(paste0(view_path, "/plot_dif_p.png")), 
       plot = plot_dif_p, width = 10, height = 6, dpi = 300)

# ggsave(file.path(paste0(view_path, "/plot_dif_ing.png")), plot = plot_dif_ing, width = 10, height = 6, dpi = 300)

diff_means = xtable(diff_means_table, digits = 1)
print(diff_means, type = "latex", include.rownames = FALSE)

diff_means_table


table1 = data %>% dplyr::summarise(num_observaciones  = n()); table1
table2 = data2 %>% dplyr::summarise(num_observaciones = n()); table2
