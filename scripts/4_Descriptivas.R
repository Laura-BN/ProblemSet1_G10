#------------------------------------------------------------------------------#
# Descriptivas
#------------------------------------------------------------------------------#
options(scipen = 999)
#------------------------------------------------------------------------------#
# Correr el master + script bases (completa + sin NAs)
#------------------------------------------------------------------------------#

source(file.path(scripts_path, "1_master.R"))
source(file.path(scripts_path, "3_Datos_limp_selec.R"))
# Nota: data y data2 base completa y sin NAs

#------------------------------------------------------------------------------#
# 1. Comparativa distribución ingresos ambas bases
#------------------------------------------------------------------------------#

summary(data$y_total_m)
summary(data2$y_total_m)

summary(data$y_total_m_ha)
summary(data2$y_total_m_ha)

#------------------------------------------------------------------------------#
# 2. Estadísticas descriptivas ----
#------------------------------------------------------------------------------#

table1 = data %>% dplyr::summarise(num_observaciones  = n()); table1
table2 = data2 %>% dplyr::summarise(num_observaciones = n()); table2


caracteristicas = list("ocu", "Mujer", "Estrato", "Formalidad", "Grupo_etario", 
                       "Reg_salud", "Cot_pension", 
                       "Tamaño_firma", "Max_nivel_educacion", "Ocupacion")

variables = c("agrupacion", "categoria", "proporcion", "y_total_m_ha", "n")

data_list = list()

for(i in caracteristicas) {
  
table2 =  data2 %>% 
          dplyr::group_by(!!sym(i)) %>% 
          
          dplyr::summarise(n   = n(),
                           y_total_m_ha = sum(y_total_m_ha, na.rm = T) / n) %>%  
           dplyr::mutate(agrupacion  = i, 
                         total = sum(n),
                         proporcion = round((n/total)*100, 1)) %>%
           dplyr::rename(categoria = !!sym(i)) %>% 
           dplyr::filter(!is.na(categoria)) %>% 
           dplyr::select(all_of(variables)) 

data_name <- paste0(i)
data_list[[data_name]] <- table2
}

data_combinada <- do.call(rbind, data_list) 
table2_ <- xtable(data_combinada, digits = 1)
print(table2_, type = "latex", include.rownames = FALSE)


# PLOT

ggplot(data2, aes(x = y_total_m_ha)) +
  geom_density(aes(y = ..density.. * 100), color = "black", fill = "gray", alpha = 0.5, size = 0.5, adjust = 1.5) +  # Solo densidad, con relleno semitransparente
  geom_vline(xintercept = median(data2$y_total_m_ha, na.rm = TRUE), 
             linetype = "dashed", color = "#66CD00", linewidth = 0.8) +
  geom_vline(xintercept = sum((data2$ocu * data2$y_total_m_ha) * data2$fex_c, na.rm = TRUE) / 
               sum(data2$ocu * data2$fex_c, na.rm = TRUE), 
             linetype = "dashed", color = "#FF1493", linewidth = 0.8) +
  ggtitle("Distribución ingreso por hora - Bogotá 2018") +
  xlab("Pesos 2018") +
  ylab("Densidad") +  
  scale_x_continuous(breaks = seq(0, max(data2$y_total_m_ha, na.rm = TRUE), by = 20000), 
                     labels = scales::comma) +  
  coord_cartesian(xlim = c(0, 75000)) +  
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text  = element_text(size = 12))

library(ggplot2)
library(scales)  # Para formatear números en los ejes

ggplot(data2, aes(x = y_total_m_ha_f)) +
  geom_histogram(aes(y = ..density..),  
                 color = "#FFFFFF", fill = "#97FFFF", bins = 230) +
  geom_density(color = "red", size = 1.2, adjust = 1.5) +  # Suavizar curva de densidad
  geom_vline(xintercept = median(data2$y_total_m_ha_f, na.rm = TRUE), 
             linetype = "dashed", color = "#66CD00", linewidth = 0.8) +
  geom_vline(xintercept = sum((data2$ocu * data2$y_total_m_ha_f) * data2$fex_c, na.rm = TRUE) / 
               sum(data2$ocu * data2$fex_c, na.rm = TRUE), 
             linetype = "dashed", color = "#FF1493", linewidth = 0.8) +  
  ggtitle("Distribución ingreso por hora - Bogotá 2018") +
  xlab("Pesos 2018") +
  ylab("Densidad") +  # Cambiado para mayor precisión
  scale_x_continuous(breaks = seq(0, max(data2$y_total_m_ha_f, na.rm = TRUE), by = 100000), 
                     labels = scales::comma) +  # Formatear números con comas
  coord_cartesian(xlim = c(0, 75000)) +  # Ajuste sin eliminar datos
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))


tN = sum(data2$fex_c[data2$y_total_m_ha_f > 0], na.rm = TRUE)

sum(data2$fex_c[data2$y_total_m_ha_f >= 300000], na.rm = TRUE) / tN*100
sum(data2$fex_c[data2$y_total_m_ha_f >= 200000], na.rm = TRUE) / tN*100
sum(data2$fex_c[data2$y_total_m_ha_f < 60000], na.rm = TRUE) / tN*100
sum(data2$fex_c[data2$y_total_m_ha_f >= 60000], na.rm = TRUE) / tN*100

t1 = sum(data2$y_total_m_ha_f>0)  
sum(data2$y_total_m_ha_f < 60000) / t1 * 100
sum(data2$y_total_m_ha_f >= 60000) / t1 * 100


# Summary ingreso por hora ajustado
design <- svydesign(ids = ~1, data = data2, weights = ~fex_c)
svymean(~y_total_m_ha_f, design, na.rm = TRUE)  # Media ponderada
svyquantile(~y_total_m_ha_f, design, c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)  # Cuantiles ponderados

library(survey)
library(dplyr)
library(xtable)

# Definir diseño con factor de expansión
design <- svydesign(ids = ~1, data = data2, weights = ~fex_c)

# Calcular estadísticas ponderadas
media_pond <- as.numeric(svymean(~y_total_m_ha_f, design, na.rm = TRUE))  # Media ponderada
cuartiles_pond <- as.numeric(svyquantile(~y_total_m_ha_f, design, c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE))  # Mínimo, Q1, mediana, Q3, máximo

# Crear tabla con valores individuales
tabla <- data.frame(
  Estadística = c("Mínimo ponderado", "Percentil 25", "Mediana (P50)", "Percentil 75", "Máximo ponderado", "Media ponderada"),
  Valor = c(cuartiles_pond[1], cuartiles_pond[2], cuartiles_pond[3], cuartiles_pond[4], cuartiles_pond[5], media_pond)
)

# Convertir la tabla en formato LaTeX
print(xtable(tabla, digits = 2), include.rownames = FALSE)

