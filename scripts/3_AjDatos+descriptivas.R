#------------------------------------------------------------------------------#
# Descriptivas
#------------------------------------------------------------------------------#

options(scipen = 999)

#------------------------------------------------------------------------------#
# 1. Ajuste variables
#------------------------------------------------------------------------------#
rm(data)
data = readRDS(file.path(stores_path, "geih_2018.rds"))

#------------------------------------------------------------------------------#
# 1.1 Selección de variables características de los individuos, ocupados 
# mayores o igual 18 años
#------------------------------------------------------------------------------#
# Nota: también hacemos un filtro por ocupación dado que el salario solo aplica 
# para un segmento del total de ocupados (asalariados)

str(data)
table(data$orden)
data = data %>% 
  dplyr::filter(age >= 18 & ocu == 1 ) %>%
  dplyr::select(directorio, secuencia_p, orden, estrato1, sex, age, ocu,
                oficio, totalHoursWorked, formal, informal, p6426,
                sizeFirm, regSalud, cotPension, maxEducLevel, relab,
                hoursWorkUsual, y_salary_m_hu, y_ingLab_m_ha, y_total_m_ha, 
                y_total_m, y_ingLab_m, ingtot, ingtotob, ingtotes, y_salary_m, 
                fex_c) %>% 
    dplyr::mutate(año = 2018, 
               Grupo_etario = ifelse(age >= 18 & age <= 28, "Joven",
                              ifelse(age >= 29 & age < 50, "Adulto", 
                              ifelse(age >= 50, "Adulto_m", "NA"))),
                Edad_cat = ifelse(age >= 18 & age <= 24, "Cat_1",
                              ifelse(age >= 24 & age < 45, "Cat_2", 
                              ifelse(age >= 45, "Cat_3", "NA"))), 
                 Formalidad =  ifelse(formal   == 1, "Formal", 
                              ifelse(informal == 1, "Informal", "NA")), 
                 Mujer     =  ifelse(sex == 0, 1, 
                              ifelse(sex == 1, 0, NA)),
                 Estrato    =  estrato1,
               
                 Experiencia  = p6426,
              
              Full_time    = ifelse(hoursWorkUsual >= 48, 1, 0),
              
            Tamaño_firma   =  factor(ifelse(sizeFirm == 1, "Autoempleado", 
                              ifelse(sizeFirm == 2, "2-5 trabajadores", 
                              ifelse(sizeFirm == 3, "6-10 trabajadores", 
                              ifelse(sizeFirm == 4, "11-50 trabajadores", 
                              ifelse(sizeFirm == 5, "	>50 trabajadores", "NA")))))), 
                
       Max_nivel_educacion =    ifelse(is.na(maxEducLevel), "Ninguna",
                                ifelse(maxEducLevel == 1, "Ninguna", 
                                ifelse(maxEducLevel == 2, "Preescolar", 
                                ifelse(maxEducLevel == 3, "Primaria incompleta", 
                                ifelse(maxEducLevel == 4, "Primaria completa", 
                                ifelse(maxEducLevel == 5, "Secundaria incompleta", 
                                ifelse(maxEducLevel == 6, "Secundaria completa", 
                                ifelse(maxEducLevel == 7, "Terciaria", 
                                ifelse(maxEducLevel == 9, "N/A", "NA"))))))))),
       
       Max_nivel_educacion2 = factor(ifelse(is.na(maxEducLevel) | maxEducLevel %in% c(1, 2, 3, 5), "Sin educación completa", 
                               ifelse(maxEducLevel == 4, "Primaria completa", 
                               ifelse(maxEducLevel == 6, "Secundaria completa", 
                               ifelse(maxEducLevel == 7, "Terciaria completa", 
                               ifelse(maxEducLevel == 9, "No aplica", NA)))))), 

                     Edu_cat =  ifelse(maxEducLevel == 1 | maxEducLevel == 3 | maxEducLevel == 5, "Ninguna", 
                                ifelse(maxEducLevel == 2 | maxEducLevel == 4, "Preescolar y primaria", 
                                ifelse(maxEducLevel == 6 | maxEducLevel == 7, "Secundaria y superior", 
                                ifelse(maxEducLevel == 9, "N/A", "NA")))),

                  Ocupacion  =  ifelse(relab == 1, "Obrero/empleado", 
                                ifelse(relab == 2, "Obrero/empleado gob", 
                                ifelse(relab == 3, "Empleado domestico", 
                                ifelse(relab == 4, "Cuenta propia", 
                                ifelse(relab == 5, "Patron o empleador", 
                                ifelse(relab == 6, "Trabajador familiar sin remun", 
                                ifelse(relab == 7, "Trabajador sin remun (emp/negoc de otros hogares", 
                                ifelse(relab == 8, "Jornalero o peon", 
                                ifelse(relab == 9, "Otro", "NA"))))))))),
       
              Ocupacion_cat  =  ifelse(relab == 1 | relab == 2, "Obreros y empleados", 
                                ifelse(relab == 3, "Empleados domésticos", 
                                ifelse(relab == 4, "Trabajadores cuenta propia", 
                                ifelse(relab == 5, "Patron o empleador", 
                                ifelse(relab == 6 | relab == 7, "Ocupados sin remuneración", 
                                ifelse(relab == 8, "Jornalero o peon", 
                                ifelse(relab == 9, "Otro", "NA"))))))),
       
              Jefe_hogar_cat = ifelse(orden == 1, "Jefe hogar", 
                                ifelse(orden != 1, "No jefe hogar", "NA")),
                
               Reg_salud     =  ifelse(regSalud == 1, "R. Contributivo", 
                                ifelse(regSalud == 2, "R. Especial", 
                                ifelse(regSalud == 3, "R. Subsidiado", 
                                ifelse(regSalud == 9, "N/A", "NA")))),
                
               Cot_pension   =  ifelse(cotPension == 1, "Cotiza pensión", 
                                ifelse(cotPension == 2, "No cotiza pensión", 
                                ifelse(cotPension == 3, "Pensionado", 
                                ifelse(cotPension == 9, "N/A", "NA")))))

summary(data$y_salary_m)
summary(data$y_ingLab_m)
summary(data$y_salary_m_hu)
summary(data$y_ingLab_m_ha) 
summary(data$y_total_m_ha) # y_total_m_ha = income salaried + independents total - nominal hourly

#------------------------------------------------------------------------------#
# 1.2 Ver missing values ----
#------------------------------------------------------------------------------#

vis_dat(data) 
is.na(data$y_total_m_ha) %>% table()
data_ = data %>% mutate_all(~ifelse(!is.na(.), 1, 0))
data_ = data_ %>%  select(which(apply(data_, 2, sd) > 0))
M     = cor(data_)
corrplot(M) 

#------------------------------------------------------------------------------#
# 1.3 Imputación missing values ----
#------------------------------------------------------------------------------#

# Nota DANE: si el porcentaje de datos imputados es muy alto se crea un error  
# sistemático o sesgo en la varianza del estimador puntual (este caso)
# Usamos la técnica Hot-Deck Imputation (usada por el DANE)

data2 = data %>%
        dplyr::mutate(across(c(estrato1, Edad_cat, Edu_cat, Ocupacion_cat, 
                               Mujer, Jefe_hogar_cat), as.factor))

data2 = hotdeck(data2, variable = "y_total_m", 
                domain_var = c("estrato1", "Edad_cat", "Edu_cat",
                               "Mujer", "Jefe_hogar_cat"))

data2 = hotdeck(data2, variable = "y_total_m_ha", 
                domain_var = c("estrato1", "Edad_cat", "Edu_cat",
                               "Mujer", "Jefe_hogar_cat"))
  
# dplyr::select(estrato1, Edad_cat, Edu_cat, Ocupacion_cat, Mujer, Jefe_hogar_cat, y_total_m, y_total_m_ha, y_total_m_imp, y_total_m_ha_imp) %>% dplyr::arrange(estrato1, Edad_cat, Edu_cat, Ocupacion_cat, Mujer, Jefe_hogar_cat)

summary(data$y_total_m_ha) # Antes de la imputación
summary(data2$y_total_m_ha)
summary(data$y_total_m)    # Antes de la imputación
summary(data2$y_total_m)

#------------------------------------------------------------------------------#
# 1.4 Outliers ----
#------------------------------------------------------------------------------#

g1 = ggplot(data2, aes(x = y_total_m_ha)) +
     geom_density(aes(y = ..density.. * 100), color = "black", fill = "gray", 
                      alpha = 0.5, size = 0.5, adjust = 1.5)

t1 = sum(data2$y_total_m_ha > 0)  
sum(data2$y_total_m_ha >= 59000) / t1 * 100
sum(data2$y_total_m_ha > 500) / t1 * 100

# Aplicar winsorización 

lower_perc = quantile(data2$y_total_m_ha,  0.01)
upper_perc = quantile(data2$y_total_m_ha,  0.998)

lower_perc_ = quantile(data2$y_total_m, 0.01)
upper_perc_ = quantile(data2$y_total_m, 0.998)

data2$y_total_m_ha =  ifelse(data2$y_total_m_ha < lower_perc, lower_perc,
                      ifelse(data2$y_total_m_ha > upper_perc, upper_perc,
                             data2$y_total_m_ha))
       
data2$y_total_m =  ifelse(data2$y_total_m < lower_perc_, lower_perc_,
                      ifelse(data2$y_total_m > upper_perc_, upper_perc_,
                             data2$y_total_m))      

g2 = ggplot(data2, aes(x = y_total_m_ha)) +
     geom_density(aes(y = ..density.. * 100), color = "black", fill = "gray", 
                      alpha = 0.5, size = 0.5, adjust = 1.5)

grid.arrange(g1, g2, ncol = 2)

summary(data2$y_total_m_ha)
summary(data2$y_total_m)

vis_dat(data2) 

#------------------------------------------------------------------------------#
# 1.5 Guardar base ----
#------------------------------------------------------------------------------#
data2$Mujer <- as.numeric(as.character(data$Mujer))

data2 =  data2 %>% 
         dplyr::select(directorio, Estrato, Mujer, age, ocu, oficio, orden, 
                       totalHoursWorked, formal, informal, Tamaño_firma, 
                       Reg_salud, Cot_pension, Max_nivel_educacion, 
                       Grupo_etario, Formalidad, Ocupacion, Max_nivel_educacion2,
                       Experiencia, Full_time, Jefe_hogar_cat, 
                       ingtot, ingtotob, y_salary_m,y_ingLab_m, y_salary_m_hu, 
                       y_ingLab_m_ha, y_total_m, y_total_m_ha) 

saveRDS(data2, file.path(stores_path, "geih_2018_VF.rds"))
# bd_1 = readRDS(file.path(stores_path, "geih_2018_VF.rds"))

#------------------------------------------------------------------------------#
# 2. Estadísticas descriptivas ----
#------------------------------------------------------------------------------#

table1 = data %>% 
  dplyr::summarise(num_observaciones     = n()); table1

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

