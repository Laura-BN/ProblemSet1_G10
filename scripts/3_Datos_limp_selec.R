#------------------------------------------------------------------------------#
# Descriptivas
#------------------------------------------------------------------------------#

options(scipen = 999)

#------------------------------------------------------------------------------#
# 1. Ajuste variables
#------------------------------------------------------------------------------#

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
                  
                Formalidad =  ifelse(formal   == 1, "Formal", 
                              ifelse(informal == 1, "Informal", "NA")), 
               
                Mujer      =  ifelse(sex == 0, 1, 
                              ifelse(sex == 1, 0, NA)),
               
                Estrato    =  estrato1, 
               
              Experiencia  = p6426,
              
              Full_time    = ifelse(hoursWorkUsual >= 48, 1, 0),
              
            Tamaño_firma   =  ifelse(sizeFirm == 1 | sizeFirm == 2 | sizeFirm == 3, "Micro", 
                              ifelse(sizeFirm == 4, "Pequeña", 
                              ifelse(sizeFirm == 5, "Mediana_grande", "NA"))), 
                
       Max_nivel_educacion =  ifelse(maxEducLevel == 1, "Ninguna", 
                                ifelse(maxEducLevel == 2, "Preescolar", 
                                ifelse(maxEducLevel == 3, "Primaria incompleta", 
                                ifelse(maxEducLevel == 4, "Primaria completa", 
                                ifelse(maxEducLevel == 5, "Secundaria incompleta", 
                                ifelse(maxEducLevel == 6, "Secundaria completa", 
                                ifelse(maxEducLevel == 7, "Terciaria", 
                                ifelse(maxEducLevel == 9, "N/A", "NA")))))))),
       
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
       
              Ocupacion_cat  =  ifelse(relab == 1 | relab == 2, "Obreros_empleados", 
                                ifelse(relab == 3, "Emp_domésticos", 
                                ifelse(relab == 4, "Cuenta_propia", 
                                ifelse(relab == 5, "Patron_empleador", 
                                ifelse(relab == 6 | relab == 7, "Ocu_sin_remun", 
                                ifelse(relab == 8, "Jornalero_peon", 
                                ifelse(relab == 9, "Otro", "NA"))))))), 
       
              Jefe_hogar_cat = ifelse(orden == 1, "Jefe hogar", 
                               ifelse(orden != 1, "No jefe hogar", "NA")),
                
               Reg_salud     =  ifelse(regSalud == 1, "R. Contributivo", 
                                ifelse(regSalud == 2, "R. Especial", 
                                ifelse(regSalud == 3, "R. Subsidiado", 
                                ifelse(regSalud == 9, "N/A", "NA")))),
                
               Reg_salud_c     =  ifelse(regSalud == 1 | regSalud == 2, "R. Contributivo/Especial", 
                                ifelse(regSalud == 3, "R. Subsidiado", 
                                ifelse(regSalud == 9, "N/A", "NA"))),
       
               Cot_pension   =  ifelse(cotPension == 1, "Cotiza pensión", 
                                ifelse(cotPension == 2, "No cotiza pensión", 
                                ifelse(cotPension == 3, "Pensionado", 
                                ifelse(cotPension == 9, "N/A", "NA")))))

summary(data$y_salary_m)
summary(data$y_ingLab_m)
summary(data$y_salary_m_hu)
summary(data$y_ingLab_m_ha) 
summary(data$y_total_m) # y_total_m_ha = income salaried + independents total - nominal hourly
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
# 1.3 Eliminar missing values ----
#------------------------------------------------------------------------------#

# Nota DANE: si el porcentaje de datos imputados es muy alto se crea un error  
# sistemático o sesgo en la varianza del estimador puntual (este caso)
# Usamos la técnica Hot-Deck Imputation (usada por el DANE)

data2 = data %>%
        dplyr::filter(!is.na(y_total_m) & !is.na(y_total_m_ha)) %>% 
        dplyr::mutate(across(c(estrato1, Grupo_etario, Edu_cat, Ocupacion_cat, 
                               Mujer, Jefe_hogar_cat), as.factor))


summary(data$y_total_m_ha) # Antes eliminación
summary(data2$y_total_m_ha)
summary(data$y_total_m)    # Antes eliminación
summary(data2$y_total_m)

#------------------------------------------------------------------------------#
# 1.4 Outliers ----
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

upper_perc  = quantile(data2$y_total_m_ha, 0.99)
upper_perc_ = quantile(data2$y_total_m, 0.99)

sum(data2$y_total_m_ha > upper_perc,  na.rm = TRUE) # observaciones reemplazadas
sum(data2$y_total_m    > upper_perc_, na.rm = TRUE)

data2$y_total_m_ha =  ifelse(data2$y_total_m_ha > upper_perc, upper_perc,
                                  data2$y_total_m_ha)
       
data2$y_total_m =  ifelse(data2$y_total_m > upper_perc_, upper_perc_,
                         data2$y_total_m)      

g2 = ggplot(data2, aes(x = y_total_m_ha)) +
     geom_density(aes(y = ..density.. * 100), color = "black", fill = "gray", 
                      alpha = 0.5, size = 0.5, adjust = 1.5)  + 
  ggtitle("Después de aplicar Windsorization") +
  theme_minimal() 

grid.arrange(g1, g2, ncol = 2)

summary(data$y_total_m_ha) # Antes windsor
summary(data2$y_total_m_ha)
summary(data$y_total_m)    # Antes windsor
summary(data2$y_total_m)

vis_dat(data2) 

#------------------------------------------------------------------------------#
# 1.5 Guardar base ----
#------------------------------------------------------------------------------#

data2 =  data2 %>% 
         dplyr::select(directorio, Estrato, Mujer, age, ocu, oficio, orden, 
                       totalHoursWorked, formal, informal, Tamaño_firma, 
                       Reg_salud, Cot_pension, Max_nivel_educacion, Grupo_etario, 
                       Formalidad, Ocupacion, Experiencia, Full_time,
                       ingtot, ingtotob, y_salary_m, y_ingLab_m, 
                       y_salary_m_hu,  y_ingLab_m_ha, 
                       
                       y_total_m, y_total_m_ha) 

# saveRDS(data2, file.path(stores_path, "geih_2018_VF.rds"))
# bd_1 = readRDS(file.path(stores_path, "geih_2018_VF.rds"))
rm(data_, t1, upper_perc, upper_perc_, g1, g2, M)
