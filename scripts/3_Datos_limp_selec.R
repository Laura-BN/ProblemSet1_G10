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
                oficio, totalHoursWorked, formal, informal, p6426, cuentaPropia,
                sizeFirm, regSalud, cotPension, maxEducLevel, relab,
                hoursWorkUsual, y_salary_m_hu, y_ingLab_m_ha, y_total_m_ha, 
                y_total_m, y_ingLab_m, ingtot, ingtotob, ingtotes, y_salary_m, 
                fex_c, secuencia_p, orden, sex, age, 
                oficio, p6426, sizeFirm, regSalud, cotPension, maxEducLevel, relab,
                hoursWorkUsual, ingtotes, 
                fex_c, impa, impaes, isa, isaes,
                y_gananciaIndep_m, y_gananciaNeta_m, y_gananciaNetaAgro_m, dominio,
                clase, cuentaPropia) %>% 
    dplyr::mutate(año = 2018, 
                  
               Grupo_etario = ifelse(age >= 18 & age <= 28, "Joven",
                              ifelse(age >= 29 & age < 50, "Adulto", 
                              ifelse(age >= 50, "Adulto_m", "NA"))),
                  
                Formalidad =  ifelse(formal   == 1, "Formal", 
                              ifelse(informal == 1, "Informal", "NA")), 
               
                Mujer      =  ifelse(sex == 0, 1, 
                              ifelse(sex == 1, 0, NA)),
               
                Estrato    =  estrato1, 
               
              Experiencia_emp_act  = p6426,
              
              Full_time    = ifelse(hoursWorkUsual >= 48, 1, 0),
              
              sizeFirm     = ifelse(sizeFirm == 1, "autoempleado",
                             ifelse(sizeFirm == 2, "2-5 empl",
                             ifelse(sizeFirm == 3, "6-10 empl", 
                             ifelse(sizeFirm == 4, "11-50 empl", 
                             ifelse(sizeFirm == 5, ">50 empl", "NA"))))), 
              
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
       
       Max_nivel_educacion2 = ifelse(is.na(maxEducLevel) | maxEducLevel %in% c(1, 2, 3, 5), "Sin educación completa", 
                               ifelse(maxEducLevel == 4, "Primaria completa", 
                               ifelse(maxEducLevel == 6, "Secundaria completa", 
                               ifelse(maxEducLevel == 7, "Terciaria completa", 
                               ifelse(maxEducLevel == 9, "No aplica", NA))))),
       
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
       
               Jefe_hogar    = ifelse(orden == 1, 1, 0),
                
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
                                ifelse(cotPension == 9, "N/A", "NA")))), 
       
              # Generar la variable de ingreso en log
       
              log_y_total_m_ha = log(y_total_m_ha), 
              log_y_total_m    = log(y_total_m))

summary(data$y_salary_m)
summary(data$y_ingLab_m)
summary(data$y_salary_m_hu)
summary(data$y_ingLab_m_ha) 
summary(data$y_total_m) # y_total_m_ha = income salaried + independents total - nominal hourly
summary(data$y_total_m_ha) # y_total_m_ha = income salaried + independents total - nominal hourly
summary(data$log_y_total_m) # log y_total_m_ha
summary(data$log_y_total_m_ha) # log y_total_m_ha 
 
#------------------------------------------------------------------------------#
# 1.2 Ver missing values ----
#------------------------------------------------------------------------------#

#vis_dat(data) 
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
                               Max_nivel_educacion, Max_nivel_educacion2,
                               Tamaño_firma, Ocupacion, Jefe_hogar_cat), as.factor))

summary(data$y_total_m_ha) # Antes eliminación
summary(data2$y_total_m_ha)
summary(data$y_total_m)    # Antes eliminación
summary(data2$y_total_m)

#------------------------------------------------------------------------------#
# 1.5 Guardar base ----
#------------------------------------------------------------------------------#

data2 =  data2 %>% 
         dplyr::select(directorio, Estrato, Mujer, age, ocu, oficio, orden, fex_c,
                       totalHoursWorked, formal, informal, Tamaño_firma, sizeFirm, 
                       Reg_salud, Cot_pension, Max_nivel_educacion, Jefe_hogar,
                       Max_nivel_educacion2, Grupo_etario, cuentaPropia, 
                       Formalidad, Ocupacion, Experiencia_emp_act, Full_time,
                       ingtot, ingtotob, y_salary_m, y_ingLab_m, 
                       y_salary_m_hu,  y_ingLab_m_ha, 
                       
                       y_total_m, y_total_m_ha, 
                       log_y_total_m, log_y_total_m_ha, # variables de resultado en logaritmo

                       Reg_salud_c, cotPension, Edu_cat, Ocupacion_cat, # para la diferencia de medias
                       Jefe_hogar_cat, Full_time,
                       
                       secuencia_p, orden, sex, age, estrato1,
                       oficio, p6426, sizeFirm, regSalud, cotPension, maxEducLevel, relab,
                       hoursWorkUsual, ingtotes, 
                       fex_c, impa, impaes, isa, isaes,
                       y_gananciaIndep_m, y_gananciaNeta_m, y_gananciaNetaAgro_m, dominio,
                       clase, cuentaPropia) 

saveRDS(data2, file.path(stores_path, "geih_2018_VF.rds"))
# bd_1 = readRDS(file.path(stores_path, "geih_2018_VF.rds"))
#rm(data_, t1, upper_perc, upper_perc_, g1, g2, M)
