#-----------------------------------------------------------------------------//
# Descriptivas
#-----------------------------------------------------------------------------//

data = readRDS(file.path(stores_path, "geih_2018.rds"))

write.dta(data, file.path(stores_path, "geih_2018.dta"))

head(data)
  
