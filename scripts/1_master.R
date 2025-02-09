#-----------------------------------------------------------------------------//
# x
# Fecha: 
#-----------------------------------------------------------------------------//

rm(list = ls())

#-----------------------------------------------------------------------------//
# 1. Ruta de los archivos ----
#-----------------------------------------------------------------------------//

# Users

path_user = "F:/My Drive/1. General/3. Académico/3. Uniandes/Machine Learning + BD/Repos_GitHub"

path_main = "ProblemSet1_G10"
path_gen = file.path(path_user, path_main)

document_path = file.path(path_gen, "document") 
scripts_path  = file.path(path_gen, "scripts") 
stores_path   = file.path(path_gen, "stores")
view_path     = file.path(path_gen, "view")

#-----------------------------------------------------------------------------//
# 2. Paquetes ----
#-----------------------------------------------------------------------------//
require(pacman)
p_load(tidyverse, 
       rvest,
       dplyr,
       stargazer, 
       foreign) 
