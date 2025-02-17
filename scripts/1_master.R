#-----------------------------------------------------------------------------//
# Mastet Problem Set 1 - BDML 202501
# Fecha: 
#-----------------------------------------------------------------------------//

rm(list = ls())

#-----------------------------------------------------------------------------//
# 1. Ruta de los archivos ----
#-----------------------------------------------------------------------------//

# Users: ejecutar segùn el usuario de cada una/o

path_user = "H:/My Drive/1. General/3. Académico/3. Uniandes/Machine Learning + BD/Repos_GitHub"
path_user = "/Users/camilaortiz/Dropbox/PEG/BigData"
path_user = "G:/Mi unidad/Academia/Maestría MEcA/Big data y machine learning/Taller 1"

path_main = "ProblemSet1_G10"
path_gen = file.path(path_user, path_main)

document_path = file.path(path_gen, "document") 
scripts_path  = file.path(path_gen, "scripts") 
stores_path   = file.path(path_gen, "stores")
view_path     = file.path(path_gen, "view")

#-----------------------------------------------------------------------------//
# 2. Paquetes ----
#-----------------------------------------------------------------------------//

# Instalar paqueta pacman.
#install.packages("pacman")

# Llamar librerías
require(pacman)
p_load(tidyverse, 
       rvest,
       dplyr,
       stargazer, 
       foreign, 
       skimr, # summary data
       visdat, # visualizing missing data
       corrplot, 
       scales, 
       broom, 
       xtable, 
       gridExtra, 
       survey) 

Instalar paquete DescTools
# install.packages("DescTools")
# library(DescTools)
