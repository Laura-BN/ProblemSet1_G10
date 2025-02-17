#-----------------------------------------------------------------------------//
# Descargar base de datos
# Problem Set 1 G10 - BDML 202501
# Fecha: 
#-----------------------------------------------------------------------------//

# Loop para descargar y unir los datos
total_pages <- 10
geih_list <- list()  # Lista para almacenar las tablas

for (i in 1:total_pages) {
  url <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_", i, ".html")
  page <- read_html(url)
  tables <- page %>% html_table(fill = TRUE)
  geih_list[[i]] <- as.data.frame(tables[[1]])  # Guardar la tabla en la lista
}

# Unir todas las tablas en un solo data frame
geih_2018 <- bind_rows(geih_list)

# Guardar 
saveRDS(geih_2018, file.path(stores_path, "geih_2018.rds"))


