

# Loop para descargar y unir los datos
total_pages <- 10 

for (i in 1:total_pages) {
  url <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_", i, ".html")
  page <- read_html(url)
  tables <- page %>% html_table(fill = TRUE)
  assign(paste0("geih", i), as.data.frame(tables[[1]])) 
}

# Unir los 10 chunks en un solo data frame
geih_2018 <- bind_rows(geih1, geih2, geih3, geih4, geih5, 
                       geih6, geih7, geih8, geih9, geih10)

# Guardar 
saveRDS(geih_2018, file.path(stores_path, "geih_2018.rds"))


