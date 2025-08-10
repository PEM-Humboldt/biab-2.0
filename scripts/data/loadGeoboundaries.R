
library(sf)
library(geodata)
library(ggplot2)

# Load BON in a Box input 
input <- biab_inputs()
iso_code <- input$iso_code      # ISO3 country code
adm1_name <- input$adm1_name    # Level 1 name (can be NULL)
adm2_name <- input$adm2_name   # Level 2 name (can be NULL)


# Download and load GADM data by country and level using geodata
download_gadm_sf <- function(iso3, level) {
  gadm <- geodata::gadm(country = iso3, level = level, path = tempdir(),resolution=2 )
  sf_obj <- st_as_sf(gadm)
  if (st_crs(sf_obj)$epsg != 4326) {
    sf_obj <- st_transform(sf_obj, crs = 4326)
  }
  return(sf_obj)
}


# Plotting
plot_boundaries <- function(sf_data, title = "") {
  print(
    ggplot(sf_data) +
      geom_sf(fill = "lightblue", color = "black", size = 0.2) +
      ggtitle(title) +
      xlab("Longitude") +
      ylab("Latitude") +
      theme_minimal()
  )
}

# ADM0
adm0 <- tryCatch({
  sf0 <- download_gadm_sf(iso_code, 0)
  #plot_boundaries(sf0, paste(iso_code, "- ADM0"))
  out_path_0 <- file.path(outputFolder, "level_0_polygon.gpkg")
  st_write(sf0, out_path_0, delete_dsn = TRUE, quiet = TRUE)
  biab_output("level_0_polygon", out_path_0)
  biab_output("admin_0", iso_code)
  sf0
})

# ADM1
adm1 <- tryCatch({
  sf1 <- download_gadm_sf(iso_code, 1)
  if (!is.null(adm1_name) && nzchar(adm1_name)) {
    unit1 <- sf1[sf1$NAME_1 == adm1_name, ]
    if (nrow(unit1) == 0) {
      biab_warning(paste0("ADM1 name '", adm1_name, "' not found."))
    } else {
      #plot_boundaries(unit1, paste(adm1_name, "- ADM1"))
      out_path_1 <- file.path(outputFolder, "level_1_polygon.gpkg")
      st_write(unit1, out_path_1, delete_dsn = TRUE, quiet = TRUE)
      biab_output("level_1_polygon", out_path_1)
      biab_output("admin_1", adm1_name)
    }
  }
  sf1
}, error = function(e) biab_warning("ADM1 error occurred."))

# ADM2
adm2 <- tryCatch({
  sf2 <- download_gadm_sf(iso_code, 2)
  if (!is.null(adm2_name) && nzchar(adm2_name)) {
    unit2 <-  sf2[sf2$NAME_2 == adm2_name, ]
    if (nrow(unit2) == 0) {
      biab_warning(paste0("ADM2 name '", adm2_name, "' not found."))
    } else {
      #plot_boundaries(unit2, paste(adm2_name, "- ADM2"))
      out_path_2 <- file.path(outputFolder, "level_2_polygon.gpkg")
      st_write(unit2, out_path_2, delete_dsn = TRUE, quiet = TRUE)
      biab_output("level_2_polygon", out_path_2)
      biab_output("admin_2", adm2_name)
    }
  }
  sf2
}, error = function(e) biab_warning("ADM2 error occurred."))