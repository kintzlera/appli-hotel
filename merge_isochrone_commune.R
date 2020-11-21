# GENERATE ISOCHRONES
library(tidytransit)
library(data.table)
library(magrittr)
library(jsonlite)
library(sf)
library(dplyr)

# CUSTOM FUN IMPORT
source("functions.R")

# DEFINE PATH
OUTPUT_DIR <- "output"
ISOCHRONE_FILENAME <- "isochrone.csv"

isochrone <- jsonlite::read_json("./output/isochrone/isochrone_from_station_cycling_road.json")
geojson_communes <- sf::read_sf("data/geo/communes-20190101.json")

format_metadata_info <- function(res_text) {
  more_info <- fromJSON(res_text[[1]])$metadata$query
  id_df <- data.frame(fromJSON(more_info$id))
  loc_df <- data.frame(more_info$locations)
  names(loc_df) <- c("lon", "lat")
  out_df <- cbind(id_df, loc_df)
  out_df$group_index <- seq(from=0, to=nrow(out_df)-1)
  out_df$coord_gare <- lapply(1:nrow(out_df), function(i) c(out_df$lon[i], out_df$lat[i]))
  return(out_df)
}


list_isochrone_and_cities <- lapply(
  X = isochrone, 
  FUN = function(res_txt) {
    more_info <- format_metadata_info(res_txt)
    geojson_isochrones <- sf::read_sf(res_txt)
    geojson_isochrones <- geojson_isochrones %>% 
      dplyr::left_join(more_info)
    intersect_obj <- sf::st_intersection(geojson_isochrones, geojson_communes)
    return(intersect_obj)
  })


# Convert to dataframe
df_isochrone_and_cities <- lapply(
  X = list_isochrone_and_cities,
  FUN = function(isochrone) {
    data.frame(isochrone) %>% 
      dplyr::mutate(temps = value / 60) %>% 
      dplyr::select(gare, nom, insee, temps, coord_centre_isochrone = center, coord_gare, type_reseau)
  }) %>% dplyr::bind_rows()

if (!grepl(pattern = "\\{[0-9]+\\.[0-9]+,[0-9]+\\.[0-9]+\\}", x = df_isochrone_and_cities$coord_gare[1])) {
  df_isochrone_and_cities$coord_gare <- sapply(df_isochrone_and_cities$coord_gare, list_of_coord_to_tuple)
}
if (!grepl(pattern = "\\{[0-9]+\\.[0-9]+,[0-9]+\\.[0-9]+\\}", x = df_isochrone_and_cities$coord_centre_isochrone[1])) {
  df_isochrone_and_cities$coord_centre_isochrone <- sapply(df_isochrone_and_cities$coord_centre_isochrone, list_of_coord_to_tuple)
}

out <- df_isochrone_and_cities %>% dplyr::mutate(code_postal=NA) %>% 
  dplyr::select(code_insee = insee, code_postal, gare, nom_commune = nom,
                temps, coord_centre_isochrone, coord_gare, type_reseau) %>% 
  dplyr::arrange(gare, nom_commune, temps) 


# WRITING TO FILE JSON
if (dir.exists(OUTPUT_DIR) == FALSE) {
  dir.create(path=OUTPUT_DIR)
}

#OK
readr::write_csv(x = out,
                 path = file.path(OUTPUT_DIR, ISOCHRONE_FILENAME),
                 quote_escape = "backslash")
