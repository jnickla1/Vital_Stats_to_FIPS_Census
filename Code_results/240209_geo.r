library(sf)
library(dplyr)
library(purrr)
library(openxlsx)

# State shapefile
state_sf <- st_read("/Users/erinkim/Desktop/cd/240209_grid/cb_2022_us_state_5m/cb_2022_us_state_5m.shp")
# County shapefile
county_sf <- st_read("/Users/erinkim/Desktop/cd/240209_grid/cb_2022_us_county_5m/cb_2022_us_county_5m.shp")
# Place shapefile
city_sf <- st_read("/Users/erinkim/Desktop/cd/240209_grid/cb_2022_us_place_500k/cb_2022_us_place_500k.shp")

split_grid <- function(city_sf) {
  lon_min <- st_bbox(city_sf)[1]
  lon_max <- st_bbox(city_sf)[3]
  lat_min <- st_bbox(city_sf)[2]
  lat_max <- st_bbox(city_sf)[4]
  
  # Set grid cells (9km)
  cell_size <- 0.08
  grid_cells <- list()
  for (x in seq(lon_min, lon_max, by = cell_size)) {
      for (y in seq(lat_min, lat_max, by = cell_size)) {
          # Create polygons
          polygon <- st_polygon(list(rbind(c(x, y), c(x + cell_size, y), c(x + cell_size, y + cell_size), c(x, y + cell_size), c(x, y))))
          # Add to grid cell list
          grid_cells[[length(grid_cells) + 1]] <- polygon
      }
  }

  grid_sfc <- st_sfc(grid_cells)

  grid_sf <- st_sf(geometry = grid_sfc, crs=4269)
  return(grid_sf)
}

# Intersection by state
calculate_and_visualize <- function(state_sf, county_sf, city_sf, cell_size) {
  intersection_merge <- NULL

  for (i in 1:nrow(state_sf)) {
    state <- state_sf$STUSPS[i]
    
    # County and city data per state
    county_sf_temp <- county_sf %>%
      filter(STUSPS == state)
    city_sf_temp <- city_sf %>%
      filter(STUSPS == state)
    
    # Intersection by county
    intersection_with_county <- st_intersection(city_sf_temp, county_sf_temp)
    
    # Create 9km grid
    grid_sf <- split_grid(county_sf_temp)
    
    # Intersect with grid cells
    intersection_with_county <- st_transform(intersection_with_county, st_crs(grid_sf))
    intersection_with_grid <- st_intersection(intersection_with_county, grid_sf)
    
    # Calculate intersection with grid cells 
    intersection_with_grid$area <- st_area(st_transform(intersection_with_grid$geometry, crs = 3395)) / 10^6
    
    # Final data merge 
    intersection_merge <- rbind(intersection_merge, intersection_with_grid)
  }
}

calculate_and_visualize(state_sf, county_sf, city_sf)

# Save intersection_merge as Excel file
write.xlsx(intersection_merge, file = "results.xlsx")