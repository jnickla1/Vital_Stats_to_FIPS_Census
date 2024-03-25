library(sf)
library(dplyr)
library(purrr)
library(openxlsx)

# County shapefile
county_sf <- st_read("/Users/JohnMatthew/Downloads/cb_2018_us_county_500k/cb_2018_us_county_500k.shp")
# Place shapefile
city_sf <- st_read("/Users/JohnMatthew/Downloads/cb_2022_us_place_500k/cb_2022_us_place_500k.shp")

# Excel place list
placelist <- read.xlsx("/Users/JohnMatthew/Downloads/cb_2018_us_county_500k/code_by_state2.xlsx","Sheet1")

cntylist <- placelist[placelist$CntyYN == TRUE ,]
#county_sf$COUNTYFP


checkifFoundCounty = function(x, output){
  # accessing elements from first column
  state = as.integer(x["ST_FIPS"])
  cntyint = as.integer(x["FIPS_cnty"])
  pdcnty = formatC(cntyint, width = 3, format = "d", flag = "0")
  pdstate= formatC(state, width = 2, format = "d", flag = "0")
  found = paste(pdstate,pdcnty) %in% paste(county_sf$STATEFP, county_sf$COUNTYFP)
  #print(paste(state,pdcnty))
  #print(pdcnty)
  if (found){
    found2 = paste(pdstate,pdcnty,x["Cnty_Trim"]) %in% paste(county_sf$STATEFP, county_sf$COUNTYFP,county_sf$NAME)
    if(found2){
      return("")
    }else{
      subset1 <- county_sf[county_sf$STATEFP == state,]
      subset2 <- subset1[subset1$COUNTYFP == pdcnty,]
      return(subset2$NAME)
    }
} else{
  return("NOT FOUND")}
}

checkedCounties <- apply(cntylist,1,checkifFoundCounty )
cntylist$cnty_check <- checkedCounties
write.xlsx(cntylist, file = "/Users/JohnMatthew/Downloads/cb_2018_us_county_500k/county_checked.xlsx")
#cbind(cntylist, check = checkedCounties)


citylist <- placelist[placelist$CityYN == TRUE ,]

checkifFoundCity = function(x, output){
  # accessing elements from first column
  state = as.integer(x["ST_FIPS"])
  pdstate= formatC(state, width = 2, format = "d", flag = "0")
  found = paste(pdstate,x["City_Trim"]) %in% paste(city_sf$STATEFP,city_sf$NAME)
  found2 = paste(pdstate,x["City_Trim"]) %in% paste(city_sf$STATEFP,city_sf$NAMELSAD)
  if (found | found2){
      return("")
  } else{
    return("NOT FOUND")}
}

checkedCities <- apply(citylist,1,checkifFoundCity )
citylist$city_check <- checkedCities
write.xlsx(citylist, file = "/Users/JohnMatthew/Downloads/cb_2018_us_county_500k/city_checked.xlsx")