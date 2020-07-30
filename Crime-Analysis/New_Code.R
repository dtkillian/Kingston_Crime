library(tidyverse)
library(tabulizer)
library(stringr)
library(tidygeocoder)
library(leaflet)

proper_case <- function(x) {
  return (gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2" , x, perl=TRUE))
}

# Location of folders
folder_location <- "C:/Users/danie/Documents/GitHub/Crime-Analysis/Rproject/Crime-Analysis/data"

for(k in 1:length(list.files(folder_location))){
  location <- paste0(folder_location, "/", list.files(folder_location)[k])
  
  # Extract the table
  for(i in 1:get_n_pages(location)){
    temp <-  extract_tables(location, pages = i, guess = FALSE, columns = list(c(100, 200, 300, 400, 490)))
    temp <- do.call(cbind, temp)
    temp <- temp[-c(1:3),]
    temp <- temp %>% data.frame()
    names(temp) <- temp[1,]
    temp <- temp[-1,]
    
    if(i == 1){
      data <- temp
    }else{
      data <- rbind(data,temp)
    }
    
  }
  
  rm(i, temp)
  
  data <- data[-nrow(data),]
  
  # Clean data
  
  i <- 1
  while(i < nrow(data)){
    if("" %in% data[i+1,-ncol(data)]){
      data[i,] <- paste(data[i,],data[i+1,])
      data <- data[-(i+1),]
    }else{
      i <- i+1
    }
  }
  rm(i)
  
  data <- data %>% 
    mutate(`Call Type` = proper_case(`Call Type`),
           Disposition = proper_case(Disposition)) %>%
    mutate(Number = str_trim(Number, side = "both"),
           `Date of Call` = str_trim(`Date of Call`, side = "both"),
           `Time of Call` = str_trim(`Time of Call`, side = "both"),
           `Call Type` = str_trim(`Call Type`, side = "both"),
           Location = str_trim(Location, side = "both"),
           Disposition = str_trim(Disposition, side = "both"))%>%
    mutate(Number = str_squish(Number),
           `Date of Call` = str_squish(`Date of Call`),
           `Time of Call` = str_squish(`Time of Call`),
           `Call Type` = str_squish(`Call Type`),
           Location = str_squish(Location),
           Disposition = str_squish(Disposition)) %>%
    mutate(Location = paste0(Location, ", Kingston, NY")) %>%
    separate(Location, into = c("Location_Name", "Location"), 
             sep = "(?<=[a-zA-Z])\\s*(?=[0-9])") %>%
    mutate(Location = ifelse(is.na(Location),Location_Name,Location)) %>%
    mutate(Location_Name = ifelse(Location_Name == Location, NA, Location_Name)) %>% 
    geocode(Location,lat=Lat,long=Long,method = "cascade")
  
  if(k == 1){
    final_data <- data
  }else{
    final_data <- rbind(final_data, data)
  }
  
}
# remove records that we don't care about
# 


final_data$popup <- paste("<b>Incident #: </b>", final_data$Number, "<br>",
                    "<b>Category: </b>", final_data$`Call Type`,
                    # "<br>", "<b>Description: </b>", final_data$Descript,
                    # "<br>", "<b>Day of week: </b>", final_data$DayOfWeek,
                    "<br>", "<b>Date: </b>", final_data$`Date of Call`,
                    "<br>", "<b>Time: </b>", final_data$`Time of Call`,
                    # "<br>", "<b>PD district: </b>", final_data$PdDistrict,
                    "<br>", "<b>Resolution: </b>", final_data$Disposition,
                    "<br>", "<b>Address: </b>", final_data$Location,
                    "<br>", "<b>Longitude: </b>", final_data$Long,
                    "<br>", "<b>Latitude: </b>", final_data$Lat)

final_data[complete.cases(final_data[ , 8:9]),] %>%
  leaflet(width = "100%", height = "800px") %>% 
  addTiles() %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles(provider = "Esri.WorldStreetMap",group = "World StreetMap") %>%
  addProviderTiles(provider = "Esri.WorldImagery",group = "World Imagery") %>%
  # addProviderTiles(provider = "NASAGIBS.ViirsEarthAtNight2012",group = "Nighttime Imagery") %>%
  addMarkers(lng = ~Long, lat = ~Lat, popup = final_data$popup, clusterOptions = markerClusterOptions()) %>%
  addLayersControl(
    baseGroups = c("OSM (default)","World StreetMap", "World Imagery"),
    options = layersControlOptions(collapsed = FALSE)
  )
