library(leaflet)

# A mapping function
# dataframe -> map
# based on the Latitude and Longitude columns in the dataframe
mapping <- function(ds, DMS = FALSE) {
  # mn <- ds %>% 
  #   mutate(Latitude = as.numeric(VLatDegree),
  #          Longitude = as.numeric(VLongDegree))
  if (DMS) {
    ds_coord <- ds %>% 
      mutate(Lat_d = VLatDegree,
             Long_d = VLongDegree)
  }
  else{ds_coord <- ds %>% 
    mutate(Lat_d = paste(VLatDegree,VLatMinute,VLatSecond),
           Long_d = paste(VLongDegree,VLongMinute,VLongSecond))
  
  # convert from decimal minutes to decimal degrees
  ds_coord$Lat_d = measurements::conv_unit(ds_coord$Lat_d,
                                           from = 'deg_min_sec', 
                                           to = 'dec_deg')
  ds_coord$Long_d = measurements::conv_unit(ds_coord$Long_d, 
                                            from = 'deg_min_sec', 
                                            to = 'dec_deg')}

  map <- leaflet() %>% 
    addProviderTiles("OpenStreetMap.Mapnik") %>%
    addCircleMarkers(lng = as.numeric(ds_coord$Long_d), lat = as.numeric(ds_coord$Lat_d),
                     popup = paste(ds_coord$Genus, ds_coord$Species, 
                                   ", Location:", ds_coord$Location, 
                                   ", Collect no.:",ds_coord$`Collector Number`, 
                                   ", Lat:",ds_coord$Lat_d,
                                   ", Long:", ds_coord$Long_d),
                     clusterOptions = markerClusterOptions()) %>% 
    addEasyButton(easyButton(
      icon = "fa-crosshairs", title = "Locate Me",
      onClick = JS("function(btn, map){ map.setZoom(2);}")))
  return(map)
}

#takes a dataframe and makes the first word in the column lowercase
capitalize <- function(ds){
  split <- str_split(ds$Habitat," ",n = 2) 
  
  cap <- map(split,~str_to_sentence(.x[1]))
  
  ds$Habitat <- map2(split,cap,~c(.y,.x[2]) %>% 
                       paste(.,collapse = " "))
  
  ds$Habitat <- gsub("NA","",ds$Habitat)
  
  return(ds)
}

lower <- function(ds){
  split <- str_split(ds$`Field Notes`," ",n = 2) 
  
  cap <- map(split,~str_to_lower(.x[1]))
  
  ds$`Field Notes` <- map2(split,cap,~c(.y,.x[2]) %>% 
                       paste(.,collapse = " "))
  
  ds$`Field Notes` <- gsub("NA","",ds$`Field Notes`)
  
  return(ds)
}

# Originally Identified As
# given the name and determiner and year and formats it into the Original ID as format
# the test
# oia("gen","q","s","sub","var","for","det","yr")
# can you simplify it with map?
# note must use group_by on a unique identifier when using mutate
oia <- function(g,q,s,sub,v,f,det,yr){
  taxonomy <- c(g,q,s)
  id <- c()
  for(name in taxonomy){
    if(!is.na(name)){
      id <- append(id,name)
    }
  }
  
  prefix <- c("subsp.","var.","f.")
  lower <- c(sub,v,f)
  
  if(!is.na(sub)){
    id <- c(id,"subsp.",sub)
  }
  if(!is.na(v)){
    id <- c(id,"var.",v)
  }
  if(!is.na(f)){
    id <- c(id,"f.",f)
  }

  return(paste(c(id,"by",det,yr), collapse = " "))}

# Directions abbreviated
# changes the N E W S  to full words in the dataframe
# Come back to this to add all the possible cases
dir_abb <- function(df){
  df_loc <- df %>% 
    mutate(Location = str_replace_all(Location,"NNW(?=[:blank:])|NNW-","north-northwest"),
           Location = str_replace_all(Location,"NNE(?=[:blank:])|NNE-","north-northeast"),
           Location = str_replace_all(Location,"SSE(?=[:blank:])|SSE-","south-southeast"),
           Location = str_replace_all(Location,"SSW(?=[:blank:])|SSW-","south-southwest"),
           Location = str_replace_all(Location,"WNW(?=[:blank:])|WNW-","west-northwest"),
           Location = str_replace_all(Location,"SW(?=[:blank:])|SW-","southwest"),
           Location = str_replace_all(Location,"NW-","northwest-"),
           Location = str_replace_all(Location,"NW(?=[:blank:])","northwest"),
           Location = str_replace_all(Location,"SE(?=[:blank:])|SE-","southeast"),
           Location = str_replace_all(Location,"NE(?=[:blank:])|NE-","northeast"),
           Location = str_replace_all(Location,"S(?=[:blank:])|S-","south"),
           Location = str_replace_all(Location,"W(?=[:blank:])|W-","west"),
           Location = str_replace_all(Location,"\\(W\\)","\\(west\\)"),
           Location = str_replace_all(Location,"N(?=[:blank:])|N-|N/","north"),
           Location = str_replace_all(Location,"E(?=[:blank:])|E-","east"))
  
  return(df_loc)
}