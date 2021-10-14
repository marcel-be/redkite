library(data.table)
library(dplyr)
library(sf)

path<- "J:/redkite/marcel/"

#### Filter Shapefile (downloaded from movebank) by WKA-radius, time and individual:

redkite_shp<- st_read(paste0(path, "data_download_movebank/gis/points.shp"))
WKA_radius<- st_read(paste0(path, "data_QGIS/WKA_radius/WKA_geplant_Radius.shp"))

redkite_shp<-st_transform(redkite_shp, crs = 32632) # EPSG:32632 = UTM zone 32N
WKA_radius<- st_transform(WKA_radius, crs = 32632)

redkite_shp$timestamp<- as.POSIXct(redkite_shp$timestamp)
redkite_shp$ind_ident<- as.character(redkite_shp$ind_ident)

redkite_filter <- st_intersection(redkite_shp, WKA_radius)%>% 
  filter(timestamp > "2019-03-23 07:40:00") %>% 
  filter(timestamp < "2019-06-20 18:20:00") %>% 
  filter(grepl("Nele|Hannibal|Selma|Laia|Agathe|Friedolin|Quincy|Rapida|Karl|Jan-Ole",ind_ident)) %>% 
  filter(!grepl("_Rapunzel|_Rausch|_Ron",ind_ident)) 

levels(as.factor(redkite_filter$ind_ident))
range(redkite_filter$timestamp)

mapview(redkite_filter) + mapview(WKA_radius)

st_write(redkite_filter, paste0(path, "/data_download_movebank/gis/", "redkite_filtered_r_20190323_20190620.shp"))
st_write(redkite_filter, paste0(path, "/data_download_movebank/", "redkite_filtered_r_20190323_20190620.csv"))



###########################################################################  
#### Working with CSV (If shapefile not filtered beforehand):
## If step above was done, this can be skipped!

redkite<- fread("data_download_movebank/redkite_gps_clipped.csv")
head(redkite)
range(redkite$timestamp)
filtered<- redkite %>% 
  #filter(grepl("Nele|Hannibal|Selma|Laia|Agathe|Friedolin|Quincy|Rapida|Karl|Jan-Ole",individual)) %>% 
  filter(timestamp > "2019-03-23 07:40:00") %>% 
  filter(timestamp < "2019-06-20 18:20:00")
range(filtered$timestamp)






