library(data.table)
library(dplyr)
library(stringr)
library(sf)
library(raster)
library(ggplot2)
library(mapview)
library(move)
library(ctmm)
library(scales)
path<- "E:/redkite/"

##################################################################################################################################
#### Red Kite Movebank Data
# pre-filtered in Script "movebank_data.R"
# Filtered by timestamp >"2019-03-23 07:40:00" < "2019-06-20 18:20:00"
# clipped with WKA-radius

redkite_shp<- st_read(paste0(path,"data/data_download_movebank/gis/redkite_filtered_r_20190323_20190620.shp"))

## Alternative: load CSV:
#redkite<- fread(paste0(path, "data_download_movebank/redkite_filtered_r_20190323_20190620.csv"))
#redkite_shp<-  st_as_sf(redkite, coords = c("utm_east", "utm_north"), crs ="+proj=utm +zone=32 +datum=WGS84 +units=m")
#redkite_shp<-  st_as_sf(redkite, coords = c("utm_east", "utm_north"), crs =4326) #alternative: in case of projection issues
#redkite_shp<- st_transform(obs_shp, crs = 32632)

crs(redkite_shp) # EPSG:32632 = UTM zone 32N
redkite_shp$X<- st_coordinates(redkite_shp)[,1]
redkite_shp$Y<- st_coordinates(redkite_shp)[,2]

WKA_radius<- st_read(paste0(path,"data/data_QGIS/WKA_radius/WKA_radius_32N.shp"))

mapview(WKA_radius) + mapview(redkite_shp)

## create grid

grid<- WKA_radius %>% 
  st_make_grid(., cellsize = 250, square = T) %>% 
  st_intersection(., WKA_radius)

## Plot of raw data

ggplot() +
  geom_sf(data = WKA_radius, fill="transparent")+
  geom_sf(data = grid, fill="transparent")+
  geom_point(data = redkite_shp, aes(x = X, y = Y), col="orange")


#### Heatmap for proportion of gps-signals per grid cell

pointcount<- grid %>% 
  st_intersects(., redkite_shp) %>% 
  lengths()

pointcount_prop<- (pointcount / sum(pointcount)) * 100
grid_count <- st_sf(n = pointcount_prop, geometry = st_cast(grid, "MULTIPOLYGON"))

mapview(WKA_radius) + mapview(grid_count, zcol = "n")

p<- ggplot() +
      geom_sf(data = WKA_radius, fill="transparent", size =2, col="grey")+
      geom_sf(data = grid_count, aes(fill=log(n)), size=0.0005, col="gray70")+
      scale_fill_gradient2(low = "khaki1", mid = "orange", high = "red", na.value = "white")+
      theme(
         plot.title=element_text(size=20),
         legend.position = "none")

png(paste0(path, "output/proportions_gps/","gps_data_allwka.png"),
    width=3000, height=2300)
print(p)
dev.off()


####################################################################################################
# subset for one WKA:

wka_9<- WKA_radius %>% 
  filter(WKA_radius$number==9)


grid_9<- wka_9 %>% 
  st_make_grid(., cellsize = 250, square = T) %>% 
  st_intersection(., wka_9)

grid_9_gps_count <- grid_9 %>% 
  st_intersects(., redkite_shp) %>% 
  lengths(.) 
grid_9_gps_prop<- (grid_9_gps_count / sum(grid_9_gps_count)) * 100
grid_9_gps <-  st_sf(n = grid_9_gps_prop, geometry = st_cast(grid_9, "MULTIPOLYGON"))

grid_9_gps<- grid_9_gps %>% 
  mutate(grid_9_gps$prop_round=round(grid_9_gps_prop, digits = 2))

#mapview(wka_9) + mapview(grid_9_gps) #+ mapview(redkite_shp)

ggplot() +
  geom_sf(data = grid_9_gps, aes(fill=log(n)))+
  geom_sf(data = wka_9, fill="transparent", size =2, col="grey")+
  scale_fill_gradient2(low = "white", mid = "yellow", high = "red", na.value = "white")+ # find better gradient; Log-Scale!!!!
  geom_sf_text(data = subset(grid_9_gps, prop_round!=0),aes(label = prop_round),size=3)+
  theme(legend.position = "none")+
  ggtitle("WKA 9 - GPS Rapida")


## Subsets for all WKAs


plot_list = list()
for(i in 1:nrow(WKA_radius)){
  wka<- WKA_radius %>% 
    filter(WKA_radius$number==WKA_radius$number[i])
  
  grid<- wka %>% 
    st_make_grid(., cellsize = 250, square = T) %>% 
    st_intersection(., wka)
  
  grid_gps_count <- grid %>% 
    st_intersects(., redkite_shp) %>% 
    lengths(.) 
  
  grid_gps_prop<- (grid_gps_count / sum(grid_gps_count)) * 100
  grid_gps <-  st_sf(n = grid_gps_prop, geometry = st_cast(grid, "MULTIPOLYGON"))
  
  grid_gps$prop_round<- round(grid_gps_prop, digits = 2)
  
  p<- ggplot() +
        geom_sf(data = grid_gps, aes(fill=log(n)))+
        geom_sf(data = wka, fill="transparent", size =2, col="grey")+
        scale_fill_gradient2(low = "white", mid = "yellow", high = "red", na.value = "white")+ # find better gradient; Log-Scale!!!!
        geom_sf_text(data = subset(grid_gps, prop_round!=0),aes(label = prop_round),size=3.5)+
        ggtitle(paste0("WKA ", WKA_radius$number[i], " - GPS ",WKA_radius$name[i]))+
     theme(
       plot.title=element_text(size=20),
       legend.position = "none")
  
  plot_list[[i]] = p
}

for(i in 1:nrow(WKA_radius)){
  png(paste0(path, "output/proportions_gps/","WKA_", WKA_radius$number[i], "_GPS_",WKA_radius$name[i],".png"),
      width=1200, height=1000)
  print(plot_list[[i]])
  dev.off()
}



#########################################################################

# calculate AKDE for each WKA-radius


