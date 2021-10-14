library(data.table)
library(dplyr)
library(stringr)
library(sf)
library(raster)
library(ggplot2)
library(mapview)
pal = mapviewPalette("mapviewTopoColors")
library(move)
library(ctmm)
library(scales)

path<- "E:/redkite/"

##################################################################################################################################
#### Observer Data

#### 1. Steps in GIS
# load observer data (shp in QGIS) - Create Vertices from "Schlangenlinien" 
# clip with WKA-radius

#### 2. Data preperation - observer-table

obs<- read.csv(paste0(path, "data/data_observer/observer_data_vertices_clipped.csv"))

# assign individual names (in observer data only abbreviations)
obs$individual<- NA
obs<- obs%>%
mutate(individual = replace(individual, Individuum=="a"|Individuum=="A"|Individuum=="Agathe", "Agathe"),
       individual = replace(individual, Individuum=="f", "Friedolin"),
       individual = replace(individual, Individuum=="h", "Hannibal"),
       individual = replace(individual, Individuum=="j", "Jan-Ole"),
       individual = replace(individual, Individuum=="k", "Killer-Karl"),
       individual = replace(individual, Individuum=="l"|Individuum=="L", "Laja"),
       individual = replace(individual, Individuum=="n", "Nele"),
       individual = replace(individual, Individuum=="q", "Quincy"),
       individual = replace(individual, Individuum=="r", "Rapida"),
       individual = replace(individual, Individuum=="s"|Individuum=="S", "Selma"),
       long=X,
       lat=Y
       )
table(obs$Individuum)
table(obs$individual) # passt

#### assign "number of observation":
zeitfenster<- read.csv(paste0(path, "Untersuchungszeitfenster_for_R.csv"), sep=";")

zeitfenster<- zeitfenster %>% 
mutate(individual=str_split_fixed(individual.local.identifier, " ", 2)[,1],
       Start=as.POSIXct(Start,format="%d.%m.%Y %H:%M"),
       date=as.Date(Start, "%m/%d/%y"))

obs$date<- as.Date(obs$Datum)

for(i in 1:length(obs$individual)){
  for(j in 1:length(zeitfenster$individual))
    
    if(obs$individual[i] == zeitfenster$individual[j] & obs$date[i]==zeitfenster$date[j]){
      obs$session[i]<- zeitfenster$Session[j]
    } else{}
}

obs_senderonly<- obs %>% 
  filter(Sender=="ja"|Sender=="sender"|Sender=="Sender") # only sightings with bird+transmitter!


#### 3. create Shapefile


obs_shp<- st_as_sf(obs_senderonly, coords = c("X", "Y"), crs =4326)
obs_shp<- st_transform(obs_shp, crs = 32632)

st_write(obs_shp, paste0(path, "data_observer/shapefiles/", "observer_final.shp"))
st_write(obs_shp, paste0(path, "data_observer/", "observer_final.csv"))


#### 4. Load Observer-Shapefiles

obs_shp<-st_read(paste0(path, "data/data_observer/shapefiles/", "observer_final.shp"))
WKA_radius<- st_read(paste0(path,"data/data_QGIS/WKA_radius/WKA_radius_32N.shp"))

## 4.2 create suitable utm-shapefile for Observations: 

#obs_shp<- st_transform(obs_shp, crs="+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs")
obs_shp$long_utm32<- st_coordinates(obs_shp)[,1]
obs_shp$lat_utm32<- st_coordinates(obs_shp)[,2]

# in case of projection issues:
#obs_utm<- as.data.frame(cbind(obs_shp$individual, obs_shp$long_utm32, obs_shp$lat_utm32))
#colnames(obs_utm)<- c("individual", "X", "Y")
#obs_utm_shp <- st_as_sf(obs_utm, coords = c("X", "Y"), crs =32632)


## 4.3 create grid

grid<- WKA_radius %>% 
  st_make_grid(., cellsize = 250, square = T) %>% 
  st_intersection(., WKA_radius)


crs(WKA_radius)
crs(grid)
crs(obs_shp)

## 4.3 Plots

ggplot() +
  geom_sf(data = WKA_radius, fill="transparent")+
  geom_sf(data = grid, fill="transparent")+
  geom_point(data = obs_shp, aes(x = long_utm32, y = lat_utm32), col="orange")

mapview(obs_shp) + mapview(WKA_radius)+ mapview(grid)


#### 5. Heatmap

pointcount<- st_intersects(grid, obs_shp)
lengths(pointcount)

grid_count <- grid %>% 
  st_intersects(., obs_shp) %>% 
  lengths(.) %>% 
  st_sf(n = ., geometry = st_cast(grid, "MULTIPOLYGON"))

mapview(WKA_radius) + mapview(obs_shp) + mapview(grid_count, zcol = "n")
#mapview(grid_count, zcol = "n", col.regions = pal(20), at = seq(0, 500, 5)) # change colour setting

ggplot() +
  geom_sf(data = WKA_radius, fill="transparent")+
  geom_sf(data = grid_count, aes(fill=n))+
  scale_fill_gradient(low = "white", high = "red")


# subset for one WKA:

wka_9<- WKA_radius %>% 
  filter(WKA_radius$number==9)

grid_9<- wka_9 %>% 
  st_make_grid(., cellsize = 250, square = T) %>% 
  st_intersection(., wka_9)

grid_9_obs_count <- grid_9 %>% 
  st_intersects(., obs_shp) %>% 
  lengths(.) 

grid_9_obs_prop<- (grid_9_obs_count / sum(grid_9_obs_count)) * 100

grid_9_obs <- st_sf(n = grid_9_obs_prop, geometry = st_cast(grid_9, "MULTIPOLYGON"))

grid_9_obs$prop_round<- round(grid_9_obs_prop, digits =2)

mapview(wka_9) + mapview(grid_9_obs) #+ mapview(redkite_shp)
ggplot() +
  geom_sf(data = grid_9_obs, aes(fill=log(n)))+
  geom_sf(data = wka_9, fill="transparent", size =2, col="grey")+
  scale_fill_gradient2(low = "white", mid = "yellow", high = "red")+ # find better gradient; Log-Scale!!!!
  geom_sf_text(data = grid_9_obs,aes(label = prop_round),size=2)+
  ggtitle("WKA 9 - Observer Rapida")



##################################################################################################################

# Test AKDE calculation
obs_agathe<- obs %>%  filter(individu_1=="Agathe (H_507)")


dup<- getDuplicatedTimestamps(x=obs_agathe$individu_1,
                        timestamp=as.POSIXct(obs_agathe$timestamp, format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
                        sensorType=obs_agathe$sensor.typ)

agathe1.clean<-obs_agathe
nrow(agathe1.clean) #still same N a initial data frame, hence: No "Na" and no "zero"
# Innerhalb der Schleife wird zunaechst auf "echte Dubletten" (gleicher timestamp, name, long, lat) geprueft
# bei den uebrigen Dubletten (gleicher timestamp), werden die Distanzen berechnet und der Punkt erhalten, der besser in die Reihe zu den "umliegenden Daten" passt.
head(agathe1.clean)
colnames(agathe1.clean)[c(4,5,30)]<- c("Long", "Lat", "individual.local.identifier")
colnames(agathe1.clean)

while(length(dup <- getDuplicatedTimestamps(x=agathe1.clean$individual.local.identifier,
                                            timestamp=as.POSIXct(agathe1.clean$timestamp, format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
                                            sensorType=agathe1.clean$sensor.typ))>0){
  allrowsTOremove <- lapply(1:length(dup), function(x)
  {
    rown <- dup[[x]]
    if(any(duplicated(agathe1.clean[rown,c("timestamp", "Long", "Lat", "individual.local.identifier")]))){
      dup.coor <- duplicated(agathe1.clean[rown,c("timestamp", "Long", "Lat", "individual.local.identifier")])
      rowsTOremove <- rown[dup.coor] # remove duplicates
    }else{
      # subset for the individual, as distances should be measured only within the individual
      # create a row number ID to find the duplicated time stamps in the subset per individual
      agathe1.clean$rowNumber <- 1:nrow(agathe1.clean)
      ind <- unlist(strsplit(names(dup[x]),split="|", fixed=T))[1]
      subset <- agathe1.clean[agathe1.clean$individual.local.identifier==ind,]
      
      # if the duplicated positions are in the middle of the table
      if(subset$rowNumber[1]<rown[1] & subset$rowNumber[nrow(subset)]>max(rown)){
        # calculate total distance through the first alternate location
        dist1 <- sum(distHaversine(subset[subset$rowNumber%in%c((rown[1]-1),(max(rown)+1)),c("Long", "Lat")],
                                   subset[subset$rowNumber==rown[1],c("Long", "Lat")]))
        # calculate total distance through the second alternate location
        dist2 <- sum(distHaversine(subset[subset$rowNumber%in%c((rown[1]-1),(max(rown)+1)),c("Long", "Lat")],
                                   subset[subset$rowNumber==rown[2],c("Long", "Lat")]))
        # omit the aternate location that produces the longer route
        if(dist1<dist2){rowsTOremove <- rown[2]}else{rowsTOremove <- rown[1]}
      }
      
      # incase the duplicated timestamps are the first positions
      if(subset$rowNumber[1]==rown[1]){
        dist1 <- sum(distHaversine(subset[subset$rowNumber==(max(rown)+1),c("Long", "Lat")],
                                   subset[subset$rowNumber==rown[1],c("Long", "Lat")]))
        dist2 <- sum(distHaversine(subset[subset$rowNumber==(max(rown)+1),c("Long", "Lat")],
                                   subset[subset$rowNumber==rown[2],c("Long", "Lat")]))
        if(dist1<dist2){rowsTOremove <- rown[2]}else{rowsTOremove <- rown[1]}
      }
      
      # incase the duplicated timestamps are the last positions
      if(subset$rowNumber[nrow(subset)]==max(rown)){
        dist1 <- sum(distHaversine(subset[subset$rowNumber==(rown[1]-1),c("Long", "Lat")],
                                   subset[subset$rowNumber==rown[1],c("Long", "Lat")]))
        dist2 <- sum(distHaversine(subset[subset$rowNumber==(rown[1]-1),c("Long", "Lat")],
                                   subset[subset$rowNumber==rown[2],c("Long", "Lat")]))
        if(dist1<dist2){rowsTOremove <- rown[2]}else{rowsTOremove <- rown[1]}
      }
    }
    return(rowsTOremove)
  })
  agathe1.clean <- agathe1.clean[-unique(sort(unlist(allrowsTOremove))),]
  agathe1.clean$rowNumber <- NULL
}



obs_test<- move(x=agathe1.clean$Long,
          y=agathe1.clean$Lat,
          time=as.POSIXct(agathe1.clean$timestamp, format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
          data=agathe1.clean,
          proj=CRS("+proj=longlat +datum=WGS84"),
          animal=agathe1.clean$individual.local.identifier,
          sensor=agathe1.clean$sensor.typ)

# moveStack() and split() functions to stack and unstack 'Move' objects.

### preparing movement data ----
obs_test<-spTransform(obs_test, CRS("+init=epsg:3035")) #transforms coordinates to Europe LAEA projection

# next: ctmm package: 
# check variogram for decent akde model: https://cran.r-project.org/web/packages/ctmm/vignettes/variogram.html
# alternative: use MLL Estimation (and check variogram afterwards)
obs_ctmm <- as.telemetry(obs_test, timezone = "UTC", drop = F) #generates telemetry-object for ctmm
obs_ctmm

var_test<- variogram(obs_ctmm$Agathe)
plot(var_test)

### generate new lists to store results in ----
akde.sizes<-data.frame(row.names = names(obs_ctmm))
akde.sizes$timespan1<-NA
akde.data.kites<-list()
spdf.list.kites<-list()

### check variograms for autocorrelation pattern? (time)

### calculates akdes, stores results and spdfs in lists automatically ----
# https://cran.r-project.org/web/packages/ctmm/vignettes/akde.html
# KDE: https://www.youtube.com/watch?v=x5zLaWT5KPs
for(i in 1:length(obs_ctmm)){
  a.ouf<-ctmm.guess(obs_ctmm[[i]],CTMM=ctmm, interactive = F) #automated guess of model parameters
  M.OUF<-ctmm.fit(obs_ctmm[[i]],a.ouf) #fitting of the a.ouf model
  akde.data.kites[[i]]<-akde(obs_ctmm[[i]],M.OUF,weights=T) #calculation of akdes
  akde.sizes$timespan1[i]<-summary(akde.data.kites[[i]], units=F)$CI[2]/1000000 #stores akde size in list (sqkm)
  spdf.list.kites[[i]]<-SpatialPolygonsDataFrame.UD(akde.data.kites[[i]]) #stores akde spdf in list to plot or export it to gis
}



