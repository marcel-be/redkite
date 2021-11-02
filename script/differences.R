install.packages("ggnewscale")
library(ggnewscale)

# Difference of GPS vs. OBS:


grid_9_dif<- as.data.frame(cbind(grid_9_obs_prop,grid_9_gps_prop))
colnames(grid_9_dif)<- c("obs","gps")

grid_9_dif<- grid_9_dif %>% 
  mutate(dif= obs-gps,
         ratio= (obs+0.00001) / (gps+0.00001),
         ratio_round= round(((obs+0.00001)/(gps+0.00001)), digits=3)
         )

range(grid_9_dif$ratio)
range(grid_9_dif$ratio_round)
range(log(grid_9_dif$ratio))


grid_9_dif<- st_sf(n = grid_9_dif, geometry = st_cast(grid_9, "MULTIPOLYGON"))

ggplot() +
  geom_sf(data = grid_9_dif, aes(fill=log(n.ratio)))+
  geom_sf(data = wka_9, fill="transparent", size =2, col="grey")+
  scale_fill_gradient2(low = "blue", mid = "green", high = "red")+ 
 geom_sf_text(data = subset(grid_9_dif, n.ratio==1),aes(label = n.dif),size=2)+
  ggtitle("WKA 9 - Observer vs. GPS")+
  theme(legend.position = "none")




test<- grid_9_obs
test$n_gps<- grid_9_gps$n
test$dif_ratio<- grid_9_dif$n.ratio

ggplot(test) +
  geom_sf(data = wka_9, fill="transparent", size =2, col="grey")+
  geom_sf(aes(fill=log(n)))+
  scale_fill_gradient2(low = "white", mid = "#FFEB3B", high = "#F57F17", na.value = "transparent")+ 
  new_scale_fill() +    
  geom_sf(aes(fill=log(n_gps)))+
  scale_fill_gradient2(low = "white", mid = "#2196F3", high = "#0D47A1", na.value = "transparent")+ 
  new_scale_fill() +  
  geom_sf(aes(fill=log(dif_ratio)), alpha=0.9)+
  scale_fill_gradient2(low = "green", mid = "white", high = "violet", na.value = "transparent")+
 # geom_sf_text(data = subset(grid_9_obs, prop_round!=0),aes(label = prop_round),size=2)+
  theme(legend.position = "none")+
  ggtitle("GPS and Observer")
