library(ggnewscale)


# all wka-plots in one file: 

plot_list_gps<- readRDS("plot_list_gps.rds")
plot_list_obs<- readRDS("plot_list_obs.rds")


plot_list<- list()


for(i in 1:length(plot_list_gps)){
  plot_list[count_gps[i]]<- plot_list_gps[i]
}

for(i in 1:length(plot_list_obs)){
  plot_list[count_obs[i]]<- plot_list_obs[i]
}

ggsave(filename = "output/gps_vs_observer.pdf",
       plot = gridExtra::marrangeGrob(plot_list, nrow=1, ncol=2), 
       width = 15, height = 9)


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
  scale_fill_gradient2(low = "blue", mid = "white", high = "red")+ 
 geom_sf_text(data = subset(grid_9_dif, n.ratio!=1),aes(label = round(n.dif,digits=2)),size=2.5)+
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

  
  ggplot() +
  geom_sf(data = wka_9, fill="transparent", size =2, col="grey")+
  geom_sf(data = grid_9_difaes(fill=log(n)))+
  scale_fill_gradient2(low = "white", mid = "#FFEB3B", high = "#F57F17", na.value = "transparent")
#  geom_sf_text(data = subset(grid_9_dif, dif!=0),aes(label = dif),size=2)+
  theme(legend.position = "none")+
  ggtitle(paste0("Observer vs GPS at WKA ", WKA_radius$number[2]))

plot_list<- list()
  
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
  
  grid_observer_count <- grid %>% 
    st_intersects(., obs_shp) %>% 
    lengths(.) 
  grid_observer_prop<- (grid_observer_count / sum(grid_observer_count)) * 100

  grid_dif<- as.data.frame(cbind(grid_observer_prop,grid_gps_prop))
  colnames(grid_dif)<- c("obs","gps")
  
  grid_dif<- grid_dif %>% 
    mutate(dif= obs-gps,
           ratio= (obs+0.00001) / (gps+0.00001),
           ratio_round= round(((obs+0.00001)/(gps+0.00001)), digits=3))
           
  grid_dif<- st_sf(n = grid_dif, geometry = st_cast(grid, "MULTIPOLYGON"))
  
  p<- ggplot() +
        geom_sf(data = grid_dif, aes(fill=log(n.ratio)))+
        geom_sf(data = wka, fill="transparent", size =2, col="grey")+
        scale_fill_gradient2(low = "blue", mid = "white", high = "red")+ 
        geom_sf_text(data = subset(grid_dif, n.ratio!=1),aes(label = round(n.dif,digits=2)),size=2.5)+
        ggtitle(paste0("Observer vs GPS at WKA ", WKA_radius$number[i]))+
        theme(legend.position = "none")
  plot_list[[i]]<- p
}
plot_list[[1]]

ggsave(filename = "output/gps_vs_observer_differences.pdf",
       plot = gridExtra::marrangeGrob(plot_list, nrow=1, ncol=1), 
       width = 15, height = 9)

