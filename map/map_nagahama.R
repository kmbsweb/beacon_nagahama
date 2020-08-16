install.packages("geojsonsf")
library(geojsonsf)
library(ggplot2)
library(sf)
library(ggrepel)
library(ggthemes)

#retrieve csv,geojson
place <- read.csv("https://raw.githubusercontent.com/kmbsweb/data_process/master/201905/place_id.csv",
                  header=T, fileEncoding="Shift_JIS",as.is = T)
road <- "https://raw.githubusercontent.com/kmbsweb/data_process/master/201905/roads.json"
rail <- "https://raw.githubusercontent.com/kmbsweb/data_process/master/201905/railways.json"
river <- "https://raw.githubusercontent.com/kmbsweb/data_process/master/201905/waterways.json"

#geojson to sf
road <- geojson_sf(road)
rail <- geojson_sf(rail)
river <- geojson_sf(river)

#plot
ggplot() +
  geom_sf(data=river, color = "LightSteelblue2",size=1.0)+
  geom_sf(data=road, color = "gray",size=0.15) +
  geom_sf(data=rail, color = "black",size=0.3) +
  geom_point(data=place,aes(x=long,y=lat),color="black",size=1.7,shape=15,alpha=1)+
  geom_label_repel(data=place ,aes(x = long, y = lat, label = place_name),size =3,
                  family = "HiraKakuPro-W3", label.size = 0, alpha = 0.7,
                  box.padding = 0.5, point.padding = 0.2) +
  coord_sf(xlim = c(136.25352,136.27893), ylim = c(35.36998,35.38704))+
  theme_map()


#geom_text_repel(aes(x=134.88,y=34.80, label = "Kakogawa river"), col = "navy", size = 2.5, segment.color = NA,alpha=0.9 )
#geom_text_repel(aes(x=134.82,y=34.75, label = "Estimated\n flooding"), col = "darkred", size = 2.5, segment.color = NA,alpha=0.7)


