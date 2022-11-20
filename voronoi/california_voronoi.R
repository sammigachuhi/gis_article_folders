library(dplyr)

california <- map_data("state") %>%
  filter(region == "california")

ncdc.cali <- ncdc_locations %>% 
  filter(state=="CA")

cali_map <- 
  ggplot(data = ncdc.cali, aes(x = long, y = lat)) + 
  scale_fill_gradientn("Elevation", 
                       colors = c("seagreen","darkgreen","green1","yellow","gold4", "sienna"),
                       values = scales::rescale(c(-60,0,1000,2000,3000,4000))) + 
  scale_color_gradientn("Elevation", 
                        colours = c("seagreen","darkgreen","green1","yellow","gold4", "sienna"), 
                        values = scales::rescale(c(-60,0,1000,2000,3000,4000))) + 
  coord_quickmap() +
  theme_minimal() + 
  theme(axis.text = element_blank(), 
        axis.title = element_blank())

cali_map + 
  geom_point(aes(color = elev), size = .01) + 
  geom_path(data = california, aes(long, lat, group = group), color = "black")

cali_map + 
  geom_voronoi(aes(fill=elev), outline = california)

california <- map_data("state") %>% 
  filter(region == "california")

ncdc.cali <- ncdc_locations %>% 
  filter(state == 'CA')

cali.voronoi <- voronoi_polygon(data = ncdc.cali, 
                                x = "long", y = "lat", 
                                outline = california)

cali.voronoi <- fortify_voronoi(cali.voronoi)

ggplot(cali.voronoi) + 
  geom_polygon(aes(x = long.x, y = lat.x, fill = elev, 
                   group = group, color = elev), size = 0) + 
  scale_fill_gradientn("Elevation", 
                       colors = c("seagreen","darkgreen","green1","yellow","gold4", "sienna"), 
                       values = scales::rescale(c(-60,0,1000,2000,3000,4000))) + 
  scale_color_gradientn("Elevation", 
                        colours = c("seagreen","darkgreen","green1","yellow","gold4", "sienna"), 
                        values = scales::rescale(c(-60,0,1000,2000,3000,4000))) + 
  coord_quickmap() + 
  theme_minimal() + 
  theme(axis.text = element_blank(), 
        axis.title = element_blank())





















