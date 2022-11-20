install.packages("ggvoronoi")

library(ggvoronoi)
set.seed(45056)

x <- sample(1:200, 100)
y <- sample(1:200, 100)

points <- data.frame(x, y, 
                     distance = sqrt((x-100)^2 + (y - 100)^2))


circle <- data.frame(x = 100*(1 + cos(seq(0, 2*pi, length.out = 2500))), 
                     y = 100*(1 + sin(seq(0, 2*pi, length.out = 2500))), 
                     group = rep(1, 2500))

library(ggplot2)

ggplot(points) + 
  geom_point(aes(x, y, color=distance)) + 
  geom_path(data = circle, aes(x,y, group=group))

ggplot(points) + 
  geom_voronoi(aes(x, y, fill=distance))

ggplot(points, aes(x, y)) + 
  stat_voronoi(geom="path") + 
  geom_point()

ggplot(data = points, aes(x=x, y=y, fill=distance)) + 
  geom_voronoi(outline = circle)

ggplot(points, aes(x, y)) + 
  geom_voronoi(aes(fill=distance), outline = circle, 
               color = "#4dffb8", size = .125) + 
  scale_fill_gradient(low = "#4dffb8", high = "black", guide = F) + 
  theme_void() + 
  coord_fixed()

library(ggmap)
oxford_map <- get_stamenmap(bbox = c(-83.7398373, 39.007306, 84.0, 40.0), zoom = 5, maptype = "toner")

bounds <- as.numeric(attr(oxford_map, "bb"))

map <- ggplot(data = oxford_bikes, aes(x, y)) + 
  geom_blank() + 
  inset_ggmap(oxford_map) + 
  xlim(-85, -84) + ylim(39, 40) +
  coord_map(ylim = bounds[c(1, 3)], xlim = bounds[c(2, 4)]) + 
  theme_minimal() +
  theme(axis.text = element_blank(), 
        axis.title = element_blank())

map + geom_path(stat = "voronoi", alpha = .085, size = .25) + 
  geom_point(color = "blue", size = .25)

ox_diagram <- voronoi_polygon(oxford_bikes, x = "x", y = "y")

library(sp)

mac_joes <- SpatialPointsDataFrame(cbind(long = -84.7418, lat = 39.5101), 
                                   data = data.frame(name = "Mac & Joes"))

mac_joes %over% ox_diagram

map + geom_path(data = fortify_voronoi(ox_diagram), aes(x, y, group = group), alpha = .1, size = 1) + 
  coord_map(xlim = c(-84.746, -84.739), ylim = c(39.508, 39.514)) + 
  geom_point(data = data.frame(mac_joes), aes(long, lat), color = "red", size = 2) + 
  geom_point(size = 1.5, stroke = 1, shape = 21, color = "black", fill = "white") + 
  geom_point(data = mac_joes %over% ox_diagram, aes(x, y), color = "blue", size = 2)






