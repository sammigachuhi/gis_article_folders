# Load the usual geospatial packages
library(sp)
library(sf)
library(rgdal)
library(rgeos)

library(raster)

kenya_srtm <- raster("E:/downloads/Dukto/kesrtm/ke_srtm/ke_srtm/hdr.adf", band = 1, values = T)

# Checkout the raster
plot(kenya_srtm)
kenya_srtm

# Generate random points which will have coordinates
# Follow solution here: https://gis.stackexchange.com/questions/291446/generating-random-points-inside-raster-boundary-using-r

lin <- rasterToContour(is.na(kenya_srtm))
plot(lin)
head(lin)

pol <- as(st_union(st_polygonize(st_as_sf(lin))), 'Spatial') # st_union to dissolve geometries

kenya_admin <- readOGR("E:/downloads/Dukto/counties_shapefile/kenya_counties/ken_admbnda_adm0_iebc_20191031.shp")

pts <- spsample(kenya_admin, 200, type = "random")

head(pts@coords)
head(pts@data)

pts2 <- data.frame(pts@coords)
head(pts2)

# Convert dataframe to spatial points dataframe
## Get long and lat from data.frame object
xy <- pts2[, c(1, 2)]

pts_spdf <- SpatialPointsDataFrame(xy, 
                                   data = pts2, 
                                   proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
plot(pts_spdf)


library(mapview)

mapview(pts_spdf)

# Extract raster value by points
ras_points <- extract(kenya_srtm, pts_spdf, method = "bilinear", fun = mean, buffer = NULL, cellnumbers = F, df = T)
summary(ras_points)

# Combine raster values with point data
ras_points_combined <- cbind(pts_spdf, ras_points)

head(ras_points_combined)
summary(ras_points_combined)
names(ras_points_combined)
#ras_points_combined2 <- data.frame(ras_points_combined)
# names(ras_points_combined2)
names(ras_points_combined)[4] <- "values"
names(ras_points_combined)
head(ras_points_combined)
ras_points_combined2 <- data.frame(ras_points_combined)
head(ras_points_combined2)

# Plot a map showing the values of our combined points
library(ggplot2)
library(ggmap)


elev_map <- ggplot(data = ras_points_combined2, aes(x, y)) + 
  geom_point(aes(color = values), size = 1) + 
  scale_color_gradientn(colours = terrain.colors(10)) +
  coord_sf(default_crs = sf::st_crs(4326))

elev_map

# Create voronoi polygons
library(ggvoronoi)

elev_map + 
  geom_voronoi(aes(fill = values), 
               outline = kenya_admin) +
  scale_fill_gradientn(colors = terrain.colors(10)) + 
  labs(title = "Voronoi polygon of Kenya's Altitude")


# With polygon lines
elev_map + 
  geom_voronoi(aes(fill = values),  color = "gray85", linewidth = 0.01,
               outline = kenya_admin) +
  scale_fill_gradientn(colors = terrain.colors(10))


# try out 3d surface modeling
elev_map_3d <- elev_map + 
  geom_voronoi(aes(fill = values), 
               outline = kenya_admin) +
  scale_fill_gradientn(colors = terrain.colors(10)) + 
  labs(title = "Voronoi polygon of Kenya's Altitude")

install.packages("rasterVis")

library(rasterVis)
plot3D(elev_map_3d)


