library(terra)
library(sf)

# Load the WSF raster (GeoTIFF)
wsf <- rast("WSF2015_v2_-46_-16.tif")
coords<-read.csv("all_coords.csv")
head(coords)
brazil_coords<-subset(coords, coords$X=="Brazil")
brazil_cameras <- st_as_sf(brazil_coords,
                    coords = c("xcoord", "ycoord"),  
                    crs = 4326)
st_bbox(brazil_cameras)

target_crs <- "EPSG:31983" 
cameras_proj <- st_transform(brazil_cameras, target_crs)
library(sf)

# Convert back to lat/lon for leaflet
cameras_wgs84 <- st_transform(cameras_proj, 4326)
coords <- st_coordinates(cameras_wgs84)

leaflet() |>
  addTiles() |>
  addCircleMarkers(lng = coords[,1], lat = coords[,2])

wsf_proj <- project(wsf, target_crs, method = "near")

library(leaflet)

# Crop to cameras + 20 km buffer 
aoi_buffer <- st_buffer(st_union(cameras_proj), dist = 20000)
wsf_cropped <- crop(wsf_proj, vect(aoi_buffer))

# Settlement mask
wsf_settlement <- wsf_cropped == 255
wsf_settlement[wsf_settlement == 0] <- NA

# Distance raster + extract
dist_raster <- distance(wsf_settlement)
cameras_proj$dist_to_settlement_m <- extract(dist_raster, vect(cameras_proj))[, 2]

# Export
write.csv(st_drop_geometry(cameras_proj), "distances_brazil.csv", row.names = FALSE)
