library(sf)
# Load global HydroRIVERS
rivers <- st_read("HydroRIVERS_v10_as.shp")

# Transform cameras back to WGS84 for cropping 
nepal_wgs84 <- st_transform(nepal_cameras_proj, 4326)

# Crop rivers 
aoi_buffer <- st_buffer(st_union(nepal_wgs84), dist = 0.5)
rivers_nepal <- st_crop(rivers, aoi_buffer)

# Free memory
rm(rivers); gc()
st_crs(nepal_cameras_proj)$epsg  # should be 32644
# Now reproject both to UTM for accurate distance in metres
rivers_nepal_proj <- st_transform(rivers_nepal, "EPSG:32644")  

# Calculate full distance matrix then take minimum per camera
dist_matrix <- terra::distance(vect(nepal_cameras_proj), vect(rivers_nepal_proj), pairwise = FALSE)

# Take the minimum distance (nearest river) for each camera
nepal_cameras_proj$dist_to_water_m <- apply(dist_matrix, 1, min, na.rm = TRUE)

# Check
summary(nepal_cameras_proj$dist_to_water_m)

write.csv(st_drop_geometry(nepal_cameras_proj), "nepal_distances_water.csv", row.names = FALSE)

