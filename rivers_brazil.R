library(sf)
# Load global HydroRIVERS
rivers <- st_read("HydroRIVERS_v10_sa.shp")

# Transform cameras back to WGS84 for cropping 
brazil_wgs84 <- st_transform(brazil_cameras_proj, 4326)

# Crop rivers
aoi_buffer <- st_buffer(st_union(brazil_wgs84), dist = 0.5)
rivers_brazil <- st_crop(rivers, aoi_buffer)

# Free memory
rm(rivers); gc()

# Now reproject both to UTM for accurate distance in metres
st_crs(brazil_cameras_proj)$epsg 

rivers_brazil_proj <- st_transform(rivers_brazil, "EPSG:31983") 

# Calculate full distance matrix then take minimum per camera
dist_matrix <- terra::distance(vect(brazil_cameras_proj), vect(rivers_brazil_proj), pairwise = FALSE)

# Take the minimum distance (nearest river) for each camera
brazil_cameras_proj$dist_to_water_m <- apply(dist_matrix, 1, min, na.rm = TRUE)

# Check
summary(brazil_cameras_proj$dist_to_water_m)

write.csv(st_drop_geometry(brazil_cameras_proj), "brazil_distances_water.csv", row.names = FALSE)

