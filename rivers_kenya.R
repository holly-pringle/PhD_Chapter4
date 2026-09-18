library(sf)
# Load global HydroRIVERS
rivers <- st_read("HydroRIVERS_v10_af.shp")

# Transform cameras back to WGS84 for cropping 
kenya_wgs84 <- st_transform(kenya_cameras_proj, 4326)

# Crop rivers 
aoi_buffer <- st_buffer(st_union(kenya_wgs84), dist = 0.5)
rivers_kenya <- st_crop(rivers, aoi_buffer)

# Free memory
rm(rivers); gc()

# Now reproject both to UTM for accurate distance in metres
rivers_kenya_proj <- st_transform(rivers_kenya, "EPSG:32636")
library(terra)

# Calculate full distance matrix then take minimum per camera
dist_matrix <- terra::distance(vect(kenya_cameras_proj), vect(rivers_kenya_proj), pairwise = FALSE)

# Take the minimum distance (nearest river) for each camera
kenya_cameras_proj$dist_to_water_m <- apply(dist_matrix, 1, min, na.rm = TRUE)

# Check
summary(kenya_cameras_proj$dist_to_water_m)

write.csv(st_drop_geometry(kenya_cameras_proj), "kenya_distances.csv", row.names = FALSE)

