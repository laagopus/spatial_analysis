#try to prepare the script to extract distance from the nest. 
#I will create random polygon to prepare the script

library(sf)
library(dplyr)
library(sp)

# record the coordinates first polygon 
xs1 <- c(10.58046, 10.75, 10.79, 10.74396, 10.62443, 10.41559, 10.58046)
ys1 <- c(46.7684, 46.77, 46.73095,46.74396,46.68760,46.68195, 46.7684 )
poly_xy <- data.frame(x = xs1, y = ys1, id = "Glorenza")

# make a polygon
poly1 <- poly_xy %>%
  st_as_sf(coords = c("x", "y"), crs = st_crs("+proj=longlat +datum=WGS84 +no_defs")) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

#make a point "nest" inside the polygon 
xs_n1 <- c(10.63614)
ys_n1 <- c(46.74080)
nest_xy <-  data.frame(x = xs_n1, y = ys_n1, id = "Glorenza_nest")

# make a point
nest1 <- nest_xy %>%
  st_as_sf(coords = c("x", "y"), crs = st_crs("+proj=longlat +datum=WGS84 +no_defs")) %>%
  st_cast("POINT")

# visualize
mapview(poly1, color = "black", alpha.regions = 0, lwd = 2, layer.name = unique(poly_xy$id))+ 
  mapview(nest1, color= "red", cex= 5, layer.name="nest")
  
#distance of the nest from the edges
#in teoria è la distanza minima dal territorio chiedi a martina se va bene 
st_geometry(obj = poly1) %>% st_cast(to = 'LINESTRING') %>% st_distance(y = nest1) # 3130.172

#ok now I want to apply this code for mutiple nest and polygons 
#i need to think hoe to write the code in the case i have multiple nest in the same territoty 

#use the same polygon but draw other nest 
#make a point "nest" inside the polygon 
xs_n2 <- c(10.50396)
ys_n2 <- c(46.70003)
nest2_xy <-  data.frame(x = xs_n2, y = ys_n2, id = "Glorenza_nest2")

# make a point
nest2 <- nest2_xy %>%
  st_as_sf(coords = c("x", "y"), crs = st_crs("+proj=longlat +datum=WGS84 +no_defs")) %>%
  st_cast("POINT")

map<- mapview(poly1, color = "black", alpha.regions = 0, lwd = 2, layer.name = unique(poly_xy$id))+ 
  mapview(nest1, color= "red", cex= 5, layer.name="nest_1")+
  mapview(nest2, color= "green", cex= 5, layer.name="nest_2")

st_geometry(obj = poly1) %>% st_cast(to = 'LINESTRING') %>% st_distance(y = nest2) #1738.027

#join my nest in a single object
data_mult_nest<- rbind(nest1, nest2)

# Assuming you have a dataset with multiple points
# Example: data_mult_nest <- st_as_sf(data.frame(x = c(10.6, 10.7), y = c(46.7, 46.8)), coords = c("x", "y"), crs = 4326)

# Convert the polygon to LINESTRING
linestring <- st_geometry(poly1) %>% st_cast(to = 'LINESTRING')

# Calculate the distance from each point to the polygon edges
distances <- st_distance(data_mult_nest, linestring)

# Create a data frame with the distances
distance_df <- data.frame(
  id = data_mult_nest$id,
  distance_to_edge = as.numeric(distances)
)

# Print the distance data frame
print(distance_df)

#ok but now assume I have multiple poplygons and I want to calculate the distance from the edge of points within the different polygons 

library(sf)
library(dplyr)
library(sf)
library(dplyr)

# Sample data: Multiple polygons and points
# Assume poly_list is a list of polygons and point_list is a list of points
# Each point in point_list corresponds to a polygon in poly_list

# Example data (replace with your actual data)
#I draw 2 polygons to try 
poly1 <- st_polygon(list(matrix(c(10.58046, 46.7684,
                                  10.75, 46.77,
                                  10.79, 46.73095,
                                  10.74396, 46.74396,
                                  10.62443, 46.68760,
                                  10.41559, 46.68195,
                                  10.58046, 46.7684), ncol = 2, byrow = TRUE)))
poly2 <- st_polygon(list(matrix(c(10.8, 46.8,
                                  10.9, 46.9,
                                  10.85, 46.85,
                                  10.8, 46.8), ncol = 2, byrow = TRUE)))
poly_list <- st_polygon(list(poly1, poly2))
####################################################################

#####code to took the info from a object like on the top of the code ######
#assuming poly1 ecc are class () -> XY"      "POLYGON" "sfg
data_mult_nest<- rbind(poly1, poly2)
poly_list <- st_polygon(list(poly1, poly2))
############################################################

#change with the info of the nest from data frame 
points1 <- st_sfc(st_point(c(10.6, 46.75)), st_point(c(10.7, 46.7)), crs = 4326)
points2 <- st_sfc(st_point(c(10.85, 46.87)), st_point(c(10.82, 46.82)), crs = 4326)
point_list <- list(points1, points2)
#let's try to bring the coordinate from my original dataframe 
#load my df and make it an sf object 
nest_data<- readRDS("C:/Users/ANNA CENZI/Desktop/Golden_eagle/data/nest_final.rds", refhook = T)
#create a subset with nest_id and territory_id 
sub_nest <- nest_data[, c("individual.local.identifier", "nest_id", "bird_name", "tagging_lat_wgs84", "tagging_long_wgs84", "territory_id")]   #ok

nest_sf<- st_as_sf(sub_nest, coords = c("tagging_long_wgs84", "tagging_lat_wgs84"), crs= 4326) #7 are missing due to no coordinate or info about territory id 

nest_ls <- split(nest_sf, nest_sf$nest_id)
class(nest_ls[[1]])
########################################################
#ok this code should be fine, to be sure I should create many poly as the number of territory 

library(sf)
library(dplyr)

# Load the nest data and make it an sf object
nest_data <- readRDS("C:/Users/ANNA CENZI/Desktop/Golden_eagle/data/nest_final.rds", refhook = T)

# Create a subset with relevant columns
sub_nest <- nest_data[, c("individual.local.identifier", "nest_id", "bird_name", "tagging_lat_wgs84", "tagging_long_wgs84", "territory_id")]

# Convert to sf object
nest_sf <- st_as_sf(sub_nest, coords = c("tagging_long_wgs84", "tagging_lat_wgs84"), crs = 4326)

# Split the nest data by nest_id
nest_ls <- split(nest_sf, nest_sf$nest_id)

# Example polygons (replace with your actual polygons)
poly1 <- st_polygon(list(matrix(c(10.58046, 46.7684,
                                  10.75, 46.77,
                                  10.79, 46.73095,
                                  10.74396, 46.74396,
                                  10.62443, 46.68760,
                                  10.41559, 46.68195,
                                  10.58046, 46.7684), ncol = 2, byrow = TRUE)))
poly2 <- st_polygon(list(matrix(c(10.8, 46.8,
                                  10.9, 46.9,
                                  10.85, 46.85,
                                  10.8, 46.8), ncol = 2, byrow = TRUE)))
poly_list <- list(poly1, poly2)

# Define a common CRS (e.g., EPSG:4326)
common_crs <- 4326

# Ensure all polygons have the same CRS
poly_list <- lapply(poly_list, function(poly) st_sfc(poly, crs = common_crs))

# Function to calculate distances for each polygon and its points
calculate_distances <- function(polygon, points) {
  linestring <- st_cast(polygon, to = 'LINESTRING')
  distances <- st_distance(points, linestring)
  return(distances)
}

# Initialize a list to store results
distance_results <- list()

# Iterate over each polygon
for (i in 1:length(poly_list)) {
  polygon <- poly_list[[i]]
  
  # Iterate over each nest in nest_ls
  for (nest in nest_ls) {
    # Ensure nest has the same CRS
    nest <- st_transform(nest, crs = common_crs)
    
    distances <- calculate_distances(polygon, nest)
    
    # Create a data frame for the current polygon's distances
    distance_df <- data.frame(
      polygon_id = i,
      nest_id = nest$nest_id,
      distance_to_edge = as.numeric(distances)
    )
    
    # Append to the results list
    distance_results <- c(distance_results, list(distance_df))
  }
}

# Combine all results into a single data frame
final_distances <- do.call(rbind, distance_results)

# Print the final distances data frame
print(final_distances)
