library(threejs)

# Correctly named columns
flows <- data.frame(
  originLat = c(48.8566, 51.5074, 40.7128),      # Paris, London, New York
  originLon = c(2.3522, -0.1278, -74.0060),
  destinationLat = c(35.6895, 39.9042, 55.7558), # Tokyo, Beijing, Moscow
  destinationLon = c(139.6917, 116.4074, 37.6173)
  #value = c(10, 20, 15)
)

# Interactive globe with arcs

globejs(
  arcs = flows,
  arcsHeight = 0.4,
  arcsColor=c("#FF5733","#FF5733", "#116600"),
  #arcsColor = "#FF5733",
  arcsOpacity = 0.7,
  arcsStroke = 0.5,
  atmosphere = TRUE,
  color = "#100080"
  #color= c(10, 20, 15)
)


library(threejs)
library(maps)
data(world.cities, package="maps")
cities <- world.cities[order(world.cities$pop, decreasing=TRUE)[1:1000],]
value  <- 100 * cities$pop / max(cities$pop)
col <- colorRampPalette(c("cyan", "lightgreen"))(10)[floor(10 * value/100) + 1]
globejs(lat=cities$lat, long=cities$long, value=value, color=col, atmosphere=TRUE)

# Plot the data on the moon:
moon <- system.file("images/moon.jpg", package="threejs")
globejs(img=moon, bodycolor="#555555", lightcolor="#aaaaaa",
        lat=cities$lat, long=cities$long,
        value=value, color=col)





# http://callumprentice.github.io/apps/flight_stream/index.html
f <- flights()
# Approximate locations as factors
dest   <- factor(sprintf("%.2f:%.2f", f[,3], f[,4]))
# A table of destination frequencies
freq <- sort(table(dest), decreasing=TRUE)
# The most frequent destinations in these data, possibly hub airports?
frequent_destinations <- names(freq)[1:10]
# Subset the flight data by destination frequency
idx <- dest %in% frequent_destinations
frequent_flights <- f[idx, ]
# Lat/long and counts of frequent flights
ll <- unique(frequent_flights[, 3:4])
# Plot frequent destinations as bars, and the flights to and from
# them as arcs. Adjust arc width and color by frequency.
globejs(lat=ll[, 1], long=ll[, 2], arcs=frequent_flights,
        bodycolor="#aaaaff", arcsHeight=0.3, arcsLwd=2,
        arcsColor="#ffff00", arcsOpacity=0.15,
        atmosphere=TRUE, color="#00aaff", pointsize=0.5)




globejs( arcs=flows,
        bodycolor="#aaaaff", arcsHeight=0.3, arcsLwd=2,
        arcsColor="#ffff00", arcsOpacity=0.15,
        atmosphere=TRUE, color="#00aaff", pointsize=0.5)


##############################################################################



#install.packages("mapdeck")
library(mapdeck)

set_token("pk.eyJ1IjoibW9uZHBhbnRoZXIiLCJhIjoiY21oMmFsYWRsMGVkMTJuczlpeTM5cXNqbyJ9.ysoFayPMjYRP43wTyFb75Q")  # You need a Mapbox token

# Sample data
flows <- data.frame(
  originLon = c(2.3522, -0.1278, -74.0060),
  originLat = c(48.8566, 51.5074, 40.7128),
  destinationLon = c(139.6917, 116.4074, 37.6173),
  destinationLat = c(35.6895, 39.9042, 55.7558)
)

# Create lines with direction
flows$geometry <- mapply(function(lon1, lat1, lon2, lat2) {
  sf::st_linestring(matrix(c(lon1, lon2, lat1, lat2), ncol = 2))
}, flows$originLon, flows$originLat, flows$destinationLon, flows$destinationLat, SIMPLIFY = FALSE)

flows_sf <- sf::st_sf(geometry = sf::st_sfc(flows$geometry))

# Plot with mapdeck
mapdeck(style = mapdeck_style("dark")) %>%
  add_path(data = flows_sf, stroke_colour = "#FF5733", stroke_width = 3)




#############################################################

#long & lats



# Install required packages
#install.packages("rnaturalearth")
#install.packages("rnaturalearthdata")
#install.packages("countrycode")
#install.packages("dplyr")

# Load libraries
library(rnaturalearth)
library(countrycode)
library(dplyr)

# Get country data
countries <- ne_countries(scale = "medium", returnclass = "sf")
names(countries)
# Extract relevant fields
capital_data <- countries %>%
  transmute(
    country = name,
    iso2 = countrycode(name, origin = "country.name", destination = "iso2c"),
    capital = capital,
    latitude = latitude,
    longitude = longitude
  ) %>%
  filter(!is.na(capital) & !is.na(latitude) & !is.na(longitude))

# View the result
print(capital_data)




# Install and load the package
install.packages("CoordinateCleaner")
library(CoordinateCleaner)

# Load the built-in dataset
data("countryref")

# Extract relevant columns
capital_df <- countryref[, c("name", "iso2", "capital", "capital.lat", "capital.lon")]

# Rename columns for clarity
colnames(capital_df) <- c("country", "iso2", "capital", "latitude", "longitude")

# View the result
head(capital_df)





#######


# Install required packages
#install.packages(c("sf", "rnaturalearth", "rnaturalearthdata", "spatstat.geom", "spatstat.random"))

# Install required packages
install.packages(c("sf", "rnaturalearth", "rnaturalearthdata", "spatstat.geom", "spatstat.random", "dplyr"))

# Load libraries
library(sf)
library(rnaturalearth)
library(dplyr)
library(spatstat.geom)
library(spatstat.random)

# Load world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Transform to projected CRS (e.g., Robinson projection)
world_proj <- st_transform(world, crs = "+proj=robin")

# Function to generate random points within a polygon
generate_points_in_polygon <- function(polygon, n = 5) {
  win <- as.owin(polygon)
  points <- runifpoint(n, win)
  coords <- as.data.frame(points)
  return(coords)
}

# Generate random points for each country
results <- list()

for (i in 1:nrow(world_proj)) {
  country <- world_proj[i, ]
  iso2 <- country$iso_a2
  name <- country$name
  capital <- country$capital
  centroid <- st_centroid(country$geometry)
  capital_coords <- st_coordinates(centroid)
  
  # Generate random points
  try({
    pts <- generate_points_in_polygon(country$geometry, n = 5)
    pts$country <- name
    pts$iso2 <- iso2
    pts$capital <- capital
    pts$capital_lat <- capital_coords[2]
    pts$capital_lon <- capital_coords[1]
    results[[length(results) + 1]] <- pts
  }, silent = TRUE)
}

# Combine all results
df <- bind_rows(results)

# View sample
#head(random_points_df)



#################################

# Required packages
#install.packages(c("sf", "rnaturalearth", "rnaturalearthdata", "dplyr"))
library(sf)
library(rnaturalearth)
library(dplyr)

# Load world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Transform to projected CRS for accurate distance calculations
world_proj <- st_transform(world, crs = "+proj=robin")

# Function to generate random points within the polygon containing the capital
generate_points_near_capital <- function(country_geom, capital_point, n = 5, buffer_km = 300) {
  # Buffer around capital
  buffer <- st_buffer(capital_point, dist = buffer_km * 1000)  # convert km to meters
  
  # Intersect buffer with country geometry
  target_area <- st_intersection(country_geom, buffer)
  
  # Generate random points within the target area
  bbox <- st_bbox(target_area)
  points <- list()
  attempts <- 0
  
  while (length(points) < n && attempts < 1000) {
    lon <- runif(1, bbox["xmin"], bbox["xmax"])
    lat <- runif(1, bbox["ymin"], bbox["ymax"])
    pt <- st_point(c(lon, lat)) %>% st_sfc(crs = st_crs(world_proj))
    if (st_intersects(pt, target_area, sparse = FALSE)[1]) {
      coords <- st_coordinates(pt)
      points[[length(points) + 1]] <- data.frame(x = coords[1], y = coords[2])
    }
    attempts <- attempts + 1
  }
  
  do.call(rbind, points)
}

# Generate points for each country
results <- list()

for (i in 1:nrow(world_proj)) {
  country <- world_proj[i, ]
  iso2 <- country$iso_a2
  name <- country$name
  capital <- country$capital
  
  # Get capital point
  capital_point <- st_centroid(country$geometry)
  
  # Generate points near capital
  try({
    pts <- generate_points_near_capital(country$geometry, capital_point, n = 5)
    pts$country <- name
    pts$iso2 <- iso2
    pts$capital <- capital
    pts$capital_lat <- st_coordinates(capital_point)[2]
    pts$capital_lon <- st_coordinates(capital_point)[1]
    results[[length(results) + 1]] <- pts
  }, silent = TRUE)
}

# Combine results
df <- bind_rows(results)

# View sample
head(df)



globejs(lat=df$capital_lat, long=df$capital_lon, value=0, color="#119900", atmosphere=TRUE)





#### visuals


# Required packages
install.packages(c("sf", "rnaturalearth", "rnaturalearthdata", "ggplot2", "dplyr"))
library(sf)
library(rnaturalearth)
library(dplyr)
library(ggplot2)

# Load world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Transform to projected CRS for accurate buffering
world_proj <- st_transform(world, crs = "+proj=robin")

# Generate capital points
world_proj$capital_point <- st_centroid(world_proj$geometry)

# Function to generate random points near capital within country polygon
generate_points_near_capital <- function(country_geom, capital_point, n = 5, buffer_km = 300) {
  buffer <- st_buffer(capital_point, dist = buffer_km * 1000)
  target_area <- st_intersection(country_geom, buffer)
  bbox <- st_bbox(target_area)
  points <- data.frame(x = numeric(0), y = numeric(0))
  attempts <- 0
  
  while (nrow(points) < n && attempts < 1000) {
    lon <- runif(1, bbox["xmin"], bbox["xmax"])
    lat <- runif(1, bbox["ymin"], bbox["ymax"])
    pt <- st_point(c(lon, lat)) %>% st_sfc(crs = st_crs(world_proj))
    if (st_intersects(pt, target_area, sparse = FALSE)[1]) {
      coords <- st_coordinates(pt)
      points <- rbind(points, data.frame(x = coords[1], y = coords[2]))
    }
    attempts <- attempts + 1
  }
  
  return(points)
}





# Generate points
all_points <- list()
for (i in 1:nrow(world_proj)) {
  country <- world_proj[i, ]
  try({
    pts <- generate_points_near_capital(country$geometry, country$capital_point, n = 5)
    all_points[[length(all_points) + 1]] <- pts
  }, silent = TRUE)
}


# Combine all points into a single data frame
points_df <- bind_rows(all_points)

# Convert to sf object using coordinates
points_sf <- st_as_sf(points_df, coords = c("x", "y"), crs = st_crs(world_proj))



# Plot
ggplot() +
  geom_sf(data = world_proj, fill = "gray90", color = "white") +
  geom_sf(data = points_df, color = "red", size = 1) +
  labs(title = "Random Points Near Capital Cities on World Map") +
  theme_minimal()





################


# Install required packages
#install.packages(c( "ggspatial"))

# Load libraries
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)

# Load world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Sample flight paths: origin and destination coordinates
flights <- data.frame(
  from_lat = c(40.7128, 35.6895, 48.8566),  # New York, Tokyo, Paris
  from_lon = c(-74.0060, 139.6917, 2.3522),
  to_lat = c(51.5074, 37.7749, 55.7558),    # London, San Francisco, Moscow
  to_lon = c(-0.1278, -122.4194, 37.6173)
)

# Convert to sf lines
flight_lines <- do.call(rbind, lapply(1:nrow(flights), function(i) {
  st_linestring(matrix(c(flights$from_lon[i], flights$to_lon[i],
                         flights$from_lat[i], flights$to_lat[i]), ncol = 2))
})) %>%
  st_sfc(crs = 4326) %>%
  st_sf(geometry = .)

# Plot with arrows
ggplot(data = world) +
  geom_sf(fill = "antiquewhite") +
  geom_sf(data = flight_lines, color = "red", size = 1,
          arrow = arrow(type = "closed", length = unit(0.2, "inches"))) +
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90), expand = FALSE) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "lightblue"),
        panel.grid.major = element_line(color = "white")) +
  labs(title = "Directed Flight Paths on World Map")






###############

library(sf)

# Sample flight paths
flights <- data.frame(
  from_lat = c(40.7128, 35.6895, 48.8566),
  from_lon = c(-74.0060, 139.6917, 2.3522),
  to_lat = c(51.5074, 37.7749, 55.7558),
  to_lon = c(-0.1278, -122.4194, 37.6173)
)

# Create sf lines
flight_lines <- lapply(1:nrow(flights), function(i) {
  coords <- matrix(c(
    flights$from_lon[i], flights$from_lat[i],
    flights$to_lon[i],   flights$to_lat[i]
  ), ncol = 2, byrow = TRUE)
  st_linestring(coords)
})

# Convert to sf object
flight_sf <- st_sf(geometry = st_sfc(flight_lines, crs = 4326))

# View result
print(flight_sf)




# Plot with arrows
ggplot(data = world) +
  geom_sf(fill = "antiquewhite") +
  geom_sf(data = flight_sf, color = "red", size = 1,
          arrow = arrow(type = "closed", length = unit(0.2, "inches"))) +
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90), expand = FALSE) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "lightblue"),
        panel.grid.major = element_line(color = "white")) +
  labs(title = "Directed Flight Paths on World Map")




##############################################


library(leaflet)
library(dplyr)
library(geosphere)

# Function to add arrow decorator to polylines
add_arrow_decorator <- function(map, route_coords, color = "#3498db", 
                                weight = 2, opacity = 0.6, label = NULL) {
  # Add the main polyline
  map <- map %>%
    addPolylines(
      lng = route_coords[, 1],
      lat = route_coords[, 2],
      color = color,
      weight = weight,
      opacity = opacity,
      label = label
    )
  
  # Calculate arrow position (near the end of the line)
  n_points <- nrow(route_coords)
  arrow_pos <- round(n_points * 0.85)  # Position at 85% along the route
  
  if (arrow_pos < n_points - 5) {
    # Get direction vector for arrow
    p1 <- route_coords[arrow_pos, ]
    p2 <- route_coords[arrow_pos + 5, ]
    
    # Calculate arrow head points
    dx <- p2[1] - p1[1]
    dy <- p2[2] - p1[2]
    angle <- atan2(dy, dx)
    
    # Arrow parameters
    arrow_length <- 1.5  # degrees
    arrow_angle <- 25 * pi / 180  # 25 degrees
    
    # Calculate arrow head points
    arrow_left <- c(
      p2[1] - arrow_length * cos(angle - arrow_angle),
      p2[2] - arrow_length * sin(angle - arrow_angle)
    )
    arrow_right <- c(
      p2[1] - arrow_length * cos(angle + arrow_angle),
      p2[2] - arrow_length * sin(angle + arrow_angle)
    )
    
    # Draw arrow head as two lines forming a ">"
    map <- map %>%
      addPolylines(
        lng = c(arrow_left[1], p2[1], arrow_right[1]),
        lat = c(arrow_left[2], p2[2], arrow_right[2]),
        color = color,
        weight = weight + 1,
        opacity = opacity
      )
  }
  
  return(map)
}

# Example 1: Basic Flight Network
# ================================

# Define some major cities with coordinates
cities <- data.frame(
  city = c("New York", "London", "Tokyo", "Sydney", "Dubai", "Singapore"),
  lat = c(40.7128, 51.5074, 35.6762, -33.8688, 25.2048, 1.3521),
  lon = c(-74.0060, -0.1278, 139.6503, 151.2093, 55.2708, 103.8198)
)

# Define flight connections (edges)
connections <- data.frame(
  from = c("New York", "New York", "London", "London", "Tokyo", "Dubai", "Singapore"),
  to = c("London", "Dubai", "Tokyo", "Dubai", "Sydney", "Singapore", "Sydney")
)

# Create the base map
map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = 0, lat = 20, zoom = 2)

# Add city markers
map <- map %>%
  addCircleMarkers(
    data = cities,
    lng = ~lon,
    lat = ~lat,
    radius = 6,
    color = "#e74c3c",
    fillColor = "#e74c3c",
    fillOpacity = 0.8,
    popup = ~city,
    label = ~city
  )

# Function to create great circle routes with arrows
add_flight_routes <- function(map, cities, connections) {
  for (i in 1:nrow(connections)) {
    # Get coordinates for origin and destination
    from_city <- cities[cities$city == connections$from[i], ]
    to_city <- cities[cities$city == connections$to[i], ]
    
    # Calculate great circle route
    route <- gcIntermediate(
      c(from_city$lon, from_city$lat),
      c(to_city$lon, to_city$lat),
      n = 100,
      addStartEnd = TRUE,
      sp = FALSE
    )
    
    # Add polyline with arrow to map
    map <- add_arrow_decorator(
      map,
      route,
      color = "#3498db",
      weight = 2,
      opacity = 0.6,
      label = paste(connections$from[i], "→", connections$to[i])
    )
  }
  return(map)
}

# Add flight routes to the map
map <- add_flight_routes(map, cities, connections)

# Display the map
print(map)



############


# Random World City Network with 200 Cities and 600 Connections
# Required packages: leaflet, dplyr, geosphere, maps

# Install packages if needed
# install.packages(c("leaflet", "dplyr", "geosphere", "maps"))

library(leaflet)
library(dplyr)
library(geosphere)
library(maps)

set.seed(123)  # For reproducibility - remove or change for different random networks

# Step 1: Get world cities data
# ================================

# Load world cities from the maps package
data(world.cities)

# Filter for larger cities and sample 200 random cities
# Using cities with population > 100,000 for better distribution
major_cities <- world.cities %>%
  filter(pop > 100000) %>%
  sample_n(200) %>%
  select(name, country.etc, lat, long, pop) %>%
  mutate(city_id = 1:n(),
         city_label = paste0(name, ", ", country.etc))

head(major_cities)
cat("\nSelected", nrow(major_cities), "cities\n")

# Step 2: Generate 600 random directed connections
# =================================================

# Sample connections with replacement (allows multiple connections between same cities)
connections <- data.frame(
  from_id = sample(major_cities$city_id, 600, replace = TRUE),
  to_id = sample(major_cities$city_id, 600, replace = TRUE)
) %>%
  # Remove self-loops (city to itself)
  filter(from_id != to_id) %>%
  mutate(connection_id = 1:n())

cat("Generated", nrow(connections), "connections (after removing self-loops)\n")

# Count connections between same city pairs
connection_counts <- connections %>%
  mutate(pair = paste(pmin(from_id, to_id), pmax(from_id, to_id), sep = "-"),
         direction = ifelse(from_id < to_id, "forward", "backward")) %>%
  group_by(from_id, to_id) %>%
  mutate(
    pair_count = n(),
    pair_index = row_number()
  ) %>%
  ungroup()

# Show some statistics
cat("\nConnection Statistics:\n")
cat("Total connections:", nrow(connection_counts), "\n")
cat("Unique city pairs:", length(unique(connection_counts$pair)), "\n")
cat("Max connections between same pair:", max(connection_counts$pair_count), "\n")

# Cities with most outgoing connections
top_hubs <- connection_counts %>%
  group_by(from_id) %>%
  summarise(outgoing = n()) %>%
  arrange(desc(outgoing)) %>%
  head(5) %>%
  left_join(major_cities, by = c("from_id" = "city_id"))

cat("\nTop 5 hub cities (by outgoing connections):\n")
print(top_hubs %>% select(city_label, outgoing))


# Step 3: Function to offset parallel routes
# ===========================================

# This function creates slightly offset routes for parallel connections
create_offset_route <- function(from_lon, from_lat, to_lon, to_lat, 
                                offset_factor = 0, n_points = 100) {
  
  # Get the great circle route
  route <- gcIntermediate(
    c(from_lon, from_lat),
    c(to_lon, to_lat),
    n = n_points,
    addStartEnd = TRUE,
    sp = FALSE
  )
  
  # If no offset needed, return original route
  if (offset_factor == 0) {
    return(route)
  }
  
  # Calculate perpendicular offset
  # Find midpoint
  mid_idx <- round(nrow(route) / 2)
  
  # Calculate bearing at midpoint
  if (mid_idx < nrow(route)) {
    bearing <- bearing(route[mid_idx, ], route[mid_idx + 1, ])
    
    # Perpendicular bearing (90 degrees offset)
    perp_bearing <- (bearing + 90) %% 360
    
    # Calculate offset distance (in meters, then convert to rough degrees)
    # Base offset of ~100km per offset level
    offset_distance <- offset_factor * 100000  # meters
    
    # Apply offset to middle portion of route (40% to 60% of route)
    start_offset <- round(nrow(route) * 0.4)
    end_offset <- round(nrow(route) * 0.6)
    
    for (i in start_offset:end_offset) {
      # Gradual offset (more in middle, less at edges of offset region)
      offset_proportion <- 1 - abs(i - mid_idx) / (end_offset - start_offset)
      current_offset <- offset_distance * offset_proportion
      
      offset_point <- destPoint(
        route[i, ],
        perp_bearing,
        current_offset
      )
      route[i, ] <- offset_point
    }
  }
  
  return(route)
}


# Step 4: Function to add arrow decorator
# ========================================

add_arrow_decorator <- function(map, route_coords, color = "#3498db", 
                                weight = 1.5, opacity = 0.5, label = NULL,
                                group = NULL) {
  # Add the main polyline
  map <- map %>%
    addPolylines(
      lng = route_coords[, 1],
      lat = route_coords[, 2],
      color = color,
      weight = weight,
      opacity = opacity,
      label = label,
      group = group
    )
  
  # Calculate arrow position (near the end of the line)
  n_points <- nrow(route_coords)
  arrow_pos <- round(n_points * 0.85)
  
  if (arrow_pos < n_points - 5) {
    # Get direction vector for arrow
    p1 <- route_coords[arrow_pos, ]
    p2 <- route_coords[arrow_pos + 5, ]
    
    # Calculate arrow head points
    dx <- p2[1] - p1[1]
    dy <- p2[2] - p1[2]
    angle <- atan2(dy, dx)
    
    # Arrow parameters (smaller for dense networks)
    arrow_length <- 0.8
    arrow_angle <- 25 * pi / 180
    
    # Calculate arrow head points
    arrow_left <- c(
      p2[1] - arrow_length * cos(angle - arrow_angle),
      p2[2] - arrow_length * sin(angle - arrow_angle)
    )
    arrow_right <- c(
      p2[1] - arrow_length * cos(angle + arrow_angle),
      p2[2] - arrow_length * sin(angle + arrow_angle)
    )
    
    # Draw arrow head
    map <- map %>%
      addPolylines(
        lng = c(arrow_left[1], p2[1], arrow_right[1]),
        lat = c(arrow_left[2], p2[2], arrow_right[2]),
        color = color,
        weight = weight + 0.5,
        opacity = opacity,
        group = group
      )
  }
  
  return(map)
}


# Step 5: Create the network map
# ===============================

cat("\nCreating map... (this may take a minute with 600+ connections)\n")

# Create base map
network_map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  setView(lng = 0, lat = 20, zoom = 2)

# Add city markers
network_map <- network_map %>%
  addCircleMarkers(
    data = major_cities,
    lng = ~long,
    lat = ~lat,
    radius = 3,
    color = "#f39c12",
    fillColor = "#f39c12",
    fillOpacity = 0.8,
    stroke = TRUE,
    weight = 1,
    popup = ~paste0("<b>", city_label, "</b><br>",
                    "Population: ", format(pop, big.mark = ",")),
    group = "Cities"
  )

# Add all connections with offsets for parallel routes
# Color palette for connections
connection_colors <- c("#3498db", "#e74c3c", "#2ecc71", "#9b59b6", "#f39c12")

for (i in 1:nrow(connection_counts)) {
  # Get from and to cities
  from_city <- major_cities[major_cities$city_id == connection_counts$from_id[i], ]
  to_city <- major_cities[major_cities$city_id == connection_counts$to_id[i], ]
  
  # Calculate offset based on how many connections exist between this pair
  # If there are multiple connections, offset them
  if (connection_counts$pair_count[i] > 1) {
    # Offset alternates sides and increases with each parallel connection
    offset_magnitude <- ceiling(connection_counts$pair_index[i] / 2) * 0.3
    offset_sign <- ifelse(connection_counts$pair_index[i] %% 2 == 0, 1, -1)
    offset_factor <- offset_magnitude * offset_sign
  } else {
    offset_factor <- 0
  }
  
  # Create route with offset
  route <- create_offset_route(
    from_city$long, from_city$lat,
    to_city$long, to_city$lat,
    offset_factor = offset_factor,
    n_points = 100
  )
  
  # Choose color (cycle through palette)
  color_idx <- (connection_counts$from_id[i] %% length(connection_colors)) + 1
  route_color <- connection_colors[color_idx]
  
  # Add route with arrow
  network_map <- add_arrow_decorator(
    network_map,
    route,
    color = route_color,
    weight = 1.5,
    opacity = 0.4,
    label = paste(from_city$city_label, "→", to_city$city_label),
    group = "Connections"
  )
  
  # Progress indicator
  if (i %% 100 == 0) {
    cat("  Processed", i, "of", nrow(connection_counts), "connections\n")
  }
}

cat("\nMap creation complete!\n")

# Add layer controls
network_map <- network_map %>%
  addLayersControl(
    overlayGroups = c("Cities", "Connections"),
    options = layersControlOptions(collapsed = FALSE)
  )

# Display the map
print(network_map)

# Step 6: Save the map
# ====================

# Save as HTML file
library(htmlwidgets)
saveWidget(network_map, "/mnt/user-data/outputs/random_world_network.html", 
           selfcontained = TRUE)
cat("\nMap saved to: random_world_network.html\n")


# Step 7: Network Statistics
# ===========================

cat("\n=== NETWORK STATISTICS ===\n")
cat("Total cities:", nrow(major_cities), "\n")
cat("Total connections:", nrow(connection_counts), "\n")
cat("Average connections per city:", 
    round(nrow(connection_counts) / nrow(major_cities), 2), "\n")

# Degree distribution
in_degree <- connection_counts %>% 
  group_by(to_id) %>% 
  summarise(in_deg = n())
out_degree <- connection_counts %>% 
  group_by(from_id) %>% 
  summarise(out_deg = n())

cat("Average in-degree:", round(mean(in_degree$in_deg), 2), "\n")
cat("Average out-degree:", round(mean(out_degree$out_deg), 2), "\n")

# Parallel connections statistics
parallel_stats <- connection_counts %>%
  filter(pair_count > 1) %>%
  group_by(pair) %>%
  summarise(count = n()) %>%
  group_by(count) %>%
  summarise(num_pairs = n())

cat("\nParallel connection distribution:\n")
print(parallel_stats)

# Show some examples of cities with parallel connections
example_parallel <- connection_counts %>%
  filter(pair_count > 2) %>%
  head(3) %>%
  left_join(major_cities, by = c("from_id" = "city_id")) %>%
  left_join(major_cities, by = c("to_id" = "city_id"), suffix = c("_from", "_to"))

if (nrow(example_parallel) > 0) {
  cat("\nExample of parallel connections:\n")
  print(example_parallel %>% 
          select(city_label_from, city_label_to, pair_count, pair_index))
}


# Optional: Create a simplified version with fewer connections for testing
# =========================================================================

create_smaller_network <- function(n_cities = 50, n_connections = 150) {
  cat("\n=== Creating smaller test network ===\n")
  
  # Sample cities
  small_cities <- world.cities %>%
    filter(pop > 100000) %>%
    sample_n(n_cities) %>%
    select(name, country.etc, lat, long, pop) %>%
    mutate(city_id = 1:n(),
           city_label = paste0(name, ", ", country.etc))
  
  # Generate connections
  small_connections <- data.frame(
    from_id = sample(small_cities$city_id, n_connections, replace = TRUE),
    to_id = sample(small_cities$city_id, n_connections, replace = TRUE)
  ) %>%
    filter(from_id != to_id) %>%
    mutate(connection_id = 1:n()) %>%
    group_by(from_id, to_id) %>%
    mutate(
      pair_count = n(),
      pair_index = row_number()
    ) %>%
    ungroup()
  
  # Create map
  small_map <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    setView(lng = 0, lat = 20, zoom = 2)
  
  # Add cities
  small_map <- small_map %>%
    addCircleMarkers(
      data = small_cities,
      lng = ~long,
      lat = ~lat,
      radius = 5,
      color = "#e74c3c",
      fillColor = "#e74c3c",
      fillOpacity = 0.8,
      popup = ~city_label
    )
  
  # Add connections
  for (i in 1:nrow(small_connections)) {
    from_city <- small_cities[small_cities$city_id == small_connections$from_id[i], ]
    to_city <- small_cities[small_cities$city_id == small_connections$to_id[i], ]
    
    # Calculate offset
    if (small_connections$pair_count[i] > 1) {
      offset_magnitude <- ceiling(small_connections$pair_index[i] / 2) * 0.4
      offset_sign <- ifelse(small_connections$pair_index[i] %% 2 == 0, 1, -1)
      offset_factor <- offset_magnitude * offset_sign
    } else {
      offset_factor <- 0
    }
    
    route <- create_offset_route(
      from_city$long, from_city$lat,
      to_city$long, to_city$lat,
      offset_factor = offset_factor
    )
    
    small_map <- add_arrow_decorator(
      small_map,
      route,
      color = "#3498db",
      weight = 2,
      opacity = 0.6,
      label = paste(from_city$city_label, "→", to_city$city_label)
    )
  }
  
  cat("Small network created:", n_cities, "cities,", nrow(small_connections), "connections\n")
  
  # Save
  saveWidget(small_map, "/mnt/user-data/outputs/random_world_network_small.html", 
             selfcontained = TRUE)
  
  return(small_map)
}

# Uncomment to create a smaller test network:
# small_network_map <- create_smaller_network(n_cities = 50, n_connections = 150)
# print(small_network_map)
