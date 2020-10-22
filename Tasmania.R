# Tasmania plots

library(cartogram)
library(cowplot)
library(gridExtra)
library(ggthemes)
library(knitcitations)
library(kableExtra)
library(sp)
library(sf)
library(sugarbag)
library(tidyverse)
# remotes::install_github("wfmackey/absmapsdata")
library(absmapsdata)


invthm <- theme_map() +
theme(
panel.background = element_rect(fill = "black", colour = NA),
plot.background = element_rect(fill = "black", colour = NA),
legend.background = element_rect(fill = "transparent", colour = NA),
legend.key = element_rect(fill = "transparent", colour = NA),
text = element_text(colour = "white"),
axis.text = element_blank()
)
# function to allocate colours to regions
aus_colours <- function(sir_p50){
value <- case_when(
sir_p50 <  0.74 ~ "#33809d",
sir_p50 >= 0.74 & sir_p50 < 0.98 ~ "#aec6c7",
sir_p50 >= 0.98 & sir_p50 < 1.05 ~ "#fff4bc",
sir_p50 >= 1.05 & sir_p50 < 1.45 ~ "#ff9a64",
sir_p50 >= 1.45 ~ "#ff3500",
TRUE ~ "#FFFFFF")
return(value)
}
# Chunk 3: lungdata
sa2 <- absmapsdata::sa22011 %>%
filter(!st_is_empty(geometry)) %>%
filter(!state_name_2011 == "Other Territories") %>%
filter(!sa2_name_2011 == "Lord Howe Island")
# sa2 <- sa2 %>% rmapshaper::ms_simplify(keep = 0.05, keep_shapes = TRUE)
SIR <- read_csv("data/SIR Downloadable Data.csv") %>%
filter(SA2_name %in% sa2$sa2_name_2011) %>%
dplyr::select(Cancer_name, SA2_name, Sex_name, p50) %>%
filter(Cancer_name == "Lung", Sex_name == "Females")
ERP <- read_csv("data/ERP.csv") %>%
filter(REGIONTYPE == "SA2", Time == 2011, Region %in% SIR$SA2_name) %>%
dplyr::select(Region, Value)


# Alternative maps
# Join with sa2 sf object
sa2lung_ERP <- SIR %>%
left_join(sa2, ., by = c("sa2_name_2011" = "SA2_name")) %>%
left_join(., ERP %>%
dplyr::select(Region,
Population = Value), by = c("sa2_name_2011"= "Region")) %>%
filter(!st_is_empty(geometry))
sa2lung_ERP <- sa2lung_ERP %>%
#filter(!is.na(Population)) %>%
filter(!sa2_name_2011 == "Lord Howe Island") %>%
mutate(SIR = map_chr(p50, aus_colours)) %>%
st_as_sf()
# Chunk 4: choro
aus_ggchoro <- ggplot(sa2lung_ERP) +
geom_sf(aes(fill = SIR), size = 0.1) +
scale_fill_identity() + invthm
aus_ggchoro
# Chunk 5: fullhexmap
if (!file.exists("data/aus_hexmap.rda")) {
## Create centroids set
centroids <- sa2 %>%
create_centroids(., "sa2_name_2011")
## Create hexagon grid
grid <- create_grid(centroids = centroids,
hex_size = 0.2,
buffer_dist = 5)
## Allocate polygon centroids to hexagon grid points
aus_hexmap <- allocate(
centroids = centroids,
hex_grid = grid,
sf_id = "sa2_name_2011",
## same column used in create_centroids
hex_size = 0.2,
## same size used in create_grid
hex_filter = 10,
focal_points = capital_cities,
width = 35,
verbose = FALSE
)
save(aus_hexmap, file = "data/aus_hexmap.rda") }
load("data/aus_hexmap.rda")
# Chunk 9: tas_displays


## Plots ##
##################################################################################

# Chunk 10: hexmap
# Chunk 13: capital_cities
data(capital_cities)
# Chunk 14: tas_centroids
centroids <- create_centroids(
shp_sf = sa2 %>% filter(state_name_2011 == "Tasmania"),
sf_id = "sa2_name_2011")
hex_size <- .2
buffer_dist <- 2
bbox <- tibble(min = c(min(centroids$longitude), min(centroids$latitude)),
max = c(max(centroids$longitude), max(centroids$latitude)))
grid <- tibble::as_tibble(expand.grid(hex_long = seq(bbox$min[1] - buffer_dist,
bbox$max[1] + buffer_dist,
hex_size),
hex_lat = seq(bbox$min[2] - buffer_dist,
bbox$max[2] + buffer_dist,
hex_size)))
fort_tas <-  fortify_sfc(sa2lung_ERP) %>% filter(state_name_2011 == "Tasmania")

fort_hex <- fortify_hexagon(data = aus_hexmap %>% filter(sa2_name_2011 %in% fort_tas$sa2_name_2011),
                            sf_id = "sa2_name_2011",
                            hex_size = 0.2) %>% 
  left_join(sa2lung_ERP %>% select(sa2_name_2011, SIR, p50))

tas_centroids <- ggplot() +
geom_polygon(aes(x = long, y = lat, group = interaction(sa2_name_2011, polygon)), data = fort_tas, fill = "grey", colour = "white", size = 0.01) +
geom_point(aes(x=longitude, y = latitude), size = 2, shape = 19, colour = "white",
data = centroids) +
scale_colour_identity() +
theme_void() +
coord_equal()

tas_hexmap <- ggplot() +
geom_polygon(aes(x = long, y = lat, group = interaction(sa2_name_2011, polygon)), data = fort_tas, fill = "grey", colour = "white", size = 0.01) +
geom_polygon(aes(x = long, y = lat, group = hex_id, fill = SIR),
data = fort_hex %>% filter(sa2_name_2011 %in% fort_tas$sa2_name_2011)) +
scale_fill_identity() +
theme_void() +
coord_equal()

# Chunk 15: grid
# Find every second latitude
shift_lat <- grid %>% dplyr::select(hex_lat) %>%
dplyr::distinct() %>%
dplyr::filter(dplyr::row_number() %% 2 == 1) %>% unlist()
# Shift the longitude of every second latitude to the right to make hex structure
grid <- grid %>%
dplyr::mutate(hex_long = ifelse(hex_lat %in% shift_lat, hex_long,
hex_long + (hex_size / 2))) %>%
dplyr::mutate(id=1:NROW(.))  %>%
dplyr::mutate(assigned=FALSE)

# Chunk 16: filtergrid
g1 <- ggplot() +
geom_polygon(aes(x = long, y = lat, group = interaction(sa2_name_2011, polygon)), data = fort_tas, fill = "grey", colour = "white", size = 0.01) +
theme_void() +
geom_point(aes(x = hex_long, y = hex_lat), colour = "#55b56c", data = grid, size = 3) +
coord_equal()

ggsave(filename = "figures/algorithm/tas1.png", plot = g1,
       device = "png", bg = "transparent", dpi = 300,  width = 6, height = 6)

full_grid <- grid <- grid %>%
mutate(hex_long_int = dense_rank(hex_long)-1,
hex_lat_int = dense_rank(hex_lat)-1)
nlong <- length(unique(grid$hex_long))
nlat <- length(unique(grid$hex_lat))
# Add grid row and column values to centroids data set
centroids <- centroids %>%
mutate(
# long int for integer value of longitude column
long_int = round((longitude - min(grid$hex_long)) /
(max(grid$hex_long) - min(grid$hex_long)) * nlong, 0),
# lat int for integer value of latitude column
lat_int = round((latitude - min(grid$hex_lat)) /
(max(grid$hex_lat) - min(grid$hex_lat)) * nlat, 0)
)
# Amount of lats and longs in each group of rows and columns
lat_size <- round(nlat / 20, 0)
long_size <- round(nlong / 20, 0)
# Make a list of the min and max of the groups
# Effectively creates manual sliding windows
nlat_list <- purrr::map2(seq(1:nlat), lat_size + seq(1:nlat), c)
nlong_list <- purrr::map2(seq(1:nlong), long_size + seq(1:nlong), c)
# LATITUDE ROWS FILTER
# Function to return the centroids that fall in the latitude window
lat_window <- function(bounds, cents = centroids, maximum = nlat) {
max_int <- min(bounds[2], maximum)
cents_in <- filter(cents, between(lat_int, bounds[1], max_int))
return(cents_in)
}
# Function to return the centroids that fall in the longitude window
long_window <- function(bounds, cents = centroids, maximum = nlong) {
max_int <- bounds[2]
while (max_int > maximum) {
max_int <- max_int - 1
}
cents_in <- filter(cents, between(long_int, bounds[1], max_int))
return(cents_in)
}
# amount of latitude in sliding window
lat_windows <- purrr::map(.x = nlat_list, .f = lat_window, centroids, nlat)
# LONGITUDE COLS FILTER
long_windows <- purrr::map(.x = nlong_list, .f = long_window, centroids, nlong)
#########################################################
###                ROLLING MIN & MAX                  ###
# DEFINE FUNCTION
# find the min and max longitude for each latitude
range_rows <- purrr::map_dfr(
.x = lat_windows,
.f = function(data) {
data %>%
dplyr::summarise(
long_min = ifelse(purrr::is_empty(long_int), NA, min(data$long_int)),
long_max = ifelse(purrr::is_empty(long_int), NA, max(data$long_int))
)
}
)
# find the min and max longitude for each latitude
range_cols <- purrr::map_dfr(.x = long_windows, .f = function(data) {
data %>%
dplyr::summarise(
lat_min = ifelse(purrr::is_empty(lat_int), NA, min(data$lat_int)),
lat_max = ifelse(purrr::is_empty(lat_int), NA, max(data$lat_int))
)
})
# find the min and max longitude for each latitude
range_rows <- purrr::map_dfr(.x = lat_windows, .f = function(data) {
data %>%
dplyr::summarise(
long_min = ifelse(purrr::is_empty(long_int), NA, min(data$long_int)),
long_max = ifelse(purrr::is_empty(long_int), NA, max(data$long_int))
)
}
)
# find the min and max longitude for each latitude
range_cols <- purrr::map_dfr(.x = long_windows, .f = function(data) {
data %>%
dplyr::summarise(
lat_min = ifelse(purrr::is_empty(lat_int), NA, min(data$lat_int)),
lat_max = ifelse(purrr::is_empty(lat_int), NA, max(data$lat_int))
)
})
#########################################################
###                ROLLING AVERAGES                   ###
mean_range <- function(bounds, data) {
data[bounds[1]:min(bounds[2], NROW(data)), ] %>%
dplyr::summarise(across(ends_with("min"),
~mean(.x, na.rm = TRUE), .names = "mean_{col}"),
across(ends_with("max"),
~mean(.x, na.rm = TRUE), .names = "mean_{col}")) %>%
# in cases where all values are NA mean is Nan
# make mean value of NaN be NA
dplyr::summarise(across(starts_with("mean"), ~ifelse(.x == "NaN", NA, .x)))
}
# smooth the minimums
av_range_rows <- purrr::map_dfr(.x = nlat_list, mean_range, range_rows) %>%
bind_cols(lat_id = c(seq(1:nlat) + lat_size), .)
# smooth the minimums
av_range_cols <- purrr::map_dfr(.x = nlong_list, mean_range, range_cols) %>%
bind_cols(long_id = c(seq(1:nlong) + round(long_size / 2)), .)
# APPLY A BUFFER
# change buffer to amount of hexagons (ints) either side
hex_buffer <- floor(buffer_dist / hex_size)
grid <- grid %>%
left_join(., av_range_rows, by = c("hex_lat_int" = "lat_id")) %>%
left_join(., av_range_cols, by = c("hex_long_int" = "long_id")) %>%
rowwise() %>%
mutate(long_buffer = ifelse(between(
hex_long_int, mean_long_min - hex_buffer,
mean_long_max + hex_buffer), "in", "out")) %>%
mutate(lat_buffer = ifelse(between(
hex_lat_int, mean_lat_min - hex_buffer,
mean_lat_max + hex_buffer), "in", "out")) %>%
filter(lat_buffer == "in" | long_buffer == "in")

g2 <- ggplot() +
geom_polygon(aes(x = long, y = lat, group = interaction(sa2_name_2011, polygon)), 
             data = fort_tas, fill = "grey", colour = "white", size = 0.01) +
theme_void() + coord_equal() +
geom_point(aes(x = hex_long, y = hex_lat), colour = "#a9e8b8", data = full_grid, size = 0.5) +
geom_point(aes(x = hex_long, y = hex_lat), colour = "#55b56c", data = grid, size = 3)

ggsave(filename = "figures/algorithm/tas2.png", plot = g2,
       device = "png", bg = "transparent", dpi = 300,  width = 6, height = 6)

# Chunk 18: buffers
centroids <- centroids %>%
group_nest(sa2_name_2011) %>%
mutate(closest = purrr::map(data, closest_focal_point,
focal_points = capital_cities)) %>%
unnest(c("data", "closest")) %>%
arrange(focal_distance) %>%
mutate(rownumber = row_number())

# Choose a centroid for allocation:
f_centroid <- centroids %>%
filter(rownumber == 8)

# filter_grid_points
distance <- (((f_centroid$latitude - f_centroid$focal_latitude) ^ 2) +
((f_centroid$longitude - f_centroid$focal_longitude) ^ 2)) ^ (1 / 2)
flong <- f_centroid$longitude
flat <- f_centroid$latitude
f_dist <- 10 * hex_size
angle_width <- 35
f_grid <- grid %>%
ungroup() %>%
filter((flat - f_dist) < hex_lat & hex_lat < (flat + f_dist)) %>%
filter((flong - f_dist) < hex_long & hex_long < (flong + f_dist))
f_grid <- f_grid %>%
group_by(id) %>%
mutate(
hex_lat_c = (hex_lat - flat),
hex_long_c = (hex_long - flong)) %>%
mutate(hyp = ((hex_lat_c^2) + (hex_long_c^2))^(1 / 2))
f_angle <- f_centroid %>%
mutate(atan = atan2(latitude - focal_latitude, longitude - focal_longitude),
angle = (atan * 180 / pi),
pangle = ifelse(angle < 0, angle + 360, angle)) %>%
pull(pangle)
f_grid <- f_grid %>%
# create circle of radius: f_dist
filter(hyp < f_dist) %>%
mutate(
# geosphere takes a long time
angle = f_angle,
angle_plus = (angle + angle_width) %% 360,
angle_minus = (angle - angle_width) %% 360,
atan = atan2(hex_lat_c, hex_long_c),
hex_angle = (atan * 180 / pi),
hex_angle = ifelse(hex_angle < 0, hex_angle + 360, hex_angle))

allocated_list <- aus_hexmap %>%
  filter(points == "Hobart") %>%
  slice_min(rownumber, n = 7)
fort_allocated <- allocated_list %>%
  fortify_hexagon(., sf_id = "sa2_name_2011", hex_size = 0.2)

f1 <- ggplot() +
geom_polygon(aes(x = long, y = lat, group = interaction(sa2_name_2011, polygon)), 
             data = fort_tas, fill = "grey", colour = "white", size = 0.01) +
theme_void() + coord_equal() +
geom_point(aes(x = hex_long, y = hex_lat), colour = "#a9e8b8", data = grid, size = 0.5) +
geom_point(aes(x = hex_long, y = hex_lat), colour = "#55b56c", data = f_grid, size = 3) +
  geom_polygon(aes(x = long, y = lat, group = interaction(sa2_name_2011)), 
               data = fort_allocated, fill = "#a9e8b8", colour = "#55b56c", size = 0.01) +
geom_point(aes(x=longitude, y = latitude), data = f_centroid, fill = "#d766d9", shape = 25) +
#Hobart
geom_point(aes(x = focal_longitude, y = focal_latitude), data = f_centroid, colour = "black", 
           size = 5, shape = 4)

f1
ggsave(filename = "figures/algorithm/tas3.png", plot = f1,
       device = "png", bg = "transparent", dpi = 300,  width = 6, height = 6)
# Chunk 19: filterprocess
full_grid <- full_grid %>% filter(hex_long > bbox$min[1], hex_long < bbox$max[1],
hex_lat > bbox$min[2], hex_lat < bbox$max[2])

# Check that there were available points within hyp distance
f_grid <- f_grid %>%
# create slice of width *2 degrees from centroid
filter(angle_minus < hex_angle & hex_angle < angle_plus)
hex <- f_grid %>%
ungroup() %>%
filter(hyp == min(hyp)) %>%
select(hex_long, hex_lat, hex_id = id)

f2 <- ggplot() +
geom_polygon(aes(x = long, y = lat, group = interaction(sa2_name_2011, polygon)), 
             data = fort_tas, fill = "grey", colour = "white", 
             size = 0.01) +
geom_polygon(aes(x = long, y = lat, group = interaction(sa2_name_2011)), 
             data = fort_allocated, fill = "#a9e8b8", colour = "#55b56c", size = 0.01) +
#Hobart
geom_point(aes(x = focal_longitude, y = focal_latitude), data = f_centroid, colour = "black",
           size = 6, shape = 4) +
geom_point(aes(x=longitude, y = latitude), data = f_centroid, fill = "#d766d9", shape = 25,
           size = 2.5) +
theme_void() + 
# possible choices to select
geom_point(aes(x = hex_long, y = hex_lat), colour = "#55b56c", data = f_grid, size = 2.5) +
# selected hexagon
geom_point(aes(x = hex_long, y = hex_lat), colour = "#55b56c", fill = "#d766d9", 
           data = hex, size = 3.5, shape = 21) +
  coord_equal(xlim = c(146,148.2), ylim = c(-42.2, -44.30))

ggsave(filename = "figures/algorithm/tas4.png", plot = f2,
       device = "png", bg = "transparent", dpi = 300,  width = 6, height = 6)

f2


cowplot::plot_grid(g1, g2, f1, f2, labels = c("a", "b", "c", "d"))

