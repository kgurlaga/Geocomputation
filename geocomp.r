install.packages("remotes")
install.packages("geocompkg",
    repos = c("https://geocompr.r-universe.dev", "https://cloud.r-project.org"),
    dependencies = TRUE, force = TRUE
)

# Generate map
library(leaflet)
popup = c("Robin", "Jakub", "Jannes")
leaflet() |>
    addProviderTiles("NASAGIBS.ViirsEarthAtNight2012") |>
    addMarkers(
        lng = c(-3, 23, 11),
        lat = c(52, 53, 49),
        popup = popup
    )

library(sf)
library(terra)
library(spData)
library(spDataLarge)

vignette(package = "sf")
vignette("sf1")

class(world)
names(world)
plot(world)

summary(world["lifeExp"])

world_mini = world[1:2, 1:3]

world_dfr = st_read(system.file("shapes/world.gpkg", package="spData"))
world_tbl = read_sf(system.file("shapes/world.gpkg", package="spData"))
class(world_dfr)
class(world_tbl)

# Basic maps
plot(world[3:6])
plot(world["pop"])

world_asia = world[world$continent == "Asia",]
asia = st_union(world_asia)

plot(world["pop"], reset=FALSE)
plot(asia, add=TRUE, col ="#ff0000")

plot(world["continent"], reset = FALSE)
cex = sqrt(world$pop) / 10000
world_cents = st_centroid(world, of_largest = TRUE)
plot(st_geometry(world_cents), add = TRUE, cex = cex)

india = world[world$name_long == "India", ]
plot(st_geometry(india), expandBB = c(0, 0.2, 0.1, 1), col = "gray", lwd = 3)
plot(st_geometry(world_asia), add = TRUE)

# Creating spatial table from scratch
lnd_point = st_point(c(0.1, 51.5))
lnd_geom = st_sfc(lnd_point, crs = "EPSG:4326")
lnd_attrib = data.frame(name = "London", temperature = 25, date = as.Date("2023-06-21"))
lnd_sf = st_sf(lnd_attrib, geometry = lnd_geom)

# points
st_point(c(5, 2))
st_point(c(5, 2, 3))
st_point(c(5, 2, 1), dim = "XYM")
st_point(c(5, 2, 3, 1))

# multipoints and linestrings
multipoint__matrix = rbind(c(5, 2), c(1, 3), c(3, 4), c(3, 2))
st_multipoint(multipoint__matrix)
linestring_matrix = rbind(c(1, 5), c(4, 4), c(4, 1), c(2, 2), c(3, 2))
st_linestring(linestring_matrix)

# others
polygon_list = list(rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5)))
st_polygon(polygon_list)

polygon_border = rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5))
polygon_hole = rbind(c(2, 4), c(3, 4), c(3, 3), c(2, 3), c(2, 4))
polygon_with_hole_list = list(polygon_border, polygon_hole)
st_polygon(polygon_with_hole_list)

multilinestring_list = list(
    rbind(c(1, 5), c(4, 4), c(4, 1), c(2, 2), c(3, 2)),
    rbind(c(1, 2), c(2, 4))
)
st_multilinestring(multilinestring_list)

multipolygon_list = list(
    list(rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5))),
    list(rbind(c(0, 2), c(1, 2), c(1, 3), c(0, 3), c(0, 2)))
)
st_multipolygon(multipolygon_list)

geometrycollection_list = list(
    st_multipoint(multipoint__matrix),
    st_linestring(linestring_matrix)
)
st_geometrycollection(geometrycollection_list)

# Simple feature columns (sfc)
point1 = st_point(c(5, 2))
point2 = st_point(c(1, 3))
points_sfc = st_sfc(point1, point2)

polygon_list1 = list(rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5)))
polygon1 = st_polygon(polygon_list1)
polygon_list2 = list(rbind(c(0, 2), c(1, 2), c(1, 3), c(0, 3), c(0, 2)))
polygon2 = st_polygon(polygon_list2)
polygon_sfc = st_sfc(polygon1, polygon2)
st_geometry_type(polygon_sfc)

multilinestring_list1 = list(
    rbind(c(1, 5), c(4, 4), c(4, 1), c(2, 2), c(3, 2)),
    rbind(c(1, 2), c(2, 4))
)
multilinestring1 = st_multilinestring((multilinestring_list1))
multilinestring_list2 = list(
    rbind(c(2, 9), c(7, 9), c(5, 6), c(4, 7), c(2, 7)),
    rbind(c(1, 7), c(3, 8))
)
multilinestring2 = st_multilinestring((multilinestring_list2))
multilinestring_sfc = st_sfc(multilinestring1, multilinestring2)
st_geometry_type(multilinestring_sfc)

point_multilinestring_sfc = st_sfc(point1, multilinestring1)
st_geometry_type(point_multilinestring_sfc)

# Set CRS
points_sfc_wgs = st_sfc(point1, point2, crs = "EPSG:4326")
st_crs(points_sfc_wgs)

# sfheaders packages
v = c(1, 1)
v_sfg_sfh = sfheaders::sfg_point(obj = v)
v_sfg_sfh

v_sfg_sf = st_point(v)
print(v_sfg_sf) == print(v_sfg_sfh)

# sfheaders from matrices
m = matrix(1:8, ncol = 2)
sfheaders::sfg_linestring(obj = m)

# sfheaders dataframes
df = data.frame(x = 1:4, y = 4:1)
sfheaders::sfg_polygon(obj = df)

sfheaders::sfc_point(obj = v)
sfheaders::sfc_linestring(obj = m)
sfheaders::sfc_polygon(obj = df)

sfheaders::sf_point(obj = v)
sfheaders::sf_linestring(obj = m)
sfheaders::sf_polygon(obj = df)

df_sf = sfheaders::sf_polygon(obj = df)
st_crs(df_sf) = "EPSG:4326"

# Spherical geometry operations
sf_use_s2()

india_buffer_with_s2 = st_buffer(india, 1)
sf_use_s2(FALSE)

india_buffer_without_s2 = st_buffer(india, 1)
plot(india_buffer_with_s2)

plot(india_buffer_without_s2)
sf_use_s2(TRUE)

# Raster
raster_filepath = system.file("raster/srtm.tif", package = "spDataLarge")
my_rast = rast(raster_filepath)
class(my_rast)

single_rast = rast(raster_filepath)

new_raster = rast(nrows = 6, ncols = 6, xmin = -1.5, xmax = 1.5, ymin = -1.5, ymax = 1.5, vals = 1:36)

multi_raster_file = system.file("raster/landsat.tif", package = "spDataLarge")
multi_rast = rast(multi_raster_file)
nlyr(multi_rast)

multi_rast3 = subset(multi_rast, 3)
multi_rast4 = subset(multi_rast, "landsat_4")

multi_rast34 = c(multi_rast3, multi_rast4)

# CRS
luxembourg = world[world$name_long == "Luxembourg",]
st_area(luxembourg) / 1000000
units::set_units(st_area(luxembourg), km^2)
res(my_rast)

repr = project(my_rast, "EPSG:26912")
res(repr)

##### Attribute data operations
library(sf)
library(terra)
library(dplyr)
library(spData)
library(tidyr)

methods(class = "sf")
class(world)
dim(world)

world_df = st_drop_geometry(world)

# Vector attribute subsetting
world[1:6, ]
world[, 1:3]
world[1:6, 1:3]
world[, c("name_long", "pop")]
world[, c(T, T, F, F, F, F, F, T, T, F, F)]
world[, 888]

i_small = world$area_km2 < 10000
summary(i_small)
small_countries = world[i_small, ]

small_countries = world[world$area_km2 < 10000, ]
small_countries = subset(world, area_km2 < 10000)

world1 = select(world, name_long, pop)
names(world1)

world2 = select(world, name_long:pop)

world3 = select(world, -subregion, -area_km2)

world4 = select(world, name_long, population = pop)

pull(world, pop)
world$pop
world[["pop"]]

slice(world, 1:6)

world7 = filter(world, area_km2 < 10000)
world7 = filter(world, lifeExp > 82)

# Chaining commands with pipes
world7 = world |>
    filter(continent == "Asia") |>
    select(name_long, continent) |>
    slice(1:5)

world8 = slice(
    select(
        filter(world, continent == "Asia"),
        name_long, continent),
1:5)

world9_filtered = filter(world, continent == "Asia")
world9_selected = select(world9_filtered, continent)
world9 = slice(world9_selected, 1:5)

# Vector attribute aggregation
world_agg1 = aggregate(pop ~ continent, FUN = sum, data = world, na.rm = TRUE)
class(world_agg1)

world_agg2 = aggregate(world["pop"], by = list(world$continent), FUN = sum, na.rm = TRUE)
nrow(world_agg2)

world_agg3 = world %>%
    group_by(continent) %>%
    summarize(pop = sum(pop, na.rm = TRUE))

world_agg4 = world %>%
    group_by(continent) %>%
    summarize(Pop = sum(pop, na.rm = TRUE), Area = sum(area_km2), N = n())

world_agg5 = world %>%
    st_drop_geometry() %>%
    select(pop, continent, area_km2) %>%
    group_by(Continent = continent) %>%
    summarize(Pop = sum(pop, na.rm = TRUE), Area = sum(area_km2), N = n()) %>%
    mutate(Density = round(Pop / Area)) %>%
    slice_max(Pop, n = 3) %>%
    arrange(desc(N))

# Vector attribute joining
world_coffee = left_join(world, coffee_data)
class(world_coffee)

plot(world_coffee["coffee_production_2017"])

coffee_renamed = rename(coffee_data, nm = name_long)
world_coffee2 = left_join(world, coffee_renamed, by = join_by(name_long == nm))

world_coffee_inner = inner_join(world, coffee_data)
nrow(world_coffee_inner)

setdiff(coffee_data$name_long, world$name_long)

drc = stringr::str_subset(world$name_long, "Dem*.+Congo")
print(drc)

coffee_data$name_long[grepl("Congo,", coffee_data$name_long)] = drc
world_coffee_match = inner_join(world, coffee_data)
nrow(world_coffee_match)

coffee_world = left_join(coffee_data, world)
class(coffee_world)

# Creating attributes and removing spatial
world_new = world
world_new$pop_dens = world_new$pop / world_new$area_km2

world_new2 = world  %>% mutate(pop_dens = pop / area_km2)

world_unite = world %>% tidyr::unite("con_reg", continent:region_un, sep = ":", remove = TRUE)

world_separate = world_unite %>% tidyr::separate(con_reg, c("continent", "region_un"), sep = ":")

world %>% rename(name = name_long)

new_names = c("i", "n", "c", "r", "s", "t", "a", "p", "l", "gP", "geom")
world_new_names = world %>% setNames(new_names)


# Manipulating raster objects
elev = rast(nrows = 6, ncols = 6, xmin = -1.5, xmax = 1.5, ymin = -1.5, ymax = 1.5, vals = 1:36)

grain_order = c("clay", "silt", "sand")
grain_char = sample(grain_order, 36, replace = TRUE)
grain_fact = factor(grain_char, levels = grain_order)
grain = rast(nrows = 6, ncols = 6, xmin = -1.5, xmax = 1.5, ymin = -1.5, ymax = 1.5, vals = grain_fact)

grain2 = grain
levels(grain2) = data.frame(value = c(0, 1, 2), wetness = c("wet", "moist", "dry"))
levels(grain2)

elev[1, 1] = 0
elev[]

elev[1, c(1, 2)] = 0

two_layers = c(grain, elev)
two_layers[1] = cbind(c(1), c(4))
two_layers[]

# Summarizing raster objects
global(elev, sd)
freq(grain)
hist(elev)


### SPATIAL DATA OPERATIONS
library(sf)
library(terra)
library(dplyr)
library(spData)

# Spatial subsetting
canterbury = nz %>% filter(Name == "Canterbury")
canterbury_height = nz_height[canterbury, ]

sel_sgbp = st_intersects(x = nz_height, y = canterbury)
class(sel_sgbp)
sel_logical = lengths(sel_sgbp) > 0
canterbury_height2 = nz_height[sel_logical, ]

canterbury_height3 = nz_height %>% st_filter(y = canterbury, .predicate = st_intersects)

# Topological relations
polygon_matrix = cbind(
    x = c(0, 0, 1, 1, 0),
    y = c(0, 1, 1, 0.5, 0)
)
polygon_sfc = st_sfc(st_polygon(list(polygon_matrix)))

point_df = data.frame(
    x = c(0.2, 0.7, 0.4),
    y = c(0.1, 0.2, 0.8)
)
point_sf = st_as_sf(point_df, coords = c("x", "y"))

st_intersects(point_sf, polygon_sfc)

st_intersects(point_sf, polygon_sfc, sparse = FALSE)

st_within(point_sf, polygon_sfc)
st_touches(point_sf, polygon_sfc)

st_disjoint(point_sf, polygon_sfc, sparse = FALSE)[, 1]

st_is_within_distance(point_sf, polygon_sfc, dist = 0.2, sparse = FALSE)[, 1]

# Distance relations
nz_heighest = nz_height %>% slice_max(n = 1, order_by = elevation)
canterbury_centroid = st_centroid(canterbury)
st_distance(nz_height, canterbury_centroid)

co = filter(nz, grepl("Canter|Otag", Name))
st_distance(nz_height[1:3, ], co)

plot(st_geometry(co)[2])
plot(st_geometry(nz_height)[2:3], add = TRUE)

# DE-9IM strings
xy2sfc = function(x, y) st_sfc(st_polygon(list(cbind(x, y))))
x = xy2sfc(x = c(0, 0, 1, 1, 0), y = c(0, 1, 1, 0.5, 0))
y = xy2sfc(x = c(0.7, 0.7, 0.9, 0.7), y = c(0.8, 0.5, 0.5, 0.8))
st_relate(x, y)

st_queen = function(x, y) st_relate(x, y, pattern = "F***T****")
st_rook = function(x, y) st_relate(x, y, pattern = "F***1****")

grid = st_make_grid(x, n = 3)
grid_sf = st_sf(grid)
grid_sf$queens = lengths(st_queen(grid, grid[5])) > 0
plot(grid, col = grid_sf$queens)
grid_sf$rooks = lengths(st_rook(grid, grid[5])) > 0
plot(grid, col = grid_sf$rooks)

# Spatial joining
set.seed(2018)
(bb = st_bbox(world))
random_df = data.frame(
    x = runif(n = 10, min = bb[1], max = bb[3]),
    y = runif(n = 10, min = bb[2], max = bb[4])
)
random_points = random_df %>% st_as_sf(coords = c("x", "y"), crs = "EPSG:4326")

world_random = world[random_points, ]
nrow(world_random)
random_joined = st_join(random_points, world["name_long"])

plot(st_geometry(cycle_hire), col = "blue")
plot(st_geometry(cycle_hire_osm), add = TRUE, pch = 3, col = "red")

sel = st_is_within_distance(cycle_hire, cycle_hire_osm, dist = units::set_units(20, "m"))
summary(lengths(sel) > 0)

z = st_join(cycle_hire, cycle_hire_osm, st_is_within_distance, dist = units::set_units(20, "m"))
nrow(cycle_hire)
nrow(z)

z = z %>%
    group_by(id) %>%
    summarize(capacity = mean(capacity))
nrow(z) == nrow(cycle_hire)

plot(cycle_hire_osm["capacity"])
plot(z["capacity"])

# Spatial aggregation
nz_agg = aggregate(x = nz_height, by = nz, FUN = mean)

nz_agg2 = st_join(x = nz, y = nz_height) %>%
    group_by(Name) %>%
    summarize(elevation = mean(elevation, na.rm = TRUE))

# Joining incongruent layers
iv = incongruent["value"]
agg_aw = st_interpolate_aw(iv, aggregating_zones, extensive = TRUE)
agg_aw$value

# Spatial operations on raster data
elev = rast(system.file("raster/elev.tif", package="spData"))
grain = rast(system.file("raster/grain.tif", package = "spData"))

id = cellFromXY(elev, xy = matrix(c(0.1, 0.1), ncol = 2))
elev[id]
terra::extract(elev, matrix(c(0:1, 0.1), ncol = 2))

clip = rast(xmin = 0.9, xmax = 1.8, ymin = -0.45, ymax = 0.45, resolution = 0.3, vals = rep(1, 9))
plot(elev[clip])

elev[1:2, drop = FALSE]

rmask = elev
values(rmask) = sample(c(NA, TRUE), 36, replace = TRUE)

elev[rmask, drop = FALSE]

elev[elev < 20] = NA

# Local operations

elev + elev
elev^2
log(elev)
elev > 5

rcl = matrix(c(0, 12, 1, 12, 24, 2, 24, 36, 3), ncol = 3, byrow = TRUE)
recl = classify(elev, rcl = rcl)

multi_raster_file = system.file("raster/landsat.tif", package = "spDataLarge")
multi_rast = rast(multi_raster_file)
plot(multi_rast)
multi_rast = (multi_rast * 0.0000275) - 0.2
multi_rast[multi_rast < 0] = 0
ndvi_fun = function(nir, red) {
    (nir - red) / (nir + red)
}
ndvi_rast = lapp(multi_rast[[c(4, 3)]], fun = ndvi_fun)
plot(ndvi_rast)

# Focal operations
r_focal = focal(elev, w = matrix(1, nrow = 3, ncol = 3), fun = min)

# Zonal operations
z = zonal(elev, grain, fun = "mean")
z

# Merging raster
aut = geodata::elevation_30s(country = "AUT", path = tempdir())
plot(aut)
ch = geodata::elevation_30s(country = "CHE", path = tempdir())
plot(ch)
aut_ch = merge(aut, ch)
plot(aut_ch)

### Geometry operations

library(sf)
library(terra)
library(dplyr)
library(spData)
library(spDataLarge)

seine_simp = st_simplify(seine, dTolerance = 2000)
object.size(seine)
object.size(seine_simp)

us_states_simp1 = st_simplify(us_states, dTolerance = 100000)

us_states_simp2 = rmapshaper::ms_simplify(us_states, keep = 0.01, keep_shapes = TRUE)

us_states_simp3 = smoothr::smooth(us_states, method = "ksmooth", smoothness = 6)

# Centroids
nz_centroid = st_centroid(nz)
seine_centroid = st_centroid(seine)

nz_pos = st_point_on_surface(nz)
seine_pos = st_point_on_surface(seine)

# Buffers
seine_buff_5km = st_buffer(seine, dist = 5000)
seine_buff_50km = st_buffer(seine, dist = 50000)

# Affine transformations
nz_sfc = st_geometry(nz)
plot(nz_sfc)

nz_shift = nz_sfc + c(0, 100000)

nz_centroid_sfc = st_centroid(nz_sfc)
nz_scale = (nz_sfc - nz_centroid_sfc)* 0.5 + nz_centroid_sfc
plot(nz_scale)

rotation = function(a) {
    r = a * pi / 180
    matrix(c(cos(r), sin(r), -sin(r), cos(r)), nrow = 2, ncol = 2)
}

nz_rotate = (nz_sfc - nz_centroid_sfc) * rotation(30) + nz_centroid_sfc
plot(nz_rotate)

nz_scale_sf = st_set_geometry(nz, nz_scale)

# Clipping
b = st_sfc(st_point(c(0, 1)), st_point(c(1, 1)))
b = st_buffer(b, dist = 1)
plot(b, border = "gray")
text(x = c(-0.5, 1.5), y = 1, labels = c("x", "y"), cex = 3)

x = b[1]
y = b[2]
x_and_y = st_intersection(x, y)
plot(b, border = "gray")
plot(x_and_y, col = "lightgray", border = "gray", add = TRUE)

# Subsetting and clipping
bb = st_bbox(st_union(x, y))
box = st_as_sfc(bb)
set.seed(2024)
p = st_sample(x = box, size = 10)
p_xy1 = p[x_and_y]
plot(box, border = "gray", lty = 2)
plot(x, add = TRUE, border = "gray")
plot(y, add = TRUE, border = "gray")
plot(p, add = TRUE, cex = 3.5)
plot(p_xy1, cex = 5, col = "red", add = TRUE)
text(x = c(-0.5, 1.5), y = 1, labels = c("x", "y"), cex = 3)

bb = st_bbox(st_union(x, y))
box = st_as_sfc(bb)
set.seed(2024)
p = st_sample(x = box, size = 10)
x_and_y = st_intersection(x, y)

# way1 #
p_xy1 = p[x_and_y]

# way2 #
p_xy2 = st_intersection(p, x_and_y)

# way3 #
sel_p_xy = st_intersects(p, x, sparse = FALSE)[, 1] & st_intersects(p, y, sparse = FALSE)[, 1]
p_xy3 = p[sel_p_xy]

# geometry unions #
regions = aggregate(x = us_states[, "total_pop_15"], by = list(us_states$REGION), FUN = sum, na.rm = TRUE)
regions2 = us_states %>%
    group_by(REGION) %>%
    summarize(pop = sum(total_pop_15, na.rm = TRUE))
plot(regions2)

us_west = us_states[us_states$REGION == "West", ]
us_west_union = st_union(us_west)
plot(us_west_union)

texas = us_states[us_states$NAME == "Texas", ]
texas_union = st_union(us_west_union, texas)
plot(texas_union)

# Type transformation
# Stworzenie obiektu typu multipoint
multipoint = st_multipoint(matrix(c(1, 3, 5, 1, 3, 1), ncol = 2))

# Transformacja multipoint na linestring i poligon
linestring = st_cast(multipoint, "LINESTRING")
polyg = st_cast(multipoint, "POLYGON")

# Transformacja z linestring, polygon na multipoint
multipoint_2 = st_cast(linestring, "MULTIPOINT")
multipoint_3 = st_cast(polyg, "MULTIPOINT")
all.equal(multipoint, multipoint_2)
all.equal(multipoint, multipoint_3)

# Stworzenie obiektu typu multilinestring
multilinestring_list = list(
    matrix(c(1, 4, 5, 3), ncol = 2),
    matrix(c(4, 4, 4, 1), ncol = 2), 
    matrix(c(2, 4, 2, 2), ncol = 2))
multilinestring = st_multilinestring(multilinestring_list)
multilinestring_sf = st_sf(geom = st_sfc(multilinestring))

# Rozdzielenie 3 linii na osobne obiekty
linestring_sf2 = st_cast(multilinestring_sf, "LINESTRING")

# Dodanie atrybutów do linii i obliczenie ich długości
linestring_sf2$name = c("Riddle Rd", "Marshall Ave", "Foulke St")
linestring_sf2$length = st_length(linestring_sf2)
linestring_sf2

### Operacje geometryczne na rastrach
## Przecięcie
elev = rast(system.file("raster/elev.tif", package = "spData"))
clip = rast(xmin = 0.9, xmax = 1.8, ymin = -0.45, ymax = 0.45, resolution = 0.3, vals = rep(1, 9))
elev[clip, drop = FALSE]

## Zakres
# Dodanie kolumny i wieresza do rastra
elev = rast(system.file("raster/elev.tif", package = "spData"))
elev_2 = extend(elev, c(1, 2))
plot(elev)

elev_3 = elev + elev_2

# Dopasowanie rastra na podstawie już istniejącego
elev_4 = extend(elev, elev_2)

# punkt początkowy rastra
origin(elev_4)
origin(elev_4) = c(0.26, 0.25)

## zmiana rozdzielczości
dem = rast(system.file("raster/dem.tif", package = "spDataLarge"))
dem_agg = aggregate(dem, fact = 5, fun = mean)

dem_disagg = disagg(dem_agg, fact = 5, method = "bilinear")
identical(dem, dem_disagg)

## resampling

# Stworzenie rastra
target_rast <- rast(
    xmin = 794650, xmax = 798250,
    ymin = 8931750, ymax = 8935350,
    resolution = 300, crs = "EPSG:32717"
)

dem_resampl = resample(dem, y = target_rast, method = "bilinear")

### Raster-wektor powiązanie
library(sf)
library(terra)
library(dplyr)
library(spDataLarge)

## przycinanie rastra do zasięgu wektora

# Wczytanie danych i transformacja do odpowiedniego układu współrzędnych
srtm = rast(system.file("raster/srtm.tif", package = "spDataLarge"))
zion = read_sf(system.file("vector/zion.gpkg", package = "spDataLarge"))
zion = st_transform(zion, st_crs(srtm))

# Przycięcie rastra
srtm_cropped = crop(srtm, zion)

# Maskowanie, piksele poza zasięgiem przyjmują wartość NA
srtm_masked = mask(srtm, zion)

srtm_cropped = crop(srtm, zion)
srtm_final = mask(srtm_cropped, zion)

# Odwrócenie maskowania
srtm_inv_masked = mask(srtm, zion, inverse = TRUE)

## Ekstrakcja rastra
# Sampling rastra punktami
data("zion_points", package = "spDataLarge")
elevation = terra::extract(srtm, zion_points)
zion_points = cbind(zion_points, elevation)

# Sampling linią
zion_transect <- cbind(c(-113.2, -112.9), c(37.45, 37.2)) |>
    st_linestring() |>
    st_sfc(crs = crs(srtm)) |>
    st_sf(geometry = _)

# Stworzenie punktów w bezpośrednim sąsiedztwie linii
zion_transect$id = 1:nrow(zion_transect)
zion_transect = st_segmentize(zion_transect, dfMaxLength = 250)
zion_transect = st_cast(zion_transect, "POINT")

zion_transect = zion_transect %>%
    group_by(id) %>%
    mutate(dist = st_distance(geometry)[, 1])

zion_elev = terra::extract(srtm, zion_transect)
zion_transect = cbind(zion_transect, zion_elev)

# Sampling poligonem
zion_srtm_values = terra::extract(x = srtm, y = zion)

group_by(zion_srtm_values, ID) %>% summarize(across(srtm, list(min = min, mean = mean, max = max)))

# Raster kategoryzowany
nlcd = rast(system.file("raster/nlcd.tif", package = "spDataLarge"))
zion2 = st_transform(zion, st_crs(nlcd))
zion_nlcd = terra::extract(nlcd, zion2)
zion_nlcd %>%
    group_by(ID, levels) %>%
    count()


## Rasteryzacja
cycle_hire_osm = spData::cycle_hire_osm
cycle_hire_osm_projected = st_transform(cycle_hire_osm, "EPSG:27700")
raster_template = rast(ext(cycle_hire_osm_projected), resolution = 1000, crs = crs(cycle_hire_osm_projected))

# Tworzenie rastra z wektora, czy wektor istnieje w danym miejscu czy nie
ch_raster1 = rasterize(cycle_hire_osm_projected, raster_template)
((ch_raster1))
# Zliczenie występień wektora w komórce rastra
ch_raster2 = rasterize(cycle_hire_osm_projected, raster_template, fun="length")
plot(ch_raster2)

# Rasteryzacja na podstawie atrybutu
ch_raster3 = rasterize(cycle_hire_osm_projected, raster_template, field = "capacity", fun = sum, na.rm = TRUE)
plot(ch_raster3)

# Rasteryzacja linii
california = dplyr::filter(us_states, NAME == "California")
california_borders = st_cast(california, "MULTILINESTRING")
raster_template2 = rast(ext(california), resolution = 0.5, crs = st_crs(california)$wkt)

california_raster1 = rasterize(california_borders, raster_template2, touches = TRUE)

# Rasteryzacja poligonowa
california_raster2 = rasterize(california, raster_template2)
plot(california_raster2)

## Wektoryzacja
# Zamiana rastra na wektor - punkt
elev = rast(system.file("raster/elev.tif", package = "spData"))
elev_point = as.points(elev) %>% st_as_sf()
plot(elev_point)

# Zamiana rastra na linie
dem = rast(system.file("raster/dem.tif", package = "spDataLarge"))
cl = as.contour(dem) %>% st_as_sf()
plot(dem, axes = FALSE)
plot(cl, add = TRUE)

# Zamiana rastra na poligon
grain = rast(system.file("raster/grain.tif", package = "spData"))
grain_poly = as.polygons(grain) %>% st_as_sf()
plot(grain_poly)

### Zmiana układów współrzędnych
library(sf)
library(terra)
library(dplyr)
library(spData)
library(spDataLarge)
library(stars)

## Układ współrzędnych (CRS)
st_crs("EPSG:4326")
st_crs("ESRI:54030")

## Ustawienie systemu współrzędnych
# Ustawienie dla wektorów
vector_filepath = system.file("shapes/world.gpkg", package = "spData")
new_vector = read_sf(vector_filepath)
st_crs(new_vector)
st_crs(new_vector)$IsGeographic
st_crs(new_vector)$units_gdal
st_crs(new_vector)$srid
st_crs(new_vector)$proj4string

new_vector = st_set_crs(new_vector, "EPSG:4326")

# Ustawienie dla rastrów
raster_filepath = system.file("raster/srtm.tif", package = "spDataLarge")
my_rast = rast(raster_filepath)
cat(crs(my_rast))
crs(my_rast) = "EPSG:26912"

# Nieznany crs
london = data.frame(lon = -0.1, lat = 51.5) %>% st_as_sf(coords = c("lon", "lat"))
st_is_longlat(london)

london_geo = st_set_crs(london, "EPSG:4326")
st_is_longlat(london_geo)


## Operacje geometryczne na danych o nadanym układzie i nie

# Stworzenie trzech buforów wokół Londyna
london_buff_no_crs = st_buffer(london, dist = 1)
london_buff_s2 = st_buffer(london_geo, dist = 100000)
london_buff_s2_100_cells = st_buffer(london_geo, dist = 100000, max_cells = 100)

sf::sf_use_s2(FALSE)
london_buff_lonlat = st_buffer(london_geo, dist = 1)
sf::sf_use_s2(TRUE)

london_proj = data.frame(x = 530000, y = 180000) %>%
    st_as_sf(coords = c("x", "y"), crs = "EPSG:27700")

london_buff_projected = st_buffer(london_proj, 100000)

## Kiedy przeprowadzać transformację współrzędnych?
st_distance(london_geo, london_proj)

# UTM code
lonlat2UTM = function(lonlat) {
    utm = (floor((lonlat[1] + 180) / 6) %% 60) + 1
    if (lonlat[2] > 0) {
        utm + 32600
    } else {
        utm + 32700
    }
}

lonlat2UTM(c(174.7, -36.9))
lonlat2UTM(st_coordinates(london))

## Reprojekcja wektorów
london2 = st_transform(london_geo, "EPSG:27700")

st_distance(london2, london_proj)

st_crs(cycle_hire_osm)

crs_lnd = st_crs(london_geo)
class(crs_lnd)
names(crs_lnd)

crs_lnd$Name
crs_lnd$proj4string
crs_lnd$epsg

cycle_hire_osm_projected = st_transform(cycle_hire_osm, "EPSG:27700")
st_crs(cycle_hire_osm_projected)

crs_lnd_new = st_crs("EPSG:27700")
crs_lnd_new$Name
crs_lnd_new$proj4string
crs_lnd_new$epsg

## Reprojekcja rastrów
cat_raster = rast(system.file("raster/nlcd.tif", package = "spDataLarge"))
plot(cat_raster)
crs(cat_raster)
unique(cat_raster)

# reprojekcja kategorii
cat_raster_wgs84 = project(cat_raster, "EPSG:4326", method = "near")

# reprojekcja rastra numeric
con_raster = rast(system.file("raster/srtm.tif", package = "spDataLarge"))
plot(con_raster)

con_raster_ea = project(con_raster, "EPSG:32612", method = "bilinear")
cat(crs(con_raster_ea))

## Customowe układy
zion = read_sf(system.file("vector/zion.gpkg", package = "spDataLarge"))
plot(zion)

zion_centr = st_centroid(zion)
zion_centr_wgs84 = st_transform(zion_centr, "EPSG:4326")
st_as_text(st_geometry(zion_centr_wgs84))

my_wkt = 'PROJCS["Custom_AEQD",
 GEOGCS["GCS_WGS_1984",
  DATUM["WGS_1984",
   SPHEROID["WGS_1984",6378137.0,298.257223563]],
  PRIMEM["Greenwich",0.0],
  UNIT["Degree",0.0174532925199433]],
 PROJECTION["Azimuthal_Equidistant"],
 PARAMETER["Central_Meridian",-113.0263],
 PARAMETER["Latitude_Of_Origin",37.29818],
 UNIT["Meter",1.0]]'

 zion_aeqd = st_transform(zion, my_wkt)

world_mollweide = st_transform(world, crs = "+proj=moll")
plot(world_mollweide)

world_wintri = st_transform(world, crs = "+proj=wintri")

world_laea2 = st_transform(world, crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-74 +lat_0=40")
plot(world_laea2)


### Geographic data I/O
library(sf)
library(terra)
library(dplyr)
library(spData)

## File formats

## Data input
sf_drivers = st_drivers()
head(sf_drivers, n = 3)
summary(sf_drivers[-c(1:2)])

f = system.file("shapes/world.gpkg", package = "spData")
world = read_sf(f)
plot(world[1])

tanzania = read_sf(f, query='SELECT * FROM world WHERE name_long = "Tanzania"')
plot(tanzania[1])

tanzania_buf = st_buffer(tanzania, 50000)
tanzania_buf_geom = st_geometry(tanzania_buf)
tanzania_buf_wkt = st_as_text(tanzania_buf_geom)
tanzania_neigh = read_sf(f, wkt_filter = tanzania_buf_wkt)
plot(tanzania_neigh[1])

cycle_hire_txt = system.file("misc/cycle_hire_xy.csv", package = "spData")
cycle_hire_xy = read_sf(cycle_hire_txt, options = c("X_POSSIBLE_NAMES=X", "Y_POSSIBLE_NAMES=Y"))

world_txt = system.file("misc/world_wkt.csv", package = "spData")
world_wkt = read_sf(world_txt, options = "GEOM_POSSIBLE_NAMES=WKT")
plot(world_wkt[1])

u = "https://developers.google.com/kml/documentation/KML_Samples.kml"
download.file(u, "KML_Samples.kml")
st_layers("KML_Samples.kml")
kml = read_sf("KML_Samples.kml", layer = "Google Campus")
plot(kml[1])

raster_filepath = system.file("raster/srtm.tif", package = "spDataLarge")
single_layer = rast(raster_filepath)
plot(single_layer)

multilayer_filepath = system.file("raster/landsat.tif", package = "spDataLarge")
multilayer_rast = rast(multilayer_filepath)
plot(multilayer_rast)

myurl = paste0(
    "/vsicurl/https://zenodo.org/record/5774954/files/",
    "clm_snow.prob_esacci.dec_p.90_500m_s0..0cm_2000..2012_v2.0.tif")
snow = rast(myurl)

rey = data.frame(lon = -21.94, lat = 64.15)
snow_rey = extract(snow, rey)

write_sf(obj = world, dsn = "world.gpkg")

write_sf(obj = world, dsn = "world_many_layers.gpkg", layer = "second_layer")

st_write(obj = world, dsn = "world2.gpkg")

write_sf(cycle_hire_xy, "cycle_hire_xy.csv", layer_options = "GEOMETRY=AS_XY")
write_sf(world_wkt, "world_wkt.csv", layer_options = "GEOMETRY=AS_WKT")

writeRaster(single_layer, filename = "my_raster.tif", datatype = "INT2U")

writeRaster(x = single_layer, filename = "my_raster.tif", gdal = c("COMPRESS=NONE"), overwrite=TRUE)
writeRaster(x = single_layer, filename = "my_raster.tif", filetype = "COG", overwrite = TRUE)

download.file(url = "https://hs.pangaea.de/Maps/PeRL/PeRL_permafrost_landscapes.zip", destfile = "PeRL_permafrost_landscapes.zip", mode="wb")
unzip("PeRL_permafrost_landscapes.zip")
canada_perma_land <- read_sf("PeRL_permafrost_landscapes/canada_perma_land.shp")

library(rnaturalearth)
usa_sf = ne_countries(country = "United States of America", returnclass = "sf")
plot(usa_sf[1])

library(geodata)
worldclim_prec = worldclim_global("prec", res = 10, path = tempdir())
class(worldclim_prec)

library(osmdata)
parks = opq(bbox = "leeds uk") %>%
    add_osm_feature(key = "leisure", value = "park") %>%
    osmdata_sf()

world2 = spData::world
world3 = read_sf(system.file("shapes/world.gpkg", package = "spData"))

library(tidygeocoder)
geo_df = data.frame(address = "54 Frith St, London W1D 4SJ, UK")
geo_df = geocode(geo_df, address, method = "osm")
geo_df
geo_sf = st_as_sf(geo_df, coords = c("long", "lat"), crs = "EPSG:4326")
plot(geo_sf)

library(geometa)
md <- ISOMetadata$new()
md$validate()
xml = md$encode
md$save("my_metadata.xml")
md = readISO19139("my_metadata.xml")

library(httr)
base_url = "https://www.fao.org"
endpoint = "/fishery/geoserver/wfs"
q = list(request = "GetCapabilities")
res = GET(url = modify_url(base_url, path = endpoint), query = q)
res$url

txt = content(res, "text")
xml = xml2::read_xml(txt)
xml

library(sf)
sf::sf_use_s2(FALSE)
qf = list(request = "GetFeature", typeName = "fifao:FAO_MAJOR")
file = tempfile(fileext = ".gml")
GET(url = base_url, path = endpoint, query = qf, write_disk(file))
fao_areas = read_sf(file)

library(ows4R)

WFS <- WFSClient$new(
    url = "https://www.fao.org/fishery/geoserver/wfs",
    serviceVersion = "1.0.0",
    logger = "INFO"
)

caps = WFS$getCapabilities
features = WFS$getFeatures("fifao:FAO_MAJOR")

png(filename = "lifeExp.png", width = 500, height = 350)
plot(world["lifeExp"])
dev.off()

library(tmap)
tmap_obj = tm_shape(world) + tm_polygons(col = "lifeExp")
tmap_save(tmap_obj, filename = "lifeExp_tmap.png")

library(mapview)
mapview_obj = mapview(world, zcol = "lifeExp", legend = TRUE)
mapshot2(mapview_obj, url = "my_interactive_map.html")

###Making maps
library(sf)
library(terra)
library(dplyr)
library(spData)
library(spDataLarge)

install.packages("tmap", repos = c(
    "https://r-tmap.r-universe.dev",
    "https://cloud.r-project.org"
))

library(tmap)
library(leaflet)
library(ggplot2)

nz_elev = rast(system.file("raster/nz_elev.tif", package = "spDataLarge"))

tm_shape(nz) + tm_fill()
tm_shape(nz) + tm_borders()
tm_shape(nz) + tm_fill() + tm_borders()

map_nz = tm_shape(nz) + tm_polygons()
class(map_nz)

map_nz1 = map_nz + tm_shape(nz_elev) + tm_raster(col_alpha = 0.7)

nz_water = st_union(nz) %>%
    st_buffer(22200) %>%
    st_cast(to = "LINESTRING")
map_nz2 = map_nz1 + tm_shape(nz_water) + tm_lines()

map_nz3 = map_nz2 + tm_shape(nz_height) + tm_symbols()

tmap_arrange(map_nz1, map_nz2, map_nz3)

ma1 = tm_shape(nz) + tm_polygons(fill = "red")
ma2 = tm_shape(nz) + tm_polygons(fill = "red", fill_alpha = 0.3)
ma3 = tm_shape(nz) + tm_polygons(col = "blue")
ma4 = tm_shape(nz) + tm_polygons(lwd = 3)
ma5 = tm_shape(nz) + tm_polygons(lty = 2)
ma6 = tm_shape(nz) + tm_polygons(fill = "red", fill_alpha = 0.3, col = "blue", lwd = 3, lty = 2)
tmap_arrange(ma1, ma2, ma3, ma4, ma5, ma6)

plot(st_geometry(nz), col = nz$Land_area)
tm_shape(nz) + tm_fill(fill = nz$Land_area)

tm_shape(nz) + tm_fill(fill = "Land_area")

tm_shape(nz) + tm_polygons(fill = "Median_income")
tm_shape(nz) + tm_polygons(fill = "Median_income", fill.scale = tm_scale(breaks = c(0, 30000, 40000, 50000)))
tm_shape(nz) + tm_polygons(fill = "Median_income", fill.scale = tm_scale(n = 10))
tm_shape(nz) + tm_polygons(fill = "Median_income", fill.scale = tm_scale(values = "BuGn"))

tm_shape(nz) + tm_polygons("Median_income", fill.scale = tm_scale(values = "greens"))
tm_shape(nz) + tm_polygons("Median_income", fill.scale = tm_scale(values = "yl_gn_bu"))

tm_shape(nz) + tm_polygons("Median_income", fill.scale = tm_scale_continuous(values = "pu_gn_div", midpoint = 28000))

legend_title = expression("Area (km"^2*")")
tm_shape(nz) + tm_polygons(fill = "Land_area", fill.legend = tm_legend(title = legend_title))

tm_shape(nz) + tm_polygons(fill = "Land_area", fill.legend = tm_legend(title = legend_title, orientation = "landscape", position = tm_pos_out("center", "bottom")))

map_nz + tm_graticules() + tm_compass(type = "8star", position = c("left", "top")) + tm_scalebar(breaks = c(0, 100, 200), text.size = 1, position = c("left", "top")) + tm_title("New Zealand")

map_nz + tm_layout(scale = 4)
map_nz + tm_layout(bg.color = "lightblue")
map_nz + tm_layout(frame = FALSE)

urb_1970_2030 = urban_agglomerations %>% filter(year %in% c(1970, 1990, 2010, 2030))
tm_shape(world) + tm_polygons() + tm_shape(urb_1970_2030) + tm_symbols(fill = "black", col = "white", size = "population_millions") + tm_facets_wrap(by = "year", nrow = 2)

nz_region = st_bbox(c(xmin = 1340000, xmax = 1450000, ymin = 5130000, ymax = 5210000), crs = st_crs(nz_height)) %>% st_as_sfc()

nz_height_map = tm_shape(nz_elev, bbox = nz_region) + tm_raster(col.scale = tm_scale_continuous(values = "YlGn"), col.legend = tm_legend(position = c("left", "top"))) + tm_shape(nz_height) + tm_symbols(shape = 2, col = "red", size = 1) + tm_scalebar(position = c("left", "bottom"))

nz_map = tm_shape(nz) + tm_polygons() + tm_shape(nz_height) + tm_symbols(shape = 2, col = "red", size = 0.1) + tm_shape(nz_region) + tm_borders(lwd = 3) + tm_layout(bg.color = "lightblue")

library(grid)
norm_dim = function(obj){
    bbox = st_bbox(obj)
    width = bbox[["xmax"]] - bbox[["xmin"]]
    height = bbox[["ymax"]] - bbox[["ymin"]]
    w = width / max(width, height)
    h = height / max(width, height)
    return(unit(c(w, h), "snpc"))
}
main_dim = norm_dim(nz_region)
ins_dim = norm_dim(nz)

main_vp = viewport(width = main_dim[1], height = main_dim[2])

ins_vp = viewport(width = ins_dim[1] * 0.5, height = ins_dim[2] * 0.5, x = unit(1, "npc") - unit(0.5, "cm"), y = unit(0.5, "cm"), just = c("right", "bottom"))

grid.newpage()
print(nz_height_map, vp = main_vp)
pushViewport(main_vp)
print(nz_map, vp = ins_vp)

us_states_map = tm_shape(us_states, crs = "EPSG:9311") + tm_polygons() + tm_layout(frame = FALSE)

hawaii_map = tm_shape(hawaii) + tm_polygons() + tm_title("Hawaii") + tm_layout(frame = FALSE, bg.color = "black")
alaska_map = tm_shape(alaska) + tm_polygons() + tm_title("Alaska") + tm_layout(frame = FALSE, bg.color = "black")

us_states_map
print(hawaii_map, vp = grid::viewport(0.45, 0.1, width = 0.2, height = 0.1))
print(alaska_map, vp = grid::viewport(0.15, 0.15, width = 0.3, height = 0.3))

urb_anim = tm_shape(world) + tm_polygons() + tm_shape(urban_agglomerations) + tm_symbols(size = "population_millions") + tm_facets_wrap(by = "year", nrow = 1, ncol = 1, free.coords = FALSE)
tmap_animation(urb_anim, filename = "urb_anim.gif", delay = 2)

tmap_mode("view")
map_nz

map_nz + tm_basemap(server = "OpenTopoMap")

world_coffee = left_join(world, coffee_data, by = "name_long")
facets = c("coffee_production_2016", "coffee_production_2017")
tm_shape(world_coffee) + tm_polygons(facets) + tm_facets_wrap(nrow = 1, sync = TRUE)

tmap_mode("plot")
mapview::mapview(nz)

library(mapview)
oberfranken = subset(franconia, district == "Oberfranken")
trails %>%
    st_transform(st_crs(oberfranken)) %>%
    st_intersection(oberfranken) %>%
    st_collection_extract("LINESTRING") %>%
    mapview(color = "red", lwd = 3, layer.name = "trails") +
    mapview(franconia, zcol = "district") +
    breweries

library(mapdeck)
set_token(Sys.getenv("MAPBOX"))
crash_data = read.csv("https://git.io/geocompr-mapdeck")
crash_data = na.omit(crash_data)
ms = mapdeck_style("dark")
mapdeck(style = ms, pitch = 45, location = c(0, 52), zoom = 4) %>%
    add_grid(data = crash_data, lat = "lat", lon = "lng", cell_size = 1000, elevation_scale = 50, colour_range = hcl.colors(6, "plasma"))

pal = colorNumeric("RdYlBu", domain = cycle_hire$nbikes)
leaflet(data = cycle_hire) |>
    addProviderTiles(providers$CartoDB.Positron) |>
    addCircles(col = ~ pal(nbikes), opacity = 0.9) |>
    addPolygons(data = lnd, fill = FALSE) |>
    addLegend(pal = pal, values = ~nbikes) |>
    setView(lng = -0.1, 51.5, zoom = 12) |>
    addMiniMap()


library(shiny)
library(leaflet)
library(spData)
ui = fluidPage(
    sliderInput(inputId = "life", "Life expectancy", 49, 84, value = 80),
    leafletOutput(output = "map")
)
server = function(input, output) {
    output$map = renderLeaflet({
        leaflet() %>%
            # addProviderTiles("OpenStreetMap.BlackAndWhite") %>%
            addPolygons(data = world[world$lifeExp < input$life, ])
    })
}
shinyApp(ui, server)

