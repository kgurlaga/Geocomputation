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
