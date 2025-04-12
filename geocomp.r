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
