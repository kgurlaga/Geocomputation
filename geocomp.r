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
