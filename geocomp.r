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