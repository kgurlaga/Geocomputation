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
