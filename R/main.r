# 1. GADM Boundaries
#-------------------
setwd("D:/Downloads")
install.packages(pacman)

# install geodata and load packages

remotes::install_github(
    "ropensci/geodata"
)

pacman::p_load(geodata, tidyverse, sf)

# fetch country polygon

brazil <- geodata::gadm(
    country = "BRA",
    level = 1,
    path = tempdir()
) >
sf::st_as_sf()

# if it throws an error try this instead
brazil <- geodata::gadm(
    country = "BRA",
    level = 1,
    path = tempdir()
) 

brazil <- sf::st_as_sf(brazil)

#save the shapefile to your local drive

sf::st_write(
    obj = brazil,
    "brazil_sf.shp"
)

# quick and dirty plot

ggplot(data = brazil) +
geom_sf(
    fill = "dodgerblue",
    color = "white"
) +
theme_minimal() +
labs(title = "Brazil Administrative Boundaries")

# 2. OpenStreetMap (OSM)
#-----------------------

#install osmdata and load packages

remotes::install_github(
    "ropensci/osmdata"
)

pacman::p_load(osmdata, tidyverse)

# define area of interest

bbox <- osmdata::getbb(
    "Amsterdam, Netherlands"
)

# fetch OSM road data

roads_data <- osmdata::opq(bbox) |>
    osmdata::add_osm_feature(
        key = "highway",
        value = c(
            "motorway", "primary",
            "secondary"
        )
    ) |> osmdata::osmdata_sf()

roads <- roads_data$osm_lines

ggplot(data = roads) +
geom_sf(color = "deeppink1") +
theme_void() +
labs(title = "Amsterdam Major Roads")

# 3. ESA World Cover 2021
#------------------------

#install rstac and load the necessary packages

pacman::p_load(
    rstac, geodata, sf
)

# fetch the polygon of interest

bangladesh <- geodata::gadm(
    country = "BGD",
    level = 0,
    path = tempdir()
) |>
sf::st_as_sf()

# if it throws an error try this instead
bangladesh <- geodata::gadm(
    country = "BGD",
    level = 0,
    path = tempdir()
)

bangladesh <- sf::st_as_sf(bangladesh)

#define area of interest based on the polygon

bbox <- sf::st_bbox(bangladesh)

# query

ms_query <- rstac::stac("https://planetarycomputer.microsoft.com/api/stac/v1")

# search for ESA World cover 2021 in the collection

ms_esa_query <- rstac::stac_search(
    q = ms_query,
    collections = "esa-worldcover",
    datetime = "2021-01-01T00:00:00Z/2021-12-31T23:59:59Z",
    bbox = bbox
) |>
rstac::get_request()

#sign in to the MS Planetary Computer

ms_query_signin <- rstac::items_sign(
    ms_esa_query, rstac::sign_planetary_computer()
)

# Download ESA World Cover 2021 for the extent of Bangladesh

rstac::assets_download(
    items = ms_query_signin,
    asset_names = "map",
    output_dir = tempdir(),
    overwrite = TRUE
)

# 4. Natural Earth 
#-----------------

#install rnaturalearth and load the necessary packages

pacman::p_load(
    rnaturalearth, tidyverse
)

# get lakes

world <- rnaturalearth::ne_download(
    scale = 10,
    type = "lakes",
    category = "physical",
    returnclass = "sf"
)

#plot

ggplot(data = world) +
  geom_sf(color = "dodgerblue") +
  theme_void() +
  labs(title = "World Map with Natural Earth Data")


# 5. AWS Terrain Tiles digital elevation model 
#---------------------------------------------

#install elevatr and load the necessary packages
pacman::p_load(
    elevatr, geodata, tidyverse,
    terra
)

# get area of interest
locations <- geodata::gadm(
    country = "CHE",
    level = 0,
    path = tempdir()
) |>
sf::st_as_sf()

# if it throws an error try this instead
locations <- geodata::gadm(
    country = "CHE",
    level = 0,
    path = tempdir()
)

locations <- sf::st_as_sf(locations)

# fetch DEM
elevation_country <- elevatr::get_elev_raster(
    locations = locations, 
    z = 8, clip = "locations")

# plot
terra::plot(elevation_country)

# 6. Sentinel-2 Imagery
#----------------------

# install and load packages
pacman::p_load(
    rsi, sf, terra
)

# Define area of interest (AOI)
aoi <- sf::st_point(
    c(
        14.0951214, 46.3682864
    )
) |>
sf::st_sfc(crs = "EPSG:4326") |>
sf::st_transform(crs = "EPSG:8857") |>
sf::st_buffer(2000)

# Fetch Sentinel-2 imagery
output_filename <- "bled_august2024.tif"
sentinel_data <- rsi::get_sentinel2_imagery(
  aoi = aoi,
  start_date = "2024-08-01",
  end_date = "2024-08-10",
  output_filename = output_filename)

sentinel_raster <- terra::rast(
    output_filename 
    )

# Normalize all channels to 0-255 in one step
normalize <- function(x) {
  if (all(is.na(x))) return(x)  # Avoid processing empty rasters
  x <- (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) * 255
  return(round(x))
}

# Apply normalization across all bands
sentinel_raster_norm <- terra::app(
    sentinel_raster, normalize)

# Plot with proper scaling
terra::plotRGB(
    sentinel_raster_norm, r = 4, 
    g = 3, b = 2)

# 7. WorldClim data
#------------------

# install and load packages
pacman::p_load(
    geodata, tidyverse
)

# Fetch climate data
avg_temperature <- geodata::worldclim_country(
    country = "TUR",
    var = "tavg", res = .5, 
    path = tempdir()
    )

class(avg_temperature)

# Plot temperature data
ggplot() +
tidyterra::geom_spatraster(
    data = avg_temperature[[12]]) +
scale_fill_gradientn(
    colors = rainbow(16, rev = TRUE)
) +
  theme_minimal() +
  labs(
    title = "Average Temperature (WorldClim)", 
    fill = "Temp (°C)")

# 8. Global Biodiversity Information Facility
#--------------------------------------------

# install and load packages
pacman::p_load(
    rgbif, tidyverse
)

# Fetch species occurrence data
occurrences <- rgbif::occ_search(
    scientificName = "Panthera leo", 
    limit = 500)

# Visualize occurrences on a map
ggplot(
    data = occurrences$data) +
  geom_point(
    aes(
    x = decimalLongitude, 
    y = decimalLatitude
    ), 
    color = "blue") +
  theme_minimal() +
  labs(
    title = "GBIF Species Occurrences: Panthera leo", 
    x = "Longitude", y = "Latitude")

# 9. Tree Height
#---------------

# install and load packages
remotes::install_github(
    "TESS-Laboratory/chmloader"
)

pacman::p_load(
    chmloader, terra
)

# area of interest
lat <- 1.3615617
long <- 103.7517751

coords <- sf::st_point(
    c(long, lat)
) |>
sf::st_sfc(crs = 4326) |>
sf::st_buffer(
    dist = units::set_units(
        2, km
    )
)

# Fetch tree height data
chm <- chmloader::download_chm(
    coords,
    filename = "chm.tif"
)

# Plot tree height data

terra::plot(
    chm,
    col = hcl.colors(
        12, "viridis"
    )
)

# 10. Population density
#-----------------------

# install and load packages
remotes::install_github(
    "wpgp/wpgpDownloadR"
)

pacman::p_load(
    wpgpDownloadR, terra
)

# download 2020 population density

pop_data <- wpgpDownloadR::wpgpGetCountryDataset(
    ISO3 = "NPL", # Nepal
    covariate = "ppp_2020",
    destDir = tempdir())

# load the raster

pop <- terra::rast(pop_data)

# plot
terra::plot(pop)
