# Population_Hillshade
# ── Libraries ───────────────────────────────────────────────

# Only install if missing
if (!require("pacman")) install.packages("pacman")

# Load all needed packages at once
pacman::p_load(
  terra, elevatr, tidyverse,
  ggnewscale, ggspatial,
  geodata, sf, scales
)

# ── 1) Load Switzerland Boundary ─────────────────────────────

country_sf <- geodata::gadm(
  country = "CHE", level = 0,
  path = tempdir()
) |> sf::st_as_sf()

country_vect <- terra::vect(country_sf)

# ── 2) Load WorldPop Population Raster (100 m, 2020) ─────────

pop_100m <- terra::rast(
  "https://data.worldpop.org/GIS/Population/Global_2000_2020_Constrained/2020/BSGM/CHE/che_ppp_2020_constrained.tif"
)

# Optional: quick check
# terra::plot(pop_100m)

# ── 3) Download DEM using elevatr and clip ───────────────────

dem <- elevatr::get_elev_raster(
  country_sf,
  z = 10, clip = "locations"
)

dem_country <- terra::rast(dem) |>
  terra::crop(country_vect) |>
  terra::mask(country_vect)

# Exaggerate elevation to emphasize terrain
dem_exaggerated <- dem_country * 1.3

# Create slope and aspect for hillshade
slope <- terra::terrain(dem_exaggerated, v = "slope", unit = "radians")
aspect <- terra::terrain(dem_exaggerated, v = "aspect", unit = "radians")

# Generate shaded relief
hillshade_raw <- terra::shade(
  slope, aspect,
  angle = 40, direction = 225
)

# ── 4) Resample population raster onto hillshade grid ───────

pop_on_hillshade <- terra::resample(
  pop_100m, hillshade_raw,
  method = "bilinear"
)

# Mask the hillshade where there is population
hillshade_no_pop <- terra::ifel(
  is.na(pop_on_hillshade), hillshade_raw, NA
)

# ── 5) Convert rasters to data frames for ggplot2 ────────────

hillshade_df <- terra::as.data.frame(
  hillshade_no_pop, xy = TRUE, na.rm = TRUE
)

pop_df <- terra::as.data.frame(
  pop_on_hillshade, xy = TRUE, na.rm = TRUE
)

# Clean small population values
pop_df$che_ppp_2020_constrained[
  pop_df$che_ppp_2020_constrained <= 0.1
] <- NA

# ── 6) Plot with ggplot2 ─────────────────────────────────────

brks <- c(1, 10, 100, 1e3) # legend breaks

p <- ggplot() +
  # a) Background hillshade (no population)
  geom_tile(data = hillshade_df, aes(x, y, fill = che_ppp_2020_constrained)) +
  scale_fill_gradient(
    low = "grey70", high = "grey10", guide = "none"
  ) +
  ggnewscale::new_scale_fill() +
  
  # b) Population layer
  geom_tile(data = pop_df, aes(x, y, fill = che_ppp_2020_constrained)) +
  scale_fill_viridis_c(
    name = "Population",
    option = "plasma", alpha = 1,
    begin = .2, end = 1,
    trans = "log10", breaks = brks,
    labels = scales::comma,
    guide = guide_colourbar(
      title.position = "top",
      barheight = unit(30, "mm"),
      barwidth = unit(2, "mm"),
      ticks.color = "grey10",
      frame.colour = "grey10"
    )
  ) +

  
  # c) Country boundary
  geom_sf(data = country_sf, fill = NA,
          color = "black", linewidth = .25) +
  # d) North arrow & scale
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    height = unit(10, "mm"), width = unit(10, "mm"),
    style = north_arrow_orienteering
  ) +
  annotation_scale(location = "br",
                   pad_y = unit(2, "mm"),
                   height = unit(2, "mm")) +
  coord_sf(expand = FALSE) +
  # e) Title, subtitle, etc.
  labs(
    title = "Switzerland · Population (2020)",
    subtitle = "WorldPop 100m constrained grid",
    caption = "Data: WorldPop · SRTM via elevatr | Design: Milos Makes Maps"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = .02),
    plot.subtitle = element_text(size = 14, hjust = .02),
    plot.caption = element_text(hjust = .5),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    legend.margin = margin(t = 0, r = 5, b = 0, l = 3),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
  ) +
  theme_void()

# ── 7) Save final map ────────────────────────────────────────

ggsave(
  "switzerland_population_relief.png",
  plot = p,
  width = 8, height = 5, dpi = 600,
  bg = "white"
)
print(p)  # Show the plot in the RStudio plot pane

ggsave(
  filename = "switzerland_population_relief.png",
  plot = p,
  width = 8, height = 5, dpi = 600,
  bg = "white"
)
