# Load packages ----------
library(elevatr)
library(sf)
library(rayshader)
library(MetBrewer)
library(magick)
library(ggplot2)
library(glue)
library(scales)
library(grid)
library(spData)

# Load and prepare data ----------
# Read in GCNP boundaries
data <- st_read("C:/Users/frede/rayshader_tutorial-main/data/grca_tracts/GRCA_boundary.shp")

# Query for elevation data, clipping with the GCNP boundaries
gcnp_elev <- get_elev_raster(data, z = 10, clip = "location")

# Convert the returned raster to a matrix
mat <- raster_to_matrix(gcnp_elev)

# Set up aspect ratio and color palette ----------
w <- nrow(mat)
h <- ncol(mat)

wr <- w / max(c(w, h))
hr <- h / max(c(w, h))

if (min(c(wr, hr)) < .75) {
  if (wr < .75) {
    wr <- .75
  } else {
    hr <- .75
  }
}

pal <- "Demuth"
colors <- met.brewer(pal)

# Render in 3D ----------
rgl::rgl.close()  # Close any open rgl windows

mat %>%
  height_shade(texture = grDevices::colorRampPalette(colors)(256)) %>%
  plot_3d(
    heightmap = mat, 
    solid = FALSE, 
    zscale = 10, 
    windowsize = c(800 * wr, 800 * hr), 
    phi = 90, 
    zoom = .7, 
    theta = 0
  )

# Render high-resolution image ----------
render_highquality(
  "C:/Users/frede/rayshader_tutorial-main/plots/gcnp_highres.png", 
  parallel = TRUE, 
  samples = 300, 
  light = FALSE, 
  interactive = FALSE, 
  environment_light = "C:/Users/frede/rayshader_tutorial-main/env/phalzer_forest_01_4k.hdr", 
  intensity_env = 1.5, 
  rotate_env = 180, 
  width = round(6000 * wr), 
  height = round(6000 * hr)
)

# Add annotations ----------
text_color <- colors[1]

# Check if the high-res image exists
image_path <- "C:/Users/frede/rayshader_tutorial-main/plots/gcnp_highres.png"
if (!file.exists(image_path)) stop("High-res image not found. Ensure rendering completed successfully.")

# Load the high-res image
img <- image_read(image_path)

# Add title

# Title 'A PORTRAIT OF'
img_ <- image_annotate(img, "A Portrait of", font = "Cinzel Decorative",
                       color = text_color, size = 125, gravity = "north",
                       location = "+0+200")
# Title NAME
img_ <- image_annotate(img_, "Grand Canyon National Park", weight = 700, 
                       font = "Cinzel Decorative", location = "+0+400",
                       color = text_color, size = 200, gravity = "north")


img_ <- image_annotate(img_, "Created by Frederic Fery @fredffery | Tutorial by Spencer Schien @MrPecners", font = "Cinzel Decorative", 
                       color = text_color, size = 50, gravity = "south", 
                       location = "+0+400")




# Add area and elevation annotations
area <- as.numeric(st_area(data)) / 2.59e6  # Area in square miles
elev_range <- (max(mat, na.rm = TRUE) - min(mat, na.rm = TRUE)) * 3.281  # Elevation range in feet

img_ <- image_annotate(img_, glue("Area: {label_comma()(round(area))} sq mi"), 
                       font = "Cinzel Decorative", location = "+1200-1000", 
                       color = text_color, size = 110, gravity = "west")
img_ <- image_annotate(img_, glue("Elevation Range: {label_comma()(round(elev_range))} ft"), 
                       font = "Cinzel Decorative", location = "+1200-1300", 
                       color = text_color, size = 110, gravity = "west")

# Create and add inset map ----------
spot <- st_buffer(st_centroid(data), 100000)
loc_plot <- ggplot() + 
  geom_sf(data = us_states, fill = "transparent", color = text_color, size = 0.2) + 
  geom_sf(data = spot, fill = NA, color = colors[2]) + 
  theme_void() + 
  coord_sf(crs = 3347)

ggsave(loc_plot, filename = glue("C:/Users/frede/rayshader_tutorial-main/plots/gcnp_inset.png"), 
       width = 6, height = 4.5)

inset <- image_read("C:/Users/frede/rayshader_tutorial-main/plots/gcnp_inset.png")
new_inset <- image_scale(inset, "x1000")
img_ <- image_composite(img_, new_inset, gravity = "east", offset = "+1200-1000")

# Save final annotated image ----------
output_path <- "C:/Users/frede/rayshader_tutorial-main/plots/gcnp_fully_annotated.png"
image_write(img_, output_path)



cat("Final annotated image saved to:", output_path, "\n")
