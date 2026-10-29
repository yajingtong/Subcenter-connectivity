#!/usr/bin/env Rscript
# Map completed universal-code outputs; computations are never redone from simplified display roads.
suppressPackageStartupMessages({library(sf); library(dplyr); library(readr); library(ggplot2)
  library(ggrepel); library(terra); library(patchwork)})
a <- grep("^--file=", commandArgs(), value = TRUE)
root <- if (length(a)) normalizePath(file.path(dirname(sub("^--file=", "", a[1])), "../..")) else getwd()
setwd(root)
cities <- commandArgs(trailingOnly = TRUE)
if (!length(cities)) cities <- c("Marseille", "Montpellier", "Toulouse", "Nantes")
stopifnot(!anyDuplicated(cities))
base <- "outputs/all_cities_gt5_local_node_connectivity"
out <- file.path(base, "four_city_density_maps")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
manifest <- read_csv("scripts/all_cities_local_node_connectivity/road_sources.csv", show_col_types = FALSE)
context <- "outputs/all_41_cities_buildings_subcentre_types_updated/all_41_cities_updated_functional_subcentres.gpkg"
boundaries <- st_read(context, layer = "insee_boundaries", quiet = TRUE)
centres <- st_read(context, layer = "urban_centres", quiet = TRUE)
objects <- list(); half_size <- 0
for (city in cities) {
  message("Reading completed node measures for ", city)
  source <- file.path(base, city, "subcentre_local_node_connectivity.gpkg")
  patches <- st_read(source, layer = "subcentre_connectivity", quiet = TRUE) |> st_transform(3035)
  boundary <- boundaries[boundaries$city == city, ] |> st_transform(3035)
  centre <- centres[centres$city == city, ] |> st_transform(3035)
  row <- manifest[manifest$city == city, ]
  stopifnot(nrow(row) == 1, nrow(centre) == 1, nrow(boundary) == 1, nrow(patches) > 5)
  study <- st_read(row$road_path, layer = "study_boundary", quiet = TRUE) |> st_transform(3035)
  cp <- st_coordinates(centre)[1, 1:2]
  bb <- st_bbox(c(st_geometry(boundary), st_geometry(study), st_geometry(patches)))
  half_size <- max(half_size, abs(bb[c("xmin", "xmax")] - cp[1]), abs(bb[c("ymin", "ymax")] - cp[2]))
  objects[[city]] <- list(patches = patches, boundary = boundary, study = study, centre = centre,
    centre_xy = cp, source = row)
}
# Common square extent, rounded up to a 10 km side length and enough to retain every boundary.
half_size <- ceiling(half_size * 1.05 / 5000) * 5000
extent_rows <- list()
for (city in cities) {
  message("Preparing full network and building background: ", city)
  z <- objects[[city]]
  z$xlim <- z$centre_xy[1] + c(-half_size, half_size)
  z$ylim <- z$centre_xy[2] + c(-half_size, half_size)
  roads <- st_read(z$source$road_path, layer = z$source$road_layer, quiet = TRUE) |> st_transform(3035)
  template <- rast(xmin = z$xlim[1], xmax = z$xlim[2], ymin = z$ylim[1], ymax = z$ylim[2],
    nrows = ceiling(2*half_size/50), ncols = ceiling(2*half_size/50), crs = "EPSG:3035")
  # 50 m display raster only; all density calculations retain original full graph topology.
  road_mask <- rasterize(vect(roads), template, field = 1, background = 0, touches = TRUE)
  re <- as.vector(ext(road_mask))
  z$roads_image <- as.raster(ifelse(as.matrix(road_mask, wide = TRUE) > 0, "#AEB9C0", "#FFFFFF00"))
  z$roads_extent <- re
  writeRaster(road_mask, file.path(out, paste0(city, "_road_network_display_50m.tif")), overwrite = TRUE)
  building_path <- file.path("outputs/all_41_cities_functions_boundaries_all_buildings/building_cache", paste0(city, ".tif"))
  stopifnot(file.exists(building_path))
  r <- rast(building_path)
  stopifnot(terra::same.crs(r, template))
  z$buildings_image <- as.raster(ifelse(as.matrix(r, wide = TRUE) > 0, "#E3E7E9", "#FFFFFF00"))
  z$buildings_extent <- as.vector(ext(r))
  # Labels refer to true polygon centroids and show the value mapped, in nodes/km².
  xy <- st_coordinates(suppressWarnings(st_centroid(st_geometry(z$patches))))
  z$labels <- data.frame(x = xy[, 1], y = xy[, 2], subcentre = z$patches$patch_label)
  objects[[city]] <- z
  extent_rows[[city]] <- data.frame(city = city, xmin_m = z$xlim[1], xmax_m = z$xlim[2],
    ymin_m = z$ylim[1], ymax_m = z$ylim[2], map_side_km = 2*half_size/1000, map_crs = 3035)
  rm(roads, road_mask, template, r); gc(FALSE)
}
write_csv(bind_rows(extent_rows), file.path(out, "map_extents.csv"))
all_metrics <- bind_rows(lapply(objects, function(z) st_drop_geometry(z$patches)))
write_csv(all_metrics, file.path(out, "four_city_subcentre_node_measures.csv"))
metric_names <- c(junction_density_per_km2 = "Junctions (degree >= 3)\nper km² of subcentre",
                 street_node_density_per_km2 = "Street nodes (degree != 2)\nper km² of subcentre")
colour_limits <- list()
for (metric in names(metric_names)) {
  plots <- list()
  max_value <- ceiling(max(all_metrics[[metric]], na.rm = TRUE) / 50) * 50
  colour_limits[[metric]] <- data.frame(metric = metric, minimum = 0, maximum = max_value)
  for (city in cities) {
    z <- objects[[city]]; be <- z$buildings_extent; re <- z$roads_extent
    z$labels$label <- paste0(z$labels$subcentre, "\n", format(round(z$patches[[metric]]), trim = TRUE), "/km²")
    sx <- z$xlim[1] + .07 * diff(z$xlim); sy <- z$ylim[1] + .07 * diff(z$ylim)
    p <- ggplot() +
      annotation_raster(z$buildings_image, xmin = be[1], xmax = be[2], ymin = be[3], ymax = be[4]) +
      annotation_raster(z$roads_image, xmin = re[1], xmax = re[2], ymin = re[3], ymax = re[4]) +
      geom_sf(data = z$boundary, aes(linetype = "INSEE urban-area boundary"), fill = NA, color = "#52616B", linewidth = .45) +
      geom_sf(data = z$study, aes(linetype = "Road study boundary"), fill = NA, color = "#52616B", linewidth = .4) +
      geom_sf(data = z$patches, aes(fill = .data[[metric]]), color = "#411B1B", linewidth = .32, alpha = .9) +
      scale_fill_gradientn(colors = c("#FFF5F0", "#FCBBA1", "#FB6A4A", "#CB181D", "#67000D"),
        limits = c(0, max_value), name = metric_names[[metric]],
        guide = guide_colorbar(order = 1, barheight = unit(50, "mm"))) +
      geom_sf(data = z$centre, aes(shape = "Urban centre / CBD"), fill = "#FFD700", color = "#222222", size = 2.5) +
      geom_label_repel(data = z$labels, aes(x, y, label = label), seed = 218, max.overlaps = Inf,
        max.time = 4, size = 2.8, box.padding = .4, point.padding = .2, min.segment.length = 0,
        label.size = .12, fill = "white", color = "#222222") +
      scale_linetype_manual(values = c("INSEE urban-area boundary" = "dashed", "Road study boundary" = "dotted"),
        name = NULL, guide = guide_legend(order = 2, override.aes = list(fill = NA, shape = NA))) +
      scale_shape_manual(values = c("Urban centre / CBD" = 23), name = NULL,
        guide = guide_legend(order = 3, override.aes = list(fill = "#FFD700"))) +
      annotate("segment", x = sx, xend = sx + 10000, y = sy, yend = sy, linewidth = .65) +
      annotate("text", x = sx + 5000, y = sy + diff(z$ylim)*.025, label = "10 km", size = 2.8) +
      coord_sf(crs = 3035, datum = NA, xlim = z$xlim, ylim = z$ylim, expand = FALSE) +
      labs(title = city) + theme_void(base_size = 10) +
      theme(plot.title = element_text(face = "bold", size = 17, hjust = .5),
        legend.position = "right", legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 9), plot.margin = margin(7,7,7,7))
    plots[[city]] <- p
    stem <- file.path(out, paste0(city, "_", metric))
    ggsave(paste0(stem, ".png"), p, width = 24, height = 22, units = "cm", dpi = 300, device = ragg::agg_png, bg = "white")
    ggsave(paste0(stem, ".pdf"), p, width = 24, height = 22, units = "cm", useDingbats = FALSE, bg = "white")
  }
  caption <- paste0("Full undirected road-graph degrees, assigned to subcentre polygons; densities use polygon area. Degree-2 geometry vertices are excluded from street-node counts.\n",
    "Blue-gray background: the full road network (50 m display raster); light gray: available building footprints. Labels show subcentre and density.\n",
    "Every city uses the same ", 2*half_size/1000, " × ", 2*half_size/1000, " km extent and colour scale. OSM snapshots: Marseille 27 September 2026; other cities 1 September 2026.")
  combined <- wrap_plots(plots, ncol = 2, guides = "collect") +
    plot_annotation(caption = caption, theme = theme(plot.caption = element_text(size = 9, hjust = 0),
      plot.margin = margin(8,8,8,8))) & theme(legend.position = "right")
  stem <- file.path(out, paste0("Four_cities_", metric))
  ggsave(paste0(stem, ".png"), combined, width = 44, height = 42, units = "cm", dpi = 300, device = ragg::agg_png, bg = "white")
  ggsave(paste0(stem, "_preview.png"), combined, width = 44, height = 42, units = "cm", dpi = 110, device = ragg::agg_png, bg = "white")
  ggsave(paste0(stem, ".pdf"), combined, width = 44, height = 42, units = "cm", useDingbats = FALSE, bg = "white")
}
write_csv(bind_rows(colour_limits), file.path(out, "shared_colour_scales.csv"))
writeLines(c("Node-density maps from the universal road-graph/local-node computation outputs.",
  "Junction density counts graph nodes with degree >=3 per subcentre polygon km².",
  "Street-node density counts graph nodes with degree !=2 (all are nonisolated) per subcentre polygon km².",
  "Graph degrees are computed on each complete study road network before assigning nodes to polygons.",
  "Degree-2 shape vertices remain in the routing graph and are excluded from these two density measures.",
  "The complete cached igraph objects and full metric node coordinates are retained in ../graph_cache/.",
  "Road background is a 50 m display raster of all retrieved road features; display rasterization does not affect graph construction or densities.",
  "All panels share the same square geographic extent and a common colour scale per density metric; see map_extents.csv and shared_colour_scales.csv.",
  "INSEE and circular road study boundaries, available building footprints, CBD and subcentre labels are retained.",
  "Label locations use true area-weighted polygon centroids for annotation only. No flows or travel demand are inferred.",
  "One-way restrictions, access and turn restrictions and road speeds are not applied. Geometry-defined topology and clipping endpoints affect node degrees.",
  "Boundary clipping can introduce artificial dead ends; sources have different September 2026 snapshot dates."
), file.path(out, "README.txt"))
message("Saved density maps: ", normalizePath(out))
