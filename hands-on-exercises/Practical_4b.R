# ══════════════════════════════════════════════════════════════════════════
# ZERAFSHAN BASIN — METHOD 2: ISOHYETAL
# Code author: G Mamadjanova 
# ══════════════════════════════════════════════════════════════════════════

# ----------------------------------------------------------------------------- #
#                       4b-amaliy mashg'ulot / Practical 4b                     #
# Daryo havzasiga yoqqan yog'in qatlamini izogietlar usuli bilan hisoblash     #
#     Calculate the average depth of rainfall (ADR) over the catchment using    #
#                             isohyetal method                                  #
# ----------------------------------------------------------------------------- #

# Code description in Uzbek |  R version >4.5.0
# 4b-amaliy mashg'ulotda Zarafshon daryosi havzasiga yoqqan yog'in qatlamini 
# izogietlar usuli bilan hisoblash ko'rstailgan. Meteostansiyalarda yil 
# davomida kuzatilgan yillik yog'in qiymatlari asosida izogietlar har 25mm oraliq
# da o'tkazilgan va havza maydoni uchun yillik oqim qiymatini hisoblash ishlari 
# ko'rsatilgan. Buyruqlarni yurguzish to'g'ri amalga oshirilganda olingan natijalar
# Figure_4b.png dagi grafik, zerafshan_method2_bands.csv va zerafshan_method2_results.csv
# fayllari ko'rinishida bo'ladi.

# ----- 1. Ishni bajarish uchun muhim bo'lgan paketlar to'plami-----------------

pkgs <- c("sf", "dplyr", "ggplot2", "ggrepel", "terra", "gstat", "metR", "scales", 
          "rnaturalearth", "rnaturalearthdata", "ggspatial", "ggnewscale")
install.packages(setdiff(pkgs, rownames(installed.packages())),
                 dependencies = TRUE)
lapply(pkgs, library, character.only = TRUE)

sf_use_s2(FALSE)

# ----- 2. Fayllar joylashuvi --------------------------------------------------
path_gpkg     <- "C:/Users/jt231697/Downloads/hands_on_exercise/Practical_4/GitHUB/Data/basin/zerafshan_uzbekistan_HydroBASIN.gpkg"
path_stations <- "C:/Users/jt231697/Downloads/hands_on_exercise/Practical_4/GitHUB/Data/zerafshan_stations.csv"
path_rivers   <- "C:/Users/jt231697/Downloads/hands_on_exercise/Practical_4/GitHUB/Data/rivers/zerafshan_uzbekistan_HydroRIVERS.gpkg"
path_out      <- "C:/Users/jt231697/Downloads/hands_on_exercise/Practical_4/GitHUB/Output_files/"

# ----- 3. Ma'lumotlarni yuklash -----------------------------------------------

basin_full <- st_read(path_gpkg, layer = "zerafshan_uzb_lev06") %>%
  st_union() %>% st_sf() %>% st_transform(4326)

basin_utm      <- st_transform(basin_full, 32642)
basin_area_km2 <- as.numeric(st_area(basin_utm)) / 1e6

# ----- 4. Davlat chegaralarini ko'rsatish ------------------------------------
uzbekistan <- ne_countries(country     = "Uzbekistan",
                           scale       = "medium",
                           returnclass = "sf") %>% st_transform(4326)

neighbours <- ne_countries(
  country     = c("Tajikistan", "Kazakhstan",
                  "Kyrgyzstan", "Turkmenistan", "Afghanistan"),
  scale       = "medium",
  returnclass = "sf"
) %>% st_transform(4326)

# ----- 5. Havzani O'zbekiston hududi uchun clip qilish ------------------------

basin_uzb     <- st_intersection(basin_full, uzbekistan)
basin_uzb_utm <- st_transform(basin_uzb, 32642)

# ----- 6. Meteorologik stansiyalar ma'lumotlarini qayta ishlash ---------------

stations_df <- read.csv(path_stations) %>%
  mutate(precip_mm = as.numeric(as.character(precip_mm)))

if (any(is.na(stations_df$precip_mm))) {
  cat("WARNING: NAs in precip_mm:\n")
  print(stations_df[is.na(stations_df$precip_mm), ])
} else {
  cat("✓ No NA values in precip_mm\n")
}

stations_sf  <- st_as_sf(stations_df,
                         coords = c("lon", "lat"), crs = 4326)
stations_utm <- st_transform(stations_sf, 32642)

inside              <- st_intersects(stations_sf, basin_uzb,
                                     sparse = FALSE)[, 1]
stations_inside_utm <- stations_utm[inside, ]

cat("Havza maydoni (km²):", round(basin_area_km2, 1), "\n")
cat("Umumiy stansiyalar:  ", nrow(stations_sf), "\n")
cat("havza ichida:    ", sum(inside), "\n")
cat("Havzadan tashqari:   ", sum(!inside), "\n")

# Stansiyalar bo'yicha diagnostika 

cat("\n── Stansiyalar xulosasi ─────────────────────────\n")
cat("Havza ichida (n =", sum(inside), "):",
    round(mean(stations_utm$precip_mm[inside],  na.rm = TRUE), 1), "mm avg\n")
cat("Havzadan tashqari (n =", sum(!inside), "):",
    round(mean(stations_utm$precip_mm[!inside], na.rm = TRUE), 1), "mm avg\n")
cat("Barcha stansiyalar (n =", nrow(stations_utm), "):",
    round(mean(stations_utm$precip_mm,          na.rm = TRUE), 1), "mm avg\n")

# Havzadan tashqari stansiyalarni havza chegarasigacha bo'lgan masofa bo'yicha tartiblash
outside_utm <- stations_utm[!inside, ]
dist_km     <- as.numeric(st_distance(
  outside_utm,
  st_cast(st_boundary(basin_uzb_utm), "MULTILINESTRING")
)) / 1000

cat("\nHavzadan tashqari stansiyalar — havza chegarasigacha masofa:\n")
print(data.frame(
  Station  = outside_utm$station_name,
  Precip   = outside_utm$precip_mm,
  Dist_km  = round(dist_km, 1)
) %>% arrange(Dist_km))

# ----- 7. Daryo ma'lumotlari orasidan asosiy daryo va irmoqlarni tanlash ------

rivers_main <- st_read(path_rivers, layer = "main_river") %>%
  st_transform(4326)

rivers_trib <- st_read(path_rivers, layer = "major_tributaries") %>%
  st_transform(4326)

rivers_main_uzb <- st_intersection(rivers_main, basin_uzb)
rivers_trib_uzb <- st_intersection(rivers_trib, basin_uzb)

cat("Main river segments:", nrow(rivers_main_uzb), "\n")
cat("Tributary segments: ", nrow(rivers_trib_uzb), "\n")

# ----- 8. Izogietlarni interpolyasiya qilish orqali hisoblash -----------------

basin_ext     <- terra::ext(vect(basin_utm))
grid_template <- terra::rast(
  xmin = basin_ext$xmin, xmax = basin_ext$xmax,
  ymin = basin_ext$ymin, ymax = basin_ext$ymax,
  resolution = 500,
  crs        = "EPSG:32642"
)

grid_pts <- terra::xyFromCell(grid_template,
                              1:terra::ncell(grid_template)) %>%
  as.data.frame() %>%
  st_as_sf(coords = c("x", "y"), crs = 32642)

idw_result <- gstat::idw(
  formula   = precip_mm ~ 1,
  locations = stations_inside_utm,
  newdata   = grid_pts,
  idp       = 1.5
)

precip_raster        <- grid_template
terra::values(precip_raster) <- idw_result$var1.pred

# Mask: Havzani O'zbekiston hududi uchun mask qilish va Tojikiston hududidagi qismini olib tashlash 
precip_masked     <- terra::mask(precip_raster, vect(basin_utm))
precip_masked_uzb <- terra::mask(precip_raster, vect(basin_uzb_utm))

cat("\n── IDW surface ──────────────────────────────────\n")
cat("Min :", round(global(precip_masked, "min",  na.rm = TRUE)$min,  1), "mm\n")
cat("Max :", round(global(precip_masked, "max",  na.rm = TRUE)$max,  1), "mm\n")
cat("Mean:", round(global(precip_masked, "mean", na.rm = TRUE)$mean, 1), "mm\n")

# ----- 9. Izogietlarni 25 mm dan o'tkazish -----------------------------------

p_min_val      <- global(precip_masked, "min", na.rm = TRUE)$min
p_max_val      <- global(precip_masked, "max", na.rm = TRUE)$max
interval       <- 25
isohyet_breaks <- seq(floor(p_min_val   / interval) * interval,
                      ceiling(p_max_val / interval) * interval,
                      by = interval)
major_breaks   <- isohyet_breaks[isohyet_breaks %% 50 == 0]
minor_breaks   <- isohyet_breaks[isohyet_breaks %% 50 != 0]

cat("Precip range:", round(p_min_val, 0), "–",
    round(p_max_val, 0), "mm\n")
cat("Isohyet breaks:", paste(isohyet_breaks, collapse = " → "), "\n")

# ----- 10. Yog'in qatlamini izogietlar usulida hisoblash formulasi -------------

isohyet_results <- list()

for (i in seq_len(length(isohyet_breaks) - 1)) {
  lower <- isohyet_breaks[i]
  upper <- isohyet_breaks[i + 1]
  
  zone          <- terra::ifel(precip_masked >= lower &
                                 precip_masked < upper,
                               precip_masked, NA)
  n_cells       <- global(zone, "notNA")$notNA
  cell_area_km2 <- (res(zone)[1] / 1000)^2
  zone_area_km2 <- n_cells * cell_area_km2
  
  isohyet_results[[i]] <- data.frame(
    band            = paste0(lower, "–", upper, " mm"),
    mid_mm          = (lower + upper) / 2,
    mean_mm         = round(global(zone, "mean", na.rm = TRUE)$mean, 1),
    area_km2        = round(zone_area_km2, 2),
    weight          = round(zone_area_km2 / basin_area_km2, 4),
    weighted_precip = round((lower + upper) / 2 *
                              zone_area_km2 / basin_area_km2, 3)
  )
}

isohyet_df    <- bind_rows(isohyet_results) %>% filter(area_km2 > 0)
adr_isohyet   <- sum(isohyet_df$weighted_precip, na.rm = TRUE)
precip_volume <- adr_isohyet * basin_area_km2 / 1e6

cat("\n══ METHOD 2: Izogietlar usuli ══════════════════════════\n")
print(isohyet_df %>%
        select(band, area_km2, weight, weighted_precip) %>%
        mutate(across(where(is.numeric), ~ round(.x, 3))))
cat("Sum of weights:", round(sum(isohyet_df$weight), 4), "\n")
cat("Yog'in qatlami =", round(adr_isohyet, 2), "mm\n")
cat("Oqim hajmi (km³/yr):", round(precip_volume, 4), "\n")

# ----- 11. Raster ma'lumotni umumiy koordinata sistemisiga o'tkazish ----------

precip_geo <- terra::project(
  precip_masked_uzb,
  "EPSG:4326",
  method = "bilinear",
  res    = 0.005
)

precip_df_geo <- as.data.frame(precip_geo, xy = TRUE) %>%
  setNames(c("x", "y", "precip")) %>%
  filter(!is.na(precip))

cat("Reprojected raster points:", nrow(precip_df_geo), "\n")

# ----- 12. Meteostansiya koordinatasini umumiy koordinata sistemisiga o'tkazish 

stations_coords <- st_coordinates(st_transform(stations_utm, 4326)) %>%
  as.data.frame() %>%
  mutate(lon          = X,
         lat          = Y,
         station_name = stations_utm$station_name,
         precip_mm    = stations_utm$precip_mm,
         location     = ifelse(inside, "Inside Basin", "Outside Basin"))

# ----- 13. Geografik joylashuvni belgilash -------------------------------------
xlim_main <- c(63, 69)
ylim_main <- c(39, 41)

# ----- 14. Umumiy kartani yaratish --------------------------------------------
p2 <- ggplot() +
  
  # Qo'shni davlatlar
  geom_sf(data = neighbours,
          fill = "white", colour = "azure4", linewidth = 0.3) +
  
  # Uzbekistan
  geom_sf(data = uzbekistan,
          fill = "white", colour = "azure4", linewidth = 0.8) +
  
  # Yog'in bo'yicha raster ma'lumot
  geom_tile(data  = precip_df_geo,
            aes(x = x, y = y, fill = precip),
            alpha = 0.90) +
  
  # Yog'in bo'yicha ranglarni belgilash
  scale_fill_distiller(
    palette   = "YlGnBu",
    direction = 1,
    name      = "Yog'in\n(mm)",
    guide     = guide_colorbar(
      barheight    = unit(6, "cm"),
      barwidth     = unit(0.5, "cm"),
      ticks.colour = "grey40",
      title.hjust  = 0.5,
      frame.colour = NA
    )
  ) +
  
  new_scale_fill() +
  
  # Yordamchi izogiet chiziqlar  (25mm)
  metR::geom_contour2(
    data      = precip_df_geo,
    aes(x = x, y = y, z = precip),
    breaks    = minor_breaks,
    colour    = "royalblue3",
    linewidth = 0.25,
    alpha     = 0.6
  ) +
  
  # Asosiy izogiet chiziqlar (50mm)
  metR::geom_contour2(
    data      = precip_df_geo,
    aes(x = x, y = y, z = precip),
    breaks    = major_breaks,
    colour    = "royalblue4",
    linewidth = 0.65
  ) +
  
  # Izogiet ko'rsatkichlari
  metR::geom_text_contour(
    data          = precip_df_geo,
    aes(x = x, y = y, z = precip),
    breaks        = major_breaks,
    colour        = "grey20",
    size          = 2.8,
    fontface      = "italic",
    stroke        = 0.25,
    stroke.colour = "white",
    skip          = 0
  ) +
  
  # Irmoqlar
  geom_sf(data = rivers_trib_uzb,
          aes(colour    = "Irmoqlar",
              linewidth = "Irmoqlar")) +
  
  # Asosiy daryo
  geom_sf(data = rivers_main_uzb,
          aes(colour    = "Asosiy daryo",
              linewidth = "Asosiy daryo")) +
  
  # Havza chegarasi
  geom_sf(data     = basin_uzb,
          fill     = NA,
          aes(colour   = "Havza chegarasi",
              linetype = "Havza chegarasi"),
          linewidth = 1.0) +
  
  # Ranglar berish
  scale_colour_manual(
    values = c("Asosiy daryo"    = "#4292c6",
               "Irmoqlar"        = "#4292c6",
               "Havza chegarasi" = "coral2"),
    name   = "Belgilar",
    guide  = guide_legend(
      override.aes = list(
        linewidth = c(1.0, 0.8, 1.0), 
        linetype  = c("solid", "solid", "solid")
      )
    )
  ) +
  
  scale_linewidth_manual(
    values = c("Asosiy daryo"    = 1.0,
               "Irmoqlar"        = 0.45,
               "Havza chegarasi" = 1.0),
    guide  = "none"
  ) +
  
  scale_linetype_manual(
    values = c("Havza chegarasi" = "solid",
               "Asosiy daryo"    = "solid",
               "Irmoqlar"        = "solid"),
    guide  = "none"
  ) +
  
  # Stansiyalarni joylashtirish
  geom_point(data  = stations_coords,
             aes(x     = lon,
                 y     = lat,
                 shape = location,
                 fill  = location),
             size   = 2,
             colour = "black",
             stroke = 0.6) +
  
  scale_shape_manual(
    values = c("Inside Basin"  = 21,
               "Outside Basin" = 24),
    labels = c("Inside Basin"  = "Havza ichida",
               "Outside Basin" = "Havzadan tashqari"),
    name   = "Stansiya"
  ) +
  
  scale_fill_manual(
    values = c("Inside Basin"  = "black",
               "Outside Basin" = "white"),
    labels = c("Inside Basin"  = "Havza ichida",
               "Outside Basin" = "Havzadan tashqari"),
    name   = "Stansiya"
  ) +
  
  ggrepel::geom_text_repel(
    data               = stations_coords,
    aes(x = lon, y = lat,
        label = paste0(station_name, "\n", precip_mm, " mm")),
    size               = 2.3,
    fontface           = "bold",
    colour             = "black",
    max.overlaps       = Inf,
    box.padding        = 0.35,
    point.padding      = 0.2,
    segment.colour     = NA,
    segment.size       = 0,
    min.segment.length = Inf,
    bg.color           = "white",
    bg.r               = 0.12,
    seed               = 42
  ) +
  
  # Geografik joylashuv koordinatalari
  coord_sf(
    xlim   = xlim_main,
    ylim   = ylim_main,
    expand = FALSE
  ) +
  
  # Izogietlar usuli bo'yicha natijani kartada ko'rsatish
  annotate("label",
           x = Inf, y = -Inf, hjust = 1.02, vjust = -0.4,
           label    = paste0(
             "Izogietlar usuli bo'yicha yog'in qatlami = ",
             round(adr_isohyet, 1), " mm",
             "\nOqim hajmi = ",
             round(precip_volume, 3), " km³/yil"),
           fill     = "white", colour = "#08306b",
           size     = 3, fontface = "bold", label.size = 0.5) +
  
  annotation_scale(
    location      = "bl",
    width_hint    = 0.2,
    style         = "ticks",
    unit_category = "metric",
    text_cex      = 0.8,
    line_width    = 0.8,
    text_col      = "grey20",
    line_col      = "grey20"
  ) +
  
  annotation_north_arrow(
    location = "bl",
    pad_x    = unit(0.2, "cm"),
    pad_y    = unit(0.8, "cm"),
    style    = north_arrow_fancy_orienteering(
      fill      = c("grey40", "white"),
      line_col  = "grey40",
      text_col  = "grey20",
      text_size = 10
    ),
    height = unit(1.2, "cm"),
    width  = unit(1.2, "cm")
  ) +
  
  labs(
    title    = "Zarafshon havzasi: Izogietlar usuli",
    subtitle = paste0("Havzada joylashgan stansiyalar soni: ", sum(inside),
                      "  |  Havza maydoni: ",
                      round(basin_area_km2, 0), " km²  |  ",
                      "Interval: ", interval, " mm"),
    x = "Uzunlik",
    y = "Kenglik"
  ) +
  
  theme_bw(base_size = 13) +
  theme(
    plot.title            = element_text(face = "bold", size = 14),
    plot.subtitle         = element_text(colour = "grey40", size = 10),
    plot.background       = element_rect(fill = "white", colour = NA),
    panel.background      = element_rect(fill = "white"),
    panel.grid.major      = element_line(colour = "grey85", linewidth = 0.3),
    panel.grid.minor      = element_blank(),
    legend.title          = element_text(face = "bold"),
    legend.background     = element_rect(fill = NA, colour = NA),
    legend.box.background = element_rect(fill = NA, colour = NA),
    axis.title            = element_text(face = "bold")
  )
print(p2)

# ----- 15. Umumiy kartani png fayl ko'rinishida saqlash -----------------------

ggsave(paste0(path_out, "Figure_4b.png"),
       plot  = p2,
       width = 11, height = 7,
       dpi   = 300, bg = "white")

# ----- 16. Olingan qiymatlarni fayl ko'rinishida saqlash ----------------------

write.csv(isohyet_df,
          paste0(path_out, "zerafshan_method2_bands.csv"),
          row.names = FALSE)

write.csv(
  data.frame(
    Method         = "Isohyetal",
    Interval_mm    = interval,
    N_bands        = nrow(isohyet_df),
    ADR_mm         = round(adr_isohyet,    2),
    Basin_area_km2 = round(basin_area_km2, 0),
    Volume_km3     = round(precip_volume,  4)
  ),
  paste0(path_out, "zerafshan_method2_results.csv"),
  row.names = FALSE
)

cat("\n── Quyidagilar fayl ko'rinishida saqlandi ──────────────────────────\n")
cat("• Figure_4b.png\n")
cat("• zerafshan_method2_bands.csv\n")
cat("• zerafshan_method2_results.csv\n")