# ZERAFSHAN BASIN — METHOD 1: ADR ARITHMETIC MEAN
# Code author: G Mamadjanova 
#
# ----------------------------------------------------------------------------- #
#                       4a-amaliy mashg'ulot / Practical 4a                     #
# Daryo havzasiga yoqqan yog'in qatlamini o'rtacha arifmetik usul bilan hisoblash
#     Calculate the average depth of rainfall (ADR) over the catchment using
#                             arithmetic mean method                            #
# ----------------------------------------------------------------------------- #
#
# Code description in Uzbek |  R version >4.5.0
# 4a-amaliy mashg'ulotda Zarafshon daroyosi havzasiga yoqqan yo'g'in qatlamini 
# o'rtacha arifmetik usul bilan hisoblash ko'rstailgan. Meteostansiyalarda yil 
# davomida kuzatilgan yillik yog'in qiymatlari va havza maydoni asosida yillik 
# oqim qiymatini hisoblash mumkin.Buyruqlarni yurguzish to'g'ri amalga 
# oshirilganda olingan natijalar Figure_4a.png dagi grafik va 
# zerafshan_method1_arithmetic_mean_results.csv fayli ko'rinishida bo'ladi.

# ----- 1. Ishni bajarish uchun muhim bo'lgan paketlar to'plami-----------------

pkgs <- c("sf", "dplyr", "ggplot2", "ggrepel", "scales", "rnaturalearth", "rnaturalearthdata", "ggspatial")
install.packages(setdiff(pkgs, rownames(installed.packages())),
                 dependencies = TRUE)
lapply(pkgs, library, character.only = TRUE)

sf_use_s2(FALSE)

# ----- 2. Fayllar joylashuvi --------------------------------------------------

path_gpkg     <- "C:/Users/Data/basin/zerafshan_uzbekistan_HydroBASIN.gpkg"
path_stations <- "C:/Users/Data/zerafshan_stations.csv"
path_rivers   <- "C:/Users/Data/rivers/zerafshan_uzbekistan_HydroRIVERS.gpkg"
path_out      <- "C:/Users/Output_files/"

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

cat("Basin area (km²):", round(basin_area_km2, 1), "\n")
cat("Total stations:  ", nrow(stations_sf), "\n")
cat("Inside basin:    ", sum(inside), "\n")
cat("Outside basin:   ", sum(!inside), "\n")

# ----- 7. Daryo ma'lumotlari orasidan asosiy daryo va irmoqlarni tanlash ------

rivers_main <- st_read(path_rivers, layer = "main_river") %>%
  st_transform(4326)

rivers_trib <- st_read(path_rivers, layer = "major_tributaries") %>%
  st_transform(4326)

rivers_main_uzb <- st_intersection(rivers_main, basin_uzb)
rivers_trib_uzb <- st_intersection(rivers_trib, basin_uzb)

cat("Main river segments:", nrow(rivers_main_uzb), "\n")
cat("Tributary segments: ", nrow(rivers_trib_uzb), "\n")

# ----- 7. Yog'in qatlamini o'rtacha arifmetik usul bilan hisoblash formulasi --

adr_arithmetic <- mean(stations_inside_utm$precip_mm, na.rm = TRUE)
precip_volume  <- adr_arithmetic * basin_area_km2 / 1e6

cat("\n══ METHOD 1: ARITHMETIC MEAN ════════════════════\n")
cat("Stations used:", nrow(stations_inside_utm), "\n")
cat("ADR =", round(adr_arithmetic, 2), "mm\n")
cat("Volume (km³/yr):", round(precip_volume, 4), "\n")

# ----- 8. Meteostansiya koordinatasini umumiy karta sistemisiga o'tkazish -----

stations_coords <- st_coordinates(st_transform(stations_utm, 4326)) %>%
  as.data.frame() %>%
  mutate(lon          = X,
         lat          = Y,
         station_name = stations_utm$station_name,
         precip_mm    = stations_utm$precip_mm,
         location     = ifelse(inside, "Inside Basin", "Outside Basin"))

# ----- 9. Geografik joylashuvni belgilash -------------------------------------
xlim_main <- c(63, 69)
ylim_main <- c(38.5, 41.5)

# ----- 10. Umumiy kartani yaratish --------------------------------------------
p1 <- ggplot() +

  # Qo'shni davlatlar
  geom_sf(data = neighbours,
          fill = "white", colour = "azure4", linewidth = 0.3) +

  # Uzbekistan
  geom_sf(data = uzbekistan,
          fill = "white", colour = "azure4", linewidth = 0.8) +

  # Havza O'zbekiston hududi uchun
  geom_sf(data      = basin_uzb,
          fill      = scales::col_numeric(
            palette = "#d6eaf8",
            domain  = range(stations_coords$precip_mm)
          )(adr_arithmetic),
          colour    = "coral2",
          linewidth = 1.0,
          linetype  = "solid",
          alpha     = 0.75) +

  # Daryo irmoqlari — Shartli belgilarda ko'rsatish
  geom_sf(data = rivers_trib_uzb,
          aes(colour    = "Irmoqlar",
              linewidth = "Irmoqlar")) +
  
  # Asosi daryo — Shartli belgilarda ko'rsatish
  geom_sf(data = rivers_main_uzb,
          aes(colour    = "Asosiy daryo",
              linewidth = "Asosiy daryo")) +
  
  # Havza chegarasi — Shartli belgilarda ko'rsatish
  geom_sf(data     = basin_uzb,
          fill     = NA,
          aes(colour   = "Havza maydoni",
              linetype = "Havza maydoni"),
          linewidth = 1.0) +
  
  # Ranglar berish:
  scale_colour_manual(
    values = c("Asosiy daryo"    = "#4292c6",
               "Irmoqlar"        = "#4292c6",
               "Havza maydoni"   = "coral2"),
    name   = "Belgilar",
  
  # Chiziqli belgilarni ko'rsatish
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
             aes(x = lon, y = lat,
                 fill  = precip_mm,
                 shape = location),
             size  = 2.5) +

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

  scale_fill_distiller(
    palette   = "YlGnBu",
    direction = 1,
    name      = "Yog'in\n(mm)",
    guide     = guide_colorbar(
      barheight    = unit(6, "cm"),
      barwidth     = unit(0.5, "cm"),
      ticks.colour = "grey40",
      frame.colour = NA
    )
  ) +

  # Stansiya belgilari
  ggrepel::geom_text_repel(
    data               = stations_coords,
    aes(x = lon, y = lat,
        label = paste0(station_name, "\n", precip_mm, " mm")),
    size               = 2.5,
    fontface           = "bold",
    colour             = "black",
    box.padding        = 0.3,
    max.overlaps       = Inf,
    segment.colour     = "grey40",
    segment.size       = 0.3,
    segment.linetype   = "dashed",
    min.segment.length = 0.2,
    #bg.color           = "white",
    #bg.r               = 0.12
  ) +

  # Geografik joylashuv koordinatalari
  coord_sf(
    xlim   = xlim_main,
    ylim   = ylim_main,
    expand = FALSE
  ) +

  # O'rtacha arifmetik qiymat natijasini kartada ko'rsatish
  annotate("label",
           x = Inf, y = -Inf, hjust = 1.02, vjust = -0.4,
           label    = paste0(
             "O'rtacha arifmetik usul bo'yicha yog'in qatlami = ",
             round(adr_arithmetic, 1), " mm",
             "\nOqim hajmi = ",
             round(precip_volume, 3), " km³/yil"),
           fill     = "white", colour = "#08306b",
           size     = 4, label.size = 0.8) +

  labs(
    title    = "Zarafshon havzasi: O'rtacha arifmetik usul",
    subtitle = paste0("Havzada joylashgan stansiyalar soni: ", sum(inside),
                      "  |  Havza maydoni: ",
                      round(basin_area_km2, 0), " km²"),
    x        = "Uzunlik",
    y        = "Kenglik"
  ) +

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
    location = "tl",
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

  theme_bw(base_size = 13) +
  theme(
    plot.title            = element_text(face = "bold", size = 14),
    plot.subtitle         = element_text(colour = "grey40", size = 10),
    plot.caption          = element_text(colour = "grey50", size = 9),
    plot.background       = element_rect(fill = "white", colour = NA),
    panel.background      = element_rect(fill = "white"),
    panel.grid.major      = element_line(colour = "grey85", linewidth = 0.3),
    panel.grid.minor      = element_blank(),
    legend.title          = element_text(face = "bold"),
    legend.background     = element_rect(fill = NA, colour = NA),
    legend.box.background = element_rect(fill = NA, colour = NA),
    axis.title            = element_text(face = "bold")
  )
print(p1)

# ----- 11. Umumiy kartani png fayl ko'rinishida saqlash -----------------------
ggsave(paste0(path_out, "Figure_4a.png"),
       plot  = p1,
       width = 11, height = 7,
       dpi   = 300, bg = "white")

# ----- 12. Olingan qiymatlarni fayl ko'rinishida saqlash ----------------------
results_df <- data.frame(
  Method            = "Arithmetic Mean",
  N_stations_all    = nrow(stations_sf),
  N_stations_inside = sum(inside),
  ADR_mm            = round(adr_arithmetic, 2),
  Basin_area_km2    = round(basin_area_km2, 0),
  Volume_km3        = round(precip_volume,  4),
  Notes             = "Equal weight; inside-basin stations only"
)

write.csv(results_df,
          paste0(path_out, "zerafshan_method1_arithmetic_mean_results.csv"),
          row.names = FALSE)

cat("\n── Quyidagilar fayl ko'rinishida saqlandi ──────────────────────────\n")
cat("• Figure_4a.png\n")
cat("• zerafshan_method1_arithmetic_mean_results.csv\n")
