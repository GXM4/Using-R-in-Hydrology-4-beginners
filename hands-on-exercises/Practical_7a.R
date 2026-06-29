# Authors: ZULFIYA Kuranboyeva & GAVKHAR Mamadjanova
# ----------------------------------------------------------------------------- #
#                       7a-amaliy mashg'ulot / Practical 7a                     #
#                CRU gridlangan yog'in ma'lumotlarini tasvirlash                #
#                Visualisation of CRU gridded precipitation data                #
# ----------------------------------------------------------------------------- #
#
# Code description in Uzbek | R version >= 4.1.0
# 7a-amaliy mashg'ulotda CRU baza ma'lumotlarini R dasturlash tili orqali
# tasvirlash ko'rsatilgan. Buning uchun dastlab CRU gridlangan ma'lumotlarini
# https://crudata.uea.ac.uk/cru/data/hrg/ platformasidan yuklab olishimiz lozim.
# Ma'lumotlar NetCDF (.nc) fayl formatida bo'ladi.
# ----------------------------------------------------------------------------- #

# ---- Ishni bajarish uchun muhim bo'lgan paketlar to'plami---------------------

pkgs <- c("ncdf4", "raster", "terra", "sf", "ggplot2", "metR", "RNetCDF", "lattice", "RColorBrewer", "scales", "maps")
install.packages(setdiff(pkgs, rownames(installed.packages())),
                 dependencies = TRUE)
lapply(pkgs, library, character.only = TRUE)

# ---- Umumiy ranglar palitrasi ------------------------------------------------
# YlGnBu: sariq-yashil-ko'k - yog‘in xaritalari uchun meteorologiyada keng qo‘llanadigan
# standart rang. RColorBrewer’dagi 9 ta rang darajasi asosida tuzilgan bo‘lib,
# zaruratga qarab ko‘proq ranglarga interpolatsiya qilinadi.

precip_cols <- colorRampPalette(brewer.pal(9, "YlGnBu"))(100)

# ---- 1. Grafiklarni saqlash uchun direktoriy----------------------------------
out_dir <- "C:/Users/Figures/" #ushbu qator foydalanuvchi tomonidan tahrirlanadi

# ---- 2. NetCDF faylni o'qish -------------------------------------------------
cru_precip <- nc_open("C:/Users/Data/CRU_total_precipitation_mon_0.5x0.5_global_2019_v4.03.nc") 
#ushbu qator foydalanuvchi tomonidan tahrirlanadi

print(cru_precip)

dname <- "pr"   # yog'in uchun ishlatiladigan nom

# ---- 3. Fayl parametrlarini ajratib olish ------------------------------------
lon  <- ncvar_get(cru_precip, "lon")
nlon <- dim(lon)
head(lon)

lat  <- ncvar_get(cru_precip, "lat")
nlat <- dim(lat)
head(lat)

print(c(nlon, nlat))

time   <- ncvar_get(cru_precip, "time")
tunits <- ncatt_get(cru_precip, "time", "units")
nt     <- dim(time)
print(tunits)
print(nt)

# Yog'in qatorlarini ajratib olish
precip_array <- ncvar_get(cru_precip, dname)
dlname    <- ncatt_get(cru_precip, dname, "long_name")
dunits    <- ncatt_get(cru_precip, dname, "units")
fillvalue <- ncatt_get(cru_precip, dname, "_FillValue")

print(dlname)
print(dunits)
print(fillvalue)
dim(precip_array)

Conventions <- ncatt_get(cru_precip, 0, "Conventions")
print(Conventions)

# Vaqt o'qini kalendar ko'rinishiga o'tkazish 
tustr      <- strsplit(tunits$value, " ")   # masalan: 1900-1-1"
origin_str <- unlist(tustr)[3]              # "1900-1-1"
dates      <- as.Date(time, origin = origin_str)
print(dates)

# mavjud bo'lmagan ma'lumotlar to'rini NA bn belgilash
precip_array[precip_array == fillvalue$value] <- NA

n_valid <- length(na.omit(as.vector(precip_array[, , 1])))
cat("Number of valid cells in month 1:", n_valid, "\n")

# Yanvar oyini ajratib olish
m         <- 1
precip_slice <- precip_array[, , m]


# ---- 4. Global yog'in kartasini levelplot yordamida vizualizatsiya qilish ----

png(paste0(out_dir, "Figure_7a_1.png"),
    width = 14, height = 8, units = "in", res = 600)

grid   <- expand.grid(lon = lon, lat = lat)
cutpts <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150)

# Degree-formatted axis tick labels
lon_ticks  <- seq(-180, 180, by = 60)
lat_ticks  <- seq(-90,   90, by = 30)
lon_labels <- ifelse(lon_ticks == 0, "0\u00b0",
                     ifelse(lon_ticks > 0,
                            paste0(lon_ticks,       "\u00b0E"),
                            paste0(abs(lon_ticks),  "\u00b0W")))
lat_labels <- ifelse(lat_ticks == 0, "0\u00b0",
                     ifelse(lat_ticks > 0,
                            paste0(lat_ticks,       "\u00b0N"),
                            paste0(abs(lat_ticks),  "\u00b0S")))

# Continent outlines
continent_lines <- map("world", plot = FALSE)

levelplot(precip_slice ~ lon * lat,
          data        = grid,
          at          = cutpts,
          cuts        = length(cutpts) - 1,
          pretty      = TRUE,
          col.regions = precip_cols,
          main        = paste("CRU Yog'ingarchilikning global taqsimlanishi",
                              format(dates[m], "%B %Y")),
          xlab        = list("Longitude", cex = 1.1),
          ylab        = list("Latitude",  cex = 1.1),
          scales      = list(
            x = list(at = lon_ticks, labels = lon_labels, cex = 0.8),
            y = list(at = lat_ticks, labels = lat_labels, cex = 0.8)
          ),
          colorkey = list(
            space  = "right",
            labels = list(at     = cutpts,
                          labels = as.character(cutpts),
                          cex    = 0.75),
            title  = list("Yog'in\n(mm/oy)",
                          rot   = 90,
                          cex   = 0.95,
                          fontface = "bold"),
            width  = 1.2
          ),
          panel = function(...) {
            panel.levelplot(...)                # raster tiles
            panel.lines(                        # continent outlines on top
              x   = continent_lines$x,
              y   = continent_lines$y,
              col = "black",
              lwd = 0.5)
          })

dev.off()

# ---- 5. NetCDF faylni yopish -------------------------------------------------
nc_close(cru_precip)

# ---- 6. O'zbekiston hududini vizualizatsiya qilish ---------------------------

png(paste0(out_dir, "Figure_7a_2.png"),
    width = 10, height = 8, units = "in", res = 600)

# O'zbekiston hududini belgilsah
uzb_lon_min <- 56;  uzb_lon_max <- 73
uzb_lat_min <- 37;  uzb_lat_max <- 46

# Find grid indices within Uzbekistan bounding box
lon_idx <- which(lon >= uzb_lon_min & lon <= uzb_lon_max)
lat_idx <- which(lat >= uzb_lat_min & lat <= uzb_lat_max)

# Subset spatial slice and coordinate vectors
uzb_slice <- precip_slice[lon_idx, lat_idx]
uzb_lon   <- lon[lon_idx]
uzb_lat   <- lat[lat_idx]

# Build data frame for ggplot2
uzb_grid        <- expand.grid(lon = uzb_lon, lat = uzb_lat)
uzb_grid$precip <- as.vector(uzb_slice)

# Qo'shni davlatlar chegaralarini chizish
world_map <- map_data("world",
                      region = c("Uzbekistan", "Kazakhstan", "Kyrgyzstan",
                                 "Tajikistan", "Turkmenistan", "Afghanistan"))

# Colour breaks suited to the drier Central Asian range (max ~100 mm/month)
uzb_breaks <- c(0, 5, 10, 15, 20, 30, 40, 60, 80, 100)

print(
  ggplot() +
    # Precipitation raster layer
    geom_raster(data    = uzb_grid,
                mapping = aes(x = lon, y = lat, fill = precip),
                na.rm   = TRUE) +
    # CHANGED: YlGnBu — white/yellow = dry, dark blue = wet (precipitation convention)
    scale_fill_gradientn(
      colours  = colorRampPalette(brewer.pal(9, "YlGnBu"))(100),
      values   = rescale(uzb_breaks),
      limits   = c(0, 100),
      oob      = squish,           # values above 100 clamped to darkest blue
      na.value = "grey85",
      name     = paste0("Yog'in (", dunits$value, ")")  # single-line for rotation
    ) +
    # Vertical colourbar title (title.position="left" rotates it alongside the bar)
    guides(
      fill = guide_colorbar(
        title.position = "left",
        title.hjust    = 0.5,
        barheight      = unit(6, "cm"),
        barwidth       = unit(0.5, "cm")
      )
    ) +
    # Country / neighbour borders
    geom_polygon(data      = world_map,
                 mapping   = aes(x = long, y = lat, group = group),
                 fill      = NA,
                 colour    = "black",
                 linewidth = 0.5) +
    # Crop to Uzbekistan bounding box
    coord_fixed(xlim  = c(uzb_lon_min, uzb_lon_max),
                ylim  = c(uzb_lat_min, uzb_lat_max),
                ratio = 1) +
    # Labels
    labs(
      title    = "O'zbekiston hududida kuzatilgan yog'in miqdori",
      subtitle = paste("Month:", format(dates[m], "%B %Y"),
                       "Source: CRU | Resolution: 0.5° × 0.5°"),
      x        = "Longitude",
      y        = "Latitude"
    ) +
    theme_bw(base_size = 13) +
    theme(
      plot.title        = element_text(face = "bold", size = 15),
      plot.subtitle     = element_text(size = 11, colour = "grey40"),
      legend.position   = "right",
      legend.key.height = unit(1.8, "cm"),
      panel.grid.major  = element_line(colour = "grey85", linewidth = 0.3),
      panel.grid.minor  = element_blank(),
      legend.title      = element_text(angle = 90, hjust = 0.5,
                                       face = "bold", size = 11)
    )
)

dev.off()

cat("Barcha grafiklar quyidagi papkada saqlandi:", out_dir, "\n")
