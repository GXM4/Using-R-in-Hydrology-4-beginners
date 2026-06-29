# Code authors: ZULFIYA Kuranboyeva & GAVKHAR Mamadjanova
# ----------------------------------------------------------------------------- #
#                       7b-amaliy mashg'ulot / Practical 7b                     #
#                CRU gridlangan havo harorati ma'lumotlarini tasvirlash         #
#                Visualisation of CRU gridded temperatura data                  #
# ----------------------------------------------------------------------------- #
#
# Code description in Uzbek | R version >= 4.1.0
# 7b-amaliy mashg'ulotda CRU baza ma'lumotlarini R dasturlash tili orqali global 
# va O'zbekiston hududi uchun tasvirlash ko'rsatilgan. Buning uchun dastlab CRU 
# gridlangan ma'lumotlarini https://crudata.uea.ac.uk/cru/data/hrg/ platformasidan 
# yuklab olishimiz lozim. Ma'lumotlar NetCDF (.nc) fayl formatida bo'ladi.
# Natija: Figure_7b_1.png va Figure_7b_2.png
# ----------------------------------------------------------------------------- #

# ---- Libraries ---------------------------------------------------------------
library(ncdf4)
library(raster)
library(terra)
library(sf)
library(ggplot2)
library(metR)
library(RNetCDF)
library(lattice)
library(RColorBrewer)
library(scales)
library(maps)

# ---- Shared colour palette ---------------------------------------------------
# RdBu REVERSED: blue = cold, red = warm — standard temperature convention.
# rev() makes blue the low end and red the high end.
temp_cols <- colorRampPalette(rev(brewer.pal(11, "RdBu")))(100)

# ---- 1. Output path ----------------------------------------------------------
out_dir <- "C:/Users/Figures/"

# ---- 2. Open NetCDF file -----------------------------------------------------
# Adjust filename to match your downloaded CRU temperature file
nc_tmp <- nc_open(paste0("C:/Users/Data/CRU_mean_temperature_mon_0.5x0.5_global_2019_v4.03.nc"))
print(nc_tmp)
dname <- "tas"   # CRU faylda o'rtacha haroratning qisqacha nomlanishi

# ---- 3. Extract file parameters ----------------------------------------------
lon  <- ncvar_get(nc_tmp, "lon")
nlon <- dim(lon)
head(lon)

lat  <- ncvar_get(nc_tmp, "lat")
nlat <- dim(lat)
head(lat)

print(c(nlon, nlat))

time   <- ncvar_get(nc_tmp, "time")
tunits <- ncatt_get(nc_tmp, "time", "units")
nt     <- dim(time)
print(tunits)
print(nt)

# Extract temperature array
tmp_array <- ncvar_get(nc_tmp, dname)
dlname    <- ncatt_get(nc_tmp, dname, "long_name")
dunits    <- ncatt_get(nc_tmp, dname, "units")       # should be "degrees Celsius"
fillvalue <- ncatt_get(nc_tmp, dname, "_FillValue")

print(dlname)
print(dunits)
print(fillvalue)
dim(tmp_array)

Conventions <- ncatt_get(nc_tmp, 0, "Conventions")
print(Conventions)

# Parse time axis
tustr      <- strsplit(tunits$value, " ")
origin_str <- unlist(tustr)[3]
dates      <- as.Date(time, origin = origin_str)
print(dates)

# Replace fill values with NA
tmp_array[tmp_array == fillvalue$value] <- NA

n_valid <- length(na.omit(as.vector(tmp_array[, , 1])))
cat("Valid cells in month 1:", n_valid, "\n")

# Select January 2019 slice
m         <- 1
tmp_slice <- tmp_array[, , m]

# ---- 4. Global temperature map — levelplot -----------------------------------
png(paste0(out_dir, "Figure_7b_1.png"),
    width = 14, height = 8, units = "in", res = 600)

grid   <- expand.grid(lon = lon, lat = lat)

# Temperature breaks: -40°C to +40°C in 5° steps
cutpts <- seq(-40, 40, by = 5)

# Degree-formatted axis tick labels
lon_ticks  <- seq(-180, 180, by = 60)
lat_ticks  <- seq(-90,   90, by = 30)
lon_labels <- ifelse(lon_ticks == 0, "0\u00b0",
                     ifelse(lon_ticks > 0,
                            paste0(lon_ticks,      "\u00b0E"),
                            paste0(abs(lon_ticks), "\u00b0W")))
lat_labels <- ifelse(lat_ticks == 0, "0\u00b0",
                     ifelse(lat_ticks > 0,
                            paste0(lat_ticks,      "\u00b0N"),
                            paste0(abs(lat_ticks), "\u00b0S")))

# Continent outlines
continent_lines <- map("world", plot = FALSE)

levelplot(tmp_slice ~ lon * lat,
          data        = grid,
          at          = cutpts,
          cuts        = length(cutpts) - 1,
          pretty      = TRUE,
          col.regions = temp_cols,          # blue=cold, red=warm
          main        = paste("CRU ma'lumotlari asosida havo haroratining yer yuzida taqsimlanishi \u2014",
                              format(dates[m], "%B %Y"), "(Global)"),
          xlab        = list("Longitude", cex = 1.1),
          ylab        = list("Latitude",  cex = 1.1),
          scales      = list(
            x = list(at = lon_ticks, labels = lon_labels, cex = 0.8),
            y = list(at = lat_ticks, labels = lat_labels, cex = 0.8)
          ),
          colorkey = list(
            space  = "right",
            labels = list(at     = cutpts,
                          labels = paste0(cutpts, "\u00b0C"),
                          cex    = 0.75),
            title  = list("Harorat (\u00b0C)",
                          rot      = 90,
                          cex      = 0.95,
                          fontface = "bold"),
            width  = 1.2
          ),
          panel = function(...) {
            panel.levelplot(...)
            panel.lines(
              x   = continent_lines$x,
              y   = continent_lines$y,
              col = "black",
              lwd = 0.5)
          })

dev.off()

# ---- 5. Close NetCDF file ----------------------------------------------------
nc_close(nc_tmp)

# ---- 6. Uzbekistan temperature map -------------------------------------------
# Bounding box: 56-73 E, 37-46 N

png(paste0(out_dir, "Figure_7b_2.png"),
    width = 10, height = 8, units = "in", res = 600)

uzb_lon_min <- 56;  uzb_lon_max <- 73
uzb_lat_min <- 37;  uzb_lat_max <- 46

lon_idx <- which(lon >= uzb_lon_min & lon <= uzb_lon_max)
lat_idx <- which(lat >= uzb_lat_min & lat <= uzb_lat_max)

uzb_slice <- tmp_slice[lon_idx, lat_idx]
uzb_lon   <- lon[lon_idx]
uzb_lat   <- lat[lat_idx]

uzb_grid       <- expand.grid(lon = uzb_lon, lat = uzb_lat)
uzb_grid$temp  <- as.vector(uzb_slice)

world_map <- map_data("world",
                      region = c("Uzbekistan", "Kazakhstan", "Kyrgyzstan",
                                 "Tajikistan", "Turkmenistan", "Afghanistan"))

# Central Asia January range: -15 to +15 degrees C
uzb_temp_breaks <- seq(-15, 15, by = 5)

# Degree-formatted axis tick labels for Uzbekistan extent
uzb_lon_ticks  <- seq(56, 72, by = 4)
uzb_lat_ticks  <- seq(38, 46, by = 2)
uzb_lon_labels <- paste0(uzb_lon_ticks, "\u00b0E")
uzb_lat_labels <- paste0(uzb_lat_ticks, "\u00b0N")

print(
  ggplot() +
    geom_raster(data    = uzb_grid,
                mapping = aes(x = lon, y = lat, fill = temp),
                na.rm   = TRUE) +
    # Blue=cold, white=0 degrees C, red=warm
    scale_fill_gradientn(
      colours  = colorRampPalette(rev(brewer.pal(11, "RdBu")))(100),
      values   = rescale(uzb_temp_breaks),
      limits   = c(-15, 15),
      oob      = squish,
      na.value = "grey85",
      name     = "Harorat (\u00b0C)"   # rotation handled in theme()
    ) +
    guides(
      fill = guide_colorbar(
        title.position = "left",    # title sits left of bar = vertical orientation
        title.hjust    = 0.5,
        barheight      = unit(6, "cm"),
        barwidth       = unit(0.5, "cm")
      )
    ) +
    geom_polygon(data      = world_map,
                 mapping   = aes(x = long, y = lat, group = group),
                 fill      = NA,
                 colour    = "black",
                 linewidth = 0.5) +
    coord_fixed(xlim  = c(uzb_lon_min, uzb_lon_max),
                ylim  = c(uzb_lat_min, uzb_lat_max),
                ratio = 1) +
    # Degree-formatted axis ticks
    scale_x_continuous(breaks = uzb_lon_ticks, labels = uzb_lon_labels) +
    scale_y_continuous(breaks = uzb_lat_ticks, labels = uzb_lat_labels) +
    labs(
      title    = "O'zbekiston bo'yicha kuzatilgan o'rtacha harorat",
      subtitle = paste("Month:", format(dates[m], "%B %Y"),
                       "Source: CRU TS4.04 | Resolution: 0.5\u00b0 \u00d7 0.5\u00b0"),
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

cat("Harorat bo'yicha kartalar saqlandi:", out_dir, "\n")
