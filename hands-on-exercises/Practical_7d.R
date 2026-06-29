#  Authors: GAVKHAR Mamadjanova & ZULFIYA Kuranboyeva
# ----------------------------------------------------------------------------- #
#                       7d-amaliy mashg'ulot / Practical 7d                     #
#          O'zbekiston bo'yicha mavsumiy o'rtacha harorat kartalari             #
#          Seasonal Mean Temperature Maps for Uzbekistan (2019)                 #
# ----------------------------------------------------------------------------- #
#
# To'rtta mavsum (2x2 tartibida):
#   DJF: Qish   : Dekabr(t-1), Yanvar, Fevral  -> oylar: 12, 1, 2
#   MAM: Bahor  : Mart, Aprel, May              -> oylar:  3, 4, 5
#   JJA: Yoz    : Iyun, Iyul, Avgust            -> oylar:  6, 7, 8
#   SON: Kuz    : Sentyabr, Oktyabr, Noyabr     -> oylar:  9,10,11
#
# Eslatma: DJF uchun 2019 faylidan faqat Yanvar va Fevral mavjud;
#          Dekabr (12-oy) ham 2019 faylidan olinadi (Dekabr 2019 = DJF 2019/20
#          yoki taxminiy sifatida ishlatiladi).
#
# Natija: Figure_7d.png
# ----------------------------------------------------------------------------- #

# ---- Paketlar ----------------------------------------------------------------
pkgs <- c("ncdf4", "ggplot2", "RColorBrewer", "scales", "maps", "patchwork")
install.packages(setdiff(pkgs, rownames(installed.packages())),
                 dependencies = TRUE)
lapply(pkgs, library, character.only = TRUE)

# ---- 1. Yo'llar --------------------------------------------------------------
out_dir  <- "C:/Users/Figures/"
tmp_file <- paste0("C:/Users/Data/",
                   "CRU_mean_temperature_mon_0.5x0.5_global_2019_v4.03.nc")

# ---- 2. NetCDF faylini o'qish ------------------------------------------------
nc_tmp    <- nc_open(tmp_file)
lon       <- ncvar_get(nc_tmp, "lon")
lat       <- ncvar_get(nc_tmp, "lat")
time      <- ncvar_get(nc_tmp, "time")
tunits    <- ncatt_get(nc_tmp, "time", "units")
tmp_array <- ncvar_get(nc_tmp, "tas")
fillvalue <- ncatt_get(nc_tmp, "tas", "_FillValue")$value
dunits    <- ncatt_get(nc_tmp, "tas", "units")$value
nc_close(nc_tmp)

# Bo'sh to'r katakchalarni NA bilan almashtirish
tmp_array[tmp_array == fillvalue] <- NA

# Vaqt o'qini sanaga o'tkazish
origin_str <- unlist(strsplit(tunits$value, " "))[3]
dates      <- as.Date(time, origin = origin_str)
cat("Mavjud oylar:\n"); print(format(dates, "%B %Y"))

# ---- 3. O'zbekiston chegaralarini belgilash ---------------------------------
uzb_lon_min <- 56;  uzb_lon_max <- 73
uzb_lat_min <- 37;  uzb_lat_max <- 46

lon_idx <- which(lon >= uzb_lon_min & lon <= uzb_lon_max)
lat_idx <- which(lat >= uzb_lat_min & lat <= uzb_lat_max)

uzb_lon <- lon[lon_idx]
uzb_lat <- lat[lat_idx]

# Faqat O'zbekiston hududi uchun grid ma'lumotni kesib olish: [lon, lat, 12 oy]
uzb_array <- tmp_array[lon_idx, lat_idx, ]   # o'lchami: [nlon_uzb, nlat_uzb, 12]

# Qo'shni davlatlar chegaralari
world_map <- map_data("world",
                      region = c("Uzbekistan", "Kazakhstan", "Kyrgyzstan",
                                 "Tajikistan", "Turkmenistan", "Afghanistan"))

# O'q belgilari
uzb_lon_ticks  <- seq(56, 72, by = 4)
uzb_lat_ticks  <- seq(38, 46, by = 2)
uzb_lon_labels <- paste0(uzb_lon_ticks, "\u00b0E")
uzb_lat_labels <- paste0(uzb_lat_ticks, "\u00b0N")

# ---- 4. Mavsumiy o'rtachalarni hisoblash -------------------------------------
# DJF: Yanvar(1), Fevral(2), Dekabr(12)
# Eslatma: barcha 3 oy 2019 yil faylidan olinadi
djf_slice <- apply(uzb_array[, , c(1, 2, 12)], c(1, 2), mean, na.rm = TRUE)

# MAM: Mart(3), Aprel(4), May(5)
mam_slice <- apply(uzb_array[, , c(3, 4, 5)],  c(1, 2), mean, na.rm = TRUE)

# JJA: Iyun(6), Iyul(7), Avgust(8)
jja_slice <- apply(uzb_array[, , c(6, 7, 8)],  c(1, 2), mean, na.rm = TRUE)

# SON: Sentyabr(9), Oktyabr(10), Noyabr(11)
son_slice <- apply(uzb_array[, , c(9, 10, 11)], c(1, 2), mean, na.rm = TRUE)

cat("Mavsumiy diapazonlar (°C):\n")
cat("  DJF :", round(range(djf_slice, na.rm=TRUE), 1), "\n")
cat("  MAM :", round(range(mam_slice, na.rm=TRUE), 1), "\n")
cat("  JJA :", round(range(jja_slice, na.rm=TRUE), 1), "\n")
cat("  SON :", round(range(son_slice, na.rm=TRUE), 1), "\n")

# ---- 5. Umumiy rang shkalasi chegaralarini aniqlash --------------------------
# Barcha 4 mavsumda bir xil rang shkalasi taqqoslash uchun ishlatilinadi 
all_vals  <- c(djf_slice, mam_slice, jja_slice, son_slice)
scale_min <- floor(min(all_vals,   na.rm = TRUE) / 5) * 5   # 5 ga yaxlitlash
scale_max <- ceiling(max(all_vals, na.rm = TRUE) / 5) * 5
cat("Umumiy shkala:", scale_min, "dan", scale_max, "gacha °C\n")

temp_breaks <- seq(scale_min, scale_max, by = 5)
temp_cols   <- colorRampPalette(rev(brewer.pal(11, "RdBu")))(100)

# ---- 6. Har bir mavsum uchun jadval (data.frame) tuzish ----------------------
make_df <- function(slice_mat) {
  df        <- expand.grid(lon = uzb_lon, lat = uzb_lat)
  df$temp   <- as.vector(slice_mat)
  df
}

df_djf <- make_df(djf_slice)
df_mam <- make_df(mam_slice)
df_jja <- make_df(jja_slice)
df_son <- make_df(son_slice)

# ---- 7. Mavsum uchun ggplot ob'ekti yasaydigan funksiya ----------------
make_season_plot <- function(df, season_label, show_x = FALSE, show_y = FALSE) {
  
  p <- ggplot() +
    geom_raster(data    = df,
                mapping = aes(x = lon, y = lat, fill = temp),
                na.rm   = TRUE) +
    
    # Barcha mavsum uchun bir xil rang shkalasi
    scale_fill_gradientn(
      colours  = temp_cols,
      values   = rescale(temp_breaks),
      limits   = c(scale_min, scale_max),
      oob      = squish,
      na.value = "grey85",
      name     = paste0("Harorat (\u00b0C)")
    ) +
    guides(
      fill = guide_colorbar(
        title.position = "left",
        title.hjust    = 0.5,
        barheight      = unit(10, "cm"),
        barwidth       = unit(0.6, "cm")
      )
    ) +
    
    # Qo'shni davlatlar chegaralari
    geom_polygon(data      = world_map,
                 mapping   = aes(x = long, y = lat, group = group),
                 fill      = NA,
                 colour    = "black",
                 linewidth = 0.45) +
    
    coord_fixed(xlim  = c(uzb_lon_min, uzb_lon_max),
                ylim  = c(uzb_lat_min, uzb_lat_max),
                ratio = 1) +
    
    # O'q belgilari: pastki qator va chap ustun uchun ko'rsatiladi
    scale_x_continuous(
      breaks = uzb_lon_ticks,
      labels = if (show_x) uzb_lon_labels else rep("", length(uzb_lon_ticks))
    ) +
    scale_y_continuous(
      breaks = uzb_lat_ticks,
      labels = if (show_y) uzb_lat_labels else rep("", length(uzb_lat_ticks))
    ) +
    
    # Mavsum - subplot sarlavhasini sozlash
    ggtitle(season_label) +
    
    theme_bw(base_size = 11) +
    theme(
      plot.title        = element_text(face = "bold", size = 12, hjust = 0.5),
      legend.position   = "right",
      legend.title      = element_text(angle = 90, hjust = 0.5,
                                       face = "bold", size = 9),
      panel.grid.major  = element_line(colour = "grey88", linewidth = 0.25),
      panel.grid.minor  = element_blank(),
      axis.title        = element_blank(),
      axis.text         = element_text(size = 8),
      plot.margin       = margin(4, 4, 4, 4)
    )
  p
}

# ---- 8. To'rtta subplot qurish -----------------------------------------------
# 2x2 tartib:  DJF (yuqori chap)  | MAM (yuqori o'ng)
#              JJA (pastki chap)  | SON (pastki o'ng)
#
# show_x = TRUE  -> pastki qator (JJA, SON): lon belgilari ko'rsatiladi
# show_y = TRUE  -> chap ustun  (DJF, JJA): lat belgilari ko'rsatiladi

p_djf <- make_season_plot(df_djf, "DJF:Qish (Dek\u2013Yan\u2013Fev)",
                          show_x = FALSE, show_y = TRUE)
p_mam <- make_season_plot(df_mam, "MAM:Bahor (Mar\u2013Apr\u2013May)",
                          show_x = FALSE, show_y = FALSE)
p_jja <- make_season_plot(df_jja, "JJA:Yoz (Iyu\u2013Iyu\u2013Avg)",
                          show_x = TRUE,  show_y = TRUE)
p_son <- make_season_plot(df_son, "SON: Kuz (Sen\u2013Okt\u2013Noy)",
                          show_x = TRUE,  show_y = FALSE)

# ---- 9. patchwork bilan birlashtirish ----------------------------------------
# collect = TRUE: barcha subplot uchun bitta umumiy rang shkalasi
combined <- (p_djf | p_mam) / (p_jja | p_son) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title    = "O'zbekiston bo'yicha 2019 yilda kuzatilgan mavsumiy o'rtacha harorat",
    subtitle = paste0("CRU TS4.04  |  0.5\u00b0 \u00d7 0.5\u00b0"),
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 15, hjust = 0.5),
      plot.subtitle = element_text(size = 10, colour = "grey40", hjust = 0.5),
      plot.caption  = element_text(size = 9,  colour = "grey50", hjust = 1),
      legend.position = "right",
      legend.title  = element_text(angle = 90, hjust = 0.5,
                                   face = "bold", size = 10)
    )
  )

# ---- 10. Saqlash -------------------------------------------------------------
png(paste0(out_dir, "Figure_7d.png"),
    width = 14, height = 10, units = "in", res = 600)
print(combined)
dev.off()

cat("Mavsumiy harorat kartasi saqlandi:", out_dir, "\n")
