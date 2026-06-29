# Authors: GAVKHAR Mamadjanova & ZULFIYA Kuranboyeva 
# ----------------------------------------------------------------------------- #
#                       7c-amaliy mashg'ulot / Practical 7c                     #
#         Toshkent shahri uchun yog'in va harorat vaqt qatorini chizish         #
#     Tashkent Precipitation & Temperature Timeseries retrieved from CRU data   #
# ----------------------------------------------------------------------------- #
# Code description in Uzbek | R version >= 4.1.0
# Ushbu amaliy mashg'ulotda NETCDF fayldan Toshkent shahri uchun ajratib
# olingan harorat va yog'in qiymatlari ikki o'qli grafik (Walter-Lieth uslubi) 
# asosida vizualizatsiya qilish ko'rsatilgan. Buyruqlarni yurguzish to'g'ri amalga 
# oshirilganda olingan natija Figure_7c png dagi grafik ko'rinishida bo'ladi.
#
# NETCDF manba: https://crudata.uea.ac.uk/cru/data/hrg/
# ----------------------------------------------------------------------------- #

# ---- Ishni bajarish uchun muhim bo'lgan paketlar to'plami --------------------
pkgs <- c("ncdf4", "ggplot2", "RColorBrewer", "scales")
install.packages(setdiff(pkgs, rownames(installed.packages())),
                 dependencies = TRUE)
lapply(pkgs, library, character.only = TRUE)

# ---- 1. Fayllarni o'qish va grafikni joylashtirish ---------------------------
cru_dir  <- "C:/Users/Data/"
out_dir  <- "C:/Users/Figures/"

pr_file  <- paste0(cru_dir,
                   "CRU_total_precipitation_mon_0.5x0.5_global_2019_v4.03.nc")
tmp_file <- paste0(cru_dir,
                   "CRU_mean_temperature_mon_0.5x0.5_global_2019_v4.03.nc")

# ---- 2. Yog'in bo'yicha ma'lumotni o'qish ------------------------------------
nc_pr    <- nc_open(pr_file)
lon_pr   <- ncvar_get(nc_pr, "lon")
lat_pr   <- ncvar_get(nc_pr, "lat")
time_pr  <- ncvar_get(nc_pr, "time")
tunits   <- ncatt_get(nc_pr, "time", "units")
pr_array <- ncvar_get(nc_pr, "pr")
fv_pr    <- ncatt_get(nc_pr, "pr", "_FillValue")$value
pr_array[pr_array == fv_pr] <- NA
origin_str <- unlist(strsplit(tunits$value, " "))[3]
dates      <- as.Date(time_pr, origin = origin_str)
nc_close(nc_pr)

# ---- 3. Harorat bo'yicha ma'lumotni o'qish -----------------------------------
nc_tmp    <- nc_open(tmp_file)
lon_tmp   <- ncvar_get(nc_tmp, "lon")
lat_tmp   <- ncvar_get(nc_tmp, "lat")
tmp_array <- ncvar_get(nc_tmp, "tas")          # o'zgaruvchi nomi: tas (air_temperature)
fv_tmp    <- ncatt_get(nc_tmp, "tas", "_FillValue")$value
tmp_array[tmp_array == fv_tmp] <- NA
nc_close(nc_tmp)

# ---- 4. Toshkent bo'yicha to'r ma'lumotini ajratib olish ---------------------
tashkent_lon <- 69.29
tashkent_lat <- 41.32

pr_lon_idx  <- which.min(abs(lon_pr  - tashkent_lon))
pr_lat_idx  <- which.min(abs(lat_pr  - tashkent_lat))
tmp_lon_idx <- which.min(abs(lon_tmp - tashkent_lon))
tmp_lat_idx <- which.min(abs(lat_tmp - tashkent_lat))

cat("Yog'in to'r katagi : lon =", lon_pr[pr_lon_idx],
    " lat =", lat_pr[pr_lat_idx], "\n")
cat("Harorat to'r katagi: lon =", lon_tmp[tmp_lon_idx],
    " lat =", lat_tmp[tmp_lat_idx], "\n")

tash_pr  <- as.numeric(pr_array[ pr_lon_idx,  pr_lat_idx,  ])
tash_tmp <- as.numeric(tmp_array[tmp_lon_idx, tmp_lat_idx, ])

# ---- 5. Ma'lumotlar jadvalini tuzish -----------------------------------------
tash_df <- data.frame(
  month_num = 1:12,
  month     = factor(month.abb, levels = month.abb),
  precip    = tash_pr,
  temp      = tash_tmp
)

cat("2019 oylik yog'in (mm) :", round(tash_pr,  1), "\n")
cat("2019 oylik harorat (C) :", round(tash_tmp, 1), "\n")
cat("Yillik yog'in jami     :", round(sum(tash_pr,  na.rm = TRUE), 1), "mm\n")
cat("Yillik o'rtacha harorat:", round(mean(tash_tmp, na.rm = TRUE), 1), "C\n")

# ---- 6. Ikki Y o'qli grafik uchun shkalani hisoblash -------------------------
# ASOSIY (chap)     o'q : Harorat  — tmp_min dan tmp_max gacha (°C)
# IKKILAMCHI (o'ng) o'q : Yog'in   — pr_min  dan pr_max  gacha (mm)
#
# Chiziqli moslashtirish: yog'in -> harorat shkalasi
#   harorat_ekv = tmp_min + (yogin - pr_min) * (tmp_max - tmp_min) /
#                                              (pr_max  - pr_min)
# Teskari moslashtirish (sec_axis uchun):
#   yogin = pr_min + (harorat_ekv - tmp_min) * (pr_max - pr_min) /
#                                              (tmp_max - tmp_min)

tmp_min <- -10;  tmp_max <- 40    # chap o'q chegaralari (°C)
pr_min  <-   0;  pr_max  <- 125   # o'ng o'q chegaralari (mm)

# Masshtab koeffitsienti va siljish
k <- (tmp_max - tmp_min) / (pr_max - pr_min)   # °C / mm
b <- tmp_min - k * pr_min                       # siljish (offset)

# Yog'inni harorat shkalasiga o'tkazish
tash_df$precip_scaled <- b + k * tash_df$precip

# ---- 7. Vizualizatsiya -------------------------------------------------------
png(paste0(out_dir, "Figure_7c.png"),
    width = 11, height = 6.5, units = "in", res = 600)

print(
  ggplot(tash_df, aes(x = month_num)) +
    
    # --- Yog'in ustunchalari (harorat shkalasida, pastdan chiziladi) ---
    # geom_rect ishlatiladi: geom_col nol (0°C) dan chizadi,
    # lekin bizning o'q tmp_min = -10°C dan boshlanadi
    geom_rect(aes(xmin = month_num - 0.32,
                  xmax = month_num + 0.32,
                  ymin = -10,
                  ymax = precip_scaled,
                  fill = "Yog'in (mm)"),
              colour = "white",
              alpha  = 0.9) +
    
    # --- Harorat chizig'i ---
    geom_line(aes(y = temp, colour = "Harorat (\u00b0C)"),
              linewidth = 1.1) +
    geom_point(aes(y = temp, colour = "Harorat (\u00b0C)"),
               size  = 3.0,
               shape = 21,
               fill  = "#08306b") +
    
    # --- Harorat qiymatlarini ko'rsatish ---
    geom_text(aes(y     = temp,
                  label = paste0(round(temp, 1), "\u00b0C")),
              vjust       = -1.0,
              colour      = "#08306b",
              size        = 2.8,
              fontface    = "bold",
              show.legend = FALSE) +
    
    # --- Yog'in qiymatlarini ustunchalarda ko'rsatish ---
    geom_text(data = subset(tash_df, precip > 3),
              aes(y = precip_scaled, label = round(precip, 0)),
              vjust       = -0.5,
              colour      = "cyan4",
              size        = 2.8,
              fontface    = "bold",
              show.legend = FALSE) +
    
    # --- Shartli belgi uchun rang ---
    scale_fill_manual(
      name   = NULL,
      values = c("Yog'in (mm)" = "cyan4")
    ) +
    scale_colour_manual(
      name   = NULL,
      values = c("Harorat (\u00b0C)" = "#08306b")
    ) +
    # Shartli belgini sozlash
    guides(
      fill   = guide_legend(override.aes = list(shape = 15, size = 5,
                                                linetype = 0, alpha = 0.9)),
      colour = guide_legend(override.aes = list(shape = 21, size = 3,
                                                fill  = "#08306b",
                                                linewidth = 1))
    ) +
    
    # --- Asosiy (chap) va ikkilamchi (o'ng) o'qlarni nomlash ---
    scale_y_continuous(
      name   = "Harorat (\u00b0C)",
      limits = c(tmp_min, tmp_max),
      breaks = seq(tmp_min, tmp_max, by = 10),
      labels = paste0(seq(tmp_min, tmp_max, by = 10), "\u00b0C"),
      expand = expansion(mult = c(0.02, 0.08)),
      sec.axis = sec_axis(
        transform = ~ (. - b) / k,
        name      = "Yog'in (mm/oy)",
        breaks    = seq(0, pr_max, by = 25),
        labels    = paste0(seq(0, pr_max, by = 25), " mm")
      )
    ) +
    
    # --- X o'qi: oylar ---
    scale_x_continuous(
      breaks = 1:12,
      labels = month.abb,
      expand = expansion(add = 0.5)
    ) +
    
    theme_bw(base_size = 13) +
    theme(
      plot.title         = element_text(face = "bold", size = 14),
      plot.subtitle      = element_text(size = 10, colour = "grey40"),
      # Chap o'q (harorat)
      axis.title.y.left  = element_text(colour = "#08306b", face = "bold"),
      axis.text.y.left   = element_text(colour = "#08306b"),
      # O'ng o'q (yog'in)
      axis.title.y.right = element_text(colour = "cyan4", face = "bold",
                                        angle = 90, vjust = 0.5),
      axis.text.y.right  = element_text(colour = "cyan4"),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      # Shartli belgilar
      legend.position    = "bottom",
      legend.text        = element_text(size = 11),
      legend.key.size    = unit(0.7, "cm")
    ) +
    
    labs(
      title    = "Toshkent shahri bo'yicha o'rtacha harorat va yog'in qiymatlari (2019)",
      subtitle = paste0(
        "Lokatsiya: ", lon_pr[pr_lon_idx], "\u00b0E, ",
        lat_pr[pr_lat_idx], "\u00b0N  |  ",
        "Manba: CRU TS4.04"
      ),
      x        = "Oylar"
    )
)

dev.off()

cat("Toshkent bo'yicha grafik quyidagi papkada saqlandi:", out_dir, "\n")
