# in Uzbek
# Ma'lumot uchun yog'ingarchilik bo'yicha GPM IMERG (https://gpm.nasa.gov/data/imerg) 
# sun'iy yo'ldoshidan O'zbekiston bo'ylab 20/04/2022 sanasida olingan soatlik 
# yo'gin miqdorlarini R dasturlash tilida ggplot2 yordamida vizualizatsiya qilish
# usuli ko'rsatilgan.

library(terra)
library(ggplot2)
library(viridis)
library(dplyr)
library(tidyr)
library(scales)

# ----- tiff fayllarni o'qish va ma'lumotlarni tayyorlash -----------------------

tifpath  <- "C:/User/Data/GPM_IMERG_20220420" # Ushbu qator foydalanuvchi tomonidan tahrirlanadi
tif_files <- list.files(tifpath, pattern = "\\.tif$", full.names = TRUE)
tif_files <- sort(tif_files)
r <- rast(tif_files)

nhours <- 24
r24 <- r[[1:nhours]]

# Name layers 00–23
names(r24) <- sprintf("H%02d", 1:nhours)

# ----- raster ma'lumotni df ko'rinishga o'tkazish -----------------------------

df_r <- as.data.frame(r24, xy = TRUE, na.rm = FALSE)

df_long <- df_r |>
  pivot_longer(-c(x, y), names_to = "hour", values_to = "precip")

# ----- Vaqt bilan bog'liq ma'lumotni qayta ishlash ----------------------------

hour_numeric <- as.numeric(substr(df_long$hour, 2, 3))
df_long$label <- paste0("UTC ", sprintf("%02d", hour_numeric), ":00")
df_long$label[df_long$label == "UTC 24:00"] <- "UTC 23:59"
df_long$label <- factor(df_long$label, levels = unique(df_long$label))

# ----- Max va min qiymatlar asosisda avtomatik rang berish --------------------

min_val <- min(df_long$precip, na.rm = TRUE)
max_val <- max(df_long$precip, na.rm = TRUE) * 1.05

# ----- Geografik kenglik va uzoqlikni tahrirlash ------------------------------

deg_x <- function(x) ifelse(x == 0, "", paste0(round(x), "°E"))
deg_y <- function(x) ifelse(x == 0, "", paste0(round(x), "°N"))

# ----- Vizualizatsiya ---------------------------------------------------------
p_hourly <- ggplot(df_long, aes(x = x, y = y, fill = precip)) +
  geom_raster() +
  coord_equal() +
  scale_fill_viridis_c(
    option = "viridis",
    direction = -1,
    limits = c(min_val, max_val),
    name = "mm"
  ) +
  facet_wrap(~ label, ncol = 4) +
  labs(
    title = "GPM IMERG bo'yicha soatlik yog'in miqdorlari 20/04/2022",
    x = NULL,
    y = NULL
  ) +
  scale_x_continuous(labels = deg_x) +   # ← Longitude with °E
  scale_y_continuous(labels = deg_y) +   # ← Latitude with °N
  theme_bw() +
  theme(
    panel.grid       = element_blank(),              # grid 
    panel.border     = element_rect(color = "black"),# border back
    panel.background = element_rect(fill = "white", colour = NA), # white background
    strip.background = element_rect(fill = "azure"), # white facet strips
    strip.text       = element_text(face = "bold"),
    legend.position  = "right",
    axis.text        = element_text(size = 9),
    plot.title       = element_text(face = "bold", size = 14)
  )

# ----- Natijani fayl ko'rinishida saqlash -------------------------------------

ggsave("C:/User/Figures/Figure_8b.png",  # Ushbu qator foydalanuvchi tomonidan tahrirlanadi
       p_hourly, width = 12, height = 8, dpi = 350)

