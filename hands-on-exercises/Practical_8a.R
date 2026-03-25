# ==============================================================================
#   ###  G I D R O L O G I Y A D A   R   D A N   F O Y D A L A N I S H  ###
#   ###             U S I N G   R   I N    H Y D R O L O G Y            ###
#   ###                         Version 1.2.0                           ###  
#       Mualliflar / Authors: ZULFIYA Kuranboyeva & GAVKHAR Mamadjanova                      
# ==============================================================================
#
# ---------------------------------------------------------------------------- #
#                       8a-amaliy mashg'ulot / Practical 8a                    #
#                 GPM IMERG yog'ingrachilik ma'lumotlarini ishlash             #
#                      Analysing GPM IMERG precipitation data                  #
# ---------------------------------------------------------------------------- #

# in Uzbek
# Ma'lumot uchun yog'ingarchilik bo'yicha GPM IMERG V6 (https://gpm.nasa.gov/data/imerg) 
# sun'iy yo'ldoshidan O'zbekiston bo'ylab 20/04/2022 sanasida olingan soatlik 
# yo'gin miqdorlari asosida yog'ingarchilikning kunlik umumiy qiymatini hisoblash 
# va vizualizatsiya qilish R dasturlash tilida ko'rsatilgan.

library(terra)
library(viridis)
library(fields)

# ----- tiff fayllarni o'qish va ma'lumotlarni tayyorlash ----------------------

tifpath  <- "C:/User/Data/GPM_IMERG_20220420" # Ushbu qator foydalanuvchi tomonidan tahrirlanadi
tif_files <- list.files(tifpath, pattern = "\\.tif$", full.names = TRUE)
tif_files <- sort(tif_files)
r <- rast(tif_files)

nhours <- 24
r24 <- r[[1:nhours]]
total_24h <- sum(r24) # soatlik yo'gin ma'lumotlari asosida kunlik summani hisoblash

# ----- Colorbarni tartibga keltirish ------------------------------------------

cols <- viridis(100, direction = -1)
zlim_total  <- c(0, 80)

# ----- Yog'in kartasining parametrlarini tayyorlash ---------------------------

layout(matrix(1:1, nrow = 1, ncol = 1, byrow = TRUE))

par(oma = c(1, 1, 1, 1),
    mar = c(3, 3, 3, 8))  

plg_opts <- list(
  x       = "right",
  y       = "center",
  title   = "mm",
  title.cex = 1.1,
  title.loc = "top" 
  
)

pax_opts <- list(
  side  = 1:2,
  tick  = 1:2,
  lab   = 1:2,
  retro = TRUE
)

# ----- Visualisatsiya va natijani fayl ko'rinishda saqlash --------------------

png("C:/User/Figures/Figure_8a.png", # Ushbu qator foydalanuvchi tomonidan tahrirlanadi
    width = 3200, height = 2000, res = 350)  # HD size + resolution

plot(total_24h,
     col    = cols,
     range  = zlim_total,
     main   = "GPM IMERG kunlik yog'in miqdori 20/04/2022",
     axes   = TRUE,
     legend = TRUE,
     plg    = plg_opts,
     pax    = pax_opts)

dev.off()
