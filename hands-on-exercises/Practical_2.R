# ==============================================================================
#
#   ###  G I D R O L O G I Y A D A   R   D A N   F O Y D A L A N I S H  ###
#   ###             U S I N G   R   I N    H Y D R O L O G Y            ###
#   ###                         Version 1.2.0                           ###  
#        
#       Mualliflar / Authors: ZULFIYA Kuranboyeva & GAVKHAR Mamadjanova
# ==============================================================================
#
# ---------------------------------------------------------------------------- #
#                       2-amaliy mashg'ulot / Practical 2                      #
#  Ionlarning yil ichida taqsimlanish grafigi / Annual distribution of ions    #
# ---------------------------------------------------------------------------- #

# Code description in Uzbek   R 4.5.0
# 2-amaliy mashg'ulotda ionlarning yil ichida taqsimlanishi va suv sarfi bilan 
# minerallashuv grafiklarini 1 x 2 panel ko'rinishida  R dasturlash tilida chizish
# usuli ko'rsatilgan. Buyruqlarni yurguzish to'g'ri amalga oshirilganda olingan 
# natija Figure_2.png dagi grafik ko'rinishida bo'ladi.

# ----- 1. Ma'lumotlarni oddiy usulda kiritish ---------------------------------

months <- 1:12
Q <- c(2.1, 2.7, 3.2, 19.8, 17.1, 12.5, 8.9, 4.8, 6.4, 2.8, 6.3, 3) # suv sarfi
U <- c(192.7, 208.2, 216.6, 245.7, 314.3, 93.7, 121.9, 142.2, 225, 218.1, 223.4, 232.9) # minerallashuv

# ------  Ionlar bo'yicha ma'lumotlarni kiritish -------------------------------

Ca   <- c(34.1, 38.1, 34.1, 34.1, 44.1, 16, 20, 25.1, 40.1, 32.1, 40.1, 36.1)
Mg   <- c(8.49, 8.5, 10.92, 12.75, 9.72, 3.67, 3.67, 5.44, 12.15, 12.14, 9.72, 12.14)
Cl   <- c(4.49, 4.49, 4.74, 8.98, 7.74, 2.49, 3.24, 2.99, 2.75, 4.99, 5.24, 6.24)
SO4  <- c(49, 52.5, 55.8, 83.1, 100, 17.2, 18.7, 24.9, 45, 48, 44.2, 54.4)
HCO3 <- c(88.9, 96.1, 101, 89.3, 113, 46.9, 66.1, 77.8, 122, 110, 116, 114)
NaK  <- c(7, 7.5, 9, 17, 30, 4, 7, 5, 1, 9, 5, 9)
 
# ------ 2. Grafikni png faylda saqlash uchun joyni ko'rsatish -----------------

png("C:/Users/Figures/Figure_2.png",
    width = 8, height = 12, units = "in", res = 300)

# ------ 3. Tashqi bo‘sh joylar va ustun-qatorlarni ko'rsatish -----------------

par(oma = c(2, 2, 2, 2))
par(mfrow = c(2,1))   # 1 ustun va 2 qatorli panel

length(months)  # 12 ta qiymat - oylarni tekshirish
length(Ca)      # 12 ta qiymat ekanligini tekshirish

# ------ 4. Ionlarni grafigini ketma-ketlikda chizish --------------------------

plot(
  months, Ca,
  type = "l",
  col = "red",
  lwd = 3,
  ylim = c(0, 150),
  main = "a) Ionlarning yil ichida taqsimlanishi",
  xlab = "Oylar",
  ylab = "Ionlar (mg/L)"
)

points(months, Ca, col = "red", pch = 16, cex = 1.1)  # Ca qiymatlarini nuqtalarda belgilash  

# Mg
lines(months, Mg, col = "blue", lwd = 3)
points(months, Mg, col = "blue", pch = 16)

# Cl
lines(months, Cl, col = "springgreen", lwd = 3)
points(months, Cl, col = "springgreen", pch = 16)

# SO4
lines(months, SO4, col = "orange", lwd = 3)
points(months, SO4, col = "orange", pch = 16)

# HCO3
lines(months, HCO3, col = "black", lwd = 3)
points(months, HCO3, col = "black", pch = 16)

# Na + K
lines(months, NaK, col = "brown", lwd = 3)
points(months, NaK, col = "brown", pch = 16)

legend(
  "topleft",
  legend = c("Ca", "Mg", "Cl", "SO4", "HCO3", "Na+K"),
  col = c("red", "blue", "springgreen", "orange", "black", "brown"),
  lwd = 3,
  pch = 16,
  cex = 1,
  bty = "n",
  ncol = 3
)

# ------ 5. Ionlar grafigini ketma-ketlikda chizish ---------------------------

plot(
  months, Q,
  type = "b",
  col = "cyan4",
  lwd = 3,
  pch = 16,
  ylim = c(0, 20),
  xlab = "Oylar",
  ylab = "Suv sarfi, Q (m³/s)",
  main = "b) Suv sarfi va minerallashuv orasidagi bog‘lanish"
)

par(new = TRUE)

plot(
  months, U,
  type = "b",
  col = "orchid3",
  lwd = 3,
  pch = 17,
  axes = FALSE,
  xlab = "",
  ylab = "",
  ylim = c(0, 350)
)

axis(side = 4, col = "orchid3", col.axis = "orchid3", col.ticks = "orchid3")

mtext("Minerallashuv, U (mg/L)", side = 4, line = 3, col = "orchid3")

legend(
  "topright",
  legend = c("Q, m³/s", "U, mg/L"),
  col = c("cyan4", "orchid3"),
  lwd = 3,
  pch = c(16, 17),
  cex = 1,
  bty = "n",
)

# ------ 5. Grafikni png faylda saqlash uchun buyruqni yakunlash ---------------

dev.off()
