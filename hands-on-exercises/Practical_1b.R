# ==============================================================================
#
#   ###  G I D R O L O G I Y A D A   R   D A N   F O Y D A L A N I S H  ###
#   ###             U S I N G   R   I N    H Y D R O L O G Y            ###
#   ###                         Version 1.2.0                           ###  
#        
#       Mualliflar / Authors: ZULFIYA Kuranboyeva & GAVKHAR Mamadjanova
#                       
# ==============================================================================
#
# ---------------------------------------------------------------------------- #
#                       1b-amaliy mashg'ulot / Practical 1b                    #
#  Suv sathining kompleks garfigini chizish / Composite graph of water level   #
# ---------------------------------------------------------------------------- #

# Code description in Uzbek     R 4.5.0
# Ma'lumot uchun ushbu 1b-amaliy mashg'ulotda suv sathining kompleks grafigini 
# ustun va qatorlardan iborat 2 x 2 panel ko'rinishida R dasturlash tilida chizish
# usuli ko'rsatilgan. Buyruqlarni yurguzish to'g'ri amalga oshirilganda olingan 
# natija Figure_1b.png da ko'rsatilgan grafik ko'rinishida bo'ladi.

# ----- 1. Ma'lumotlarni o'qish ------------------------------------------------

file <- read.csv("C:/User/Data/UGAM.csv")
summary(file)
colnames(file) <- c("yil", "oy", "kun", "H", "X", "T", "Q")

# ------ 2. Grafikni png faylda saqlash uchun joyni ko'rsatish -----------------

png("C:/User/Figures/Figure_1b.png", width=12, height=12, units="in", res=300)

# ------ 3. Tashqi bo‘sh joylar va ustun-qatorlarni ko'rsatish

par(oma = c(2, 2, 2, 2))

par(mfrow = c(2,2))   # 2 ustun va 2 qatorli panel

# ------ 4. Chizmalarni ustun va qatorlarda tartib bilan chizish ---------------
# ------ 4.1. Yog‘ingarchilik (joylashuv 1x1 chapdan 1-qator, 1-ustun) ---------

plot(file$H, type='l', col="red", xlab="kun", ylab="Suv sathi, H (m)")  #chizma chizish uchun buyruq berish.
points(file$H, col="blue")
title("Suv sathining yil ichida tebranish grafigi")

# ------ 4.2. Yog'ingarchilik (joylashuv 1x2) ----------------------------------

plot(file$X, type='h', col="forestgreen", xlab="kun", ylab="Yog'ingarchilik, X (mm)")
points(file$X, col="red")
title("Yillik yog'ingarchilik miqdori") 

# ------ 4.3. Harorat (joylashuv 2x1) ------------------------------------------

plot(file$T, type='l', col="darkgreen", xlab="kun", ylab="Harorat, T (°C)")
abline(h=0, col="green")
points(file$T, col="green")
title("Havo haroratining yil ichida o'zgarishi")

# ------ 4.4. Suv sarfi (joylashuv 2x2) ----------------------------------------

plot(file$Q, type='l', col="green", xlab="kun", ylab="Suv sarfi, Q (m3/s)")
points(file$Q, col="blue")
title("Suv sarfining yil ichida tebranish grafigi")

# ------ 5. Grafikni png faylda saqlash uchun buyruqni yakunlash ---------------

dev.off()
