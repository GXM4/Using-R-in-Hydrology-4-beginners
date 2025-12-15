# ==============================================================================
#
#   ###  G I D R O L O G I Y A D A   R   D A N   F O Y D A L A N I S H  ###
#   ###             U S I N G   R   I N    H Y D R O L O G Y            ###
#   ###                         Version 1.2.0                             ###  
#        
#       Mualliflar / Authors: ZULFIYA Kuranboyeva & GAVKHAR Mamadjanova
#                       
# ==============================================================================
#
# ---------------------------------------------------------------------------- #
#                       1-amaliy mashg'ulot / Practical 1                      #
#  Suv sathining kompleks garfigini chizish / Composite graph of water level   #
# ---------------------------------------------------------------------------- #

# Readme in Uzbek    R 4.5.0
# Ma'lumot uchun ushbu 1-amaliy mashg'ulotda suv sathining kompleks grafigini 
# R dasturlash tilida chizish usuli bosqichma-bosqich ko'rsatilgan. Buyruqlarni
# yurguzish to'g'ri amalga oshirilganda olingan natija Figure_1a.png da ko'rsatilgan
# grafik ko'rinishida bo'ladi.  


# ----- 1. Ma'lumotlarni o'qish va qisqa nomlar berish -------------------------

file <- read.csv("C:/User/Data/UGAM_river.csv")
summary(file)
colnames(file) <- c("yil", "oy", "kun", "H", "X", "T", "Q")

x  <- seq_along(file$kun)   # vaqt (kun) faylda 365 kunlik seriya-ma'lumot mavjud
y1 <- file$X                # yog‘in, X 
y2 <- file$H                # suv sathi, H
y3 <- file$T                # harorat, T
y4 <- file$Q                # suv sarfi, Q

# ------ 2. Grafikni png faylda saqlash uchun joyni ko'rsatish -----------------

png("C:/User/Figures/Figure_1a.png",
    width = 12, height = 8, units = "in", res = 300)

# ------ 3. Kompleks grafik (1 oynada 4 ta parametrlarni o'zgarish grafigi) ----

par(oma = c(2, 2, 2, 2)) # Tashqi bo‘sh joylar

# ------ 3.1. Yog‘ingarchilik (ustuncha ko‘rinishida) --------------------------

plot.new()        # Bo‘sh grafik oynasini ochamiz (asosiy koordinata)
plot(x, y1,
     type = "h",  # ustunchali grafik
     xlim = c(0, 365),
     ylim = c(0, 100),
     axes = FALSE, xlab = "", ylab = "",
     lwd = 3, col = "azure4")
axis(1,
     at = c(seq(1, 360, by = 30), 365),
     labels = c(seq(1, 360, by = 30), 365),
     cex.axis = 0.9)  # pastki o‘q (kunlar)
axis(2, col.axis = "azure4", col = "azure4", col.ticks = "azure4" )  # chapdan 1-o'q, yog'in
mtext("Kunlar", side = 1, line = 3, cex = 1)      #  X o'qini  nomlash
box()

# ------ 3.2. Suv sathi (chap o‘qda, qizil chiziq) -----------------------------

par(new = TRUE)
plot.window(xlim = c(0, 365), ylim = c(200, 350))
lines(x, y2, col = "red", lwd = 2)
axis(2, col.axis = "red", col = "red", col.ticks = "red", line = 2)  # chapdan 2-o‘q, suv sathi

# ------ 3.3. Harorat (ikkinchi chap o‘q, yashil chiziq) -----------------------

par(new = TRUE)
plot.window(xlim = c(0, 365),
            ylim = c(-20, 40))
lines(x, y3, col = "springgreen2", lwd = 2)
abline(h = 0, col = "springgreen2", lty = 2)  # 0 daraja chizig‘i
axis(4,
     col.axis = "springgreen2", col = "springgreen2", col.ticks = "springgreen2",
     line = 2)  # o'ngdan 2-o'qni biroz surib chiqaramiz, harorat

# ------ 3.4. Suv sarfi (o‘ng o‘qda, ko‘k chiziq) ------------------------------

par(new = TRUE)
plot.window(xlim = c(0, 365),
            ylim = c(0, 200))
lines(x, y4, col = "blue", lwd = 2)
axis(4, col.axis = "blue", col = "blue", col.ticks = "blue")   # o‘ngdan 1-o‘q, suv sarfi

# ------- 4. Umumiy sarlavha va belgilarni joylashtirish -----------------------

title("Suv sathining kompleks grafigi", cex = 1) # umumiy sarlavha

usr <- par("usr")   # y o'qlarininig belgilarini joylashtirish uchun umumiy buyruq
mtext("X (mm)", side = 3, at = usr[1], adj = 0, col = "azure4", las = 1, cex = 0.9)
mtext("H (cm)", side = 3, at = usr[1] - 0.05 * diff(usr[1:2]), col = "red", las = 1, cex = 0.9)
mtext("T (°C)", side = 3, at = usr[1] + 1.05 * diff(usr[1:2]), col = "springgreen", las = 1, cex = 0.9)
mtext("Q (m³/s)", side = 3, at = usr[2], adj = 1, col = "blue", las = 1, cex = 0.9)

legend("topright",
       legend = c("H (suv sathi)", "Q (suv sarfi)", "T (harorat)",  "X (yog'in)"),
       bty = "n",
       col = c("red", "blue", "springgreen2", "azure4"),
       lwd = 2, cex = 0.8, lty = 1)

# ------ 5. Grafikni png faylda saqlash uchun buyruqni yakunlash ---------------

dev.off()


