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
#                       3b-amaliy mashg'ulot / Practical 3b                    #
#               Moslashgan suv sathlari orasidagi bog'lanishni aniqlash        #  
#                 Linear relationship between adjusted water level             #
# ---------------------------------------------------------------------------- #

# Readme in Uzbek   R 4.5.0
# 3b-amaliy mashg'ulotda Norin daryosi Kokjerdi va Koykerim gidropostlarida tanlab
# olingan suv sathlari orasidagi to'g'ri chiziqli bog'lanishni aniqlash usuli ko'rsatilgan. 
# Buyruqlarni yurguzish to'g'ri amalga oshirilganda olingan natijalar Figure_3b.png
# dagi grafik ko'rinishida bo'ladi.

# ----- 1. Ma'lumotlarni o'qish va tez qarab chiqish  --------------------------

file3 <- read.csv("C:/Users/jt231697/Desktop/hands_on_exercise/Practical_3/monthly_peaks_and_lows.csv", header = TRUE)
summary(file3)

# ------ 2. 3b-grafikni png faylda saqlash uchun joyni ko'rsatish --------------

png("C:/Users/jt231697/Desktop/hands_on_exercise/Practical_3/Figure_3b.png",
    width = 10, height = 10, units = "in", res = 300)

par(oma = c(2, 2, 2, 2)) # grafik atrofidagi bo‘sh joylarni ko'rsatish

# ------ 3. To'g'ri chiziqli bog'lanish tenglamasi ------------------------------

model <- lm(Koykerim ~ Kokjerdi, data = file3)
summary(model)

# ------ 4. Grafikni chizish va chiziqli bog'lanishni ko'rsatish ----------------

plot(
  file3$Kokjerdi, file3$Koykerim,
  col = "red",
  pch = 16,
  cex = 2,
  xlim = c(200, 600),
  ylim = c(100, 550),
  xlab = "Kokjerdi, H (cm)",
  ylab = "Koykerim, H (cm)",
  main = "Moslashgan suv sathlarining bog‘lanish grafigi"
)

# ------ 5. Regressiya chizig'ini o'tkazish va R qiymatini ko'rsatish -----------

abline(model, col = "blue", lwd = 3)

legend(
  "bottomright",
  bty = "n", cex = 1.2,
  legend = paste("Adjusted R² =", round(summary(model)$adj.r.squared, 3))
)

# ------ 6. Grafikni png faylda saqlash uchun buyruqni yakunlash ---------------

dev.off()