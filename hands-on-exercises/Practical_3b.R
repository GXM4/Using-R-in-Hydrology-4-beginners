#     Mualliflar / Code authors: ZULFIYA Kuranboyeva & GAVKHAR Mamadjanova     #
#                                                                              #
# ---------------------------------------------------------------------------- #
#                       3b-amaliy mashg'ulot / Practical 3b                    #
#               Moslashgan suv sathlari orasidagi bog'lanishni aniqlash        #  
#                 Linear relationship between adjusted water level             #
# ---------------------------------------------------------------------------- #
#
# Code description in Uzbek |  R version >4.5.0
# 3b-amaliy mashg'ulotda Norin daryosi Kokjerdi va Kyokerim gidropostlaridan tanlab
# olingan suv sathlari orasidagi to'g'ri chiziqli bog'lanishni aniqlash usuli ko'rsatilgan. 
# Buyruqlarni yurguzish to'g'ri amalga oshirilganda olingan natijalar Figure_3b.png
# dagi grafik ko'rinishida bo'ladi.

# ----- 1. Ma'lumotlarni o'qish va tez qarab chiqish  --------------------------

file3 <- read.csv("C:/Users/Data/monthly_peaks_and_lows_paired.csv", header = TRUE)
summary(file3)

# ------ 2. 3b-grafikni png faylda saqlash uchun joyni ko'rsatish --------------

png("C:/Users/Figures/Figure_3b.png",
    width = 6, height = 6, units = "in", res = 300)

# ------ 3. To'g'ri chiziqli bog'lanish tenglamasi ------------------------------

model <- lm(Kyokerim_match_H ~ Kokjerdi, data = file3)
summary(model)

# ------ 4. Grafikni chizish va chiziqli bog'lanishni ko'rsatish ----------------

plot(
  file3$Kokjerdi, file3$Kyokerim_match_H,
  col = "cornflowerblue",
  pch = 16,
  cex = 1.5,
  xlim = c(200, 600),
  ylim = c(100, 550),
  xlab = "Kokjerdi, H (cm)",
  ylab = "Kyokerim, H (cm)",
  main = "Moslashgan suv sathlarining bog‘lanish grafigi"
)

# ------ 5. Regressiya chizig'ini o'tkazish va R qiymatini ko'rsatish -----------

abline(model, col = "violet", lwd = 3)

legend(
  "bottomright",
  bty = "n", cex = 1.2,
  legend = paste("R² =", round(summary(model)$adj.r.squared, 3))
)

dev.off()
