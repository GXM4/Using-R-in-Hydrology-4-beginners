# ==============================================================================
#     G I D R O L O G I Y A D A   R   D A N   F O Y D A L A N I S H  
#                U S I N G   R   I N    H Y D R O L O G Y            
#                           Version 1.2.0                             
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
# 3b-amaliy mashg'ulotda Norin daryosi Kokjerdi va Kyokerim gidropostlaridan tanlab
# olingan suv sathlari orasidagi to'g'ri chiziqli bog'lanishni aniqlash usuli ko'rsatilgan. 
# Buyruqlarni yurguzish to'g'ri amalga oshirilganda olingan natijalar Figure_3b.png
# dagi grafik ko'rinishida bo'ladi.

# ----- 1. Ma'lumotlarni o'qish va tez qarab chiqish  --------------------------

file3 <- read.csv("C:/User/Data/UGAM_river.csv",
#ushbu qator foydalanuvchi tomonidan tahrirlanadi 
        header = TRUE
)
summary(file3)

x<-file3$Kokjerdi
y<-file3$Kyokerim_match_H
# ------ 2. 3b-grafikni png faylda saqlash uchun joyni ko'rsatish --------------

png("C:/Users/Figures/Figure_3b.png",
    width = 6, height = 6, units = "in", res = 300) 
#ushbu qator foydalanuvchi tomonidan tahrirlanadi 

# ------ 3. To'g'ri chiziqli bog'lanish tenglamasi -----------------------------

model <- lm(Kyokerim_match_H ~ Kokjerdi, data = file3)
summary(model)

# ------ 4. Grafikni chizish va chiziqli bog'lanishni ko'rsatish ---------------

plot(x, y, pch = 16, col = "cornflowerblue",
     cex = 1.5,
     xlim = c(340, 510),
     ylim = c(150, 400),
     xlab = "Kokjerdi, H (cm)",
     ylab = "Kyokerim, H (cm)",
     main = "Moslashgan suv sathlarining bog‘lanish grafigi")


abline(model, col = 4, lwd = 3)# Regressiya chizig'ini o'tkazish

segments(x0 = x, x1 = x, y0 = y, y1 = predict(model),
         lwd = 1, col = "red") #segmentlar va tenglamalar bo'yicha buyruqni berish

coef <- round(coef(model), 2) #koeffitsiyentni yaxlitlash

# ------ 5. R qiymati va umumiy tenglamani ko'rsatish --------------------------

legend(
  "bottomright",
  bty = "n", cex = 1.1,
  legend = c(
    paste("y =", model_coef[1], ifelse(model_coef[2] >= 0, "+", "-"), abs(model_coef[2]), "x"),
    paste("R² =", round(summary(model)$adj.r.squared, 3))
  )
)

dev.off()
