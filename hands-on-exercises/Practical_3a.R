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
#                       3a-amaliy mashg'ulot / Practical 3                      #
#                 Moslashgan suv sathlari grafigini chizish                    #
#                  Plotting the adjusted water level graph                     #
# ---------------------------------------------------------------------------- #

# Readme in Uzbek   R 4.5.0
# 3a-amaliy mashg'ulotning 1-qismida Norin daryosi Kokjerdi va Koykerim gidropostlarida
# kuzatilgan suv sathlari grafigi chiziladi. Gidropostlarda yil davomida kuzatilgan 
# suv sathlarining minimum va maksimum qiymatlari har bir oy uchun ajratib olish va 
# ularni csv faylda saqlash usuli ko'rsatilgan. Buyruqlarni yurguzish to'g'ri amalga 
# oshirilganda olingan natijalar Figure_3a.png dagi grafik va 
# monthly_peaks_and_lows.csv fayli ko'rinishida bo'ladi.

# ----- 1. Ma'lumotlarni o'qish, sana va qisqa nomlar berish -------------------

file1<-read.csv("C:/Users/jt231697/Desktop/hands_on_exercise/Practical_3/norin_kokjerdi.csv")
colnames(file1) 
summary(file1)

file2<-read.csv("C:/Users/jt231697/Desktop/hands_on_exercise/Practical_3/norin_koykerim.csv")
colnames(file2)
summary(file2)

file1$date <- as.Date(sprintf("%04d-%02d-%02d", file1$year, file1$month, file1$day))
file2$date <- as.Date(sprintf("%04d-%02d-%02d", file2$year, file2$month, file2$day))

x <- 1:365      # X o'qiga 365 kunni kiritish buyrug'i
y1 <- file1$H
y2 <- file2$H

# ------ 2. 3a-grafikni png faylda saqlash uchun joyni ko'rsatish --------------

png("C:/Users/jt231697/Desktop/hands_on_exercise/Practical_3/Figure_3a.png",
    width = 12, height = 8, units = "in", res = 300)

par(oma = c(2, 2, 2, 2)) # grafik atrofidagi bo‘sh joylarni ko'rsatish

# ------ 3. Moslashgan suv sathlarini chizish ----------------------------------

# Norin-Kokjerdi
plot(
  x, y1,
  type = "l",
  col = "red",
  lwd = 2,
  xlim = c(0, 365),
  ylim = c(150, 550),
  xlab = "Kunlar",
  ylab = "Suv sathi, H (cm)",
  main = "Moslashgan suv sathlari"
)

# Norin-Koykerim
lines(x, y2, col = "limegreen", lwd = 2)

# Belgilarni kiritish
legend(
  "topleft",
  legend = c("Norin–Kokjerdi", "Norin–Koykerim"),
  col = c("red", "limegreen"),
  lwd = 2,
  lty = 1,
  cex = 1,
  bty = "n"
)

# ------ 4. Moslashgan suv sathlari bo'yicha kunlarni ajratib olish ------------

# 4.1. Ustunlarni tanlash
file1_sub <- file1[, c("date", "H")]
file2_sub <- file2[, c("date", "H")]

# 4.2. Suv sathlari bo'yicha ustunlarga yangi nom berish
colnames(file1_sub)[2] <- "Kokjerdi"
colnames(file2_sub)[2] <- "Koykerim"

# 4.3. Ma'lumotlarni sanalar bo'yicha birlashtirish
data <- merge(file1_sub, file2_sub, by = "date")

# 4.4. Yuqori qiymatli suv sathlarini aniqlash
data$common_level <- pmin(data$Kokjerdi, data$Koykerim, na.rm = TRUE)

# 4.5. Yil davomida kuzatilgan yuqori 30 ta qiymatni tanlab olish
#n <- min(30, nrow(data))
#top_30 <- data[order(-data$common_level), ][1:n, ]

# Natijani tekshirish
#top_30

#data$day_index <- as.integer(data$date - min(data$date)) + 1
#top_30$day_index <- as.integer(top_30$date - min(data$date)) + 1

#points(top_30$day_index, top_30$Kokjerdi, pch=19, col="red", cex=1.2)
#points(top_30$day_index, top_30$Koykerim, pch=19, col="limegreen", cex=1.2)

# 4.6.

data$month <- as.integer(format(data$date, "%m"))
data$day_index <- as.integer(data$date - min(data$date)) + 1

# 4.7. Oylar ichida kuzatilgan 3 ta yuqori qiymatlarni do.call 
# funksiyasi yordamida ajratib olish

k <- 3 

top_monthly <- do.call(rbind, lapply(split(data, data$month), function(df) {
  df <- df[order(-df$common_level), ]
  head(df, min(k, nrow(df)))
}))

# 4.8. Ajratilgan yuqori suv sathlari qiymatlarini nuqtalarda ko'rsatish

points(top_monthly$day_index, top_monthly$Kokjerdi, pch = 5, col = "violet", cex = 1)
points(top_monthly$day_index, top_monthly$Koykerim, pch = 5, col = "violet", cex = 1)

# 4.9. Har bir oy davomida kuzatilgan 3 ta past qiymatlarni ajratib olish

k <- 3

low_monthly <- do.call(rbind, lapply(split(data, data$month), function(df) {
  df <- df[order(df$common_level), ]
  head(df, min(k, nrow(df)))
}))

# 4.10. Ajratilgan past suv sathlari qiymatlarini nuqtalarda ko'rsatish

points(low_monthly$day_index, low_monthly$Kokjerdi, pch = 6, col = "orange", cex = 1)
points(low_monthly$day_index, low_monthly$Koykerim, pch = 6, col = "orange", cex = 1)

# 4.11. belgilarni legendaga kiritish

legend(
  "topright",
  legend = c("yuqori", "past"),
  col = c("violet", "orange"),
  pch = c(5,6),
  cex = 1,
  bty = "n"
)

# ------ 5. Moslashgan suv sathlari bo'yicha qiymatlarni fayl ko'rinishida saqlash

# 5.1. Yuqorida ajratib olingan qiymatlarni belgilab olish

top_monthly$type <- "Peak"
low_monthly$type <- "Low"

# 5.2. Suv sathlari qiymatlarini 1 faylda saqlash uchun birlashtirish
peaks_lows <- rbind(top_monthly, low_monthly)

# 5.3. Tanlangan qiymatlarni sanalar bo'yicha joylashtirish
peaks_lows <- peaks_lows[order(peaks_lows$date), ]

# 5.4. Qiymatlarni CSV faylda saqlash
write.csv(
  peaks_lows,
  "C:/Users/jt231697/Desktop/hands_on_exercise/Practical_3/monthly_peaks_and_lows.csv",
  row.names = FALSE
)

dev.off()