#     Mualliflar / Code authors: ZULFIYA Kuranboyeva & GAVKHAR Mamadjanova     #  
#                                                                              #
# ---------------------------------------------------------------------------- #
#                     3a-amaliy mashg'ulot / Practical 3a                      #
#                 Moslashgan suv sathlari grafigini chizish                    #
#                  Plotting the adjusted water level graph                     #
# ---------------------------------------------------------------------------- #
#
# Code description in Uzbek |  R version >4.5.0
# 3a-amaliy mashg'ulotning 1-qismida Norin daryosi Kokjerdi va Koykerim gidropostlarida
# kuzatilgan suv sathlari grafigi chiziladi. Gidropostlarda yil davomida kuzatilgan 
# suv sathlarining minimum va maksimum qiymatlari har bir oy uchun ajratib olish va 
# ularni csv faylda saqlash usuli ko'rsatilgan. Buyruqlarni yurguzish to'g'ri amalga 
# oshirilganda olingan natijalar Figure_3a.png dagi grafik va 
# monthly_peaks_and_lows.csv fayli ko'rinishida bo'ladi.

# ----- 1. Ma'lumotlarni o'qish, sana va qisqa nomlar berish -------------------

file1 <- read.csv("C:/Users/Data/norin_kokjerdi.csv")
colnames(file1)
summary(file1)

file2 <- read.csv("C:/Users/Data/norin_kyokerim.csv")
colnames(file2)
summary(file2)

file1$date <- as.Date(sprintf("%04d-%02d-%02d", file1$year, file1$month, file1$day))
file2$date <- as.Date(sprintf("%04d-%02d-%02d", file2$year, file2$month, file2$day))

# Ma’lumotlar uzunligiga mos ketma-ketlikdan foydalanish
y1 <- file1$H
y2 <- file2$H
x  <- seq_along(y1)

# ------ 2. Grafik sozlamalari -------------------------------------------------

png("C:/Users/Figures/Figure_3a.png",
    width = 12, height = 8, units = "in", res = 300)

par(oma = c(2, 2, 2, 2)) # grafik atrofidagi bo‘sh joylarni ko'rsatish

# ------ 3. Moslashgan suv sathlarini chizish ----------------------------------

# 3.1. Norin-Kokjerdi bo'yicha
plot(
  x, y1,
  type = "l",
  xlim = c(0, max(x)),
  ylim = c(150, 550),
  axes = FALSE, xlab = "", ylab = "",
  lwd = 3, col = "cornflowerblue"
)
axis(1,
     at = c(seq(1, 360, by = 30), 365),
     labels = c(seq(1, 360, by = 30), 365),
     cex.axis = 1)  # pastki o‘q (kunlar)
axis(2, cex.axis = 1)
mtext("Kunlar", side = 1, line = 3, cex = 1)  #  X o'qini  nomlash
mtext("Suv sathi, H (cm)", side = 2, line = 3, cex = 1)  #  Y o'qini  nomlash
box()

# 3.2. Norin-Kyokerim
lines(x, y2, col = "cyan2", lwd = 3)

legend(
  "topleft",
  legend = c("Norin–Kokjerdi (yuqori)", "Norin–Kyokerim (quyi)"),
  col    = c("cornflowerblue", "cyan2"),
  lwd    = 2,
  lty    = 1,
  cex    = 0.8,
  bty    = "n"
)

title("Kundalik suv sathining yil ichida tebranishi va moslashgan suv sathlari", cex = 1) # umumiy sarlavha

# ------ 4. Ma'lumotlarni tayyorlash -------------------------------------------

file1_sub <- file1[, c("date", "H")]
file2_sub <- file2[, c("date", "H")]

colnames(file1_sub)[2] <- "Kokjerdi"
colnames(file2_sub)[2] <- "Kyokerim"

data <- merge(file1_sub, file2_sub, by = "date")
data$common_level <- pmin(data$Kokjerdi, data$Kyokerim, na.rm = TRUE)

data$month     <- as.integer(format(data$date, "%m"))
data$day_index <- as.integer(data$date - min(data$date)) + 1

# ------ 5. Kokjerdi bo'yicha katta/kichik qiymatlarni tanlab olish ------------

k <- 2    # har oy uchun 2 tadan katta/kichik qiymatlar

# 5.1. PEAKS: yuqori stansiya (Kokjerdi) bo'yicha baland qiymatli suv sathi

top_monthly <- do.call(rbind, lapply(split(data, data$month), function(df) {
  df <- df[order(-df$Kokjerdi), ]      # faqat Kokjerdi bo'yicha tartiblaymiz
  head(df, min(k, nrow(df)))
}))

# 5.2. LOWS: yuqori stansiya (Kokjerdi) bo'yicha past qiymatli suv sathi

low_monthly <- do.call(rbind, lapply(split(data, data$month), function(df) {
  df <- df[order(df$Kokjerdi), ]       # past qiymatlar Kokjerdi bo'yicha
  head(df, min(k, nrow(df)))
}))

# ------ 6. Kyokerim uchun mos suv sathlarni tanslash - matching ---------------

# 6.1. Macth_unique funksiyasi o'zgartitilmasin!!!

match_unique <- function(events_df, all_data, col_name, is_peak = TRUE, window = 1) {
  # events_df: top_monthly yoki low_monthly (ustunlar orasida 'date' bo'lishi kerak)
  # all_data: to'liq 'data' (date, day_index, Kyokerim va h.k.)
  # col_name: "Kyokerim"
  # is_peak: TRUE -> max, FALSE -> min
  # window: ma'lum kunga qadar qidirish (0..window)
  
  used_dates <- as.Date(character(0))  # ishlatilgan Kyokerim sanalari
  
  n <- nrow(events_df)
  match_date      <- as.Date(rep(NA, n))
  match_H         <- rep(NA_real_,    n)
  match_day_index <- rep(NA_integer_, n)
  gap_days        <- rep(NA_integer_, n)
  
  for (i in seq_len(n)) {
    d <- events_df$date[i]
    
    # Shu kun va keyingi 0–window kun oralig'i
    sub <- all_data[
      all_data$date >= d &
        all_data$date <= (d + window) &
        !(all_data$date %in% used_dates),
    ]
    
    if (nrow(sub) == 0 || all(is.na(sub[[col_name]]))) {
      next  # mos keluvchi Kyokerim yo'q -> NA qoldiramiz
    }
    
    idx <- if (is_peak) which.max(sub[[col_name]]) else which.min(sub[[col_name]])
    sel <- sub[idx, ]
    
    match_date[i]      <- sel$date
    match_H[i]         <- sel[[col_name]]
    match_day_index[i] <- sel$day_index
    gap_days[i]        <- as.integer(sel$date - d)
    
    used_dates <- c(used_dates, sel$date)  # bu sanani boshqa eventlar uchun ishlatmaymiz
  }
  
  data.frame(
    match_date      = match_date,
    match_H         = match_H,
    match_day_index = match_day_index,
    gap_days        = gap_days
  )
}

# 6.2. PEAKS: Kyokerim peak 0–1 kun keyin

peak_matches <- match_unique(
  events_df = top_monthly,
  all_data  = data,
  col_name  = "Kyokerim",
  is_peak   = TRUE,
  window    = 1
)

top_monthly$Kyokerim_match_date      <- peak_matches$match_date
top_monthly$Kyokerim_match_H         <- peak_matches$match_H
top_monthly$Kyokerim_match_day_index <- peak_matches$match_day_index
top_monthly$gap_days                 <- peak_matches$gap_days

# 6.3. LOWS: Kyokerim low 0–1 kun keyin

low_matches <- match_unique(
  events_df = low_monthly,
  all_data  = data,
  col_name  = "Kyokerim",
  is_peak   = FALSE,
  window    = 1
)

low_monthly$Kyokerim_match_date      <- low_matches$match_date
low_monthly$Kyokerim_match_H         <- low_matches$match_H
low_monthly$Kyokerim_match_day_index <- low_matches$match_day_index
low_monthly$gap_days                 <- low_matches$gap_days

# ------ 7. Nuqtalarni grafikda ko'rsatish -------------------------------------

# 7.1. Kokjerdi bo'yicha peak/low
points(top_monthly$day_index, top_monthly$Kokjerdi,
       pch = 5, col = "violet", cex = 1)
points(low_monthly$day_index, low_monthly$Kokjerdi,
       pch = 6, col = "orange", cex = 1)

# 7.2. Kyokerim uchun mos keluvchi peak/low
points(top_monthly$Kyokerim_match_day_index, top_monthly$Kyokerim_match_H,
       pch = 5, col = "blue", cex = 1)
points(low_monthly$Kyokerim_match_day_index, low_monthly$Kyokerim_match_H,
       pch = 6, col = "darkgreen", cex = 1)

# 7.3 Beligilarni grafikda ko'rsatish
legend(
  "topright",
  legend = c("Kokjerdi (peak)", "Kokjerdi (low)",
             "Kyokerim (moslashgan, peak)", "Kyokerim (moslashgan, low)"),
  col = c("violet", "orange", "blue", "darkgreen"),
  pch = c(5, 6, 5, 6),
  cex = 0.8,
  bty = "n"
)

# ------ 8. Natijalarni faylga saqlash -----------------------------------------

# 8.1. Fayl uchun tegishli ma'lumotlarni analiz qilish
top_monthly$type <- "Peak"
low_monthly$type <- "Low"

peaks_lows <- rbind(top_monthly, low_monthly)
peaks_lows <- peaks_lows[order(peaks_lows$date), ]

# 8.2. Fayl uchun kerakli ma'lumotlarni tanlab olish
peaks_lows_out <- peaks_lows[, c(
  "date",
  "Kokjerdi",
  "Kyokerim",
  "Kyokerim_match_date",
  "Kyokerim_match_H",
  "gap_days",
  "type"
)]

# 8.3. Ustunlarni nomlash
names(peaks_lows_out) <- c(
  "date",
  "Kokjerdi",
  "Kyokerim",
  "Kyokerim_match_date",
  "Kyokerim_match_H",
  "gap_days",
  "type"
)

# 8.4. Faylni csv formatda saqlash
write.csv(
  peaks_lows_out,
  "C:/Users/Data/monthly_peaks_and_lows_paired.csv",
  row.names = FALSE
)

dev.off()
