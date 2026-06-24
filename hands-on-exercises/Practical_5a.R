#Authors: GAVKHAR Mamadjanova & ZULFIYA Kuranboyeva 
#----------------------------------------------------------------------------- #
#                       5a-amaliy mashg'ulot / Practical 5a                     #
# Y=a+b*x ko'rinishidagi tenglamaning noma'lum parametrlarini 3 usul bilan aniqlash
#     Estimation of the unknown parameters of the hydrological relationship 
#                           Y=a+b*x using three different methods
# 
# Authors: GAVKHAR Mamadjanova & ZULFIYA Kuranboyeva
# ----------------------------------------------------------------------------- #
#
# Ushbu amaliy mashg'ulotda berilgan o'zgaruvchilar:
# t - havo xarorati (°C) va h - suv yuzasidan bug'lanish qatlami (mm)
# Tenglama quyidagi holatga keltiriladi:
# h = a + b*t va bu yerdan a va b parametrlar hisoblablanadi.

# ----- 1. Boshlang'ich ma'lumotlarni kiritish ---------------------------------

h <- c(75.0, 77.0, 81.0, 83.0, 84.0, 86.0, 87.0)
t <- c(14.5, 20.5, 25.8, 31.7, 35.9, 40.5, 45.4)

n <- length(h)   # kuzatuvlar soni = 7

# ----- 2. Hisoblash jadvaliga kerakli ustunlarni tayyorlash -------------------
# t^2, h*t va ularning yig'indilari

t_squ <- t^2    # t ning kvadrati
ht    <- h * t  # h va t ko'paytmasi

cat("=== Yig'indi qiymatlar ===\n")
cat("  sum(h)     =", sum(h),     "\n")
cat("  sum(t)     =", sum(t),     "\n")
cat("  sum(t^2)   =", sum(t_squ), "\n")
cat("  sum(h*t)   =", sum(ht),    "\n\n")

# ----- 3. h = f(t) bog'lanish grafigini chizish -------------------------------

plot(t, h,
     type = "b",        # "b" = nuqtalar + chiziq
     pch  = 16,         # to'ldirilgan doira belgisi
     col  = "grey",
     lwd  = 3,
     main = "h = f(t) bog'lanishi",
     xlab = "Havo harorati (t, °C)",
     ylab = "Bug'lanish (h, mm)")
grid()

# ----- 4. TANLANGAN NUQTALAR USULI (1-usul) -----------------------------------

cat("=== 1-usul: TANLANGAN NUQTALAR USULI ===\n")
cat("  >>> Grafik oynasida chiziq ustidan 2 ta nuqtani cherting! <<<\n\n")

# locator(2) — foydalanuvchi grafikda 2 marta chertguncha kutadi,
#              so'ng chertilgan nuqtalarning x va y koordinatalarini beradi
nuqtalar <- locator(2)

# Chertilgan nuqtalarni t va h sifatida saqlash
# Kichigidan kattasiga tartiblash (t bo'yicha)
tartib <- order(nuqtalar$x)

t1 <- nuqtalar$x[tartib[1]]
h1 <- nuqtalar$y[tartib[1]]

t2 <- nuqtalar$x[tartib[2]]
h2 <- nuqtalar$y[tartib[2]]

cat("  Siz tanlagan nuqtalar:\n")
cat("  1-nuqta: t1 =", round(t1, 2), ", h1 =", round(h1, 2), "\n")
cat("  2-nuqta: t2 =", round(t2, 2), ", h2 =", round(h2, 2), "\n\n")

# Tanlangan nuqtalarni grafik ustiga chizish
abline(h = h1, v = t1, col = "blue",  lty = 2, lwd = 3)
abline(h = h2, v = t2, col = "green", lty = 2, lwd = 3)

# points() — chertilgan aniq joyga belgi qo'yish
# pch = 19 -> to'ldirilgan doira, cex = 1.8 -> belgi kattaligi
points(t1, h1, col = "blue",  pch = 19, cex = 1.8)
points(t2, h2, col = "green", pch = 19, cex = 1.8)

legend("bottomright",
       legend = c("1-nuqta", "2-nuqta"),
       col    = c("blue", "green"),
       lty    = 2, pch = 19, bty = "n")

# Tenglama sistemasi (qiymatlar avtomatik chiqadi):
#   h1 = a + b * t1
#   h2 = a + b * t2
#
# Matritsa shaklida: A1 * [a, b]' = B1
# rbind() — qatorlardan matrits tuzadi
A1 <- rbind(c(1, t1),
            c(1, t2))
B1 <- c(h1, h2)

# solve() — tenglama sistemasini yechadi
yechim1 <- solve(A1, B1)

a_1 <- yechim1[1]   # a parametri
b_1 <- yechim1[2]   # b parametri

cat("  a =", round(a_1, 2), "\n")
cat("  b =", round(b_1, 2), "\n")
cat("  Tenglama: h* =", round(a_1, 2), "+", round(b_1, 2), "* t\n\n")

# Hisoblangan h* qiymatlari — har bir t uchun
h_1 <- a_1 + b_1 * t

cat("  h_1 qiymatlari:\n")
print(round(h_1, 2))
cat("  sum(h_1) =", round(sum(h_1), 2), "\n\n")

# ----- 5. O'RTACHALAR USULI (2-usul) ------------------------------------------
#
# 7 ta kuzatuv ikki guruhga bo'linadi: birinchi 4 ta, keyin 3 ta
# Har bir guruh uchun yig'indilar hisoblanadi:
#   Σ1(h) = 4*a + b * Σ1(t)   --> 1-guruh (1-4 qatorlar)
#   Σ2(h) = 3*a + b * Σ2(t)   --> 2-guruh (5-7 qatorlar)
# =============================================================

cat("=== 2-usul: O'RTACHALAR USULI ===\n")

# Guruhlar bo'yicha yig'indilar
# [1:4] — birinchi 4 ta element (1,2,3,4-qatorlar)
# [5:7] — keyingi 3 ta element  (5,6,7-qatorlar)
sum_h1 <- sum(h[1:4])
sum_t1 <- sum(t[1:4])

sum_h2 <- sum(h[5:7])
sum_t2 <- sum(t[5:7])

cat("  1-guruh (1-4 qatorlar): sum(h) =", sum_h1, ", sum(t) =", sum_t1, "\n")
cat("  2-guruh (5-7 qatorlar): sum(h) =", sum_h2, ", sum(t) =", sum_t2, "\n")
cat("  Tuzilgan sistema:\n")
cat("   ", sum_h1, "= 4*a + b *", sum_t1, "\n")
cat("   ", sum_h2, "= 3*a + b *", sum_t2, "\n\n")

# Matritsa shaklida yechish
# Birinchi ustun — a ning koeffitsienti (1-guruhda 4, 2-guruhda 3)
# Ikkinchi ustun — b ning koeffitsienti (yig'indi t qiymatlari)
A2 <- rbind(c(4, sum_t1),
            c(3, sum_t2))
B2 <- c(sum_h1, sum_h2)

yechim2 <- solve(A2, B2)

a_2 <- yechim2[1]
b_2 <- yechim2[2]

cat("  a =", round(a_2, 2), "\n")
cat("  b =", round(b_2, 2), "\n")
cat("  Tenglama: h* =", round(a_2, 2), "+", round(b_2, 2), "* t\n\n")

# Hisoblangan h* qiymatlari
h_2 <- a_2 + b_2 * t

cat("  h_2 qiymatlari:\n")
print(round(h_2, 2))
cat("  sum(h_2) =", round(sum(h_2), 2), "\n\n")

# ----- 6. ENG KICHIK KVADRATLAR USULI (EKK) (3-usul) --------------------------

# Barcha kuzatuvlarni hisobga olgan holda normal tenglamalar sistemasini tuzish:
#   Σ(h)   = n*a    + b * Σ(t)
#   Σ(h*t) = a*Σ(t) + b * Σ(t^2)

cat("=== 3-usul: ENG KICHIK KVADRATLAR USULI (EKK) ===\n")
cat("  Tuzilgan sistema:\n")
cat("  ", sum(h),  "= 7*a + b *", sum(t), "\n")
cat("  ", sum(ht), "= a *", sum(t), "+ b *", sum(t_squ), "\n\n")

# Matrits shaklida yechish
A3 <- rbind(c(n,      sum(t)),
            c(sum(t), sum(t_squ)))
B3 <- c(sum(h), sum(ht))

yechim3 <- solve(A3, B3)

a_3 <- yechim3[1]
b_3 <- yechim3[2]

cat("  a =", round(a_3, 2), "\n")
cat("  b =", round(b_3, 2), "\n")
cat("  Tenglama: h* =", round(a_3, 2), "+", round(b_3, 2), "* t\n\n")

# Hisoblangan h* qiymatlari
h_3 <- a_3 + b_3 * t

cat("  h_3 qiymatlari:\n")
print(round(h_3, 2))
cat("  sum(h_3) =", round(sum(h_3), 2), "\n\n")

# ----- 7. Xatoliklarni hisoblash ----------------------------------------------
#
# Tenglama: Δ = h_kuzatilgan - h*_hisoblangan
# Qaysi usulda sum(|Δ|) kichik bo'lsa o'sha usul aniqroq hisoblanadi


cat("=== XATOLIKLARNI HISOBLASH ===\n\n")

Δ_1 <- h - h_1   # 1-usul xatoliklari
Δ_2 <- h - h_2   # 2-usul xatoliklari
Δ_3 <- h - h_3   # 3-usul xatoliklari

# ----- 8-QADAM: NATIJALAR JADVALI ---------------------------------
# Nazariya jadvalidagi barcha ustunlar: t^2, h*t,
# h1*, h2*, h3*, Δ1, Δ2, Δ3

natijalar <- data.frame(
  Nr      = 1:n,
  h       = h,
  t       = t,
  t2      = round(t_squ,   2),    # t^2
  ht      = round(ht,      2),    # h * t
  h1_hisob = round(h_1,    2),    # 1-usul hisoblangan
  h2_hisob = round(h_2,    2),    # 2-usul hisoblangan
  h3_hisob = round(h_3,    2),    # 3-usul hisoblangan
  Δ1  = round(Δ_1, 2),    # 1-usul xatoligi
  Δ2  = round(Δ_2, 2),    # 2-usul xatoligi
  Δ3  = round(Δ_3, 2)     # 3-usul xatoligi
)

cat("Natijalar jadvali (1-jadval bilan solishtiring):\n")
print(natijalar, row.names = FALSE)

# Yig'indi qator
cat("\nYig'indilar:\n")
cat("  sum(h)      =", sum(h),             "\n")
cat("  sum(t)      =", sum(t),             "\n")
cat("  sum(t^2)    =", sum(t_squ),         "\n")
cat("  sum(h*t)    =", sum(ht),            "\n")
cat("  sum(h1*)    =", round(sum(h_1), 2), "\n")
cat("  sum(h2*)    =", round(sum(h_2), 2), "\n")
cat("  sum(h3*)    =", round(sum(h_3), 2), "\n")
cat("  sum(Δ1) =", round(sum(Δ_1), 2), "\n")
cat("  sum(Δ2) =", round(sum(Δ_2), 2), "\n")
cat("  sum(Δ3) =", round(sum(Δ_3), 2), "\n\n")

# ----- 9. XULOSA — qaysi usul aniqroq? ----------------------------------------
# Nazariyaga ko'ra: sum(|Δ|) kichik bo'lgan usul afzal hisoblanadi

abs_sum <- c(sum(abs(Δ_1)),
             sum(abs(Δ_2)),
             sum(abs(Δ_3)))

usul_nomi <- c("Usul 1 (Tanlangan nuqtalar)",
               "Usul 2 (O'rtachalar)",
               "Usul 3 (EKK)")

eng_yaxshi <- which.min(abs_sum) # xatolilar summasi kichik bo'lgan usulni tanlash  

cat("Xatoliklar taqqoslash:\n")
cat("  sum(|Δ1|) =", round(abs_sum[1], 2), "\n")
cat("  sum(|Δ2|) =", round(abs_sum[2], 2), "\n")
cat("  sum(|Δ3|) =", round(abs_sum[3], 2), "\n\n")

cat("XULOSA:", usul_nomi[eng_yaxshi], "eng aniq natija berdi.\n")
cat("  sum(|Δ|) =", round(abs_sum[eng_yaxshi], 2), "\n\n")

 
 
