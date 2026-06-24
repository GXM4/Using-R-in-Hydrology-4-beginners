# Authors: GAVKHAR Mamadjanova & ZULFIYA Kuranboyeva
#------------------------------------------------------------------------------ #
#                       5b-amaliy mashg'ulot / Practical 5b                     #
# Y=a*x^b ko'rinishidagi tenglamaning noma'lum parametrlarini 2 usul bilan aniqlash
#     Estimation of the unknown parameters of the hydrological relationship 
#                           Y=a*x^b using two different methods
# ----------------------------------------------------------------------------- #
# Code description in Uzbek |  R version >4.5.0
#
# Ushbu anmaliy mashg'ulotda berilgan o'zgaruvchilar Q — suv sarfi (m^3/s)
# ρ — suvning loyqaligi (g/m^3). Tenglama quyidagi holatga keltiriladi:
# Tenglama: ρ = a * Q^b
# Logarifmik holat: lg(ρ) = lg(a) + b * lg(Q)
#
# ----- 1. Boshlang'ich ma'lumotlarni kiritish ---------------------------------

Q <- c(10.77, 10.83, 10.88, 10.93, 11.13, 11.25, 11.33, 11.36)
ρ <- c(39.5, 40.4, 43.0, 45.0, 54.0, 63.0, 64.0, 78.0)
n <- length(Q)  # kuzatuvlar soni = 8

# ----- 2. O'zgaruvchilarni logarifmlarda ifodalash ----------------------------
# log10() — o'nli logarifm funksiyasi

lgQ <- log10(Q)  # Q qiymatlar logarifmi
lgR <- log10(ρ)  # ρ qiymatlar logarifmi

cat("Logarifmik qiymatlar yig'indisi:\n")
cat("  sum(lgQ) =", round(sum(lgQ), 4), "\n")
cat("  sum(lgR) =", round(sum(lgR), 4), "\n\n")

# ----- 3. Grafiklarni chizish -------------------------------------------------
# par(mfrow) — bir oynada bir nechta grafik joylashtirish

par(mfrow = c(1, 2))  # 1 qator, 2 ustun

# --- Chizma 1: ρ = f(Q) to'g'ridan-to'g'ri bog'lanish -----
plot(Q, ρ,
     type = "b",          # "b" = nuqtalar + chiziq
     pch  = 16,           # to'ldirilgan doira belgisi
     col  = "steelblue",
     lwd  = 2,
     main = "ρ = f(Q) bog'lanishi",
     xlab = "Q, m^3/s",
     ylab = "ρ, g/m^3")
grid()

# --- Chizma 2: lg(ρ) = f(lg(Q)) logarifmik bog'lanish -----
plot(lgQ, lgR,
     type = "b",
     pch  = 16,
     col  = "darkorange",
     lwd  = 2,
     main = "lg(ρ) = f(lg(Q)) logarifmik bog'lanish",
     xlab = "lg(Q)",
     ylab = "lg(ρ)")
grid()

# locator(2) funksiyasi foydalanuvchi grafikda 2 marta chertguncha kutadi,
#              so'ng chertilgan nuqtalarning x va y koordinatalarini beradi
nuqtalar <- locator(2)

# Chertilgan nuqtalarni lgQ va lgR sifatida saqlash
# order() — kichigidan kattasiga tartiblash (lgQ bo'yicha)
tartib <- order(nuqtalar$x)

lgQ1 <- nuqtalar$x[tartib[1]]
lgR1 <- nuqtalar$y[tartib[1]]

lgQ2 <- nuqtalar$x[tartib[2]]
lgR2 <- nuqtalar$y[tartib[2]]

cat("Tanlangan nuqtalar:\n")
cat(" 1-nuqta: lgQ1 =", round(lgQ1, 4), ", lgR1 =", round(lgR1, 4), "\n")
cat(" 2-nuqta: lgQ2 =", round(lgQ2, 4), ", lgR2 =", round(lgR2, 4), "\n\n")

# Tanlangan nuqtalarni grafik ustiga chizish
# abline() — gorizontal/vertikal chiziq, v = vertikal, h = gorizontal
abline(v = lgQ1, h = lgR1, col = "blue",  lty = 2, lwd = 2)
abline(v = lgQ2, h = lgR2, col = "green", lty = 2, lwd = 2)

# points() — chertilgan aniq joyga belgi qo'yish
points(lgQ1, lgR1, col = "blue",  pch = 19, cex = 1.8)
points(lgQ2, lgR2, col = "green", pch = 19, cex = 1.8)

legend("topleft",
       legend = c("1-nuqta", "2-nuqta"),
       col    = c("blue", "green"),
       lty    = 2, pch = 19, bty = "n")

par(mfrow = c(1, 1))  # grafik oynasini oddiy holatga qaytarish

# ----- 4. TANLANGAN NUQTALAR USULI (1-usul) -----------------------------------
#
# Logarifmik grafikdan 2 ta nuqta tanlangach quyidagi tenglamalar sistemasi tuziladi:
#   lg(ρ1) = lg(a) + b * lg(Q1)
#   lg(ρ2) = lg(a) + b * lg(Q2)
#
# Noma'lumlar: lg(a) va b
# Yechim: solve() funksiyasi bilan amalga oshiriladi

cat("=== 1-usul: TANLANGAN NUQTALAR USULI ===\n")
cat("Tanlangan nuqtalar:\n")
cat("  1-nuqta: lgQ1 =", lgQ1, ",  lgR1 =", lgR1, "\n")
cat("  2-nuqta: lgQ2 =", lgQ2, ",  lgR2 =", lgR2, "\n\n")

# Tenglama sistemasini matrits shaklida yozamiz:
# | 1  lgQ1 | | lg(a) |   | lgR1 |
# | 1  lgQ2 | |   b   | = | lgR2 |
#
# rbind() — qatorlarni birlashtirish orqali matrit tuzadi
A1 <- rbind(c(1, lgQ1),
            c(1, lgQ2))
B1 <- c(lgR1, lgR2)

# solve(A, B) — Ax = B tenglamasini yechadi, x = [lg(a), b]
yechim1 <- solve(A1, B1)

lga_1 <- yechim1[1]  # lg(a) qiymati
b_1 <- yechim1[2]    # b qiymati
a_1 <- 10^lga_1      # a = 10^lg(a)

cat("Natija:\n")
cat("  lg(a) =", round(lga_1, 5), "\n")
cat("  a     =", a_1, "  (juda kichik son, bu normal)\n")
cat("  b     =", round(b_1, 5), "\n")
cat("  Tenglama: ρ* = a * Q^b\n\n")

# Hisoblangan ρ_1 qiymatlari — har bir Q uchun
ρ_1 <- a_1 * Q^b_1

# ----- 5. O'RTACHALAR USULI (2-usul) ------------------------------------------
#
# 8 qatorli qiymatlar 2 ta teng guruhga bo'linadi (4+4).
# Har bir guruh uchun yig'indilar hisoblanadi va
# tenglama sistemasi tuziladi:
#   sum1(lgR) = 4*lg(a) + b * sum1(lgQ)
#   sum2(lgR) = 4*lg(a) + b * sum2(lgQ)

cat("=== 2-usul: O'RTACHALAR USULI ===\n")

# Guruhlar bo'yicha yig'indilar
# [1:4]  — birinchi 4 ta element
# [5:8]  — keyingi 4 ta element
sum_lgQ1 <- sum(lgQ[1:4])
sum_lgR1 <- sum(lgR[1:4])

sum_lgQ2 <- sum(lgQ[5:8])
sum_lgR2 <- sum(lgR[5:8])

cat("Guruhlar bo'yicha yig'indilar:\n")
cat("  1-guruh (1-4 -qatorlar): sum(lgQ) =", round(sum_lgQ1, 4),
    ",  sum(lgR) =", round(sum_lgR1, 4), "\n")
cat("  2-guruh (5-8 qatorlar): sum(lgQ) =", round(sum_lgQ2, 4),
    ",  sum(lgR) =", round(sum_lgR2, 4), "\n")
cat("Tuzilgan sistema:\n")
cat(" ", round(sum_lgR1, 4), "= 4*lg(a) + b *", round(sum_lgQ1, 4), "\n")
cat(" ", round(sum_lgR2, 4), "= 4*lg(a) + b *", round(sum_lgQ2, 4), "\n\n")

A2 <- rbind(c(4, sum_lgQ1),
            c(4, sum_lgQ2))
B2 <- c(sum_lgR1, sum_lgR2)

yechim2 <- solve(A2, B2)

lga_2 <- yechim2[1]
b_2 <- yechim2[2]
a_2 <- 10^lga_2

cat("Natija:\n")
cat("  lg(a) =", round(lga_2, 5), "\n")
cat("  a     =", a_2, "\n")
cat("  b     =", round(b_2, 5), "\n")
cat("  Tenglama: ρ_2 = a * Q^b\n\n")

# Hisoblangan ρ_2 qiymatlari — har bir Q uchun
ρ_2 <- a_2 * Q^b_2

# ----- 6. XATOLIKLARNI HISOBLASH ----------------------------------------------
# Har bir kuzatuv uchun:
#   Δ = ρ_kuzatilgan - ρ_2_hisoblangan#
# Natijani baholash uchun:
#   sum(Δ) — yig'indi xatolik (ishorali) 
#   sum(abs(Δ))— yig'indi xatolik (ishorasiz, mutlaq)
#   Qaysi usulda sum(|Δ|) kichik bo'lsa — o'sha usul aniqroq hisoblanadi

cat("=== XATOLIKLARNI HISOBLASH ===\n\n")

Δ_1 <- ρ - ρ_1   # 1-usul xatoliklari
Δ_2 <- ρ - ρ_2   # 2-usul xatoliklari


# ----- 7. NATIJALAR JADVALI ---------------------------------------------------

natijalar <- data.frame(
  Nr = 1:n,
  Q = Q,
  ρ_obs = ρ,                    # kuzatilgan qiymat
  ρ1_hisob = round(ρ_1, 2),     # 1-usul hisoblangan
  ρ2_hisob = round(ρ_2, 2),     # 2-usul hisoblangan
  Δ1 = round(Δ_1, 2),           # 1-usul xatoligi
  Δ2 = round(Δ_2, 2),           # 2-usul xatoligi
  absΔ1 = round(abs(Δ_1), 2),   # 1-usul |xatolik|
  absΔ2 = round(abs(Δ_2), 2)    # 2-usul |xatolik|
)

cat("Natijalar jadvali:\n")
print(natijalar, row.names = FALSE)

# Yig'indi xatoliklar
cat("\nYig'indi xatoliklar:\n")
cat("  sum(Δ1)     =", round(sum(Δ_1), 3),
    "  (1-usul ishorali yig'indi)\n")
cat("  sum(Δ2)     =", round(sum(Δ_2), 3),
    "  (2-usul ishorali yig'indi)\n")
cat("  sum(|Δ1|)   =", round(sum(abs(Δ_1)), 3),
    "  (1-usul mutlaq yig'indi)\n")
cat("  sum(|Δ2|)   =", round(sum(abs(Δ_2)), 3),
    "  (2-usul mutlaq yig'indi)\n\n")

# ----- 8. XULOSA — qaysi usul aniqroq?  ---------------------------------------
# Nazariyaga ko'ra: sum(|Δ|) kichik bo'lgan usul afzal

if (sum(abs(Δ_1)) < sum(abs(Δ_2))) {
  cat("XULOSA: 1-usul (Tanlangan nuqtalar) aniqroq.\n")
  cat("  sum(|Δ1|) =", round(sum(abs(Δ_1)), 3),
      " < sum(|Δ2|) =", round(sum(abs(Δ_2)), 3), "\n\n")
} else {
  cat("XULOSA: 2-usul (O'rtachalar) aniqroq.\n")
  cat("  sum(|Δ2|) =", round(sum(abs(Δ_2)), 3),
      " < sum(|Δ1|) =", round(sum(abs(Δ_1)), 3), "\n\n")
}


