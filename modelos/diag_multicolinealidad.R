
# MODELO 2: DIAGNÓSTICO Y MULTICOLINEALIDAD


library(car)      # para VIF
library(lmtest)   # para Breusch-Pagan
library(nortest)  # para Anderson-Darling

# ── 1. VIF — Detección de multicolinealidad ───────────────────
vif_vals <- vif(modelo_multiple)

cat("\n── VIF por variable ──\n")
print(round(vif_vals, 2))

# Regla general:
# VIF < 5  → sin problema
# VIF 5-10 → multicolinealidad moderada
# VIF > 10 → multicolinealidad severa

cat("\n── Variables con VIF > 5 ──\n")
print(vif_vals[vif_vals > 5])

# Gráfico de VIF
par(mar = c(4, 14, 3, 2))
barplot(sort(vif_vals, decreasing = FALSE),
        horiz  = TRUE, las = 1,
        col    = ifelse(sort(vif_vals, decreasing = FALSE) > 10,
                        "#E05C5C",
                 ifelse(sort(vif_vals, decreasing = FALSE) > 5,
                        "#E8A838", "#5B8DB8")),
        border = "white",
        xlab   = "VIF",
        main   = "Factor de Inflación de Varianza (VIF)")
abline(v = 5,  col = "#E8A838", lwd = 2, lty = 2)
abline(v = 10, col = "#E05C5C", lwd = 2, lty = 2)
legend("bottomright",
       legend = c("VIF < 5 (OK)", "VIF 5-10 (moderado)", "VIF > 10 (severo)"),
       fill   = c("#5B8DB8", "#E8A838", "#E05C5C"),
       bty    = "n")

# ── 2. Prueba de normalidad de residuos ───────────────────────
residuos <- residuals(modelo_multiple)

# Histograma de residuos
par(mar = c(4, 4, 3, 2))
hist(residuos, breaks = 60,
     col    = "#5B8DB8", border = "white",
     main   = "Distribución de residuos — Modelo Múltiple",
     xlab   = "Residuos", ylab = "Frecuencia")
abline(v = 0, col = "#E05C5C", lwd = 2, lty = 2)

# Q-Q Plot
qqnorm(residuos, pch = 16,
       col  = rgb(0.36, 0.54, 0.72, 0.3),
       main = "Q-Q Plot de residuos")
qqline(residuos, col = "#E05C5C", lwd = 2)

# Test de normalidad (muestra por velocidad)
set.seed(42)
muestra_res <- sample(residuos, 5000)
ad_test <- ad.test(muestra_res)
cat("\n── Anderson-Darling (normalidad) ──\n")
print(ad_test)
cat("Interpretación: p <0.05 → residuos NO son normales\n")

# ── 3. Prueba de homocedasticidad ────────────────────────────
bp_test <- bptest(modelo_multiple)
cat("\n── Breusch-Pagan (homocedasticidad) ──\n")
print(bp_test)
cat("Interpretación: p <0.05 → hay heterocedasticidad\n")

# Residuos vs Fitted
par(mar = c(4, 4, 3, 2))
plot(fitted(modelo_multiple), residuos,
     pch  = 16, col = rgb(0.36, 0.54, 0.72, 0.3),
     main = "Residuos vs Valores Ajustados",
     xlab = "Valores ajustados",
     ylab = "Residuos")
abline(h = 0, col = "#E05C5C", lwd = 2, lty = 2)
lines(lowess(fitted(modelo_multiple), residuos),
      col = "#E8A838", lwd = 2)

# ── 4. Modelo reducido sin variables con alta multicolinealidad
# Basado en VIF: se elimina beds (redundante con bedrooms y accommodates)
# y price_per_guest (derivada de price, causa data leakage)

modelo_reducido <- lm(price ~ accommodates + bedrooms + bathrooms +
                        review_scores_rating + reviews_per_month +
                        availability_365 + minimum_nights +
                        is_superhost + is_instant_book +
                        host_response_rate_num +
                        room_type + high_rated + high_availability,
                      data = train_df)

summary(modelo_reducido)

# VIF del modelo reducido
vif_reducido <- vif(modelo_reducido)
cat("\n── VIF modelo reducido ──\n")
print(round(vif_reducido, 2))

# Comparación de R² ajustado
cat("\n── Comparación R² ajustado ──\n")
cat("Modelo completo: ", round(summary(modelo_multiple)$adj.r.squared, 4), "\n")
cat("Modelo reducido: ", round(summary(modelo_reducido)$adj.r.squared, 4), "\n")

# Métricas modelo reducido en test
pred_reducido <- predict(modelo_reducido, newdata = test_df)
rmse_red <- sqrt(mean((test_df$price - pred_reducido)^2))
mae_red  <- mean(abs(test_df$price - pred_reducido))
r2_red   <- cor(test_df$price, pred_reducido)^2

cat("\n── Métricas Test — Modelo Reducido ──\n")
cat("RMSE: ", round(rmse_red, 2), "\n")
cat("MAE:  ", round(mae_red,  2), "\n")
cat("R²:   ", round(r2_red,   4), "\n")