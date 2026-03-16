
# MODELO 2: REGRESIÓN LINEAL MÚLTIPLE


# ── 1. Ajuste del modelo completo ────────────────────────────
modelo_multiple <- lm(price ~ bedrooms + accommodates + bathrooms +
                        beds + review_scores_rating + reviews_per_month +
                        availability_365 + minimum_nights +
                        is_superhost + is_instant_book +
                        host_response_rate_num + host_acceptance_rate_num +
                        room_type + price_per_guest + beds_per_bedroom +
                        high_rated + high_availability,
                      data = train_df)

summary(modelo_multiple)

# ── 2. Métricas en entrenamiento ─────────────────────────────
r2_mult      <- summary(modelo_multiple)$r.squared
r2_adj_mult  <- summary(modelo_multiple)$adj.r.squared
rmse_train_m <- sqrt(mean(modelo_multiple$residuals^2))

cat("\n── Métricas en Train ──\n")
cat("R²:        ", round(r2_mult,      4), "\n")
cat("R² adj:    ", round(r2_adj_mult,  4), "\n")
cat("RMSE train:", round(rmse_train_m, 2), "\n")

# ── 3. Coeficientes ordenados por importancia ────────────────
coefs <- sort(abs(coef(modelo_multiple)[-1]), decreasing = TRUE)

par(mar = c(4, 14, 3, 2))
barplot(rev(coefs),
        horiz = TRUE, las = 1,
        col   = "#5B8DB8", border = "white",
        xlab  = "Valor absoluto del coeficiente",
        main  = "Importancia de variables — Modelo Múltiple")

# ── 4. Diagnóstico de residuos ────────────────────────────────
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
plot(modelo_multiple,
     col = rgb(0.36, 0.54, 0.72, 0.4),
     pch = 16, cex = 0.5)
par(mfrow = c(1, 1))

# ── 5. Análisis de correlación entre predictores ─────────────
library(corrplot)

vars_numericas <- train_df %>%
  select(bedrooms, accommodates, bathrooms, beds,
         review_scores_rating, reviews_per_month,
         availability_365, minimum_nights,
         is_superhost, is_instant_book,
         host_response_rate_num, host_acceptance_rate_num,
         price_per_guest, beds_per_bedroom,
         high_rated, high_availability)

matriz_cor <- cor(vars_numericas, use = "complete.obs")

corrplot(matriz_cor,
         method  = "color",
         type    = "upper",
         tl.cex  = 0.7,
         addCoef.col = "black",
         number.cex  = 0.5,
         title   = "Correlación entre predictores",
         mar     = c(0, 0, 2, 0))

# ── 6. Predicción en conjunto de prueba ──────────────────────
pred_multiple <- predict(modelo_multiple, newdata = test_df)

rmse_mult  <- sqrt(mean((test_df$price - pred_multiple)^2))
mae_mult   <- mean(abs(test_df$price - pred_multiple))
r2_test_m  <- cor(test_df$price, pred_multiple)^2

cat("\n── Métricas en Test ──\n")
cat("RMSE: ", round(rmse_mult,   2), "\n")
cat("MAE:  ", round(mae_mult,    2), "\n")
cat("R²:   ", round(r2_test_m,   4), "\n")

# ── 7. Predicho vs Real ───────────────────────────────────────
par(mar = c(4, 4, 3, 2))
plot(test_df$price, pred_multiple,
     pch = 16, col = rgb(0.43, 0.75, 0.56, 0.3),
     main = "Predicho vs Real — Modelo Múltiple",
     xlab = "Precio real (USD)",
     ylab = "Precio predicho (USD)")
abline(0, 1, col = "#E05C5C", lwd = 2, lty = 2)
legend("topleft", legend = "Línea ideal (y = x)",
       col = "#E05C5C", lty = 2, lwd = 2, bty = "n")

# ── 8. Comparación rápida con modelo simple ───────────────────
cat("\n── Comparación Simple vs Múltiple ──\n")
cat(sprintf("%-20s %8s %8s %8s\n", "Modelo", "RMSE", "MAE", "R²"))
cat(sprintf("%-20s %8.2f %8.2f %8.4f\n", "Simple",   rmse_simple, mae_simple, r2_test))
cat(sprintf("%-20s %8.2f %8.2f %8.4f\n", "Múltiple", rmse_mult,   mae_mult,   r2_test_m))