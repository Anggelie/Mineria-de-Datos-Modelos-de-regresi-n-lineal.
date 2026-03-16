# MODELO 1: REGRESIÓN LINEAL SIMPLE
# Variable predictora: accommodates (r = 0.5345, la más alta)

# ── 1. Ajuste del modelo ──────────
modelo_simple <- lm(price ~ accommodates, data = train_df)
summary(modelo_simple)

# ── 2. Interpretación del summary 
# Coeficientes
cat("\n── Coeficientes ──\n")
cat("Intercepto:   ", round(coef(modelo_simple)[1], 2), "\n")
cat("accommodates: ", round(coef(modelo_simple)[2], 2), "\n")
cat("\nInterpretación: por cada huésped adicional,",
    "el precio sube ~$", round(coef(modelo_simple)[2], 2), "por noche\n")

# R² del modelo
r2 <- summary(modelo_simple)$r.squared
cat("\nR²:      ", round(r2, 4), "\n")
cat("R² adj:  ", round(summary(modelo_simple)$adj.r.squared, 4), "\n")
cat("RMSE train:", round(sqrt(mean(modelo_simple$residuals^2)), 2), "\n")

# ── 3. Gráfico: recta de regresión sobre los datos 
par(mar = c(4, 4, 3, 2))
plot(train_df$accommodates, train_df$price,
     pch = 16, col = rgb(0.36, 0.54, 0.72, 0.3),
     main = "Regresión Lineal Simple: price ~ accommodates",
     xlab = "Capacidad (accommodates)",
     ylab = "Precio por noche (USD)")
abline(modelo_simple, col = "#E05C5C", lwd = 2)
legend("topleft",
       legend = paste0("y = ", round(coef(modelo_simple)[1],2),
                       " + ", round(coef(modelo_simple)[2],2), "x",
                       "\nR² = ", round(r2, 4)),
       col = "#E05C5C", lty = 1, lwd = 2, bty = "n")

# ── 4. Diagnóstico de residuos ────
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
plot(modelo_simple, col = rgb(0.36, 0.54, 0.72, 0.4),
     pch = 16, cex = 0.5)
par(mfrow = c(1, 1))

# ── 5. Predicción en conjunto de prueba 
pred_simple <- predict(modelo_simple, newdata = test_df)

# Métricas
rmse_simple <- sqrt(mean((test_df$price - pred_simple)^2))
mae_simple  <- mean(abs(test_df$price - pred_simple))
r2_test     <- cor(test_df$price, pred_simple)^2

cat("\n── Métricas en Test ──\n")
cat("RMSE: ", round(rmse_simple, 2), "\n")
cat("MAE:  ", round(mae_simple,  2), "\n")
cat("R²:   ", round(r2_test,     4), "\n")

# ── 6. Gráfico: predicho vs real ─
par(mar = c(4, 4, 3, 2))
plot(test_df$price, pred_simple,
     pch = 16, col = rgb(0.43, 0.75, 0.56, 0.3),
     main = "Predicho vs Real — Modelo Simple",
     xlab = "Precio real (USD)",
     ylab = "Precio predicho (USD)")
abline(0, 1, col = "#E05C5C", lwd = 2, lty = 2)
legend("topleft", legend = "Línea ideal (y = x)",
       col = "#E05C5C", lty = 2, lwd = 2, bty = "n")