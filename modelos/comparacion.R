
# EVALUACIÓN FINAL Y COMPARACIÓN DE MODELOS


# ── 1. Tabla comparativa de métricas 
resultados <- data.frame(
  Modelo = c("Simple (accommodates)",
             "Múltiple completo",
             "Múltiple reducido",
             "Ridge",
             "Lasso"),
  RMSE = round(c(rmse_simple,
                 rmse_mult,
                 rmse_red,
                 rmse_ridge,
                 rmse_lasso), 2),
  MAE  = round(c(mae_simple,
                 mae_mult,
                 mae_red,
                 mae_ridge,
                 mae_lasso), 2),
  R2   = round(c(r2_test,
                 r2_test_m,
                 r2_red,
                 r2_ridge,
                 r2_lasso), 4)
)

cat("\n══ TABLA COMPARATIVA DE MODELOS ══\n")
print(resultados)

# ── 2. Gráfico de barras — RMSE por modelo 
colores <- c("#A8C8E8", "#5B8DB8", "#2E6DA4", "#E8A838", "#6DBF8E")

par(mar = c(6, 5, 4, 2))
bp <- barplot(resultados$RMSE,
              names.arg = resultados$Modelo,
              col       = colores,
              border    = "white",
              ylim      = c(0, max(resultados$RMSE) * 1.2),
              main      = "Comparación de RMSE en conjunto de prueba",
              ylab      = "RMSE (USD)",
              las       = 2,
              cex.names = 0.75)
text(bp, resultados$RMSE + 1,
     labels = resultados$RMSE,
     cex = 0.85, font = 2)

# ── 3. Gráfico de barras — R² por modelo 
par(mar = c(6, 5, 4, 2))
bp2 <- barplot(resultados$R2,
               names.arg = resultados$Modelo,
               col       = colores,
               border    = "white",
               ylim      = c(0, 1),
               main      = "Comparación de R² en conjunto de prueba",
               ylab      = "R²",
               las       = 2,
               cex.names = 0.75)
text(bp2, resultados$R2 + 0.02,
     labels = resultados$R2,
     cex = 0.85, font = 2)
abline(h = 0.5, col = "#E05C5C", lwd = 2, lty = 2)
legend("topright", legend = "R² = 0.5",
       col = "#E05C5C", lty = 2, lwd = 2, bty = "n")

# ── 4. Predicho vs Real — todos los modelos 
par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))

modelos_pred <- list(
  list(pred = pred_simple,           nombre = "Simple"),
  list(pred = pred_multiple,         nombre = "Múltiple completo"),
  list(pred = pred_reducido,         nombre = "Múltiple reducido"),
  list(pred = as.vector(pred_ridge), nombre = "Ridge"),
  list(pred = as.vector(pred_lasso), nombre = "Lasso")
)

for (m in modelos_pred) {
  plot(y_test, m$pred,
       pch  = 16,
       col  = rgb(0.36, 0.54, 0.72, 0.2),
       main = m$nombre,
       xlab = "Real",
       ylab = "Predicho",
       xlim = c(0, 800),
       ylim = c(0, 800))
  abline(0, 1, col = "#E05C5C", lwd = 2, lty = 2)
}
par(mfrow = c(1, 1))

# ── 5. Distribución de errores por modelo ─
errores <- data.frame(
  Simple   = y_test - pred_simple,
  Multiple = y_test - pred_multiple,
  Reducido = y_test - pred_reducido,
  Ridge    = y_test - as.vector(pred_ridge),
  Lasso    = y_test - as.vector(pred_lasso)
)

par(mar = c(5, 4, 3, 2))
boxplot(errores,
        col    = colores,
        border = "gray30",
        outline = FALSE,
        main   = "Distribución de errores por modelo",
        ylab   = "Error (Real - Predicho)",
        las    = 2)
abline(h = 0, col = "#E05C5C", lwd = 2, lty = 2)

# ── 6. Identificar el mejor modelo ──
mejor_rmse <- resultados$Modelo[which.min(resultados$RMSE)]
mejor_r2   <- resultados$Modelo[which.max(resultados$R2)]

cat("\n══ MEJOR MODELO ══\n")
cat("Por RMSE más bajo: ", mejor_rmse, "\n")
cat("Por R² más alto:   ", mejor_r2,   "\n")
cat("\n── Métricas del mejor modelo ──\n")
print(resultados[which.min(resultados$RMSE), ])