# MODELO 3: REGULARIZACIÓN — RIDGE Y LASSO

library(glmnet)

# ── 1. Preparar matrices para glmnet ───
# glmnet requiere matriz X y vector y por separado

vars_modelo <- c("accommodates", "bedrooms", "bathrooms",
                 "review_scores_rating", "reviews_per_month",
                 "availability_365", "minimum_nights",
                 "is_superhost", "is_instant_book",
                 "host_response_rate_num",
                 "high_rated", "high_availability",
                 "beds_per_bedroom", "room_type")

# Matrix con dummies automáticas para room_type
X_train <- model.matrix(~ . - 1, data = train_df[, vars_modelo])
X_test  <- model.matrix(~ . - 1, data = test_df[,  vars_modelo])

y_train <- train_df$price
y_test  <- test_df$price

cat("Dimensiones X_train:", dim(X_train), "\n")
cat("Dimensiones X_test: ", dim(X_test),  "\n")

# ── 2. RIDGE (alpha = 0) ──────────

# Cross-validation para encontrar lambda óptimo
set.seed(42)
cv_ridge <- cv.glmnet(X_train, y_train,
                      alpha  = 0,
                      nfolds = 10)

lambda_ridge <- cv_ridge$lambda.min
cat("\nLambda óptimo Ridge:", round(lambda_ridge, 4), "\n")

# Gráfico de CV
plot(cv_ridge,
     main = "Ridge — Cross-Validation para lambda")

# Modelo Ridge final
modelo_ridge <- glmnet(X_train, y_train,
                       alpha  = 0,
                       lambda = lambda_ridge)

# Coeficientes Ridge
coef_ridge <- coef(modelo_ridge)
cat("\n── Coeficientes Ridge ──\n")
print(round(coef_ridge, 4))

# Trayectoria de coeficientes Ridge
modelo_ridge_path <- glmnet(X_train, y_train, alpha = 0)
par(mar = c(4, 4, 3, 2))
plot(modelo_ridge_path, xvar = "lambda", label = TRUE,
     main = "Ridge — Trayectoria de coeficientes")
abline(v = log(lambda_ridge), col = "#E05C5C", lwd = 2, lty = 2)

# Predicción Ridge en test
pred_ridge <- predict(modelo_ridge,
                      newx   = X_test,
                      s      = lambda_ridge)

rmse_ridge <- sqrt(mean((y_test - pred_ridge)^2))
mae_ridge  <- mean(abs(y_test - pred_ridge))
r2_ridge   <- cor(y_test, as.vector(pred_ridge))^2

cat("\n── Métricas Test — Ridge ──\n")
cat("RMSE: ", round(rmse_ridge, 2), "\n")
cat("MAE:  ", round(mae_ridge,  2), "\n")
cat("R²:   ", round(r2_ridge,   4), "\n")

# ── 3. LASSO (alpha = 1) ──────────

set.seed(42)
cv_lasso <- cv.glmnet(X_train, y_train,
                      alpha  = 1,
                      nfolds = 10)

lambda_lasso <- cv_lasso$lambda.min
cat("\nLambda óptimo Lasso:", round(lambda_lasso, 4), "\n")

# Gráfico de CV
plot(cv_lasso,
     main = "Lasso — Cross-Validation para lambda")

# Modelo Lasso final
modelo_lasso <- glmnet(X_train, y_train,
                       alpha  = 1,
                       lambda = lambda_lasso)

# Coeficientes Lasso — variables seleccionadas (≠ 0)
coef_lasso <- coef(modelo_lasso)
vars_seleccionadas <- coef_lasso[coef_lasso[, 1] != 0, ]

cat("\n── Variables seleccionadas por Lasso ──\n")
print(round(vars_seleccionadas, 4))
cat("\nTotal variables eliminadas por Lasso:",
    sum(coef_lasso[-1] == 0), "de", nrow(coef_lasso) - 1, "\n")

# Trayectoria de coeficientes Lasso
modelo_lasso_path <- glmnet(X_train, y_train, alpha = 1)
par(mar = c(4, 4, 3, 2))
plot(modelo_lasso_path, xvar = "lambda", label = TRUE,
     main = "Lasso — Trayectoria de coeficientes")
abline(v = log(lambda_lasso), col = "#E05C5C", lwd = 2, lty = 2)

# Predicción Lasso en test
pred_lasso <- predict(modelo_lasso,
                      newx = X_test,
                      s    = lambda_lasso)

rmse_lasso <- sqrt(mean((y_test - pred_lasso)^2))
mae_lasso  <- mean(abs(y_test - pred_lasso))
r2_lasso   <- cor(y_test, as.vector(pred_lasso))^2

cat("\n── Métricas Test — Lasso ──\n")
cat("RMSE: ", round(rmse_lasso, 2), "\n")
cat("MAE:  ", round(mae_lasso,  2), "\n")
cat("R²:   ", round(r2_lasso,   4), "\n")

# ── 4. Comparación Ridge vs Lasso — coeficientes ──
coef_comparacion <- data.frame(
  Variable = rownames(coef_ridge)[-1],
  Ridge    = as.vector(coef_ridge)[-1],
  Lasso    = as.vector(coef_lasso)[-1]
)

par(mfrow = c(1, 2), mar = c(4, 10, 3, 1))

barplot(rev(coef_comparacion$Ridge),
        names.arg = rev(coef_comparacion$Variable),
        horiz = TRUE, las = 1,
        col   = "#5B8DB8", border = "white",
        main  = "Coeficientes Ridge",
        xlab  = "Valor")

barplot(rev(coef_comparacion$Lasso),
        names.arg = rev(coef_comparacion$Variable),
        horiz = TRUE, las = 1,
        col   = "#6DBF8E", border = "white",
        main  = "Coeficientes Lasso",
        xlab  = "Valor")
par(mfrow = c(1, 1))