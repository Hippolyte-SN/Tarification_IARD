# 1.1 Fonction Gini (commune)

gini_coef <- function(y, pred) {
  ord <- order(pred)
  y <- y[ord]
  n <- length(y)
  G <- 2 * sum(y * seq_len(n)) / (n * sum(y)) - (n + 1)/n
  return(G)
}

# 1.2 Validation Fréquence

validation_frequence <- function(model, data_test, y_name) {
  
  y <- data_test[[y_name]]
  pred <- predict(model, newdata = data_test, type = "response")
  
  # --- Métriques prédictives
  rmse <- sqrt(mean((y - pred)^2))
  mae  <- mean(abs(y - pred))
  
  # --- Déviance & pseudo R²
  pseudo_r2 <- 1 - model$deviance/model$null.deviance
  
  # --- Dispersion
  dispersion <- model$deviance / model$df.residual
  
  # --- Gini
  gini <- gini_coef(y, pred)
  
  # --- Calibration par déciles
  decile <- cut(pred,
                breaks = quantile(pred, probs = seq(0,1,0.1)),
                include.lowest = TRUE)
  
  calib_table <- aggregate(y ~ decile, FUN = mean)
  calib_table$pred_mean <- aggregate(pred ~ decile, FUN = mean)[,2]
  
  # --- Sortie
  output <- list(
    Type = family(model)$family,
    RMSE = rmse,
    MAE = mae,
    Dispersion = dispersion,
    Pseudo_R2 = pseudo_r2,
    Gini = gini,
    AIC = AIC(model),
    Calibration = calib_table
  )
  
  return(output)
}

# 1.3 Graphiques Fréquence

diagnostic_frequence <- function(model, data_test, y_name) {
  
  y <- data_test[[y_name]]
  pred <- predict(model, newdata = data_test, type = "response")
  
  par(mfrow=c(1,2))
  
  # Observé vs prédit
  plot(pred, y,
       xlab="Fréquence prédite",
       ylab="Fréquence observée",
       pch=16)
  abline(0,1,col="red",lwd=2)
  
  # Résidus
  plot(model$fitted.values,
       residuals(model, type="deviance"),
       xlab="Valeurs ajustées",
       ylab="Résidus de déviance")
  abline(h=0,col="red")
  
  par(mfrow=c(1,1))
}

# 2.1 Validation Sévérité

validation_cout <- function(model, data_test, y_name) {
  
  y <- data_test[[y_name]]
  pred <- predict(model, newdata = data_test, type = "response")
  
  # --- Métriques prédictives
  rmse <- sqrt(mean((y - pred)^2))
  mae  <- mean(abs(y - pred))
  
  # --- Pseudo R²
  pseudo_r2 <- 1 - model$deviance/model$null.deviance
  
  # --- Gini
  gini <- gini_coef(y, pred)
  
  # --- Ratio Observé / Prédit
  ratio_global <- sum(y) / sum(pred)
  
  # --- Calibration par déciles
  decile <- cut(pred,
                breaks = quantile(pred, probs = seq(0,1,0.1)),
                include.lowest = TRUE)
  
  calib_table <- aggregate(y ~ decile, FUN = mean)
  calib_table$pred_mean <- aggregate(pred ~ decile, FUN = mean)[,2]
  
  # --- Sortie
  output <- list(
    Type = paste(family(model)$family,
                 "(link =", family(model)$link, ")"),
    RMSE = rmse,
    MAE = mae,
    Pseudo_R2 = pseudo_r2,
    Gini = gini,
    Ratio_Obs_Pred = ratio_global,
    AIC = AIC(model),
    Calibration = calib_table
  )
  
  return(output)
}

# 2.2 Graphiques Sévérité

diagnostic_cout <- function(model, data_test, y_name) {
  
  y <- data_test[[y_name]]
  pred <- predict(model, newdata = data_test, type = "response")
  
  par(mfrow=c(2,2))
  
  # Observé vs prédit
  plot(pred, y,
       xlab="Coût prédit",
       ylab="Coût observé",
       pch=16)
  abline(0,1,col="red",lwd=2)
  
  # Résidus
  plot(model$fitted.values,
       residuals(model, type="deviance"),
       xlab="Valeurs ajustées",
       ylab="Résidus de déviance")
  abline(h=0,col="red")
  
  # QQ-plot
  qqnorm(residuals(model))
  qqline(residuals(model), col="red")
  
  # Histogramme résidus
  hist(residuals(model), main="Résidus",
       xlab="Résidus")
  
  par(mfrow=c(1,1))
}





