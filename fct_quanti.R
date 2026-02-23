library(dplyr)
library(ggplot2)
library(scales)
library(gt)
library(psych)


## fonction univarié pour les variables quanti
fct_quanti <- function(
    x,
    nom,
    type = c("hist", "boxplot", "density"),
    bins = 30
) {
  
  type <- match.arg(type)
  
  # ---- Vérifications ----
  if (!is.numeric(x)) {
    stop("x doit être numérique")
  }
  
  if (length(x) < 2) {
    stop("x doit être un vecteur de longueur > 1")
  }
  
  x <- x[!is.na(x)]
  
  # ---- Statistiques ----
  stats <- tibble(
    Indicateur = c(
      "Moyenne", "Écart-type", "Médiane",
      "1er quartile", "3e quartile",
      "Minimum", "Maximum"
    ),
    Valeur = c(
      mean(x), sd(x), median(x),
      quantile(x, 0.25), quantile(x, 0.75),
      min(x), max(x)
    )
  )
  
  # ---- Format table ----
  print(
    stats %>%
      gt() %>%
      fmt_number(Valeur, decimals = 2) %>%
      tab_header(
        title = paste("Statistiques descriptives de", nom)
      )
  )
  
  df <- tibble(x = x)
  
  # ---- Graphique ----
  p <- switch(
    
    type,
    
    "hist" =
      ggplot(df, aes(x = x)) +
      geom_histogram(
        aes(y = after_stat(density)),
        bins = bins,
        fill = "#2C7FB8",
        alpha = 0.7
      ) +
      geom_density(color = "#F03B20", linewidth = 1),
    
    "boxplot" =
      ggplot(df, aes(y = x)) +
      geom_boxplot(fill = "#7FCDBB", width = 0.3),
    
    "density" =
      ggplot(df, aes(x = x)) +
      geom_density(fill = "#2C7FB8", alpha = 0.6)
  )
  
  p <- p +
    labs(
      title = paste("Distribution de", nom),
      x = nom,
      y = ifelse(type == "boxplot", "", "Densité")
    ) +
    theme_minimal()
  
  print(p)
  
  invisible(stats)
}



