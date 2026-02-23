library(dplyr)
library(ggplot2)
library(scales)
library(gt)
library(rstatix)


biv_stat <- function(
    x,
    y,
    nom_x = "X",
    nom_y = "Y",
    corr_methods = c("pearson", "spearman", "kendall"),
    show_tests = TRUE,
    show_effect_size = TRUE,
    bins = 30
) {
  
  # ---- Détection des types ----
  x_quanti <- is.numeric(x)
  y_quanti <- is.numeric(y)
  
  df <- tibble(x = x, y = y) |> na.omit()
  
    # ==========================================================
    # CAS 1 : QUANTI ~ QUANTI
    # ==========================================================
  if (x_quanti & y_quanti) {
    
    cat("Analyse quantitative / quantitative\n\n")
    
    # ---- Graphique ----
    p <- ggplot(df, aes(x, y)) +
      geom_point(alpha = 0.3, color = "#2C7FB8") +
      geom_smooth(method = "lm", se = TRUE, color = "#F03B20") +
      labs(
        title = paste(nom_y, "en fonction de", nom_x),
        x = nom_x,
        y = nom_y
      ) +
      theme_minimal()
    
    print(p)
    
    # ---- Régression ----
    reg <- lm(y ~ x, data = df)
    print(summary(reg))
    
    if (show_effect_size) {
      cat("\nR² =", round(summary(reg)$r.squared, 3), "\n")
    }
    
    # ---- Corrélations ----
    if (show_tests) {
      for (m in corr_methods) {
        cat("\nCorrélation", m, "\n")
        print(cor.test(df$x, df$y, method = m))
      }
    }
  }
  
    # ==========================================================
    # CAS 2 : QUALI ~ QUANTI
    # ==========================================================
  else if (!x_quanti & y_quanti) {
    
    cat("Analyse qualitative / quantitative\n\n")
    
    df$x <- as.factor(df$x)
    
    # ---- Graphique ----
    p <- ggplot(df, aes(x, y)) +
      geom_boxplot(fill = "#7FCDBB") +
      labs(
        title = paste(nom_y, "selon", nom_x),
        x = nom_x,
        y = nom_y
      ) +
      theme_minimal()
    
    print(p)
    
    k <- nlevels(df$x)
    
    # ---- Tests ----
    if (show_tests) {
      if (k == 2) {
        print(wilcox.test(y ~ x, data = df))
        if (show_effect_size)
          print(rstatix::wilcox_effsize(df, y ~ x))
      } else {
        print(kruskal.test(y ~ x, data = df))
        if (show_effect_size)
          print(rstatix::kruskal_effsize(df, y ~ x))
      }
    }
  }
  
    # ==========================================================
    # CAS 3 : QUANTI ~ QUALI
    # ==========================================================
  else if (x_quanti & !y_quanti) {
    
    cat("Analyse quantitative / qualitative (symétrique)\n\n")
    
    df$y <- as.factor(df$y)
    
    p <- ggplot(df, aes(y, x)) +
      geom_boxplot(fill = "#FDAE6B") +
      labs(
        title = paste(nom_x, "selon", nom_y),
        x = nom_y,
        y = nom_x
      ) +
      theme_minimal()
    
    print(p)
  }
  
    # ==========================================================
    # CAS 4 : QUALI ~ QUALI
    # ==========================================================
  else {
    
    cat("Analyse qualitative / qualitative\n\n")
    
    tab <- table(x, y)
    print(tab)
    
    print(chisq.test(tab))
    
    print(
      as.data.frame(tab) |>
        gt() |>
        tab_header(
          title = paste("Table de contingence :", nom_x, "vs", nom_y)
        )
    )
  }
  
  invisible(NULL)
}




