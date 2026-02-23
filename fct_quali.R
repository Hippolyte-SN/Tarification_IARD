library(dplyr)
library(ggplot2)
library(forcats)
library(scales)
library(gt)


## fonction univarié pour les variables quali
fct_quali <- function(y, nom, max_modalites = 10, pie = FALSE) {
  
  # Vérifications
  if (is.numeric(y)) {
    stop("x doit être une variable qualitative")
  }
  
  y <- as.factor(y)
  
  if (nlevels(y) > max_modalites) {
    stop("x doit avoir moins de ", max_modalites, " modalités")
  }
  
  # Table de fréquences
  tab <- tibble(modalite = y) %>%
    count(modalite, name = "Effectif") %>%
    mutate(
      Frequence = Effectif / sum(Effectif),
      Pourcentage = percent(Frequence)
    ) %>%
    arrange(desc(Effectif))
  
  # ---- Format table ----
  print(
    tab %>%
      gt() %>%
      cols_label(
        modalite = nom,
        Effectif = "Effectif",
        Frequence = "Fréquence",
        Pourcentage = "Pourcentage"
      ) %>%
      fmt_number(Frequence, decimals = 3) %>%
      tab_header(
        title = paste("Distribution de", nom)
      )
  )
  
  # ---- GRAPHIQUE ----
  if (!pie | nlevels(y) >= 5) {
    
    p <- ggplot(tab, aes(x = fct_reorder(modalite, Frequence), y = Frequence)) +
      geom_col(fill = "#2C7FB8", width = 0.7) +
      coord_flip() +
      geom_text(
        aes(label = Pourcentage),
        hjust = -0.1,
        size = 4
      ) +
      scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.15))) +
      labs(
        title = paste("Répartition de", nom),
        x = nom,
        y = "Fréquence"
      ) +
      theme_minimal()
    
    print(p)
    
  } else {
    
    p <- ggplot(tab, aes(x = "", y = Effectif, fill = modalite)) +
      geom_col(width = 1) +
      coord_polar("y") +
      labs(
        title = paste("Répartition de", nom),
        fill = nom
      ) +
      theme_void()
    
    print(p)
  }
  
  invisible(tab)
}




