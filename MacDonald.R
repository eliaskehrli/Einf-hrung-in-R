# Laden Sie die erforderlichen Pakete
library(haven)
library(psych)
library(knitr)
library(dplyr)

# SPSS-Datei importieren
data_spss <- read_sav("/Users/eliaskehrli/Downloads/FEKW_1.sav")

# Daten für Skala 1 extrahieren
skala1_items <- data_spss[, c("be1", "be2", "be3", "be4")]

# McDonald's Omega für Skala 1 berechnen
omega_skala1 <- omega(skala1_items, nfactors = 1)

# Funktion zum Formatieren der Omega-Ausgabe im Markdown-Format
format_omega_output <- function(omega_result) {
  # Header
  cat("# Omega-Ergebnisse für Skala 1 (be1 - be4)\n\n")
  
  # Call-Informationen
  cat("```plaintext\n")
  cat("Omega \n")
  cat(paste("Call:", deparse(omega_result$call), "\n"))
  cat("```\n\n")
  
  # Reliabilitätskoeffizienten
  cat("## Reliabilitätskoeffizienten\n\n")
  
  # Überprüfen, ob Werte vorhanden sind
  omega_h_value <- if(!is.null(omega_result$omega_h)) round(omega_result$omega_h, 2) else NA
  omega_lim_value <- if(!is.null(omega_result$omega_lim)) round(omega_result$omega_lim, 2) else NA
  
  reliability_table <- data.frame(
    Metrik = c("**Cronbach's Alpha (α)**", 
               "**Guttman's Lambda 6 (G6)**", 
               "**Omega Hierarchical (ω<sub>h</sub>)**", 
               "**Omega H asymptotic**", 
               "**Omega Total (ω<sub>t</sub>)**"),
    Wert = c(round(omega_result$alpha, 2),
             round(omega_result$G6, 2),
             omega_h_value,
             omega_lim_value,
             round(omega_result$omega.tot, 2))
  )
  print(kable(reliability_table, format = "markdown", col.names = c("Metrik", "Wert"), escape = FALSE))
  
  # Schmid-Leiman Faktorladungen
  cat("\n## Schmid-Leiman Faktorladungen (größer als 0.2)\n\n")
  if(!is.null(omega_result$schmid$sl) && is.data.frame(omega_result$schmid$sl)) {
    sl_loadings <- omega_result$schmid$sl
    sl_loadings <- sl_loadings %>% 
      mutate(Item = rownames(sl_loadings)) %>% 
      select(Item, everything())
    print(kable(sl_loadings, format = "markdown", digits = 2))
  } else {
    cat("Schmid-Leiman Faktorladungen sind nicht verfügbar für eine Ein-Faktor-Lösung.\n")
  }
  
  # Summe der Quadrate
  cat("\n## Summe der Quadrate\n\n")
  if(!is.null(omega_result$schmid$sl) && is.data.frame(omega_result$schmid$sl)) {
    sums_of_squares <- data.frame(
      Faktor = c("**g**", "**F1\\***", "**h2**"),
      `Summe der Quadrate` = c(
        round(sum(omega_result$schmid$sl$g^2, na.rm = TRUE), 1),
        NA,  # F1* ist nicht verfügbar bei einer Ein-Faktor-Lösung
        round(sum(omega_result$schmid$sl$h2, na.rm = TRUE), 1)
      )
    )
    print(kable(sums_of_squares, format = "markdown", col.names = c("Faktor", "Summe der Quadrate"), escape = FALSE))
  } else {
    cat("Summe der Quadrate sind nicht verfügbar für eine Ein-Faktor-Lösung.\n")
  }
  
  # Modellanpassungsstatistiken
  cat("\n## Modellanpassungsstatistiken\n\n")
  if(!is.null(omega_result$stats) && !is.null(omega_result$stats$dof)) {
    cat("- **Freiheitsgrade**:", omega_result$stats$dof, "\n")
    cat("- **Fit**:", round(omega_result$stats$fit, 2), "\n")
    cat("- **Anzahl der Beobachtungen**:", if(!is.null(omega_result$n.obs)) omega_result$n.obs else "Nicht verfügbar", "\n")
    chi_square <- if(!is.null(omega_result$stats$chi)) round(omega_result$stats$chi, 2) else NA
    p_value <- if(!is.null(omega_result$stats$p) && !is.na(omega_result$stats$p)) round(omega_result$stats$p, 4) else NA
    cat("- **Chi-Quadrat**:", chi_square)
    if(!is.na(p_value)) {
      cat(" (p <", p_value, ")\n")
    } else {
      cat(" (p-Wert nicht verfügbar)\n")
    }
    cat("- **Root Mean Square of the Residuals (RMSR)**:", if(!is.null(omega_result$stats$rms)) round(omega_result$stats$rms, 2) else "Nicht verfügbar", "\n")
    cat("- **DF-korrigiertes RMSR**:", if(!is.null(omega_result$stats$crms)) round(omega_result$stats$crms, 2) else "Nicht verfügbar", "\n")
    if(!is.null(omega_result$stats$RMSEA)) {
      cat("- **RMSEA-Index**:", round(omega_result$stats$RMSEA[1], 3), 
          "(10% Konfidenzintervalle:", round(omega_result$stats$RMSEA[2], 3), "–", round(omega_result$stats$RMSEA[3], 3), ")\n")
    } else {
      cat("- **RMSEA-Index**: Nicht verfügbar\n")
    }
    cat("- **BIC**:", if(!is.null(omega_result$stats$BIC)) round(omega_result$stats$BIC, 2) else "Nicht verfügbar", "\n")
  } else {
    cat("Modellanpassungsstatistiken sind nicht verfügbar.\n")
  }
  
  # Maße der Faktorscore-Adequanz
  cat("\n## Maße der Faktorscore-Adequanz\n\n")
  if(!is.null(omega_result$stats$beta)) {
    adequacy_table <- data.frame(
      Maß = c("**Korrelation der Scores mit den Faktoren**", 
              "**Multiples R-Quadrat der Scores mit Faktoren**", 
              "**Minimale Korrelation der Faktorscore-Schätzungen**"),
      g = c(round(omega_result$stats$beta[1], 2), 
            round(omega_result$stats$R2[1], 2), 
            round(omega_result$stats$minbeta[1], 2))
    )
    print(kable(adequacy_table, format = "markdown", col.names = c("Maß", "g"), escape = FALSE))
  } else {
    cat("Maße der Faktorscore-Adequanz sind nicht verfügbar.\n")
  }
  
  # Omega-Werte für Subskalen
  cat("\n## Gesamt-, General- und Gruppen-Omega für jede Untergruppe\n\n")
  omega_group_value <- if(!is.null(omega_result$omega_h)) round(omega_result$omega.tot - omega_result$omega_h, 2) else NA
  omega_subscales <- data.frame(
    `Omega-Typ` = c("**Omega Total für Gesamtscores und Subskalen**",
                    "**Omega General für Gesamtscores und Subskalen**",
                    "**Omega Group für Gesamtscores und Subskalen**"),
    g = c(round(omega_result$omega.tot, 2),
          omega_h_value,
          omega_group_value)
  )
  print(kable(omega_subscales, format = "markdown", col.names = c("Omega-Typ", "g"), escape = FALSE))
  
  # Hinweise
  cat("\n---\n\n")
  cat("**Hinweise:**\n\n")
  cat("- **g**: Generalfaktor\n")
  cat("- **F1\\***: Erster Gruppenfaktor (falls vorhanden)\n")
  cat("- **h2**: Kommunalität\n")
  cat("- **u2**: Einzigartigkeit (1 - h2)\n")
  cat("- **p2**: Prozentsatz der Varianz, die durch den Generalfaktor erklärt wird\n")
  cat("- **com**: Komplexität des Items (Anzahl der Faktoren, auf die das Item lädt)\n")
}

# Funktion aufrufen, um die Omega-Ausgabe zu formatieren und anzuzeigen
format_omega_output(omega_skala1)
