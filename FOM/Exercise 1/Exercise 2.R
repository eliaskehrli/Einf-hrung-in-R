# Installieren von notwendigen Pakete, die noch nicht installiert sind
install.packages("haven")
install.packages("psych")
install.packages("dplyr")

# Lade die benötigten Pakete
library(haven)
library(psych)
library(knitr)
library(dplyr)

# SPSS-Datei importieren
data_spss <- read_sav("/Users/eliaskehrli/Downloads/FEKW_1.sav")

# Funktion zum Anzeigen der Ergebnisse in formatierten Tabellen
zeige_alpha_ergebnisse <- function(result, skala_name) {
  cat("\n###", skala_name, "\n")
  
  # Cronbach's Alpha und Zusammenfassende Statistiken
  cat("\n#### Cronbach's Alpha and Summary Statistics\n")
  
  # Verfügbare Metriken aus result$total extrahieren
  available_metrics <- names(result$total)
  
  # Vektoren für Metriknamen und entsprechende Werte erstellen
  metric_names <- c()
  metric_values <- c()
  
  # Alle möglichen Metriken und ihre entsprechenden Namen definieren
  possible_metrics <- list(
    "raw_alpha" = "Raw Alpha",
    "std.alpha" = "Standardized Alpha",
    "G6.smc" = "G6(smc)",
    "average_r" = "Average r",
    "SNR" = "S/N",
    "ase" = "ASE",
    "mean" = "Mean",
    "sd" = "SD",
    "median_r" = "Median r"
  )
  
  # Durch mögliche Metriken iterieren und hinzufügen, wenn verfügbar
  for (metric in names(possible_metrics)) {
    if (metric %in% available_metrics) {
      metric_names <- c(metric_names, possible_metrics[[metric]])
      metric_values <- c(metric_values, result$total[[metric]])
    }
  }
  
  # DataFrame der Zusammenfassenden Statistiken erstellen
  summary_stats <- data.frame(
    Metric = metric_names,
    Value = metric_values
  )
  
  print(kable(summary_stats, format = "markdown", col.names = c("Metric", "Value"), digits = 3))
  
  # 95% Confidence Boundaries
  cat("\n**95% Confidence Boundaries:**\n")
  
  # Berechne die Konfidenzgrenzen, wenn möglich
  if (!is.null(result$total$raw_alpha) && !is.null(result$total$ase)) {
    # Manuelle Berechnung der Konfidenzintervalle
    alpha <- result$total$raw_alpha
    ase <- result$total$ase
    z_value <- qnorm(0.975)  # für 95% Konfidenzintervall
    lower_bound <- alpha - z_value * ase
    upper_bound <- alpha + z_value * ase
    conf_boundaries <- data.frame(
      Metric = c("Lower Bound", "Alpha", "Upper Bound"),
      Value = c(lower_bound, alpha, upper_bound)
    )
    print(kable(conf_boundaries, format = "markdown", digits = 3))
  } else {
    cat("Confidence boundaries not available.\n")
  }
  
  # Reliabilität, wenn ein Item entfernt wird
  cat("\n#### Reliability if an Item is Dropped\n")
  if (!is.null(result$alpha.drop)) {
    alpha_drop <- result$alpha.drop %>%
      mutate(Item = rownames(result$alpha.drop)) %>%
      select(Item, everything())
    print(kable(alpha_drop, format = "markdown", digits = 3))
  } else {
    cat("No data available for reliability if an item is dropped.\n")
  }
  
  # Item-Statistiken
  cat("\n#### Item Statistics\n")
  if (!is.null(result$item.stats)) {
    item_stats <- result$item.stats %>%
      mutate(Item = rownames(result$item.stats)) %>%
      select(Item, everything())
    print(kable(item_stats, format = "markdown", digits = 3))
  } else {
    cat("No item statistics available.\n")
  }
  
  # Antworthäufigkeiten ohne fehlende Werte
  if (!is.null(result$response.freq)) {
    cat("\n#### Non-missing Response Frequencies\n")
    response_freq <- as.data.frame(result$response.freq)
    response_freq <- response_freq %>%
      mutate(Item = rownames(response_freq)) %>%
      select(Item, everything())
    print(kable(response_freq, format = "markdown", digits = 3))
  }
}

# Berechnung von Cronbach's Alpha für jede Skala mit n.iter für Konfidenzintervalle
# Für Skala 2 verwenden wir check.keys = TRUE, um negativ korrelierte Items zu korrigieren

# Skala 1
result_skala1 <- alpha(data_spss[, c("be1", "be2", "be3", "be4")], n.iter = 1000)

# Skala 2
result_skala2 <- alpha(data_spss[, c("g1", "g2", "g3", "g4")], n.iter = 1000, check.keys = TRUE)

# Skala 3
result_skala3 <- alpha(data_spss[, c("i1", "i2", "i3", "i4")], n.iter = 1000)

# Skala 4
result_skala4 <- alpha(data_spss[, c("v1", "v2", "v3", "v4")], n.iter = 1000)

# Ergebnisse für jede Skala anzeigen
zeige_alpha_ergebnisse(result_skala1, "Skala 1 (be1 - be4)")
zeige_alpha_ergebnisse(result_skala2, "Skala 2 (g1 - g4)")
zeige_alpha_ergebnisse(result_skala3, "Skala 3 (i1 - i4)")
zeige_alpha_ergebnisse(result_skala4, "Skala 4 (v1 - v4)")

----------------------------------

# Benötigte Pakete laden
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

# Ergebnisse anzeigen
print(omega_skala1)

