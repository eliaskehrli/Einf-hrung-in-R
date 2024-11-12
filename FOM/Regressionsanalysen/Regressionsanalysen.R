# Eine Regressionsanalyse durchführen


# Schritt 1, nötige Pakete laden 
library(haven)
library(psych)
library(knitr)
library(dplyr)


# Schritt 2, Datensatz einlesen
data_spss <- read_sav("/Users/eliaskehrli/Downloads/Volleyball.sav")

# Schritt 3, Regressionsanalyse durchführen Überblick über Datensatz erlangen
str(data_spss)
View(data_spss) # zeigt die ersten 100 Zeilen des Datensatzes an

# Schritt 4, Streudiagramm erstellen
plot(data_spss$faehig, data_spss$intell)

# Schritt 5, Regressionsanalyse durchführen und tabelle der Ergebnisse erstellen
regression_model <- lm(intell ~ faehig, data = data_spss)
summary(regression_model) # zeigt die Ergebnisse der Regressionsanalyse a
kable(summary(regression_model)$coefficients, format = "markdown", col.names = c("Variable", "Schätzer", "Std. Fehler", "t-Wert", "p-Wert")) # zeigt die Ergebnisse in einer formatierten Tabelle a

# Schritt 6, Ergebnisse mit Regressionsgerade graphisch darstellen
plot(data_spss$faehig, data_spss$intell)
abline(regression_model, col = "red")


