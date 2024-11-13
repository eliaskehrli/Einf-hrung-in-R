# Load packages
library(lavaan)
library(psych)

# Load data from the folder where the data is stored
my.data <- read.csv("/Users/eliaskehrli/Library/Mobile Documents/com~apple~CloudDocs/Uni/Master/Sport/SSR Basismodul/R-Stats/Strukturmodellierungsgleichungen/burnoutsport200.csv")

# Show the data set
View(data = my.data)
psych::describe(my.data)


# Define the model for a SEM (confirmatory factor analysis)
my.model <- '
# latente Konstrukte definieren
Perfektionismus =~ standards + selbstkritik + alles.nichts
Arbeitsanforderungen =~ menge + zeitdruck
OverCommitment =~ aufopferung + nicht.abschalten + gruebeln
Burnout =~ erschoepfung + depersonalisation

# Zusammenhänge zwischen latenten Konstrukten definieren
OverCommitment ~ Perfektionismus + Arbeitsanforderungen
Burnout ~ OverCommitment + Arbeitsanforderungen
Perfektionismus ~~ Arbeitsanforderungen
'
# SEM-Modell schaetzen

?lavaan::sem
sem.ergebnis <- lavaan::sem(model = my.model,
                            data = my.data,
                            estimator = "mlr")
# Note that the more specific function cfa() could have been 
# used instead of sem()
?"summary,lavaan-method" 
# Wie immer: Der Wust ist ohne viel Hintergrundwissen schwer 
# zu lesen und zu verstehen. Aber illustrative Anwendungsbeispiele 
# mit den wesentlichen Parametern finden sich in der Hilfe ganz am Ende

# Ausgabeoptionen mit mehr und mehr Ergebnissen
summary(sem.ergebnis)
summary(sem.ergebnis, fit.measures = TRUE, standardized = TRUE)
summary(sem.ergebnis, fit.measures = TRUE, standardized = TRUE,
        rsquare = TRUE)

# Load necessary packages
library(semPlot)

# Plot the SEM path diagram with customized appearance
semPaths(
  sem.ergebnis,
  layout = "tree2",                      # Structured layout
  whatLabels = "std",                    # Show standardized estimates on the paths
  edge.label.cex = 0.8,                  # Adjust path label size
  label.cex = 1.2,                       # Adjust node label size
  edge.color = c(rep("darkgreen", 8), rep("blue", 4)), # Adjust path colors as needed
  color = list(lat = "lightblue", man = "lightgrey"),  # Colors for latent and manifest variables
  sizeMan = 8,                           # Size for manifest variable boxes
  nCharNodes = 0                         # Show full variable names
)

