# Load packages
library(lavaan)
library(psych)
library(semPlot)

# Load data from the folder where the data is stored
my.data <- read.csv("/Users/eliaskehrli/Library/Mobile Documents/com~apple~CloudDocs/Uni/Master/Sport/SSR Basismodul/R-Stats/Strukturmodellierungsgleichungen/burnoutsport200.csv")

# Show the data set
View(data = my.data)
psych::describe(my.data)

# Elimieren des Effekts von Arbeitsanforderungen auf Burnout
# aus dem Modell

bo.modell.pfadfehlt <- '

 # Messmodelle
   Perfektionismus =~ standards + selbstkritik + alles.nichts
   Anforderungen   =~ menge + zeitdruck
   Overcommitment  =~ aufopferung + nicht.abschalten + gruebeln
   Burnout         =~ erschoepfung + depersonalisation 

 # Strukturmodell
   Overcommitment  ~  d * Perfektionismus + a * Anforderungen
   Burnout         ~  b * Overcommitment 
   Anforderungen   ~~ Perfektionismus  # Default: Korrelation exogener Faktoren
   
 # Indirekter Effekt
   perf.oc.bo := d * b
   anf.oc.bo  := a * b
'

# SEM-Modell schaetzen

?lavaan::sem
sem.ergebnis <- lavaan::sem(model = bo.modell.pfadfehlt,
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

#Legend for the plot
#blue arrows = regression paths
#green arrows = factor loadings

