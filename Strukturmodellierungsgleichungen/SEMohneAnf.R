# Clear the workspace to avoid conflicts with previous models
rm(list = ls())

# Load necessary packages
library(lavaan)
library(psych)
library(semPlot)

# Load data
my.data <- read.csv("/Users/eliaskehrli/Library/Mobile Documents/com~apple~CloudDocs/Uni/Master/Sport/SSR Basismodul/R-Stats/Strukturmodellierungsgleichungen/burnoutsport200.csv")

# Show the data set
psych::describe(my.data)

# Define the model without Anforderungen
bo.modell.varfehlt <- '

 # Measurement models
   Perfektionismus =~ standards + selbstkritik + alles.nichts
   Overcommitment  =~ aufopferung + nicht.abschalten + gruebeln
   Burnout         =~ erschoepfung + depersonalisation 

 # Structural model
   Overcommitment  ~  d * Perfektionismus 
   Burnout         ~  b * Overcommitment 

 # Indirect effect
   perf.oc.bo := d * b
'

# Fit the SEM model
bo.ergebnis.varfehlt <- sem(model = bo.modell.varfehlt, 
                            data = my.data, 
                            estimator = "mlr")

# Display summary with fit measures and standardized results
summary(bo.ergebnis.varfehlt, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# Modell ohne die latente Variable Anforderungen
semPaths(bo.ergebnis.varfehlt, 
         rotation = 2, what = "std",  curvePivot = FALSE, exoVar = FALSE, 
         edge.label.cex = 1.25, sizeMan = 7.5, sizeMan2 = 7.5)
title("Reduziertes Modell\n(ohne Anforderungen)", line = 2,
      sub = "Burnout-Daten",
      cex.main = 1.0, font.main = 1, col.main = "blue",
      cex.sub  = 1.0, font.sub  = 1, col.sub  = "black")
