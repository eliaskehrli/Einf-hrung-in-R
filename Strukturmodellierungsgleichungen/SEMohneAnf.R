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

# Plot the SEM path diagram with customized appearance
semPaths(
  bo.ergebnis.varfehlt,
  layout = "tree2",                      # Structured layout
  whatLabels = "std",                    # Show standardized estimates on the paths
  edge.label.cex = 0.8,                  # Adjust path label size
  label.cex = 1.2,                       # Adjust node label size
  edge.color = c(rep("darkgreen", 8), rep("blue", 4)), # Adjust path colors as needed
  color = list(lat = "lightblue", man = "lightgrey"),  # Colors for latent and manifest variables
  sizeMan = 8,                           # Size for manifest variable boxes
  nCharNodes = 0                         # Show full variable names
)

# Add legend explanation:
# - Blue arrows represent regression paths
# - Green arrows represent factor loadings
