# Install packages
install.packages("lavaan", dependencies = TRUE)
install.packages("psych", dependencies = TRUE)
--------------------------------------------
# Load packages
library(lavaan)
library(psych)

# Load data from the folder where the data is stored
my.data <- read.csv("/Users/eliaskehrli/Library/Mobile Documents/com~apple~CloudDocs/Uni/Master/Sport/SSR Basismodul/R-Stats/Strukturmodellierungsgleichungen/burnoutsport200.csv")

# Show the data set
View(data = my.data)
psych::describe(my.data)


# Define the model for a CFA (confirmatory factor analysis)
my.model <- '
# latente Konstrukte definieren
Perfektionismus =~ standards + selbstkritik + alles.nichts
Arbeitsanforderungen =~ menge + zeitdruck
OverCommitment =~ aufopferung + nicht.abschalten + gruebeln
Burnout =~ erschoepfung + depersonalisation

# Zusammenhänge zwischen latenten Konstrukten definieren
Burnout ~~ Perfektionismus + Arbeitsanforderungen + OverCommitment
Perfektionismus ~~ Arbeitsanforderungen + OverCommitment
Arbeitsanforderungen ~~ OverCommitment
'
# CFA-Modell schaetzen

?lavaan::sem
cfa.ergebnis <- lavaan::sem(model = my.model,
                            data = my.data,
                            estimator = "mlr")
# Note that the more specific function cfa() could have been 
# used instead of sem()
?"summary,lavaan-method" 
# Wie immer: Der Wust ist ohne viel Hintergrundwissen schwer 
# zu lesen und zu verstehen. Aber illustrative Anwendungsbeispiele 
# mit den wesentlichen Parametern finden sich in der Hilfe ganz am Ende

# Ausgabeoptionen mit mehr und mehr Ergebnissen
summary(cfa.ergebnis)
summary(cfa.ergebnis, fit.measures = TRUE, standardized = TRUE)
summary(cfa.ergebnis, fit.measures = TRUE, standardized = TRUE,
        rsquare = TRUE)










