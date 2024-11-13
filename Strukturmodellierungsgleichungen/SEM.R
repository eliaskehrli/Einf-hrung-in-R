# Install packages
install.packages("lavaan", dependencies = TRUE)
install.packages("psych", dependencies = TRUE)

# Load packages
library(lavaan)
library(psych)

# Load data from the folder where the data is stored
my.data <- read.csv("/Users/eliaskehrli/Library/Mobile Documents/com~apple~CloudDocs/Uni/Master/Sport/SSR Basismodul/R-Stats/Strukturmodellierungsgleichungen/burnoutsport200.csv")

# Show the data set
View(data = my.data)
psych::describe(my.data)


# Define the model
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

# Fit the model
fit <- cfa(model= my.model, data = my.data)

# Show the results
summary(fit,fit.measures = TRUE)








