sink(Ausgabe_Burnout_fuer_SR.txt)
#-----------------------------------------------------------------------
# SSR-Basismodul Mulativariate Statistics
# Strukturgleichungsmmodellierung
# R-Skript zum Arbeitsauftrag Burnout (Mediation)
#
# Author: Juerg Schmid
# Date:   November 15, 2022 | August 2, 2023
#-----------------------------------------------------------------------


# Mise-en-place ----
#-----------------------------------------------------------------------
# Dieses Skript in einem neuen RStudio-Projekt speichern
# und damit auch gleich das Arbeitsverzeichnis (working 
# directory) definieren, z. B. Example_SEM_Burnout.Rproj

# I happen to know in advance that we need at least the
# following packages, so we might as well install (if 
# necessary) and load them up front. I like to have this
# type of code at the very beginning of a script file
# to find it easily. However, if you follow strictly
# this Arbeitsauftrag, you will also be prompted later on
# to install and load, for example, lavaan.

install.packages("haven", dependencies = TRUE)
install.packages("lavaan", dependencies = TRUE)
install.packages("semPlot", dependencies = TRUE)
install.packages("lavaanPlot", dependencies = TRUE)

library("haven")
library("lavaan")
library("semPlot")
library("lavaanPlot")


# Aufgabe a ----
#-----------------------------------------------------------------------
# Aktuelle PDF-Version des Tutorials: http://lavaan.ugent.be/)
# Aktuelle Online-Version: http://lavaan.ugent.be/tutorial/index.html


# Aufgabe b ----
#-----------------------------------------------------------------------
# Daten einlesen und inspizieren
# Option: csv-Format
?read.csv
burnoutsport200 <- utils::read.csv("burnoutsport200.csv", header = TRUE)
str(burnoutsport200)
View(burnoutsport200)

# Option sav-Format
install.packages("haven", dependencies = TRUE)
library("haven")
?read_sav
burnoutsport200 <- haven::read_sav("burnoutsport200.sav")
str(burnoutsport200)
View(burnoutsport200)

# Option dat-Format
?read.delim
burnoutsport200 <- utils::read.delim("burnoutsport200.dat")
str(burnoutsport200)
View(burnoutsport200)

# You may want to try file.choose(), a handy function in case  
# you have to look for a file someplace outside the project folder
burnoutsport200 <- haven::read_sav(file.choose())

# It happens sometimes, but no always that the first variable name 
# gets a cryptic name extension (e.g., ì..id). Maybe you want 
# to change it
## Option by hand
fix(burnoutsport200)
## Option using R code, e.g., using the names() function in the 
# base package: The first variable name is overwritten
base::names(burnoutsport200)[1] <- "id"
View(burnoutsport200)


# Aufgabe c ----
#-----------------------------------------------------------------------
# R-Paket lavaan lokal (auf dem eigenen Rechner) speichern  
# und in den Arbeitsspeicher laden. Hat man diesen Schritt
# weiter oben ("Mise-en-place, etwa Zeilen 12-34) schon 
# gemacht, ist er natuerlich obsolet

install.packages("lavaan", dependencies = TRUE)
library("lavaan")

cfa.modell <- '

 # Messmodelle zur Erfassung von vier Konstrukten
   Perfektionismus =~ standards + selbstkritik + alles.nichts
   Anforderungen   =~ menge + zeitdruck
   Overcommitment  =~ aufopferung + nicht.abschalten + gruebeln
   Burnout         =~ erschoepfung + depersonalisation 

 # Kovarianz/Korelation zwischen allen Faktoren (explizite Spezifikation
 # von 6 KovarianzenKorrelationen. Default ist Kovarianz/Korrelation
 # zwischen allen exogenen Faktoren)
   Perfektionismus ~~ Anforderungen + Overcommitment + Burnout
   Anforderungen   ~~ Overcommitment + Burnout
   Overcommitment  ~~ Burnout
'


# Aufgabe d ----
#-----------------------------------------------------------------------
# CFA-Modell schaetzen

?lavaan::sem
cfa.ergebnis <- lavaan::sem(model = cfa.modell,
                            data = burnoutsport200,
                            estimator = "mlr")
# Note that the more specific function cfa() could have been 
# used instead of sem()


# Aufgabe e ----
#-----------------------------------------------------------------------
# Ergebnisse inspizieren, speziell die Fit-Indices und
# die standardisierten Parameterschaetzungen


?"summary,lavaan-method" 
# Wie immer: Der Wust ist ohne viel Hintergrundwissen schwer 
# zu lesen und zu verstehen. Aber illustrative Anwendungsbeispiele 
# mit den wesentlichen Parametern finden sich in der Hilfe ganz am Ende

# Ausgabeoptionen mit mehr und mehr Ergebnissen
summary(cfa.ergebnis)
summary(cfa.ergebnis, fit.measures = TRUE, standardized = TRUE)
summary(cfa.ergebnis, fit.measures = TRUE, standardized = TRUE,
        rsquare = TRUE)


# Aufgabe f ----
#-----------------------------------------------------------------------
# SEM-Modell definieren - zunaechst noch ohne Parameter-Labels

bo.modell.nolabels <- '

 # Messmodelle zur Erfassung von vier Konstrukten
   Perfektionismus =~ standards + selbstkritik + alles.nichts
   Anforderungen   =~ menge + zeitdruck
   Overcommitment  =~ aufopferung + nicht.abschalten + gruebeln
   Burnout         =~ erschoepfung + depersonalisation 

 # Strukturmodell
   Overcommitment  ~  Perfektionismus + Anforderungen
   Burnout         ~  Overcommitment  + Anforderungen
   Anforderungen   ~~ Perfektionismus  # Default: Korrelation exogener Faktoren
'


# Aufgabe g ----
#-----------------------------------------------------------------------
# SEM-Modell schaetzen
?lavaan::sem
bo.nolabels.ergebnis <- lavaan::sem(model = bo.modell.nolabels,
                                    data = burnoutsport200,
                                    estimator = "mlr")

# Ergebnisse inspizieren, speziell die Fit-Indices und
# die standardisierten Parameterschaetzungen
summary(bo.nolabels.ergebnis, fit.measures = TRUE, standardized = TRUE,
        rsquare = TRUE)


# Aufgabe h ----
#-----------------------------------------------------------------------
# Ergaenzen des Modells, damit die indirekten Effekte und
# der Gesamteffekt (und die Signifikanztests) berechnet
# werden (siehe den Effekt davon unter "defined parameters" 
# am Ende der nachmaligen Ausgabe)

bo.modell <- '

# Messmodelle zur Erfassung von vier Konstrukten
  Perfektionismus =~ standards + selbstkritik + alles.nichts
  Anforderungen   =~ menge + zeitdruck
  Overcommitment  =~ aufopferung + nicht.abschalten + gruebeln
  Burnout         =~ erschoepfung + depersonalisation 

# Strukturmodell
  Overcommitment  ~  d * Perfektionismus + a * Anforderungen
  Burnout         ~  b * Overcommitment  + c * Anforderungen
  Anforderungen   ~~ Perfektionismus  # Default: Korrelation exogener Faktoren
   
# Indirekte Effekte und Gesamteffekt
# Zur gewaehlten - willkuerlichen - Nomenklatur:
# perf.oc.bo: Effekt von Perfektionismus (perf) 
# via Over-Comittment (.oc.) auf Burnout (.bo)
  perf.oc.bo := d * b
  anf.oc.bo  := a * b
  ges.anf.bo := c + a * b
'


# Aufgabe i ----
#-----------------------------------------------------------------------
# Schaetzen des Modells
bo.ergebnis <- sem(model = bo.modell, 
                   data = burnoutsport200,
                   estimator = "mlr")

# Inspizieren der Ergebnisse
summary(bo.ergebnis, fit.measures = TRUE, 
        standardized = TRUE, rsquare = TRUE)


# Aufgabe j ----
#-----------------------------------------------------------------------
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

bo.ergebnis.pfadfehlt <- sem(model = bo.modell.pfadfehlt,
                             data = burnoutsport200, 
                             estimator = "mlr")

summary(bo.ergebnis.pfadfehlt, fit.measures = TRUE, 
        standardized = TRUE, rsquare = TRUE)


# Aufgabe k ----
#-----------------------------------------------------------------------
# Eliminieren der latenten Variablen Arbeitsanforderungen aus 
# dem Modell

bo.modell.varfehlt <- '

 # Messmodelle
   Perfektionismus =~ standards + selbstkritik + alles.nichts
   Overcommitment  =~ aufopferung + nicht.abschalten + gruebeln
   Burnout         =~ erschoepfung + depersonalisation 

 # Strukturmodell
   Overcommitment  ~  d * Perfektionismus 
   Burnout         ~  b * Overcommitment 

 # Indirekter Effekt
   perf.oc.bo := d * b
'

bo.ergebnis.varfehlt <- sem(model = bo.modell.varfehlt, 
                            data = burnoutsport200, 
                            estimator = "mlr")

summary(bo.ergebnis.varfehlt, fit.measures = TRUE, standardized = TRUE)

# Pro memoria: Erneutes Ausgeben ausgewählter Fitmss zu den Modellen
fitMeasures(cfa.ergebnis, fit.measures =
            c("cfi.robust","tli.robust", 
              "rmsea.ci.lower.robust", "rmsea.robust",
              "rmsea.ci.upper.robust", "srmr"))
fitMeasures(bo.nolabels.ergebnis, fit.measures =
              c("cfi.robust","tli.robust", 
                "rmsea.ci.lower.robust", "rmsea.robust",
                "rmsea.ci.upper.robust", "srmr"))
fitMeasures(bo.ergebnis, fit.measures =
              c("cfi.robust","tli.robust", 
                "rmsea.ci.lower.robust", "rmsea.robust",
                "rmsea.ci.upper.robust", "srmr"))
fitMeasures(bo.ergebnis.pfadfehlt, fit.measures =
              c("cfi.robust","tli.robust", 
                "rmsea.ci.lower.robust", "rmsea.robust",
                "rmsea.ci.upper.robust", "srmr"))
fitMeasures(bo.ergebnis.varfehlt, fit.measures =
               c("cfi.robust","tli.robust", 
                 "rmsea.ci.lower.robust", "rmsea.robust",
                 "rmsea.ci.upper.robust", "srmr"))


# Aufgabe l ----
#-----------------------------------------------------------------------
# Vergleichen der einzelnen Modelle unter Verwendung des Likelihood-
# Ratio-Tests (LR-Test) und der Funktion anova(). Das letzte
# Modell enthaelt nicht dieselben Variablen wie die anderen
# Modelle (Wegfall des Faktors fuer Anforderungen und die beiden
# Indikatoren). Es ist damit nicht in den anderen Modellen
# "genestet" und kann nicht mittels LR-Test verglichen werden.

anova(cfa.ergebnis, bo.ergebnis, bo.ergebnis.pfadfehlt)

# Unfortunately, there is a warning regarding a negative 
# scaling factor. We'd have to hunt for an explanation and
# a workaround, but it seems to get rather sticky. I found 
# (August 2, 2023) in the lavaan group a thread: 
# https://groups.google.com/g/lavaan/c/CbXVkO95Scw
# For the time being, we are happy to be able to compare
# the CFA model and the (full) SEM model.

# Note that the failure of the LR test is no a catastrophe
# because many authors argue that it is better to only
# compare models using CFI and RMSEA on a descriptive level.
# Differences in CFI of less than .01 and in RMSEA of .015 
# are considered to indicate a minor deterioration of 
# model fit


# Aufgabe m ----
#-----------------------------------------------------------------------
install.packages("semPlot", dependencies = TRUE)
library("semPlot")

?semPlot; ?semPaths
# Further information: http://sachaepskamp.com/semPlot

# Very simple graph
semPaths(bo.ergebnis)   

# Enhanced graph (displaying at least standardized parameters)
semPaths(bo.ergebnis, 
         rotation = 2, 
         what = "std", 
         curvePivot = FALSE, 
         exoVar = FALSE, 
         edge.label.cex = 1.25,
         sizeMan = 7.5, sizeMan2 = 7.5)

# A figure caption would help things, wouldn't you say so?
?title
title("Komplettes Modell", line = 2,
      sub = "Burnout-Daten",
      cex.main = 1.0, font.main = 1, col.main = "blue",
      cex.sub  = 1.0, font.sub  = 1, col.sub  = "black")

# lavaanPlot is an R package that helps displaying results 
# produced with lavaan, see
# https://www.alexlishinski.com/post/lavaanplot-0-5-1/
install.packages("lavaanPlot", dependencies = TRUE)
library("lavaanPlot")
?lavaanPlot

lavaanPlot(model = bo.ergebnis, 
           node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "grey"), 
           coefs = TRUE, covs = TRUE, 
           stars = c("regress", "latent", "covs"),
           stand = TRUE, sig = 0.05)

# Modell ohne Pfad von Anforderungen auf Burnout
semPaths(bo.ergebnis.pfadfehlt, 
         rotation = 2, what = "std",  curvePivot = FALSE, exoVar = FALSE, 
         edge.label.cex = 1.25, sizeMan = 7.5, sizeMan2 = 7.5)
title("Reduziertes Modell\n(ohne Pfad von Anforderungen auf Burnout)", 
      line = 2, sub = "Burnout-Daten",
      cex.main = 1.0, font.main = 1, col.main = "blue",
      cex.sub  = 1.0, font.sub  = 1, col.sub  = "black")

# Modell ohne die latente Variable Anforderungen
semPaths(bo.ergebnis.varfehlt, 
         rotation = 2, what = "std",  curvePivot = FALSE, exoVar = FALSE, 
         edge.label.cex = 1.25, sizeMan = 7.5, sizeMan2 = 7.5)
title("Reduziertes Modell\n(ohne Anforderungen)", line = 2,
      sub = "Burnout-Daten",
      cex.main = 1.0, font.main = 1, col.main = "blue",
      cex.sub  = 1.0, font.sub  = 1, col.sub  = "black")


# Aufgabe n ----
#-----------------------------------------------------------------------
# Allenfalls Workspace aufraeumen
ls()                             # Listing der Objekte im Workspace
sink()
rm("...", "...", "...", "...")   # Oder auch ohne Anfuehrungszeichen

rm(ergebnis1, ergebnis2, ergebnis3, ergebnis4, ergebnis5)
rm(burnspo)
