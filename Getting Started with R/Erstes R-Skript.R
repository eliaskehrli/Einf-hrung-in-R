#---------------------------------------------------------------
# SSR-Basismodul Mulativariate Statistics
# Einfuehrung in R
# R-Skript zu "Arbeitsauftraege R-Intro"

# Author: Juerg Schmid
# Date: May 2019 | August 2023
#---------------------------------------------------------------


#---------------------------------------------------------------
# Installation von R/RStudio und Anlegen eines RStudio-Projekts
# 1) Installieren Sie R und RStudio.
# 2) Erstellen Sie in RStudio ein Projekt - z. B. mit 
#    dem Namen "Einfuehrung in R".
# 3) Erstellen Sie ein erstes und spaeter zu erweiterndes  
#    R-Skript, das vorderhand neben dem Autor und dem 
#    Erstellungsdatum nur Ueberschriften enthaelt, z. B.
#       - SSR-Basismodul Multivariate Statistics
#       - Einfuehrung in R
#       - Kapitel 1: Installation und Laden von R-Paketen
# 4) Speichern Sie dieses Skript z. B. unter dem  
#    Namen "Arbeitsauftrag_R-Intro.R".
# 5) Machen Sie sich mit der Programmoberflaeche in 
#    RStudio etwas vertraut, indem Sie einen Blick in 
#    die Menues werfen und die verschiedenen Reiter 
#    durchsehen.
# 6) Schliessen Sie das Projekt und RStudio, 
#    oeffnen Sie es erneut und vergewissern Sie sich, 
#    dass im Projekt organisatorisch alles 
#    seine Richtigkeit hat.


# Arbeitsauftrag 2 ----
#---------------------------------------------------------------
# R-Pakete installieren und laden
# 1) Installieren und laden Sie folgende R-Pakete, die 
#    im Rahmen dieser Einfuehrung verwendet werden: 
#    foreign, stats, car und Hmisc.

install.packages("foreign", dependencies = TRUE) 
# dependencies ist wichtig weil foreign von car abhaengt
install.packages("car", dependencies = TRUE)
install.packages("Hmisc", dependencies = TRUE)

library("foreign")
library("stats")
library("car")
library("Hmisc")

# 2) Finden Sie R-Pakete, die z. B. fuer Item- und  
#    Skalenanalysen (speziell fuer die Berechnung 
#    von Cronbach alpha), Faktor-, Varianz- und 
#    Clusteranalysen verwendet werden koennen.
?? "Cronbach alpha"
# Das R-Paket psych könnte hilfreich sein

?? "factor analysis"
# Puh! Aber auch hier scheint das R-Paket psych hilfreich

# Auch eine Internet-Recherche liesse sich durchfuehren.
# Mit Google und der Serie von Schluesselwoertern 
# "R package cronbach alpha" findet man einiges.

# 3) Installieren Sie die gewuenschten R-Pakete unter 
#    Verwendung der Menuesteuerung in RStudio.
install.packages("psych", dependencies = TRUE)
library("psych")
# Etc.


# Arbeitsauftrag 3 ----
#---------------------------------------------------------------
# Daten einlesen
# Die Datei MVArbeitsengegement25 liegt auf ILIAS in  
# mehreren Formaten vor, naemlich .sav, .xls, .txt, 
# .dat und .csv. 
# 1) Lesen Sie einige dieser Dateien in den verschiedenen
#    Formaten unter Verwendung von R-Code und der 
#    Menuesteuerung ein.

# .sav-Format
#---------------------------------------------------------------
library("foreign")
ae25 <- read.spss(file = "MVArbeitsengagement25.sav",
                  to.data.frame = TRUE, 
                  use.value.labels = FALSE, 
                  use.missings = TRUE)

# .csv-Format
#---------------------------------------------------------------
# Es stehen mindestens zwei Optionen zur Verfuegung:
ae25csv <- read.csv(file = "MVArbeitsengagement25.csv", 
                    header = TRUE,
                    na.strings = "999")
ae25csv <- read.table("MVArbeitsengagement25.csv", 
                      header = TRUE, sep = ",",
                      na.strings = "999")

# Von Hand die erste kryptische Variablenbezeichnung (ï..id) 
# korrigieren, z. B. im Datensatz ae25csv
fix(ae25csv)

# .dat-Format
#---------------------------------------------------------------
ae25dat <- read.table("MVArbeitsengagement25.dat", 
                      header = TRUE, 
                      sep = "\t", na.strings = "999")

# .txt-Format
#---------------------------------------------------------------
ae25txt <- read.table("MVArbeitsengagement25.txt", 
                      header = TRUE, sep = "\t",
                      na.strings = "999")

# 2) Vergewissern Sie sich, dass der Prozess jeweils 
#    erwartungsgemaess abgelaufen ist, und zwar auf der 
#    Ebene des Gesamtdatensatzes sowie auf der Ebene 
#    einzelner Untersuchungseinheiten, d. h. bezueglich 
#    Anzahl Untersuchungseinheiten, Stichprobenkennwerte, 
#    fehlende Werte und Format der Variablen.
View(ae25)
names(ae25)
head(ae25)
head(ae25, 6)
tail(ae25)
dim(ae25)    
nrow(ae25)
ncol(ae25)
summary(ae25)


# Arbeitsauftrag 4 ----
#---------------------------------------------------------------
# Variablen auswaehlen
# 1) Vollziehen Sie die Beispiele von den Folien zum Thema 
#    Variablen auswaehlen nach.

# Informationen zum Beispieldatensatz (ae25) ausgeben
# Falls noetig: R-Paket psych installieren und laden

install.packages("psych", dependencies = TRUE)
library("psych")

# Man kann die Ausgabe nicht nur auf den Bildschirm lenken.
# Vergleichen Sie den Effekt der drei Anweisungen
describe(ae25)
dscr1.out <- describe(ae25)
dscr1.out

# Variablenselektion unter Verwendung der Variablenposition:
ae4vars <- ae25[ , c(1,  3:5)]
ae4vars

# Variablenselektion unter Verwendung des Variablennamens:
(ae4vars <- ae25[c("id", "pehrgeiz", "pkreativ","pleistung")])

# 2) Vollziehen Sie auch nach, was folgende Funktionen 
#    bzw. Anweisungen bewirken:
#    myvars <- c("id", "pehrgeiz", "pkreativ", "pleistung")
#    ae5vars <- ae25[myvars] 

myvars <- c("id", "pehrgeiz", "pkreativ", "pleistung")
myvars
ae4vars <- ae25[myvars]
ae4vars


# Arbeitsauftrag 5 ----
#---------------------------------------------------------------
# Faelle auswaehlen
# Ziel dieses Auftrags ist es, jene Untersuchungseinheiten 
# aus dem Datensatz zu waehlen, die bezueglich ihres 
# Arbeitsengagements (Median) ueber- bzw. 
# unterdurchschnittlich sind.
# 1) Finden Sie zunaechst ein R-Paket, genauer: 
#    eine Funktion (in einem paket), mit welcher der Median   
#    einer Variablen bestimmt werden kann, und zwar unter  
#    Verwendung der internen Hilfestellung oder z. B. der 
#    internetbasierten Hilfestellung Quick-R.

?median
stats::median(ae25, na.rm = TRUE)              # Geht nicht
stats::median(ae25[, 9], na.rm = TRUE)         # Geht
stats::median(ae25[, "konlohn"], na.rm = TRUE) # Geht
stats::median(ae25[, c(1,8:9)], na.rm = TRUE)  # Geht nicht
stats::median(ae25$konlohn, na.rm = TRUE)      # Geht
stats::median(ae25[konlohn], na.rm = TRUE)     # Geht nicht
stats::median(ae25["konlohn"], na.rm = TRUE)   # Geht nicht

# https://www.statmethods.net/stats/descriptives.html
# R provides a wide range of functions for obtaining 
# summary statistics and thus the median
?summary
base::summary(ae25)               # Geht
base::summary(ae25[, 9])          # Geht
base::summary(ae25[, c(1,8:9)])   # Geht
base::summary(ae25$konlohn)       # Geht
base::summary(ae25["konlohn"])    # Geht
base::summary(ae25[, "konlohn"])  # Geht
base::summary(ae25[konlohn])      # Geht nicht

?fivenum 
# Liefert Tukey's min, lower-hinge, median, upper-hinge, max
stats::fivenum(ae25)             # Geht nicht
stats::fivenum(ae25$konlohn)     # Geht
stats::fivenum(ae25[, 9])        # Geht
stats::fivenum(ae25[,c(1,8:9)])  # Geht nicht
stats::fivenum(ae25[konlohn])    # Geht nicht
stats::fivenum(ae25["konlohn"])  # Geht nicht

?Hmisc::describe
# n, nmiss, unique, mean, percentiles,
# 5 lowest and 5 highest scores
Hmisc::describe(ae25)            # Geht
Hmisc::describe(ae25$konlohn)    # Geht
Hmisc::describe(ae25[,9])        # Geht
Hmisc::describe(ae25[,c(1,8:9)]) # Geht
Hmisc::describe(ae25[konlohn])   # Geht nicht
Hmisc::describe(ae25["konlohn"]) # Geht nicht

??psych
psych::describe(ae25)            # Geht
psych::describe(ae25$konlohn)    # Geht
psych::describe(ae25[,9])        # Geht
psych::describe(ae25[,c(1,8:9)]) # Geht
psych::describe(ae25[konlohn])   # Geht nicht
psych::describe(ae25["konlohn"]) # Geht
psych::describe(ae25[myvars])    # Geht

# 2) Legen Sie die beiden Gruppen in zwei separaten Dateien ab.
(highincome <- subset(ae25, konlohn >= 2600))
(lowincome  <- subset(ae25, konlohn < 2600))

# 3) Wie viele Faelle umfassen die beiden Datensaetze?
?nrow
?dim
nrow(highincome)
dim(lowincome)


# Arbeitsauftrag 6 ----
#---------------------------------------------------------------
# Variablen rekodieren
# 1) Bilden Sie eine neue Dummyvariable, einen Faktor, 
#    namens female mit den Auspraegungen 0 = "maennlich" 
#    und 1 = "weiblich".

install.packages("car", dependencies = TRUE)
library("car")

# Option 1: Mit recode()
ae25$r1sex <- car::recode(ae25$sex, "0 = 1; 1 = 0") 	

# Option 2: Subtraktion
ae25$r2sex <- 1 - ae25$sex

# Option 3: Direkt Zuweisung
ae25$r3sex[ae25$sex == 0] <- 1
ae25$r3sex[ae25$sex == 1] <- 0	

# 2) Kontrollieren Sie die Korrektheit Ihrer Anweisungen.
summary(ae25[, c(3, 15:17)])
View(ae25)


# Arbeitsauftrag 7 ----
#---------------------------------------------------------------
# Umgehen mit R-Objekten
# 1) Vollziehen Sie die Arbeitsschritte nach, die im Kapitel 
#    ueber Objekte angegangen worden sind.
dscr2.out <- psych::describe(ae25$konlohn)
dscr2.out
str(dscr2.out)
(m <- dscr2.out$mean)
(sd <- dscr2.out$sd)

# 2) Versuchen Sie insbesondere Cohens d zu berechnen, 
#    und zwar unter Verwendung 
#    - der Taschenrechnerfunktion, 
#    - von R-Code auf der Basis einer R-Ausgabe 
#    - der entsprechenden Funktion des R-Pakets psych
?describeBy
(dscr3.out <- describeBy(ae25$konlohn, group = ae25$sex))

# Grobe Schaetzung (anhand der Streuung der Gesamtstichprobe)
str(dscr3.out)
(mfemale <- dscr3.out$'0'$mean)
(mmale <- dscr3.out$'1'$mean)
(sd <- dscr2.out$sd)
(dgrob <- (mfemale - mmale) / dscr2.out$sd)

# Genaue Schaetzung (anhand der gepoolten Streuung)
(m0 <- dscr3.out$'0'$mean)
(m1 <- dscr3.out$'1'$mean)
(sd0 <- dscr3.out$'0'$sd)
(sd1 <- dscr3.out$'1'$sd)
(n0 <- dscr3.out$'0'$n)
(n1 <- dscr3.out$'1'$n)
(dgenau <- (m0 - m1) / sqrt((sd0^2*n0 + sd0^2*n1)/(n0 + n1)))

psych::cohen.d(ae25[c(3,9)],"sex")


# Arbeitsauftrag 8 ----
#---------------------------------------------------------------
# Summen-/Mittelwerte berechnen 
# 1) Ueben Sie nochmals das Einlesen eines Datensatzes 
#    (KognitiverTest).

# .sav-Datei: Resultat des Vorgehens mittels Menuesteuerung 
# ("Environment > Import Dataset")
library(haven)
kt <- read_sav("KognitiverTest.sav")
View(kt)

# xlx(x)-Datei (Excel-File): Resultat des Vorgehens mittels
# Menuesteuerung im Fenster unten rechts (Files & Doppelklick)
# Achtung: Semikolon als Trennzeichen spezifizieren
library(readxl)
kt <- read_excel("KognitiverTest.xlsx")
View(kt)

# 2) Schaetzen Sie (quick and dirty) die Testhalbierungs-
#    reliabilitaet der Skala Schlussfolgerndes Denken.
#    - Berechnen Sie zunaechst einen Skalenwert fuer die eine
#      Haelfte der Items und einen Skalenwert fuer die andere 
#      Haelfte der zehn Items berechnen (0 = "Nicht 
#      korrekte geloeste Aufgabe", 1 = "Korrekt geloeste 
#      Aufgabe").

# Erste vs. zweite Testhaelfte
kt$half1 <- rowMeans(kt[2:6], na.rm = TRUE)
kt$half2 <- rowMeans(kt[7:11], na.rm = TRUE)

# Gerade vs. ungerade Items
kt$odd <- rowMeans(kt[c(2, 4, 6, 8, 10)], na.rm = TRUE)
kt$even <- rowMeans(kt[c(3, 5, 7, 9, 11)], na.rm = TRUE)

#    - Bestimmen Sie die Spearman-Korrelation der beiden 
#      Skalenwerte (ohne Adjustierung fuer die Testlaenge). 
#      Konsultieren Sie die Hilfe in R, um sich ueber die 
#      Funktion corr.test()zu informieren.
?corr.test
psych::corr.test(kt$half1, kt$half2, use = "pairwise", 
                 method = "spearman")
psych::corr.test(kt$odd, kt$even, use = "pairwise", 
                 method = "spearman")


# Arbeitsauftrag 9 ----
#---------------------------------------------------------------
# RStudio-Projekt schliessen
# Schliessen Sie die Arbeitssitzung im R-Projekt "Einfuehrung 
# in R" ab.
# 1) Speichern Sie das Skript-File.
#    Per Menuesteuerung in RStudio

# 2) Pruefen Sie, welche Objekte (Environment) allenfalls 
#    vom Workspace entfernt werden sollen. Die Funktion ls() 
#    (fuer engl. list, ohne jegliche Spezifikation in Klammern) 
#    kann insofern hilfreich sein, als sie eine Auflistung 
#    aller Objekte im Workspace liefert.
ls()

# 3) Entfernen (engl. remove) Sie irrelevante Objekte mit 
#    der Funktion rm()
rm(ae25csv, ae4vars, ae5upn, ae5vars, d, dgenau, dscr1.out, dscr2.out,
   dscr3.out, highincome, lowincome, m, m0, m1, sd, sd0, sd1, 
   n0, n1, mfemale, mmale, myvars, sdfemale, sdmale)

# 4) Speichern Sie das RStudio-Projekt.
#    Per Menuesteuerung in RStudio
