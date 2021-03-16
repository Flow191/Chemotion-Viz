library(readr)
library(rinchi)
library(magrittr)
library(classyfireR)
library(dplyr)

# Die erhaltene Exceldatei aus Chemotion Repository muss vor dem Start dieses
# Skriptes leicht angepasst werden. Dazu zählt: 
# 1. Entfernen der Lösungsmittel (solvents),der Reaktionspartner (reactants) 
# und der SMILES Duplikate
# 2. Speichern der XLSX-Datei als CSV-Datei

##############################################################################

# Einlesen der SMILES-Spalte aus CSV-Datei
smiles <- read.csv("Data/sample_export_16.03.2021_8.12_noDup.csv")[ ,5]

# Umwandlung von Smiles zu InChiKey
# https://rdrr.io/github/CDK-R/rinchi/man/getinchi.html#heading-0
inchikey <- sapply(smiles, get.inchi.key)

# InChIKeys benutzen um zu klassifizieren
Classification_List <- purrr::map(inchikey, get_classification)

# Zugriff auf die class der 1. Substanz
Classification_List[[1]]@classification[["Classification"]][3]

# Schleife in der die class der ersten 10 Substanzen angezeigt wird
# NULL = leeres Objekt (zum Beispiel der "Datentyp" einer Funktion ohne Rückgabewert)
# Fehler: trying to get slot "classification" from an object of a basic class 
# ("NULL") with no slots
for (i in 15:16) {
  print(Classification_List[[i]]@classification[["Classification"]][3])
}

# Schleife in der das Objekt der ersten 10 Substanzen angezeigt wird
for (i in 1:10) {
  print(Classification_List[[i]])
}

