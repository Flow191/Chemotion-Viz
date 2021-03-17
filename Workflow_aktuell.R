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

# Klassifizierungsgrad angeben: 
# "kingdom"=1; "superclass"=2; "class"=3; "subclass"=4 
level=3

# Schleife in der die class aller Substanzen angezeigt wird
class <- vector("character", 2343)
for (i in 1:length(Classification_List[])) {
  if( is.null(Classification_List[[i]]) == 'TRUE') { 
  } else { 
    class[i] <- Classification_List[[i]]@classification[["Classification"]][level]}
}
class[class==""] <- NA
class_no_NA <- class[!is.na(class)]
class_sorted <- sort(class_no_NA)
