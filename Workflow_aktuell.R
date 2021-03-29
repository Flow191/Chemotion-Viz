library(readr)
library(rinchi)
library(magrittr)
library(classyfireR)
library(dplyr)
library(sunburstR)
library(Rcpp)
library(RMassBank)
library(ggplot2)

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
class <- vector("character", length(Classification_List[]))
for (i in 1:length(Classification_List[])) {
  if( is.null(Classification_List[[i]]) == 'TRUE') { 
  } 
  else if (is.null(Classification_List[[i]]@classification[["Classification"]][level]) == 'TRUE'){
  }
  else { 
    class[i] <- Classification_List[[i]]@classification[["Classification"]][level]
  }
}

# Leere Strings als NA markieren und entfernen
class[class==""] <- NA
class[!is.na(class)]

# Sortieren und zählen der classes
class_sorted <- sort(class)
df <- data.frame(class_sorted)
n_classes <- count(df,class_sorted)

# Ein erster Sunburst Plot
sunburst(data = data.frame(n_classes), legend = FALSE)

# Smiles to Mass
mass_func <- sapply(smiles, smiles2mass)
df_mass <- data.frame(as.list(mass_func))
substance_mass <- t(df_mass)
mass <- data.frame(substance= row.names(substance_mass), substance_mass, row.names=NULL) 

# Plot MW 
ggplot(mass,aes(substance,substance_mass))+
  geom_col(color="darkblue")+
  labs(title="Substance histogram plot",x="Substance", y = "Substance MW(g/mol)")

ggplot(mass,aes(substance_mass))+
  geom_histogram(binwidth=20,color="darkblue", fill="lightblue")+
  labs(title="Molar mass histogram plot",x="MW(g/mol)", y = "Count")

#Test