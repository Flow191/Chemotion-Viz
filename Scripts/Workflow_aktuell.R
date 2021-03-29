library(readr)
library(rinchi)
library(magrittr)
library(classyfireR)
library(dplyr)
library(sunburstR)
library(Rcpp)
library(RMassBank)
library(ggplot2)
library(plotly)

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
Classification_List <- sapply(inchikey, get_classification)

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
class_dash <- gsub("-", "_", class_sorted)
df <- data.frame(class_dash)
n_classes <- count(df,class_dash)

# Ein erster Sunburst Plot
sunburst(data = data.frame(n_classes), legend = FALSE)

# Smiles to Mass
substance_mass <- sapply(smiles, smiles2mass)
df_mass <- data.frame(substance_mass)
mass <- data.frame(substance= row.names(df_mass), df_mass, row.names=NULL)

# Plot MW 
ggplot(mass,aes(substance,substance_mass))+
  geom_col(color="darkblue")+
  labs(title="Substances vs Molar mass",x="Substance", y = "Exact mass (g/mol)")

histogram <- ggplot(mass,aes(substance_mass))+
  geom_histogram(binwidth=20,color="darkblue", fill="lightblue")+
  labs(title="Histogram plot: Molar mass vs count",x="Exact mass (g/mol)", y = "Count")

ggplotly(histogram)
