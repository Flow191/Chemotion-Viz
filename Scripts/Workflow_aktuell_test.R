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
library(rcdk)
library(metfRag)


library(ggfortify)
d <- iris[1:4]
pca_res <- prcomp(d, scale. = TRUE)
mtcars

autoplot(pca_res, data = iris, colour = 'Species',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)


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
n_classes <- dplyr::count(df,class_dash)

# Ein erster Sunburst Plot
sunburst(data = data.frame(n_classes), legend = FALSE)

# Smiles to Mass
substance_mass <- sapply(smiles, smiles2mass)
df_mass <- data.frame(substance_mass)
mass <- data.frame(substance= row.names(df_mass), df_mass, row.names=NULL) 

# Plot MW 
ggplot(mass,aes(substance,substance_mass))+
  geom_col(color="darkblue")+
  labs(title="Substance histogram plot",x="Substance", y = "Substance MW(g/mol)")

g <- ggplot(mass,aes(substance_mass))+
  geom_histogram(binwidth=20,color="darkblue", fill="lightblue")+
  labs(title="Molar mass histogram plot",x="MW(g/mol)", y = "Count")

ggplotly(g)

#hierarische Clustering:
mols <- parse.smiles(smiles)
dummy <- mapply(set.property, mols, "Score", c(1:2343))
plotCluster(mols, h=0.2)
########################################################################################

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

# Schleife in der das Objekt der ersten 10 Substanzen angezeigt wird mit if-Bedingung
for (i in 1:10) {
  if( is.null(Classification_List[[i]]) == 'TRUE') { 
    print('Objekt ist leer')
  } else { 
    print(Classification_List[[i]])}
  }

# Schleife in der die class der ersten 10 Substanzen angezeigt wird mit if-Bedingung
for (i in 1:length(Classification_List[[i]])) {
  if( is.null(Classification_List[[i]]) == 'TRUE') { 
    print('Objekt ist leer')
  } else { 
    print(Classification_List[[i]]@classification[["Classification"]][3])}
}

for (i in 1:2343) {
  if( is.null(Classification_List[[i]]) == 'TRUE') { 
    print('Objekt ist leer')
  } else { 
    print(Classification_List[[i]]@classification[["Classification"]][3])}
}

# Test
classes <- vector("character", 2343)
for (i in 1:2343)) {
  if( is.null(Classification_List[[i]]) == 'TRUE') { 
    #print('Objekt ist leer')
  } else { 
    classes[i] <- Classification_List[[i]]@classification[["Classification"]][3]}
}
classes

classes <- list("character", 2343)
for (i in 1:length(Classification_List[[i]])) {
  if( is.null(Classification_List[[i]]) == 'TRUE') { 
    #print('Objekt ist leer')
  } else { 
    classes[i] <- Classification_List[[i]]@classification[["Classification"]][3]}
}
classes
