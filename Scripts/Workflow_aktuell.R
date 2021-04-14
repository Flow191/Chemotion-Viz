library(readr)
library(rinchi)
library(magrittr)
library(classyfireR)
library(plyr)
library(dplyr)
library(sunburstR)
library(Rcpp)
library(RMassBank)
library(ggplot2)
library(plotly)
library(metfRag)

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

# Schleife in der die classes aller Substanzen als Liste angezeigt wird
class <- vector("character", length(Classification_List[]))
for (i in 1:length(Classification_List[])) {
  if( is.null(Classification_List[[i]]) == 'TRUE') { 
  } 
  else if (is.null(Classification_List[[i]]@classification[["Classification"]][1]) == 'TRUE'){
  }
  else { 
    class[i] <-as.data.frame(Classification_List[[i]]@classification[["Classification"]])
  }
}

# Klassifikationen der Substanzen als Datenframe:
df <- plyr::ldply(class, rbind)

# Zeigt wie viele Substancen bis bestimmten Level klassifiziert wurden:
# substances_per_class<- df[complete.cases(df$`7`), ]

# Formatierung des Datenframes für den Sunburst-Plot:
df$classes <- gsub('; NA','', paste(df$"1",df$"2",df$"3", df$"4", df$"5", df$"6", df$"7",df$"8" ,sep = "; "))
#df <- df[df$classes!='NA',]
df <-ddply(df,.(classes),summarize, count=length(classes) )
df <- df[-1, ] 
df$classes <- gsub('-','_',df$classes)
df$classes <- gsub(';','-',df$classes)

# Sunburst Plot der Substanzklassen:
sunburst(data = df, legend = FALSE)

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

# Hierarisches Clustering
mols <- parse.smiles(smiles)
dummy <- mapply(set.property, mols, "Score", c(1:2343))
vect <- sprintf("C%d", 1:2343)
dummy <- mapply(set.property, mols, "Identifier", c(vect))
plotCluster(mols, h=0.2)
