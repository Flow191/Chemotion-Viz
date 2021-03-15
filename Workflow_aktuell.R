library(magrittr)
library(classyfireR)
library(readr)
library(dplyr)

# Einlesen der SMILES aus CSV-Datei
smiles <- read.csv("Data/Samples_SMILES.csv")

# Smiles als Vektor ausgeben
smiles_vec <- pull(smiles)

# Funktion zur Umwandlung von Smiles zu InChiKey
smiles2inchikeys <- function(smiles) {
  if (length(smiles)<1) {
    warning("Empty smiles list")
    return (character(0))
  }
  
  inchikeys <- sapply(smiles, function(smile) {
    filename=tempfile()
    cat(smile, file=filename)
    
    con <- pipe(paste('obabel -i smi ',filename ,' -o inchi -xK  -O /dev/stdout 2>/dev/null | tr -d "\\t" ', sep=""))
    
    inchikey <- readLines(con)
    close(con)
    
    system(paste("rm ", filename))
    ifelse(length(inchikey)>0, inchikey, "") 
  })
  inchikeys
}

# Funktionsausgabe als Vektor
inchikey_vec <- smiles2inchikeys(smiles_vec)

# InChIKeys benutzen um zu klassifizieren
Classification_List <- purrr::map(inchikey_vec, get_classification)

#Zugriff auf die class der 1. Substanz
Classification_List[[8]]@classification[["Classification"]][3]
