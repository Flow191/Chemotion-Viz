library(classyfireR)
library(tidyverse)

# Einlesen der txt-Datei mit den InChiKeys
data <- read_table("Data/InChiKeys.txt")

# Keys als Vektor ausgeben
InChI_Keys <- pull(data)

# InChIKeys benutzen um zu klassifizieren
Classification_List <- purrr::map(InChI_Keys, get_classification)

#Zugriff auf die class der 1. Substanz
Classification_List[[1]]@classification[["Classification"]][3]


