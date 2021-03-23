# ---- packages ----
library(readr)
library(rinchi)
library(magrittr)
library(classyfireR)
library(dplyr)
library(sunburstR)
library(Rcpp)
library(RMassBank)

# ---- smiles ----
smiles <- read.csv("~/R/Chemotion/Chemotion/Data/sample_export_16.03.2021_8.12_noDup.csv")[ ,5]
head(smiles,3)

# ---- smiles_to_InChiKey ---- 
inchikey <-sapply(smiles, get.inchi.key)

# ---- classification ----
Classification_List <- purrr::map(inchikey, get_classification)
head(Classification_List,3)

# ---- level ----
level=3

# ---- loop ----
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

# ---- n_substances ----
i

# ---- null ----
class[class==""] <- NA
class[!is.na(class)]

# ---- sort ----
class_sorted <- sort(class)
df <- data.frame(class_sorted)
(n_classes <- count(df,class_sorted))

# ---- n_substances_class ----
sum(n_classes$n)

# ---- plot ----
sunburst(data = data.frame(n_classes), legend = FALSE)

# ---- smiles2mass ----
mass_func <- sapply(smiles, smiles2mass)
df_mass <- data.frame(as.list(mass_func))
substance_mass <- t(df_mass)
mass <- data.frame(substance= row.names(substance_mass), substance_mass, row.names=NULL)
head(mass,3)