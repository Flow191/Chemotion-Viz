# ---- packages ----
library(readr)
library(rinchi)
library(magrittr)
library(classyfireR)
library(dplyr)
library(sunburstR)

# ---- smiles ----
smiles <- read.csv("~/R/Chemotion/Chemotion/Data/sample_export_16.03.2021_8.12_noDup.csv")[ ,5]

# ---- smiles_to_InChiKey ---- 
inchikey <-sapply(smiles, get.inchi.key)

# ---- classification ----
Classification_List <- purrr::map(inchikey, get_classification)

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
i

# ---- null ----
class[class==""] <- NA
class[!is.na(class)]

# ---- sort ----
class_sorted <- sort(class)
df <- data.frame(class_sorted)
(n_classes <- count(df,class_sorted))
sum(n_classes$n)

# ---- plot ----
sunburst(data = data.frame(n_classes), legend = FALSE)