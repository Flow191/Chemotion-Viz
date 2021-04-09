# ---- packages ----
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
library(metfRag)

# ---- smiles ----
smiles <- read.csv("~/R/Chemotion/ChemotionViz/Data/sample_export_16.03.2021_8.12_noDup.csv")[ ,5]
head(smiles,3)

# ---- smiles_to_InChiKey ---- 
inchikey <-sapply(smiles, get.inchi.key)

# ---- classification ----
Classification_List <- sapply(inchikey, get_classification)
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
class_dash <- gsub("-", "_", class_sorted)
df <- data.frame(class_dash)
(n_classes <- dplyr::count(df,class_dash))

# ---- n_substances_class ----
sum(n_classes$n)

# ---- plot ----
sunburst(data = data.frame(n_classes), legend = FALSE)

# ---- smiles2mass ----
substance_mass <- sapply(smiles, smiles2mass)
df_mass <- data.frame(substance_mass)
mass <- data.frame(substance= row.names(df_mass), df_mass, row.names=NULL)
head(mass,3)

#---- plot2 ----
ggplot(mass,aes(substance,substance_mass))+
  geom_col(color="darkblue")+
  labs(title="Substances vs Molar mass",x="Substance", y = "Exact mass (g/mol)")

histogram <- ggplot(mass,aes(substance_mass))+
  geom_histogram(binwidth=20,color="darkblue", fill="lightblue")+
  labs(title="Histogram plot: Molar mass vs count",x="Exact mass (g/mol)", y = "Count")

ggplotly(histogram)

#---- clustering ----
mols <- parse.smiles(smiles)
dummy <- mapply(set.property, mols, "Score", c(1:2343))
plotCluster(mols, h=0.2)