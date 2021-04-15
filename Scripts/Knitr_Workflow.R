# ---- packages ----
library(readr)
library(rinchi)
library(magrittr)
library(classyfireR)
library(plyr)
library(dplyr)
library(sunburstR)
library(Rcpp)
library(rcdk)
library(ggplot2)
library(plotly)
library(metfRag)

# ---- smiles ----
smiles <- read.csv("~/R/Chemotion/ChemotionViz/Data/sample_export_16.03.2021_8.12_noDup.csv")[ ,5]
#smiles <- read.csv("../Data/sample_export_16.03.2021_8.12_noDup.csv")[ ,5]
head(smiles,3)

# ---- smiles_to_InChiKey ---- 
inchikey <-sapply(smiles, get.inchi.key)

# ---- classification ----
Classification_List <- sapply(inchikey, get_classification)
head(Classification_List,3)

# ---- loop ----
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

# ---- n_substances ----
i

# ---- dataframe ---- 
df <- plyr::ldply(class, rbind)
df

# ---- formatting ----
df$classes <- gsub('; NA','', paste(df$"1",df$"2",df$"3", df$"4", df$"5", df$"6", df$"7",df$"8" ,sep = "; "))
df <-ddply(df,.(classes),summarize, count=length(classes) )
df <- df[-1, ] 
df$classes <- gsub('-','_',df$classes)
df$classes <- gsub(';','-',df$classes)

# ---- n_substances_class ----
sum(df$count)

# ---- sunburst ----
sunburst(df, legend = FALSE)

# ---- smiles2mass ----
smiles2mass <- function(SMILES){
  massfromformula <- parse.smiles(SMILES)[[1]]
  do.typing(massfromformula)
  do.aromaticity(massfromformula)
  convert.implicit.to.explicit(massfromformula)
  do.isotopes(massfromformula)
  mass <- get.exact.mass(massfromformula)
  return(mass)}
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