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
library(data.table)
library(treemap)

# Einlesen der InChiKeys aus der txt-Datei
my_data <- read.table("Data/Inchi_massbank.txt")[,3]

# InChIKeys benutzen um zu klassifizieren
Classification_List <- sapply(my_data, get_classification)

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
char_list <-lapply(class, as.character)
df <- plyr::ldply(char_list, rbind)
save.image(file = "classification_massbank.RData") 

# Formatierung des Datenframes für den Sunburst-Plot:
col_names <- rename(df, kingdom=1,superclass=2, class=3, subclass=4, level5=5,level6=6, level7=7,level8=8, level9=9,level10=10)
df <- col_names %>% dplyr::filter(!(kingdom==""))
df$classes <- gsub('; NA','', paste(df$"kingdom",df$"superclass",df$"class", df$"subclass", df$"level5", df$"level6", df$"level7",df$"level8" ,df$"level9",df$"level10", sep = "; "))
df <-ddply(df,.(classes),summarize, size=length(classes) )

# Sunburst Plot der Substanzklassen:
d3_tree <- sunburstR:::csv_to_hier(df,delim = ";")
sunburst(d3_tree, legend = FALSE,colors=c("#ff4040","#ff423d","#ff453a","#ff4838","#fe4b35","#fe4e33","#fe5130","#fd542e","#fd572b","#fc5a29","#fb5d27","#fa6025","#f96322","#f96620","#f7691e","#f66c1c","#f56f1a","#f47218","#f37517","#f17815","#f07c13","#ee7f11","#ed8210","#eb850e","#e9880d","#e88b0c","#e68e0a","#e49209","#e29508","#e09807","#de9b06","#dc9e05","#d9a104","#d7a403","#d5a703","#d2aa02","#d0ad02","#ceb001","#cbb301","#c9b600","#c6b800","#c3bb00","#c1be00","#bec100","#bbc300","#b8c600","#b6c900","#b3cb01","#b0ce01","#add002","#aad202","#a7d503","#a4d703","#a1d904","#9edc05","#9bde06","#98e007","#95e208","#92e409","#8ee60a","#8be80c","#88e90d","#85eb0e","#82ed10","#7fee11","#7cf013","#78f115","#75f317","#72f418","#6ff51a","#6cf61c","#69f71e","#66f920","#63f922","#60fa25","#5dfb27","#5afc29","#57fd2b","#54fd2e","#51fe30","#4efe33","#4bfe35","#48ff38","#45ff3a","#42ff3d","#40ff40","#3dff42","#3aff45","#38ff48","#35fe4b","#33fe4e","#30fe51","#2efd54","#2bfd57","#29fc5a","#27fb5d","#25fa60","#22f963","#20f966","#1ef769","#1cf66c","#1af56f","#18f472","#17f375","#15f178","#13f07c","#11ee7f","#10ed82","#0eeb85","#0de988","#0ce88b","#0ae68e","#09e492","#08e295","#07e098","#06de9b","#05dc9e","#04d9a1","#03d7a4","#03d5a7","#02d2aa","#02d0ad","#01ceb0","#01cbb3","#00c9b6","#00c6b8","#00c3bb","#00c1be","#00bec1","#00bbc3","#00b8c6","#00b6c9","#01b3cb","#01b0ce","#02add0","#02aad2","#03a7d5","#03a4d7","#04a1d9","#059edc","#069bde","#0798e0","#0895e2","#0992e4","#0a8ee6","#0c8be8","#0d88e9","#0e85eb","#1082ed","#117fee","#137cf0","#1578f1","#1775f3","#1872f4","#1a6ff5","#1c6cf6","#1e69f7","#2066f9","#2263f9","#2560fa","#275dfb","#295afc","#2b57fd","#2e54fd","#3051fe","#334efe","#354bfe","#3848ff","#3a45ff","#3d42ff","#4040ff","#423dff","#453aff","#4838ff","#4b35fe","#4e33fe","#5130fe","#542efd","#572bfd","#5a29fc","#5d27fb","#6025fa","#6322f9","#6620f9","#691ef7","#6c1cf6","#6f1af5","#7218f4","#7517f3","#7815f1","#7c13f0","#7f11ee","#8210ed","#850eeb","#880de9","#8b0ce8","#8e0ae6","#9209e4","#9508e2","#9807e0","#9b06de","#9e05dc","#a104d9","#a403d7","#a703d5","#aa02d2","#ad02d0","#b001ce","#b301cb","#b600c9","#b800c6","#bb00c3","#be00c1","#c100be","#c300bb","#c600b8","#c900b6","#cb01b3","#ce01b0","#d002ad","#d202aa","#d503a7","#d703a4","#d904a1","#dc059e","#de069b","#e00798","#e20895","#e40992","#e60a8e","#e80c8b","#e90d88","#eb0e85","#ed1082","#ee117f","#f0137c","#f11578","#f31775","#f41872","#f51a6f","#f61c6c","#f71e69","#f92066","#f92263","#fa2560","#fb275d","#fc295a","#fd2b57","#fd2e54","#fe3051","#fe334e","#fe354b","#ff3848","#ff3a45","#ff3d42","#ff4040"))


###################################

class2 <- vector("character", length(Classification_List[]))
for (i in 1:length(Classification_List[])) {
  if( is.null(Classification_List[[i]]) == 'TRUE') { 
  }
  else { 
    class2[i] <-as.data.frame(Classification_List[[i]]@meta[["smiles"]])
  }
}
df2 <- plyr::ldply(class2, rbind)
df3 <- as.data.frame(my_data)
df4 <- df3[df2[,1]=="",3]
dd <- as.data.frame(df4)
rr <- parse.inchi(df4)
write.table(dd,"unclass_substances_massbank.txt",sep="\t",row.names=FALSE)

Classification_Listy <- sapply(df4, get_classification)

# Klassifikationen der Substanzen als Datenframe:
df <- plyr::ldply(class, rbind)

#dfd <- dplyr::count(df, "1","2","3","4","5","6","7","8","9","10")
#data.frame(table(df$c("1")))
#library(ggplot2)
#library(plotly)
#fig <- plot_ly(df,type = 'sunburst')

# Zeigt wie viele Substancen bis bestimmten Level klassifiziert wurden:
# substances_per_class<- df[complete.cases(df$`7`), ]

# Formatierung des Datenframes für den Sunburst-Plot:
df$classes <- gsub('; NA','', paste(df$"1",df$"2",df$"3", df$"4", df$"5", df$"6", df$"7",df$"8" ,df$"9",df$"10", sep = "; "))
#df$classes <- gsub('; NA','', paste(df$"2",df$"3", df$"4", df$"5", df$"6", df$"7",df$"8" ,df$"9",df$"10", sep = "; "))
df <-ddply(df,.(classes),summarize, count=length(classes) )
df <- df[-1, ] 
df$classes <- gsub('-','_',df$classes)
df$classes <- gsub(';','-',df$classes)

#library(htmltools)
#library(d3r)
#tree <- d3_nest(df, value_cols = "count")
#tree

# Sunburst Plot der Substanzklassen:
sunburst(data = df, legend = FALSE)
# Sunburst Plot der Substanzklassen mit totalen Zahlen:
sunburst(data = df, count=TRUE, legend = FALSE)
sunburst(tree, valueField = "count", withD3 = TRUE)
#sund2b(df, width="100%")

inchikey <- sapply("ClCC(=O)N(C(COC)=C)C1=C(C=CC=C1C)CC", get.inchi.key)
