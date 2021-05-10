library(readr)
library(magrittr)
library(classyfireR)
library(plyr)
library(dplyr)
library(data.table)
library(networkD3)

data <- read.csv("Data/reaction_export_30.04_0932.csv")
df <- data %>% dplyr::filter(!(type=="")) %>% 
  dplyr::filter(!(type=="3 solvent")) %>% dplyr::filter(!(type=="2 reactant"))

starting_mats <- function(value) {
  inds <- which(value == lead(value))  
  sort(unique(c(inds, inds + 1, inds +2)))
}
more_start_mats <- df %>% slice(starting_mats(type))
one_start_mat <- df %>% anti_join(., more_start_mats)
one_start_mats <- one_start_mat[-c(51:53,80:82,325:327), ] 

start_mats <- one_start_mats %>% 
  dplyr::filter(!(type=="4 product"))

nrow(start_mats)

products <- one_start_mats %>% 
  dplyr::filter(!(type=="1 starting mat"))


Classification_List_start_mats <- sapply(start_mats$InChI, get_classification)
Classification_List_products <- sapply(products$InChI, get_classification)

class <- vector("character", length(Classification_List_start_mats[]))
for (i in 1:length(Classification_List_start_mats[])) {
  if( is.null(Classification_List_start_mats[[i]]) == 'TRUE') { 
  } 
  else if (is.null(Classification_List_start_mats[[i]]@classification[["Classification"]][1]) == 'TRUE'){
  }
  else { 
    class[i] <-as.data.frame(Classification_List_start_mats[[i]]@classification[["Classification"]])
  }
}

class2 <- vector("character", length(Classification_List_products[]))
for (i in 1:length(Classification_List_products[])) {
  if( is.null(Classification_List_products[[i]]) == 'TRUE') { 
  } 
  else if (is.null(Classification_List_products[[i]]@classification[["Classification"]][1]) == 'TRUE'){
  }
  else { 
    class2[i] <-as.data.frame(Classification_List_products[[i]]@classification[["Classification"]])
  }
}

char_list <-lapply(class, as.character)
char_list2 <-lapply(class2, as.character)
df_start_mats <- plyr::ldply(char_list, rbind)
df_products <- plyr::ldply(char_list2, rbind)
sum(df_products$"1"!="")

dt1 <- setDF(data.table(df_start_mats[,2]))
dt2 <- setDF(data.table(df_products[,2]))
dt1$V2 <- dt2$V1

same_class <- dt1[which(dt1$V1 == dt1$V2), ]
same_classes <-ddply(same_class,.(V1,V2),summarize, size=length(V1) )
sum(same_classes$size)

different_class <- dt1[which(dt1$V1 != dt1$V2), ]
different_classes <-ddply(different_class,.(V1,V2),summarize, size=length(V1) )
sum(different_classes$size)

#save.image(file = "reactions.RData") 
#load("../ChemotionViz/reactions.RData")

colnames(different_classes) <- c("source", "target", "value")
different_classes$target <- paste0(different_classes$target, '.')

# From these flows we need to create a node data frame: it lists every entities involved in the flow

nodes <- data.frame(
  name=c(as.character(different_classes$source), as.character(different_classes$target)) %>% unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
different_classes$IDsource <- match(different_classes$source, nodes$name)-1
different_classes$IDtarget <- match(different_classes$target, nodes$name)-1


ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'
# Make the Network
p <- sankeyNetwork(Links = different_classes, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name",sinksRight=FALSE,colourScale=ColourScal,nodeWidth=40,fontSize=13)
p

############
#rownames(different_classes)[rownames(different_classes) == "Benzenoids"] = "Benzenoids_"
#different_classes <- subset(different_classes, source %in% c("Benzenoids_"))
#different_classes$IDtarget <- c(1:length(different_classes$target))

fig <- plot_ly(
  type = "sankey",
  orientation = "h",
  
  node = list(
    label = c("Benzenoids",different_classes$target),
    color = c("#7fc97f","#beaed4","#fdc086","#ffff99","#386cb0","#f0027f","#bf5b17","#666666"),
    pad = 15,
    thickness = 20,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  
  link = list(
    source = different_classes$IDsource,
    target = different_classes$IDtarget,
    value =  different_classes$value
  )
)
fig <- fig %>% layout(
  title = "Basic Sankey Diagram",
  font = list(
    size = 10
  )
)

fig
###########
