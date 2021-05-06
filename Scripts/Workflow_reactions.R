library(readr)
library(rinchi)
library(magrittr)
library(classyfireR)
library(plyr)
library(dplyr)
library(data.table)
library(sunburstR)

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

start_mats <- one_start_mats %>% dplyr::filter(!(type=="")) %>% 
  dplyr::filter(!(type=="3 solvent")) %>% dplyr::filter(!(type=="2 reactant")) %>% 
  dplyr::filter(!(type=="4 product"))
#start_mats[5]
write.csv(products[5],"Data/test2.csv", row.names = FALSE)

products <- one_start_mats %>% dplyr::filter(!(type=="")) %>% 
  dplyr::filter(!(type=="3 solvent")) %>% dplyr::filter(!(type=="2 reactant")) %>% 
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

dt1 <- setDF(data.table(df_start_mats[,2]))
dt2 <- setDF(data.table(df_products[,2]))
dt1$V2 <- dt2$V1

same_class <- dt1[which(dt1$V1 == dt1$V2), ]
same_classes <-ddply(same_class,.(V1,V2),summarize, size=length(V1) )

different_class <- dt1[which(dt1$V1 != dt1$V2), ]
different_classes <-ddply(different_class,.(V1,V2),summarize, size=length(V1) )

