library(readr)
library(rinchi)
library(magrittr)
library(classyfireR)
library(plyr)
library(dplyr)
library(data.table)

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
different_class <- dt1[which(dt1$V1 != dt1$V2), ]



################################

dt1$V3 <- NULL
dt1[dt1$V1 == dt1$V2,"V3"] <- "h"
df[df$x1 < df$x2,"winner"] <- "d"




u <- rbind(dt1,dt2)
new_dataset <- dt1 %>% right_join(dt2,by=c("V1"))
dd <- merge(dt1$"V1",dt2$"V1",all= TRUE)

s <- all.equal(df_start_mats[,2],df_products[,2])
newdata <- setdiff(df_start_mats, df_products)
#dd <- df_start_mats[df_products[,2],2]


library(sqldf)
a1NotIna2 <- sqldf('SELECT * FROM df_start_mats$2 EXCEPT SELECT * FROM df_products$2')

library(compare)
h <- compare(df_start_mats$`2`,df_products$`2`,allowAll=TRUE)

library(arsenal)
tt <- summary(comparedf(df_start_mats, df_products))


################
d <- df %>%
  slice(which(row_number() %% 2 == 1))
d2 <- df %>%
  slice(which(row_number() %% 1))
d2 <- df %>% anti_join(., d,na_matches=c("never"))
