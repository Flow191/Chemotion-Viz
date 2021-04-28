smiles2mass <- function(SMILES){
  massfromformula <- parse.smiles(SMILES)[[1]]
  do.typing(massfromformula)
  do.aromaticity(massfromformula)
  convert.implicit.to.explicit(massfromformula)
  do.isotopes(massfromformula)
  mass <- get.exact.mass(massfromformula)
  return(mass)}

classyfire2df <- function() {
for (i in 1:length(Classification_List[])) {
  if( is.null(Classification_List[[i]]) == 'TRUE') { 
  } 
  else if (is.null(Classification_List[[i]]@classification[["Classification"]][1]) == 'TRUE'){
  }
  else { 
    class[i] <-as.data.frame(Classification_List[[i]]@classification[["Classification"]])
  }
}
char_list <-lapply(class, as.character)
df <- plyr::ldply(char_list, rbind)
return(df)}

tm_func <- function() {
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
  char_list <-lapply(class, as.character)
  df <- plyr::ldply(char_list, rbind)
  df_table <- as.data.table(df)
  df_table[,size := .N,by=c("1","2","3","4","5","6","7","8","9")]
  duplicated(df_table)
  df_df <- as.data.frame(df_table[!duplicated(df_table), ])
  df_tm <- df_df[-c(131), ]
  return(df_tm)
}

bar_func <- function() {
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
  char_list <-lapply(class, as.character)
  df <- plyr::ldply(char_list, rbind)
  col_names <- rename(df, kingdom=1,superclass=2, class=3, subclass=4, level5=5,level6=6, level7=7,level8=8, level9=9)
  df <- col_names %>% dplyr::filter(!(kingdom==""))
  df <- df %>% dplyr::filter(!(kingdom=="Inorganic compounds"))
  
  df$classes <- gsub('; NA','', paste(df$"superclass", sep = "; "))
  df <-ddply(df,.(classes),summarize, size=length(classes) )
  return(df)
}