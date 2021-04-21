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