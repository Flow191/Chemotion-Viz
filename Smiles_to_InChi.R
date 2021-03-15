## Takes longer, but guarantees a result that is as long
## as the input, not guaranteed with previous version
smiles2inchikeys <- function(smiles) {
  if (length(smiles)<1) {
    warning("Empty smiles list")
    return (character(0))
  }
  
  inchikeys <- sapply(smiles, function(smile) {
    filename=tempfile()
    cat(smile, file=filename)
    
    con <- pipe(paste('obabel -i smi ',filename ,' -o inchi -xK  -O /dev/stdout 2>/dev/null | tr -d "\\t" ', sep=""))
    
    inchikey <- readLines(con)
    close(con)
    
    system(paste("rm ", filename))
    ifelse(length(inchikey)>0, inchikey, "") 
  })
  inchikeys
}

smiles2inchikeys("C")