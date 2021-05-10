install.packages("devtools")
library("devtools")

## CRAN

install.packages(c("readr", "magrittr",
	"plyr", "dplyr", "sunburstR", "Rcpp", "ggplot2", "plotly", 
	"rmarkdown", "knitr", "remotes","rcdk","data.table","treemap","networkD3"))


## Bioc

#BiocManager::install("RMassBank")


## GitHub

install_github("CDK-R/rinchi")
install_github("aberHRML/classyfireR", build_vignettes = FALSE)
install_github("c-ruttkies/MetFragR/metfRag")
