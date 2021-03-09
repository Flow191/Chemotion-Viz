##### Skript um InChiKey zu erhalten#####

###Variante 1: Importieren der Smiles###

library(classyfireR,readxl,rjson)

# Datenimport der Smiles aus Excel
data1 <- read_excel("Data/Samples_onlySMILES.xlsx")

# Umwandlung in eine Liste von Vektoren
vector <- data1$`canonical smiles`

# Problem: Umwandlung von Vektorlisten in ein Zeilenvektor für die Query
#Input ist jetzt:
#[1] "NNc1ccc(cc1)Br.Cl"                                                                 
#[2] "CC1=Nc2c(C1(C)C)cc(cc2)/C=C/C(C(C(C(C(C(C(F)(F)F)(F)F)(F)F)(F)F)(F)F)(F)F)(F)F"    
#[3] "..."
#Input müsste sein:
#[1] "NNc1ccc(cc1)Br.Cl" "CC1=Nc2c(C1(C)C)cc(cc2)/C=C/C(C(C(C(C(C(C(F)(F)F)(F)F)(F)F)(F)F)(F)F)(F)F)(F)F"

# Erstellung der InChiKeys
Query <-
  submit_query(label = 'query_test',
               input = vector,
               type = 'STRUCTURE')

Query

# Klassifizierung der Substanzen
classification(Query)


###Variante 2: JSON-Datei einlesen###

# Datenimport der JSON-Datei
data2=fromJSON(file="Data/Klassifiziert.json")

# Auslesen der InChiKeys
data2[["entities"]][[1]][["inchikey"]]
data2[["entities"]][[2]][["inchikey"]]

# Problem:
# In der JSON Datei werden nur die erste 10 Substanzen angezeigt, in der Datei steht drin, dass es 112 Seiten gibt mit 1114 Elementen. Ich habe noch nicht herausgefunden, wie man auf die anderen Seiten kommt.
