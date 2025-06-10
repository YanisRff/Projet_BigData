data <- read.csv("~/Documents/cours/A3/S6/Projets/Projet_BigData/vessel-total-clean.csv")
library(skimr)
library(dplyr)
View(data)

###Description du jeu de données
head(data)
str(data)
summary(data)

skim(data)

### Gestion des doublons
doublon <- function(data) {
  n_avant <- nrow(data)
  cat("Nombre de lignes avant suppression :", n_avant, "\n")
  
  doublons <- data %>%
    group_by(MMSI, BaseDateTime) %>%
    filter(n() > 1) %>%
    arrange(MMSI, BaseDateTime)
  
  cat("Nombre de doublons trouvés :", nrow(doublons), "\n")
  
  data_unique <- data %>%
    distinct(MMSI, BaseDateTime, .keep_all = TRUE)
  
  n_apres <- nrow(data_unique)
  cat("Nombre de lignes après suppression :", n_apres, "\n")

  return(data_unique)
}

data_clean <- doublon(data = data)

###Gestion des valeurs manquantes
supprime_lignes_incompletes <- function(vessel.total.clean) {
  vessel.total.clean <- as.data.frame(lapply(vessel.total.clean, function(col) {
    if (is.character(col)) {
      # Remplace les cellules contenant \N ou \n par NA
      col[grepl("\\\\N", col) | grepl("\n", col)] <- NA  
    }
    return(col)
  }))
  print(rowSums(is.na(vessel.total.clean)))  # Affiche le nombre de NA par ligne
  data_sans_na <- vessel.total.clean[complete.cases(vessel.total.clean), ]  # Supprime les lignes avec NA
  return(data_sans_na)
}
result <- supprime_lignes_incompletes(vessel.total.clean)
vessel.total.clean

###Gestion des aberrations

