data <- read.csv("~/Documents/Cours_A3/projet_a3_bd/Projet_BigData/vessel-total-clean.csv")
library(dplyr)
View(data)
data <- vessel.total.clean
data[data == "\\N"]<- NA
View(data)






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
n <- nrow(data)
#Mediane des cargo


#mediane des cargo selon les type de bateau
data$Cargo <- as.numeric(data$Cargo)
data$Cargo[data$Cargo == 0 & data$VesselType >=60 & data$VesselType <=69]<- NA
data$Cargo[data$Cargo == 99 & data$VesselType >=60 & data$VesselType <=69]<- NA
median(data$Cargo[data$VesselType>=60 & data$VesselType<=69],na.rm=TRUE)

data$Cargo[data$Cargo == 0 & data$VesselType >=80 & data$VesselType<=89]<- NA
data$Cargo[data$Cargo == 99 & data$VesselType >=80 & data$VesselType<=89]<- NA
median(data$Cargo[data$VesselType>=80 & data$VesselType<=89],na.rm=TRUE)

#mediane des width

data$Width <- as.numeric(data$Width)
mean(data$Width[data$Width<60],na.rm=TRUE)

mean(data$Width[data$Width>=60 & data$Width<=69],na.rm=TRUE)

mean(data$Width[data$Width>=70 & 79],na.rm=TRUE)

mean(data$Width[data$Width>=80 & 89],na.rm=TRUE)

#mediane des draft

data$Draft <- as.numeric(data$Draft)
mean(data$Draft[data$Draft<60],na.rm=TRUE)

mean(data$Draft[data$Draft>=60 & data$Draft<=69],na.rm=TRUE)

mean(data$Draft[data$Draft>=70 & 79],na.rm=TRUE)

mean(data$Draft[data$Draft>=80 & 89],na.rm=TRUE)

val_aber <- function(data = data){
  n <- nrow(data)
  print(nrow(data))
  #si le beateau n'est pas dans le golf, si vitesse = 0, si cap (reel et ideal) supérieur à 360, on enleve
  #subset -> donne condition sur df, si condition pas respecté, donnee non copié, marche comme un filtre
  data_filtered <- subset(data, LAT>=20 & LAT<=30 & LON>=(-98) & LON<=(-78) & Heading<360 & Draft>=0.5  & Width>=3 & Length >=10 & SOG <36)
  data_filtered[data_filtered$Heading == 0 | data_filtered$SOG == 0 | data_filtered$COG == 0]<- 0
  print(nrow(data_filtered))}
val_aber(data)
print(nrow(data_filtered))


