install.packages(c("skimr","dplyr","ggplot2", "sf", "maps"))
library(skimr)
library(dplyr)
library(ggplot2)
library(sf)
library(maps)

data <- read.csv("~/Documents/cours/A3/S6/Projets/Projet_BigData/vessel-total-clean.csv")
View(data)
data <- vessel.total.clean
data[data == "\\N"]<- NA
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

nettoyer_données <- function(data, methode = "moy") {
  #1- remplace les \N ou \n par NA 
  data <- as.data.frame(lapply(data, function(col) {
    if (is.character(col)) {
      # Remplace les cellules contenant \N ou \n par NA
      col[grepl("\\\\N", col) | grepl("\n", col)] <- NA  
    }
    return(col)
  }))
  #2- supprime les NA dans IMO
  data <- data[!is.na(data$IMO) & !is.na(data$callsign), ]
  
  #3 remplace les NA dans width, draft et cargo par la moyenne ou la mediane
  if(methode == "moy"){
    if("width" %in% names(data)){
      data$width <- as.numeric(data$width)
      data$width[is.na(data$width)] <- mean(data$width, na.rm = TRUE)
    }
    if("draft" %in% names(data)){
      data$draft <- as.numeric(data$draft)
      data$draft[is.na(data$draft)] <- mean(data$draft, na.rm = TRUE)
    }
    if("cargo" %in% names(data)){
      data$cargo <- as.numeric(data$cargo)
      data$cargo[is.na(data$cargo)] <- 70
    }
    med_cargo <- median(data$cargo[!is.na(data$cargo) & data$cargo != 0 & data$cargo !=99, na.rm = TRUE])
    #1er remplacement
    condition1 <- data$vesseltype <=60 &(is.na(data$cargo) | data$cargo %in% c(0, 99))
    data$cargo[condition1] <- med_cargo
    
    #2eme condition
    condition2<- data$vesseltype > 61 & is.na(data$cargo)
    data$cargo[condition2] <- med_cargo
  }else{
    stop("la methode doit etre 'moy' (moyenne) ")
  }
  return(data_nettoyer)
}
data_nettoyer <- nettoyer_données(data)


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




###Affichage graphiques

###Affichage map
draw_traj <- function(data) {
  MMSI_list <- unique(data$MMSI)
  MMSI_vect <- unlist(MMSI_list)
  data_new <- subset(data, MMSI == MMSI_vect[1])
  
  for(i in 2:n){
    
  }
}
print(draw_traj(data = data))
