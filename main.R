data <- read.csv("~/Documents/cours/A3/S6/Projets/Projet_BigData/vessel-total-clean.csv")
library(dplyr)
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

