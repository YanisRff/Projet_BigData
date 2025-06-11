install.packages(c("skimr","dplyr","ggplot2", "shiny", "maps", "viridis"))
library(skimr)
library(dplyr)
library(ggplot2)
library(shiny)
library(maps)
library(viridis)

data <- read.csv("~/Documents/cours/A3/S6/Projets/Projet_BigData/vessel-total-clean.csv")
data[data == "\\N"]<-NA
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
      col[grepl("\\N", col) | grepl("\n", col)] <- NA
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
sorted_data <- data %>% arrange(desc(BaseDateTime))

world <- map_data("world")

villes <- data.frame(
  nom = c("San Antonio", "Houston", "Orlando", "Tampa", "Miami", "Havana", "Cancun", "Mérida"),
  long = c(-98.4951405, -95.3676974, -81.3790304, -82.458444, -80.19362, -82.3589631, -86.8425761, -89.6237402),
  lat = c(29.4246002, 29.7589382, 28.5421109, 27.9477595, 25.7741728, 23.135305, 21.1527467, 20.9670759)
)

get_ville_counts <- function(traj, villes, radius = 1) {
  villes$nb_passages <- sapply(1:nrow(villes), function(i) {
    sum(
      abs(traj$LAT - villes$lat[i]) <= radius &
        abs(traj$LON - villes$long[i]) <= radius
    )
  })
  return(villes)
}

# UI
ui <- fluidPage(
  titlePanel("Trajectoires des bateaux"),
  sidebarLayout(
    sidebarPanel(
      selectInput("mmsi", "Sélectionnez un bateau :", 
                  choices = c("Tous les bateaux" = "all", 
                              setNames(unique(sorted_data$MMSI), 
                                       unique(sorted_data$VesselName)))),
      radioButtons("carte_mode", "Type de carte :",
                   choices = c("Dynamique" = "dynamic", "Fixe" = "fixed"),
                   selected = "fixed"),
      radioButtons("show_hotspots", "Afficher les points chauds :", 
                   choices = c("Oui" = "yes", "Non" = "no"), 
                   selected = "yes")
    ),
    mainPanel(
      plotOutput("trajPlot")
    )
  )
)

# Server
server <- function(input, output) {
  output$trajPlot <- renderPlot({
    traj <- if (input$mmsi == "all") sorted_data else sorted_data %>% filter(MMSI == input$mmsi)
    
    if (input$carte_mode == "dynamic") {
      lon_lim <- c(min(traj$LON) - 0.5, max(traj$LON) + 0.5)
      lat_lim <- c(min(traj$LAT) - 0.5, max(traj$LAT) + 0.5)
    } else {
      lon_lim <- c(-98, -78)
      lat_lim <- c(20, 30)
    }
    
    villes_compte <- get_ville_counts(traj, villes)
    print(villes_compte)
    
    p <- ggplot() +
      geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "lightgray", color = "black") +
      geom_line(data = traj, aes(x = LON, y = LAT, group = MMSI, color = factor(MMSI)), size = 0.7) + 
      scale_color_viridis_d(option = "plasma") +
      geom_point(data = traj, aes(x = LON, y = LAT), color = "red", alpha = 0.5) +
      coord_quickmap(xlim = lon_lim, ylim = lat_lim) +
      labs(title = ifelse(input$mmsi == "all", 
                          "Trajectoires de tous les bateaux", 
                          paste("Trajectoire de", unique(traj$VesselName))),
           x = "Longitude", y = "Latitude", size = "Passages", color = "Passages") +
      theme_minimal() +
      guides(color = guide_legend(nrow = 3, byrow = TRUE))+ 
      theme(legend.position = "bottom", legend.box = "horizontal")
    
    if (input$show_hotspots == "yes") {
      print(villes_compte)
      p <- p +
        geom_point(data = villes_compte, aes(x = long, y = lat, size = nb_passages), shape = 21, fill = "orange") +
        geom_text(data = villes_compte, aes(x = long, y = lat, label = nom), nudge_y = 0.5, size = 3, fontface = "bold") +
        geom_text(data = villes_compte, aes(x = long, y = lat, size = nb_passages*0.0001, label = nb_passages), nudge_y = 0, size = 3, fontface = "bold") +
        scale_size_continuous(range = c(6, 14))
    }
    p
  })
}

shinyApp(ui = ui, server = server)
