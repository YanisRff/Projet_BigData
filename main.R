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



###Gestion des valeurs manquantes
nettoyer_données <- function(data) {

  #3 remplace les NA dans width, draft et cargo par la moyenne ou la mediane
  data$Width[is.na(data$Width)  & data$VesselType <60] <- med$moy_d_50
  data$Width[is.na(data$Width)  & data$VesselType >= 60 & data$VesselType <= 69] <- med$moy_d_60
  data$Width[is.na(data$Width)  & data$VesselType >= 70 & data$VesselType <= 79] <- med$moy_d_70
  data$Width[is.na(data$Width)  & data$VesselType >= 80 & data$VesselType <= 89] <- med$moy_d_80
  
  data$Draft[is.na(data$Draft)  & data$VesselType<60] <- med$moy_w_50
  data$Draft[is.na(data$Draft)  & data$VesselType >= 60 & data$VesselType <= 69] <- med$moy_w_60
  data$Draft[is.na(data$Draft)  & data$VesselType >= 70 & data$VesselType <= 79] <- med$moy_w_70
  data$Draft[is.na(data$Draft)  & data$VesselType >= 80 & data$VesselType <= 89] <- med$moy_w_80
  
  data$Cargo[is.na(data$Cargo)  & data$VesselType >= 60 & data$VesselType <= 69] <- med$med_cargo1
  data$Cargo[is.na(data$Cargo)  & data$VesselType >= 70 & data$VesselType <= 79] <- med$med_cargo2
  data$Cargo[is.na(data$Cargo)  & data$VesselType >= 80 & data$VesselType <= 89] <- med$med_cargo3
  
  return(data)
}


###Gestion des aberrations
n <- nrow(data)


med <- function(data) {
  # met en numerique
  data$Cargo <- as.numeric(as.character(data$Cargo))
  data$Width <- as.numeric(as.character(data$Width))
  data$Draft <- as.numeric(as.character(data$Draft))
  
  # mediane cargo selon type 
  data$Cargo[data$Cargo == 0 & data$VesselType >= 60 & data$VesselType <= 69] <- NA
  data$Cargo[data$Cargo == 99 & data$VesselType >= 60 & data$VesselType <= 69] <- NA
  med_cargo1 <- median(data$Cargo[data$VesselType >= 60 & data$VesselType <= 69], na.rm = TRUE)

  med_cargo2 <- median(data$Cargo[data$VesselType >= 70 & data$VesselType <= 79], na.rm = TRUE)
  
  med_cargo3 <- median(data$Cargo[data$VesselType >= 80 & data$VesselType <= 89], na.rm = TRUE)
  
  # moyenne width selon type 
  moy_w_50 <- mean(data$Width[data$VesselType < 60], na.rm = TRUE)
  moy_w_60 <- mean(data$Width[data$VesselType >= 60 & data$VesselType <= 69], na.rm = TRUE)
  moy_w_70 <- mean(data$Width[data$VesselType >= 70 & data$VesselType <= 79], na.rm = TRUE)
  moy_w_80 <- mean(data$Width[data$VesselType >= 80 & data$VesselType <= 89], na.rm = TRUE)
  
  # moyenne draft selon type 
  moy_d_50 <- mean(data$Draft[data$VesselType < 60], na.rm = TRUE)
  moy_d_60 <- mean(data$Draft[data$VesselType >= 60 & data$VesselType <= 69], na.rm = TRUE)
  moy_d_70 <- mean(data$Draft[data$VesselType >= 70 & data$VesselType <= 79], na.rm = TRUE)
  moy_d_80 <- mean(data$Draft[data$VesselType >= 80 & data$VesselType <= 89], na.rm = TRUE)
  
  
  list(
    med_cargo1 = med_cargo1,
    med_cargo2 = med_cargo2,
    med_cargo3 = med_cargo3,
    moy_w_50 = moy_w_50,
    moy_w_60 = moy_w_60,
    moy_w_70 = moy_w_70,
    moy_w_80 = moy_w_80,
    moy_d_50 = moy_d_50,
    moy_d_60 = moy_d_60,
    moy_d_70 = moy_d_70,
    moy_d_80 = moy_d_80
  )
}
 


val_aber <- function(data = data){
  n <- nrow(data)
  print(nrow(data))
  #si le beateau n'est pas dans le golf, si vitesse = 0, si cap (reel et ideal) supérieur à 360, on enleve
  #subset -> donne condition sur df, si condition pas respecté, donnee non copié, marche comme un filtre
  data_filtered <- subset(data, LAT>=20 & LAT<=30 & LON>=(-98) & LON<=(-78) & Heading<360 & Draft>=0.5  & Width>=3 & Length >=10 & SOG <36)
  for (i in 1:nrow(data_filtered)) {
    if (data_filtered$Heading[i] == 0 | data_filtered$SOG[i] == 0 | data_filtered$COG[i] == 0) {
      data_filtered$Heading[i] <- 0
      data_filtered$SOG[i] <- 0
      data_filtered$COG[i] <- 0
      
    }}
  return(data_filtered)
  }


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
    ville_lat <- villes$lat[i]
    ville_long <- villes$long[i]
    
    traj %>%
      filter(abs(LAT - ville_lat) <= radius, abs(LON - ville_long) <= radius) %>%
      distinct(MMSI) %>%     # on garde un seul passage par bateau
      nrow()
  })
  return(villes)
}


# UI
ui <- fluidPage(
  titlePanel("Trajectoires des bateaux"),
  sidebarLayout(
    sidebarPanel(
      selectInput("mmsi", "Sélectionnez un bateau :", 
                  choices = {
                    noms_mmsi <- sorted_data %>%
                      select(VesselName, MMSI) %>%
                      distinct() %>%
                      arrange(VesselName)   # sort alphabetically by VesselName
                    
                    c("Tous les bateaux" = "all", setNames(noms_mmsi$MMSI, noms_mmsi$VesselName))
                  }),
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


### TEST
data[data == "\\N"]<-NA
View(data)
print(nrow(data))
doublons<-doublon(data = data)
aber <- val_aber(doublons)
print(nrow(aber))
View(aber)
print(med(aber))
med <- med(aber)
nettoy <- nettoyer_données(aber)
print(nrow(nettoy))


shinyApp(ui = ui, server = server)

