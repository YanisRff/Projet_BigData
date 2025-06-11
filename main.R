install.packages(c("skimr","dplyr","ggplot2", "shiny", "maps","tibble","purrr"))
library(skimr)
library(dplyr)
library(ggplot2)
library(shiny)
library(maps)
library(tibble)
library(purrr)

data <- read.csv("~/Documents/cours/A3/S6/Projets/Projet_BigData/vessel-total-clean.csv")
data <- vessel.total.clean
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
  data <- data[!is.na(data$IMO) & !is.na(data$CallSign), ]

  #3 remplace les NA dans width, draft et cargo par la moyenne ou la mediane
  if(methode == "moy"){
    if("width" %in% names(data)){
      data$Width <- as.numeric(data$Width)
      data$Width[is.na(data$Width)] <- mean(data$Width, na.rm = TRUE)
    }
    if("draft" %in% names(data)){
      data$Draft <- as.numeric(data$Draft)
      data$Draft[is.na(data$Draft)] <- mean(data$Draft, na.rm = TRUE)
    }
    if("cargo" %in% names(data)){
      data$Cargo <- as.numeric(data$Cargo)
      data$Cargo[is.na(data$Cargo)] <- 70
    }
    med_cargo <- median(data$Cargo[!is.na(data$Cargo) & data$Cargo != 0 & data$Cargo !=99], na.rm = TRUE)
    #1er remplacement
    condition1 <- data$VesselType <=60 &(is.na(data$Cargo) | data$Cargo %in% c(0, 99))
    data$Cargo[condition1] <- med_cargo

    #2eme condition
    condition2<- data$VesselType > 61 & is.na(data$Cargo)
    data$Cargo[condition2] <- med_cargo
  }else{
    stop("la methode doit etre 'moy' (moyenne) ")
  }
  data
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

ui <- fluidPage(
  titlePanel("Trajectoires des bateaux"),
  sidebarLayout(
    sidebarPanel(
      selectInput("mmsi", "Sélectionnez un bateau :", 
                  choices = setNames(unique(sorted_data$MMSI), 
                                     unique(sorted_data$VesselName)))
    ),
    mainPanel(
      plotOutput("trajPlot")
    )
  )
)
server <- function(input, output) {
  output$trajPlot <- renderPlot({
    traj <- sorted_data %>% filter(MMSI == input$mmsi)
    
    lon_lim <- c(min(traj$LON) - 0.5, max(traj$LON) + 0.5) # Adjust the padding as needed
    lat_lim <- c(min(traj$LAT) - 0.5, max(traj$LAT) + 0.5) # Adjust the padding as needed

    ggplot() +
      geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "lightgray", color = "black") +
      geom_path(data = traj, aes(x = LON, y = LAT), color = "blue", size = 1) +
      geom_point(data = traj, aes(x = LON, y = LAT), color = "red") +
      coord_quickmap(xlim = lon_lim, ylim = lat_lim) + # This will zoom to the specified limits
      labs(title = paste("Trajectoire de", unique(traj$VesselName)),
           x = "Longitude", y = "Latitude") +
      theme_minimal()
  })
}
shinyApp(ui = ui, server = server)

## histogramme par type de bateaux
top_n <-10
plot_data <- data_nettoyer %>%
      group_by(VesselType) %>%
      summarise(count = n()) %>% # compte le nbre de bateau par type
      arrange(desc(count)) %>% # tri du + frequent au moins
      slice_head(n = top_n) # affiche les 1ere categorie avec le + grand nbre
    
ggplot(plot_data, aes(x = reorder(VesselType, -count), y= count)) + 
      geom_bar(stat = "identity", fill = "pink") + 
      labs(title = paste("Top", top_n, "types de bateaux"), x= "Type des bateau", y= "Nombre d'observations") +
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

## histogramme par affluence des ports
LON_ports <- c(-98.4951405, -95.3676974, -81.3790304, -82.458444, -80.19362, -82.3589631, -86.8425761, -89.6237402)
LAT_ports <- c(29.4246002, 29.7589382, 28.5421109, 27.9477595, 25.7741728, 23.135305, 21.1527467, 20.9670759)

# Crée un tableau des ports
ports <- tibble(LAT = LAT_ports, LON = LON_ports)

# Garder les lignes correspondant à ces coord avec tolerance de 0.05
expanded <- data_nettoyer %>%
  tidyr::crossing(ports, .name_repair = "unique")
colnames(expanded)


filtered_data <- expanded %>%
  filter(abs(LAT - LAT1) < 0.05 & abs(LON - LON1) < 0.05) %>%
  distinct(across(names(data_nettoyer)))


#compte le nbre de bateaux
plot_data <- filtered_data %>%
  group_by(LAT, LON) %>%
  summarise(nb_bateaux = n_distinct(MMSI), .groups = "drop") %>%
  arrange(desc(nb_bateaux)) 
  

ggplot(plot_data, aes(x = reorder(paste(LAT,LON), -nb_bateaux), y = nb_bateaux)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(
    title = "Ports les plus fréquentés(selon coord)", x= "Ports", y = "Nombre de bateaux uniques") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  


