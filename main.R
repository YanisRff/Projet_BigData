install.packages(c("skimr","dplyr","ggplot2", "shiny", "maps"))
library(skimr)
library(dplyr)
library(ggplot2)
library(shiny)
library(maps)

data <- read.csv("~/Documents/cours/A3/S6/Projets/Projet_BigData/vessel-total-clean.csv")
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