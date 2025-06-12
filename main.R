install.packages(c("skimr","dplyr","ggplot2", "shiny", "maps","tibble","purrr", "viridis", "corrplot", "readr", "tidymodels", "glmnet"))

###Init
init_data <- function(){
  library(skimr)
  library(dplyr)
  library(ggplot2)
  library(shiny)
  library(maps)
  library(tibble)
  library(purrr)
  library(viridis)
  library(corrplot)
  library(readr)
  library(tidymodels)
  library(glmnet)
  
  
  data <- read.csv("~/Documents/cours/A3/S6/Projets/Projet_BigData/vessel-total-clean.csv")
  data[data == "\\N"]<-NA
  
  return(data)
}
init_villes <- function(){
  villes <- data.frame(
    nom = c("San Antonio", "Houston", "Orlando", "Tampa", "Miami", "Havana", "Cancun", "Mérida"),
    long = c(-98.4951405, -95.3676974, -81.3790304, -82.458444, -80.19362, -82.3589631, -86.8425761, -89.6237402),
    lat = c(29.4246002, 29.7589382, 28.5421109, 27.9477595, 25.7741728, 23.135305, 21.1527467, 20.9670759)
  )
  return(villes)
}

###Description du jeu de données
data_description <- function(data){
  head(data)
  str(data)
  summary(data)
  
  skim(data)
}

###Nettoyage des données
##Gestion des doublons
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

##Gestion des valeurs manquantes
nettoyer_donnees <- function(data) {

  #3 remplace les NA dans width, draft et cargo par la moyenne ou la mediane
  data$Width[is.na(data$Width)  & data$VesselType >= 60 & data$VesselType <= 69] <- med$moy_d_60
  data$Width[is.na(data$Width)  & data$VesselType >= 70 & data$VesselType <= 79] <- med$moy_d_70
  data$Width[is.na(data$Width)  & data$VesselType >= 80 & data$VesselType <= 89] <- med$moy_d_80
  
  data$Draft[is.na(data$Draft)  & data$VesselType >= 60 & data$VesselType <= 69] <- med$moy_w_60
  data$Draft[is.na(data$Draft)  & data$VesselType >= 70 & data$VesselType <= 79] <- med$moy_w_70
  data$Draft[is.na(data$Draft)  & data$VesselType >= 80 & data$VesselType <= 89] <- med$moy_w_80
  
  data$Cargo[is.na(data$Cargo)  & data$VesselType >= 60 & data$VesselType <= 69] <- med$med_cargo1
  data$Cargo[is.na(data$Cargo)  & data$VesselType >= 70 & data$VesselType <= 79] <- med$med_cargo2
  data$Cargo[is.na(data$Cargo)  & data$VesselType >= 80 & data$VesselType <= 89] <- med$med_cargo3
  
  return(data)
}

##Gestion des aberrations
med <- function(data) {
  # met en numerique
  data$Cargo <- as.numeric(as.character(data$Cargo))
  data$Width <- as.numeric(as.character(data$Width))
  data$Draft <- as.numeric(as.character(data$Draft))
  data$Length <- as.numeric(as.character(data$Length))
  
  
  # mediane cargo selon type 
  data$Cargo[data$Cargo == 0 & data$VesselType >= 60 & data$VesselType <= 69] <- NA
  data$Cargo[data$Cargo == 99 & data$VesselType >= 60 & data$VesselType <= 69] <- NA
  med_cargo1 <- median(data$Cargo[data$VesselType >= 60 & data$VesselType <= 69], na.rm = TRUE)

  med_cargo2 <- median(data$Cargo[data$VesselType >= 70 & data$VesselType <= 79], na.rm = TRUE)
  
  med_cargo3 <- median(data$Cargo[data$VesselType >= 80 & data$VesselType <= 89], na.rm = TRUE)
  
  # moyenne width selon type 
  moy_w_60 <- mean(data$Width[data$VesselType >= 60 & data$VesselType <= 69], na.rm = TRUE)
  moy_w_70 <- mean(data$Width[data$VesselType >= 70 & data$VesselType <= 79], na.rm = TRUE)
  moy_w_80 <- mean(data$Width[data$VesselType >= 80 & data$VesselType <= 89], na.rm = TRUE)
  
  # moyenne draft selon type 
  moy_d_60 <- mean(data$Draft[data$VesselType >= 60 & data$VesselType <= 69], na.rm = TRUE)
  moy_d_70 <- mean(data$Draft[data$VesselType >= 70 & data$VesselType <= 79], na.rm = TRUE)
  moy_d_80 <- mean(data$Draft[data$VesselType >= 80 & data$VesselType <= 89], na.rm = TRUE)
  
  #moyenne length selon type
  
  moy_l_60 <- mean(data$Length[data$VesselType>=60 & data$VesselType<=69], na.rm = TRUE)
  moy_l_70 <- mean(data$Length[data$VesselType>=70 & data$VesselType<=79], na.rm = TRUE)
  moy_l_80 <- mean(data$Length[data$VesselType>=80 & data$VesselType<=89], na.rm = TRUE)
  

  
  
  list(
    med_cargo1 = med_cargo1,
    med_cargo2 = med_cargo2,
    med_cargo3 = med_cargo3,
    moy_w_60 = moy_w_60,
    moy_w_70 = moy_w_70,
    moy_w_80 = moy_w_80,
    moy_d_60 = moy_d_60,
    moy_d_70 = moy_d_70,
    moy_d_80 = moy_d_80,
    moy_l_60 = moy_l_60,
    moy_l_70 = moy_l_70,
    moy_l_80 = moy_l_80
  )
}
val_aber <- function(data = data){
  n <- nrow(data)
  cat("il y a",nrow(data),"lignes avant le tri des val aberrantes","\n")
  
  #subset -> donne condition sur df, si condition pas respecté, donnee non copié, marche comme un filtre
  data_filtered <- subset(data,LAT >= 20 & LAT <= 30 & LON >= (-98) & LON <= (-78) & Draft>=0.5  & Width>=3 & Length >=10 & SOG <36)

  data_filtered$SOG[data_filtered$SOG == 0 | data_filtered$COG == 0 | data_filtered$Heading == 0]<-0
  data_filtered$COG[data_filtered$SOG == 0 | data_filtered$COG == 0 | data_filtered$Heading == 0]<-0
  data_filtered$Heading[data_filtered$SOG == 0 | data_filtered$COG == 0 | data_filtered$Heading == 0]<-0
  
  
  cat("il y a",nrow(data_filtered),"lignes apres le tri des val aberrantes")
  return(data_filtered)
}

###Affichage graphiques
##Top des bateaux
plot_top_vessel_types_by_range <- function(data_nettoyer, top_n = 3) {
  
  data <- data_nettoyer %>%
    mutate(VesselType_grouped = case_when(
      VesselType >= 60 & VesselType <= 69 ~ "passenger",
      VesselType >= 70 & VesselType <= 79 ~ "cargo",
      VesselType >= 80 & VesselType <= 89 ~ "tanker",
    ))
  
  plot_data <- data %>%
    group_by(VesselType_grouped) %>%
    summarise(count = n(), .groups = "drop") %>%
    arrange(desc(count)) %>%
    slice_head(n = top_n)
  
  ggplot(plot_data, aes(x = reorder(VesselType_grouped, -count), y = count, fill = VesselType_grouped)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("passenger" = "pink", "cargo" = "green", "tanker" = "yellow")) +
    labs(
      title = paste("Top", top_n, "types de bateaux regroupés par plage VesselType"),x = "Type de bateau",y = "Nombre d'observations") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
}

##Top des bateaux camembert
plot_camembert <- function(data_nettoyer){
  data <- data_nettoyer %>%
    mutate(VesselType_grouped = case_when(
      VesselType >= 60 & VesselType <= 69 ~ "passenger",
      VesselType >= 70 & VesselType <= 79 ~ "cargo",
      VesselType >= 80 & VesselType <= 89 ~ "tanker",
    ))
  camembert_data <- data%>%
    group_by(VesselType_grouped) %>%
    summarise(count = n(), .groups = "drop") %>%
    mutate(percentage = round(100 * count / sum(count), 1),
           label = paste0(VesselType_grouped, " (", percentage, "%)"))
  
  ggplot(camembert_data, aes(x = "", y = count, fill = VesselType_grouped)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    scale_fill_manual(values = c("passenger" = "pink", "cargo" = "green", "tanker" = "yellow")) +
    geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
    labs(title = "Répartition des types de bateaux (camembert)", x = "", y = "") +
    theme_void() + 
    theme(legend.position = "none")
  
}

##Affichage utilisation ports
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
afficher_ports_top <- function(data,villes){
  ports_count <- get_ville_counts(data, villes)
  print(ports_count)
  ggplot(ports_count, aes(x = reorder(nom,-nb_passages), y = nb_passages)) + 
    geom_bar(stat="identity", fill="steelblue")+
    geom_text(aes(label=nb_passages), vjust=1.6, color="white", size=3.5) + 
    labs(title = "Ports les plus utilisées", x = "Ports", y = "Affluence") +
    theme_minimal()
  
}

##histogrammes lien
histo_VesselType_mean <- function(med){
  
  #Histo VT and Width
  Width_y = c(med$moy_w_60, med$moy_w_70, med$moy_w_80)
  print(Width_y)
  ggplot(mapping =aes(x = reorder(c("Passager", "Cargo", "Tanker"),Width_y), y = Width_y)) + 
    geom_bar(stat="identity", fill="steelblue")+
    geom_text(aes(label=round(Width_y, digits = 1)), vjust=1.6, color="white", size=3.5)+
    labs(title="Bar Plot between VesselType and Width", x = "VesselType", y= "Width")+
    theme_minimal()
  
  #Histo VT and Draft
  Draft_y = c(med$moy_d_60, med$moy_d_70, med$moy_d_80)
  print(Draft_y)
  ggplot(mapping =aes(x = reorder(c("Passager", "Cargo", "Tanker"),Draft_y), y = Draft_y)) + 
    geom_bar(stat="identity", fill="steelblue")+
    geom_text(aes(label=round(Draft_y, digits = 1)), vjust=1.6, color="white", size=3.5)+
    labs(title="Bar Plot between VesselType and Draft", x = "VesselType", y= "Draft")+
    theme_minimal()
  
  #Histo VT and Length
  Length_y = c(med$moy_l_60, med$moy_l_70, med$moy_l_80)
  print(Length_y)
  ggplot(mapping =aes(x = reorder(c("Passager", "Cargo", "Tanker"),Length_y), y = Length_y)) + 
    geom_bar(stat="identity", fill="steelblue")+
    geom_text(aes(label=round(Length_y, digits = 1)), vjust=1.6, color="white", size=3.5)+
    labs(title="Bar Plot between VesselType and Length", x = "VesselType", y= "Length")+
    theme_minimal()
}

###Affichage map
map_init <- function(data, villes){
  sorted_data <- data %>% arrange(desc(BaseDateTime))
  
  world <- map_data("world")
  
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
  shinyApp(ui = ui, server = server)
}

###Etude des corrélations
##Matrice de corrélations
corr_mat <- function(data){
  df_corr <- data[, c("MMSI", "LAT", "LON", "SOG", "COG", "Heading", "VesselType", "Status", "Length", "Width", "Draft", "Cargo")]
  df_corr <- as.data.frame(lapply(df_corr, function(x) as.numeric(as.character(x))))
  df_corr_clean <- na.omit(df_corr)
  mat_corr <- cor(df_corr_clean)
  library(ggcorrplot)
  ggcorrplot(mat_corr, lab=TRUE,hc.order = TRUE,
             outline.color = "white",
             ggtheme = ggplot2::theme_gray,
             colors = c("#6D9EFF", "white", "#E46726"), lab_size = 3, title = "Matrice de corrélation")
}

##Regress Linear & Correlation
reduction <- function(data){
  data_reduit <-data[seq(1, nrow(data), by=1000),]
  return(data_reduit)
}
Regres <- function(data, data1, data2){
  ggplot(data, mapping = aes(x=data[[data1]], y=data[[data2]])) + geom_point(color = "steelblue", size=3.5)+
  labs(title=paste("scatter diagram :",data1,"and",data2), x =data1 , y= data2)
}

##Khi2
test_khi <- function(data, param){
  passagers <- data[data$VesselType >= 60 & data$VesselType <= 69, param]
  cargo <- data[data$VesselType >= 70 & data$VesselType <= 79, param]
  tanker <- data[data$VesselType >= 80 & data$VesselType <= 89, param]
  freq_passagers <- table(passagers)
  freq_cargo <- table(cargo)
  freq_tanker <- table(tanker)
  all_modalities <- sort(unique(c(names(freq_passagers), names(freq_cargo), names(freq_tanker))))
  contingency_table <- rbind(
    passagers = as.numeric(freq_passagers[all_modalities]),
    cargo     = as.numeric(freq_cargo[all_modalities]),
    tanker    = as.numeric(freq_tanker[all_modalities])
  )
  contingency_table[is.na(contingency_table)] <- 0
  colnames(contingency_table) <- all_modalities
  mosaicplot(contingency_table, main = paste(param, "& VesselType Mosaic Plot"),
             xlab = "VesselType",
             ylab = param,
             las = 1,
             shade = TRUE,
             off = 30)
  khi2_result <- chisq.test(contingency_table)
  return(khi2_result)
}

###Prediction VesselType
prediction <- function(pred){
  pred$VesselType = as.factor(pred$VesselType)
  set.seed(421)
  split <- initial_split(pred, prop = 0.8, strata = VesselType)
  train <- split %>% 
    training()
  test <- split %>% 
    testing()
  train_clean <- train %>%
    select(-id, -MMSI, -BaseDateTime, -VesselName, -IMO, -CallSign, -LAT, -LON, -TransceiverClass, -Status, -SOG, -COG, -Heading)
  
  train_clean$Length <- as.numeric(train_clean$Length)
  train_clean$Width <- as.numeric(train_clean$Width)
  train_clean$Draft <- as.numeric(train_clean$Draft)
  
  test$Length <- as.numeric(test$Length)
  test$Width <- as.numeric(test$Width)
  test$Draft <- as.numeric(test$Draft)
  
  table(train_clean$VesselType)
  model <- multinom_reg(mixture = double(1), penalty = double(1)) %>%
    set_engine("glmnet") %>%
    set_mode("classification") %>%
    fit(VesselType ~ ., data = train_clean)
  
  tidy(model)
  summary(model)
  # Class Predictions
  pred_class <- predict(model,
                        new_data = test,
                        type = "class")
  
  # Class Probabilities
  pred_proba <- predict(model,
                        new_data = test,
                        type = "prob")
  results <- test %>%
    select(VesselType) %>%
    bind_cols(pred_class, pred_proba)
  
  print(accuracy(results, truth = VesselType, estimate = .pred_class))
  
  conf_mat(results, truth = VesselType,
           estimate = .pred_class)
}

###Main
data <- init_data()
villes <- init_villes()
data_description(data)
doublons<-doublon(data)
med <- med(doublons)
nettoy <- nettoyer_donnees(doublons)
aber <- val_aber(nettoy)

data_clean <- aber

plot_top_vessel_types_by_range(data_clean, top_n = 3)
plot_camembert(data_clean)
afficher_ports_top(data = data, villes = villes)
histo_VesselType_mean(med)

corr_mat(data_clean)
red <- reduction(data_clean)
Regres(red, 'Width', 'Length')
Regres(red, 'Draft', 'Length')
Regres(red, 'Width', 'Draft')

print(test_khi(data_clean, 'Cargo'))
print(test_khi(data_clean, 'Status'))

prediction(data_clean)

map_init(data_clean, villes)
