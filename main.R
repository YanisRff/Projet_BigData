doublon <- function(data) {
  n <- nrow(data)
  print(n)
  
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      identic <- (data$MMSI[i] == data$MMSI[j]) & (data$BaseDateTime[i] == data$BaseDateTime[j])
      if (identic) {
        print(paste(data$id[i], " is similar to ", data$id[j]))
      }
    }
  }
}

doublon(data = data)


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