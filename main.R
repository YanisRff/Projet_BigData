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
