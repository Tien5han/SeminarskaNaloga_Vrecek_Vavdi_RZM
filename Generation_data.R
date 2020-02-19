library(MASS)

generation.data <- function(N = 300){
  podatki <- NULL
  n.skupin <- round(N/3, 0)
  mi.1 <- c(0, 0, 0, 0)
  mi.2 <- c(0.5,.05,1,4)
  mi.3 <- c(1,1,2,6)
  povprecje.skupin <- list(mi.1, mi.2, mi.3)
  r <- 0.3 #smisel življenja
  Sigma <- rbind(c(1.0, r, r, r),
                 c(r, 1.0, r, r),
                 c(r, r, 1.0, r),
                 c(r, r, r, 1.0)
  )
  for(i.mi in 1:3){
    dataset <- as.data.frame(mvrnorm(n = n.skupin, mu = povprecje.skupin[[i.mi]], Sigma = Sigma))
    dataset$skupina <- rep(i.mi, n.skupin)
    podatki <- rbind(podatki, dataset)
  }
  podatki <- as.data.frame(podatki)
  colnames(podatki) <- c("X1", "X2", "X3", "X4", "skupina")
  return(podatki)
}

generation.data.stara <- function(N = 300){
  podatki <- NULL
  n.skupin <- round(N/3, 0)
  povprecje.skupin <- c(3,5,7)
  r <- 0.3 #smisel življenja
  Sigma <- rbind(c(1.0, r, r, r),
                 c(r, 1.0, r, r),
                 c(r, r, 1.0, r),
                 c(r, r, r, 1.0)
  )
  for(i.mi in 1:3){
    dataset <- as.data.frame(mvrnorm(n = n.skupin, mu = rep(povprecje.skupin[i.mi],4), Sigma = Sigma))
    dataset$skupina <- rep(i.mi, n.skupin)
    podatki <- rbind(podatki, dataset)
  }
  podatki <- as.data.frame(podatki)
  colnames(podatki) <- c("X1", "X2", "X3", "X4", "skupina")
  return(podatki)
}


get.NA.MAR <- function(dataSet, prop.NA = 0.1, moc.mehanizma = 4, plot=F){
  #INPUT:
  #dataSet - podatki, katerim želim nastaviti manjkajoče vrednosti
  #prop.NA - razmerje NA vrednosti
  #moc.mehanizma - moč mehanizma
  #OUTPUT:
  #podatki - datasets NA vrednostmi

  podatki.plot <- dataSet
  konst <- 3.14
  N <- nrow(dataSet)
  #Manjkajoče vrednosti
  nMiss <- round(prop.NA *N)
  verjetnosti.na <- (dataSet[,1] - min(dataSet[,1]) + konst)^moc.mehanizma/(sum(dataSet[,1]))^moc.mehanizma

  set.na.2 <- sample(length(verjetnosti.na), size = nMiss, prob = verjetnosti.na)
  dataSet[set.na.2, 2] <- NA #X1 vpliva na X2

  set.na.3 <- sample(length(verjetnosti.na), size = nMiss, prob = verjetnosti.na)
  dataSet[set.na.3, 3] <- NA #X1 vpliva na X3

  set.na.4 <- sample(length(verjetnosti.na), size = nMiss, prob = verjetnosti.na)
  dataSet[set.na.4, 4] <- NA #X1 vpliva na X4
  #za risanje slik
  if(plot){
    podatki.plot$na.2<- 1 #za risanje grafa
    podatki.plot[set.na.2, "na.2"] <- 4
    podatki.plot$na.3<- 1 #za risanje grafa
    podatki.plot[set.na.3, "na.3"] <- 4
    podatki.plot$na.4<- 1 #za risanje grafa
    podatki.plot[set.na.4, "na.4"] <- 4
    #return(podatki.plot)
    p1 <- plot(podatki.plot$X1, podatki.plot$X2, col = podatki.plot$na.2,
               main = "Povezanost med spremenljivko X1 \n in manjkajocimi podatki X2",
               xlab = "X1", ylab = "X2")
    legend("bottomright", legend = c("obstojeci", "manjkajoci"), col= c(1,4), pch = 1)
    p2 <- plot(podatki.plot$X1, podatki.plot$X3, col = podatki.plot$na.3,
               main = "Povezanost med spremenljivko X1 \n in manjkajocimi podatki X3",
               xlab = "X1", ylab = "X3")
    legend("bottomright", legend = c("obstojeci", "manjkajoci"), col= c(1,4), pch = 1)
    p3 <- plot(podatki.plot$X1, podatki.plot$X4, col = podatki.plot$na.4,
               main = "Povezanost med spremenljivko X1 \n in manjkajocimi podatki X4",
               xlab = "X1", ylab = "X4")
    legend("bottomright", legend = c("obstojeci", "manjkajoci"), col= c(1,4), pch = 1)
    return(dataSet)
  }
  else{
    return(dataSet)
  }
}

get.NA.NMAR <- function(dataSet, prop.NA = 0.1, moc.mehanizma = 4, plot=F){
  #INPUT:
  #dataSet - podatki, katerim želim nastaviti manjkajoče vrednosti
  #prop.NA - razmerje NA vrednosti
  #moc.mehanizma - moč mehanizma
  #OUTPUT:
  #podatki - datasets NA vrednostmi

  podatki.plot <- dataSet #za risanje
  konst <- 3.14
  N <- nrow(dataSet)
  nMiss <- round(prop.NA * N)

  verjetnosti.na <- (dataSet[,2] - min(dataSet[,2]) + konst)^moc.mehanizma/(sum(dataSet[,2]))^moc.mehanizma
  set.na.2 <- sample(length(verjetnosti.na), size = nMiss, prob = abs(verjetnosti.na))
  dataSet[set.na.2, 2] <- NA #X2 vpliva na X2

  verjetnosti.na.3 <-  (dataSet[,3] - min(dataSet[,3]) + konst)^moc.mehanizma/(sum(dataSet[,3]))^moc.mehanizma
  set.na.3 <- sample(length(verjetnosti.na.3), size = nMiss, prob = abs(verjetnosti.na.3))
  dataSet[set.na.3, 3] <- NA #X3 vpliva na X3

  verjetnosti.na.4 <-  (dataSet[,4] - min(dataSet[,4]) + konst)^moc.mehanizma/(sum(dataSet[,4]))^moc.mehanizma
  set.na.4 <- sample(length(verjetnosti.na.4), size = nMiss, prob = abs(verjetnosti.na.4))
  dataSet[set.na.4, 4] <- NA #X4 vpliva na X4

  #za risanje slik
  if(plot){
    podatki.plot$na.2<- 1 #za risanje grafa
    podatki.plot[set.na.2, "na.2"] <- 4
    podatki.plot$na.3<- 1 #za risanje grafa
    podatki.plot[set.na.3, "na.3"] <- 4
    podatki.plot$na.4<- 1 #za risanje grafa
    podatki.plot[set.na.4, "na.4"] <- 4
    #return(podatki.plot)
    p1 <- plot(podatki.plot$X1, podatki.plot$X2, col = podatki.plot$na.2,
               main = "Povezanost med spremenljivko X1 \n in manjkajocimi podatki X2",
               xlab = "X1", ylab = "X2")
    legend("bottomright", legend = c("obstojeci", "manjkajoci"), col= c(1,4), pch = 1)
    p2 <- plot(podatki.plot$X1, podatki.plot$X3, col = podatki.plot$na.3,
               main = "Povezanost med spremenljivko X1 \n in manjkajocimi podatki X3",
               xlab = "X1", ylab = "X3")
    legend("bottomright", legend = c("obstojeci", "manjkajoci"), col= c(1,4), pch = 1)
    p3 <- plot(podatki.plot$X1, podatki.plot$X4, col = podatki.plot$na.4,
               main = "Povezanost med spremenljivko X1 \n in manjkajocimi podatki X4",
               xlab = "X1", ylab = "X4")
    legend("bottomright", legend = c("obstojeci", "manjkajoci"), col= c(1,4), pch = 1)
    return(dataSet)
  }
  else{
    return(dataSet)
  }
}

get.NA.MCAR <- function(dataSet, prop.NA = 0.1){
  #INPUT:
  #N - stevilo enot v skupini
  #prop.NA - razmerje NA vrednosti
  #OUTPUT:
  #podatki - datasets NA vrednostmi
  N <- nrow(dataSet)
  nMiss <- round(prop.NA *N)
  dataSet[sample(N, size = nMiss), 2] <- NA
  dataSet[sample(N, size = nMiss), 3] <- NA
  dataSet[sample(N, size = nMiss), 4] <- NA
  return(dataSet)
}
