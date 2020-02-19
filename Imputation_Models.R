library("DMwR")
library("MASS")
library("norm")
library("missForest")
library("mice")
library("fpc")
library("DescTools")

rngseed(45) #Za potrebe funkcije imp.norm

#######MICE 1#############
lda_mice <- function(miss_df, test_df, m=10, maxit=20){
  #Imputiram manjkajoče vrednosti z mice
  imp <- mice(miss_df, m = m, printFlag = FALSE, maxit = maxit)
  #Pogledam summary imputacij (ce predhodno definiram, da je spr. faktor, mi sam da npr., logisticno regresijo)
  #Vidimo imputirane vrednosti iz vseh 5ih podatkovij za sprem X2 -> bi bilo smiselno pogledati konvergenco?
  imp$imp$X2
  #shranimo vsa imp. podatkovja v long format
  imp_tot <- complete(imp, "long", include = FALSE)
  #v vektor shranim napovedane razrede za vsa podatkovja (po stolpcih so podatkovja)
  xclas_tot <- NULL
  for(i in 1:m){
    mod <- lda(skupina ~ X1 + X2 + X3 + X4, imp_tot[(i-1)*nrow(miss_df)+1:((i-1)*nrow(miss_df)+nrow(miss_df)),])
    xclas <- cbind(predict(mod, test_df[,1:4])$class)
    xclas_tot <- cbind(xclas_tot, xclas)
  }
  #Glasujem (pogledam) katerih skupin po vrstici (enoti) je največ
  imp_class <- NULL
  for(i in 1:nrow(xclas_tot)){
    clas <- if(length(xclas_tot[which(xclas_tot[i,] == 1)]) > length(xclas_tot[which(xclas_tot[i,] == 2)]) &
               length(xclas_tot[which(xclas_tot[i,] == 1)]) > length(xclas_tot[which(xclas_tot[i,] == 3)])){
      "1"
    } else if(length(xclas_tot[which(xclas_tot[i,] == 2)]) > length(xclas_tot[which(xclas_tot[i,] == 1)]) &
              length(xclas_tot[which(xclas_tot[i,] == 2)]) > length(xclas_tot[which(xclas_tot[i,] == 3)])){
      "2"
    } else{
      "3"
    }
    imp_class <- c(imp_class, clas)
  }
  #delez pravilno razvrscenih
  lda_prop <- mean(imp_class==test_df[,5])
  return(lda_prop)
}


######MICE 2###############
lda_mice_2 <- function(data.train, data.test){
  n.skupine <- nrow(data.train)/3
  rez.pos <- NULL
  miceImp <-  mice(data.train[,1:4], m=10, maxit=15)
  for(i in 1:10){
    popolni.i <- complete(miceImp, i)
    popolni.i$skupina <- rep(c(1,2,3), times = rep(n.skupine,3))
    rez.i <- make.lda(popolni.i, data.test)
    rez.pos[i] <- rez.i
  }
  return(mean(rez.pos))
}




#######KNNimputation#########

lda_knn <- function(miss_df, test_df, k){

  #funkcija rabi stolpec, ki pove vrstico (faktor)
  miss_df[,6] <- factor(seq(1,nrow(miss_df)))
  knn_imp <- knnImputation(miss_df, k)

  #LDA model z imputiranimi vrednostmi
  knn_mod <- lda(skupina ~ X1 + X2 + X3 + X4, knn_imp)
  knn_class <- predict(knn_mod, test_df[,1:4])$class

  #delez pravilno razvrscenih
  prop <-  mean(knn_class==test_df[,5])
}


#######EM-algoritem############

lda_EM.algoritem <- function(data.train, data.test){
  #EM-algoritem
  dataPrep <- prelim.norm(as.matrix(data.train[,1:4]))
  thetahat <- em.norm(dataPrep, showits = F)
  EM.matrix <- imp.norm(dataPrep, thetahat)
  dataEM <- as.data.frame(EM.matrix)
  colnames(dataEM) <- c("X1", "X2", "X3", "X4")
  dataEM$skupina <- data.train$skupina

  #LDA model z imputiranimi vrednostmi
  em_mod <- lda(skupina ~ X1 + X2 + X3 + X4, dataEM)
  em_class <- predict(em_mod, data.test[,1:4])$class

  #delez pravilno razvrscenih
  prop <-  mean(em_class==data.test[,5])

  return(prop)
}

##########RANDOM FOREST########
lda_rf <- function(miss_df, test_df, maxiter = 10, OOB = TRUE){

  #Imputiram manjkajoče vrednosti z rf
  imp <- missForest(miss_df, maxiter = maxiter,
                    variablewise = OOB)

  #LDA model
  rf_mod <- lda(skupina ~ X1 + X2 + X3 + X4, imp$ximp)
  rf_class <- predict(rf_mod, test_df[,1:4])$class

  #Delez pravilno razvrščenih enot
  k <- list(prop = mean(rf_class==test_df[,5]), OOB = imp$OOBerror)
  return(k)
}


########COMPLETE CASES##########
lda_complete.cases <- function(miss_df, test_df){
  miss_df.complete <- miss_df[complete.cases(miss_df),]
  cc_model <- lda(skupina ~ X1 + X2 + X3 + X4, miss_df.complete)
  cc_class <- predict(cc_model, test_df[,1:4])$class

  #delez pravilno razvrscenih
  prop <-  mean(cc_class==test_df[,5])
  return(prop)
}

########PERFECT CASES############

lda_perfect.cases <- function(perfect_df, test_df){
  pc_model <- lda(skupina ~ X1 + X2 + X3 + X4, perfect_df)
  pc_class <- predict(pc_model, test_df[,1:4])$class

  #delez pravilno razvrscenih
  prop <-  mean(pc_class==test_df[,5])
  return(prop)
}




make.imputation <- function(data.perfect, data.NA, df.test){
  comp <- lda_perfect.cases(data.perfect, df.test)
  pair <- lda_complete.cases(data.NA, df.test)
  rf <- lda_rf(data.NA, df.test)
  em <- lda_EM.algoritem(data.NA, df.test)
  knn <- lda_knn(data.NA, df.test, k =10)
  mice <- lda_mice(data.NA, df.test)
  lda.mean.rez <- c(
    "perfect.data" = comp,
    "complete.data" = pair,
    "knn.imputation"= knn,
    "EM.imputation" = em,
    "MICE.imputation" = mice,
    "RandomForest.imputation" = rf$prop)
  return(lda.mean.rez)
}
