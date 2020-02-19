setwd("D:/Faks/Uporabna statistika/Racunsko zahtevne metode/RZM_Seminarska")

#PARALENE SIMULACIJE
source("Generation_data.r")
source("Imputation_Models.R")
library(parallel)
library(doParallel)
library(gridExtra)
library(magrittr)
library(foreach)
library(doRNG)
rngseed(45)

# # poglej stevilo jedr in odstej enega (za nase delo)
no_cores <- detectCores() - 1


# # doloci "skupine" za delo
cl <- makeCluster(no_cores, outfile = "")
#
# # registriraj "parallel beckend"
clusterExport(cl, c("lda_mice", "lda_knn", "lda_EM.algoritem", "lda_rf",
                    "lda_complete.cases", "lda_perfect.cases", "generation.data",
                    "get.NA.MAR", "get.NA.NMAR", "get.NA.MCAR", "make.imputation"))

registerDoParallel(cl)


pon <- 500
sampleSize <- 300
delez_na <- c(0.3, 0.4, 0.5, 0.6)
moc <- c(1, 2, 5, 8, 12)
zasnova <- expand.grid(delez_na,  moc)
zasnova <- do.call(rbind, replicate(pon, zasnova, simplify=FALSE)) %>% `colnames<-`(c("delez_na", "moc_mehanizma"))


start_time <- Sys.time()
rez <- foreach(i = 1:nrow(zasnova), .combine = "rbind", .errorhandling = "pass",
               .packages = c("MASS", "mice", "missForest",
                             "fpc", "DescTools", "DMwR", "norm")) %dorng% {
                               rngseed(45)
                               missNA.i <- zasnova[i, "delez_na"]
                               moc.i <- zasnova[i, "moc_mehanizma"]
                               data.perfect <- generation.data.stara(N = sampleSize)
                               data.test <- generation.data(N = 1200)
                               data.NA.MAR <- get.NA.MAR(dataSet = data.perfect,
                                                         prop.NA = missNA.i,
                                                         moc.mehanizma = moc.i)
                               data.NA.NMAR <- get.NA.NMAR(dataSet =data.perfect,
                                                           prop.NA =missNA.i,
                                                           moc.mehanizma = moc.i)
                               data.NA.MCAR <- get.NA.MCAR(dataSet = data.perfect,
                                                           prop.NA = missNA.i)

                               imputation.MAR <- make.imputation(data.perfect,data.NA.MAR, data.test)
                               imputation.NMAR <- make.imputation(data.perfect,data.NA.NMAR, data.test)
                               imputation.MCAR <- make.imputation(data.perfect,data.NA.MCAR, data.test)

                               trentutniDF <- NULL
                               trentutniDF <- rbind(trentutniDF, c("delez_na" = missNA.i, "moc_mehanizma" = moc.i, "mehanizem" = "MAR", imputation.MAR) )
                               trentutniDF <- rbind(trentutniDF, c("delez_na" = missNA.i, "moc_mehanizma" = moc.i, "mehanizem" = "NMAR", imputation.NMAR) )
                               trentutniDF <- rbind(trentutniDF, c("delez_na" = missNA.i, "moc_mehanizma" = 0,     "mehanizem" = "MCAR", imputation.MCAR))
                               trentutniDF

                             }
# rez.d <- as.data.frame(rez)
# rez.d <- as.data.frame(apply(rez[,-3],2, as.numeric))
# rez.d$mehanizem <- rez[,3]

end_time <- Sys.time()
time<- end_time - start_time
time
saveRDS(rez, paste("data_LDA_imputation_data_diff_avg", Sys.Date(),".RDS", sep = ""))
stopCluster(cl)
registerDoSEQ()

rez.d <- as.data.frame(rez)


# rez.d[10498,] <- NA
# rez.d[23144,] <- NA
# rez.d[25974,] <- NA
rez.dokoncen <- as.data.frame(apply(rez.d[,-3],2, as.numeric))
rez.dokoncen$mehanizem <- unlist(rez.d[,3])

saveRDS(object = as.data.frame(rez.d), paste("data_LDA_imputation_data_diff_avg", Sys.Date(),".RDS", sep = ""))



#rez je list, ga spravimo v matriko
rez.d <- matrix(NA, nrow(rez), ncol(rez))
rez.d[,1] <- as.numeric(unlist(rez.df[,1]))
rez.d[,2] <- as.numeric(as.character(rez[,2]))
rez.d[,3] <- unlist(rez.df[,3])
rez.d[,4] <- as.numeric(as.character(rez[,4]))
rez.d[,5] <- as.numeric(unlist(rez.df[,5]))
rez.d[,6] <- as.numeric(as.character(rez[,6]))
rez.d[,7] <- as.numeric(unlist(rez.df[,7]))
rez.d[,8] <- as.numeric(as.character(rez[,8]))
rez.d[,9] <- as.numeric(unlist(rez.df[,9]))


#v primerih, ko je v foreachu prislo do napake, napako shrani oziroma jo vrne kot NA
which(is.na(rez.d))

#odstranimo NA
rez.na.omit <- na.omit(rez.d) %>% `colnames<-`(colnames(rez))


library(ggplot2)
library(reshape2)

#melted dataframe
mdf <- melt(data.frame(rez.na.omit), id.vars = colnames(data.frame(rez.na.omit)[1:3]),
            measure.vars = colnames(data.frame(rez.na.omit)[4:9]))

#value in delez_na nista shranjena numericno (ko delez_na shranim numericno, ga spremeni v 1,2,3,4, zato z ifelsem popravim nazaj; kmetija!!)
mdf$value <- as.numeric(mdf$value)
mdf$delez_na <- as.numeric(mdf$delez_na)
mdf$delez_na <- as.numeric(ifelse(mdf$delez_na == "1", 0.3,
                                  ifelse(mdf$delez_na == "2", 0.4,
                                         ifelse(mdf$delez_na == "3", 0.5, 0.6))))

#preuredim vrstni red faktorjev
mdf$moc_mehanizma <- factor(mdf$moc_mehanizma, levels(mdf$moc_mehanizma)[c(1,2,4,5,6,3)])

#narisem graf vseh skupaj
ggplot(mdf, mapping = aes(x = delez_na, y = value, group = variable, col = variable)) +
  scale_x_continuous(name = "Delez manjkajocih vrednosti") +
  scale_y_continuous(name = "Delez pravilno razvrscenih enot") +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  facet_wrap(mehanizem~moc_mehanizma)

#MCAR mehanizem
mdfMC <- mdf[mdf$mehanizem=="MCAR",]

#narisem graf za MCAR
ggplot(mdfMC, mapping = aes(x = delez_na, y = value, group = variable, col = variable)) +
  scale_x_continuous(name = "Delez manjkajocih vrednosti") +
  scale_y_continuous(name = "Delez pravilno razvrscenih enot") +
  stat_summary(fun.y = mean, geom = "point") +
  ggtitle("MCAR") +
  stat_summary(fun.y = mean, geom = "line")

ggsave("MCAR_sim500.png")


#MAR mehanizem
mdfM <- mdf[mdf$mehanizem=="MAR",]

#narisem graf za MAR
ggplot(mdfM, mapping = aes(x = delez_na, y = value, group = variable, col = variable)) +
  scale_x_continuous(name = "Delez manjkajocih vrednosti") +
  scale_y_continuous(name = "Delez pravilno razvrscenih enot") +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  ggtitle("MAR") +
  facet_wrap(~moc_mehanizma)

ggsave("MAR_sim500.png")


#NMAR mehanizem
mdfNM <- mdf[mdf$mehanizem=="NMAR",]

#narisem graf za NMAR
ggplot(mdfNM, mapping = aes(x = delez_na, y = value, group = variable, col = variable)) +
  scale_x_continuous(name = "Delez manjkajocih vrednosti") +
  scale_y_continuous(name = "Delez pravilno razvrscenih enot") +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  ggtitle("NMAR") +
  facet_wrap(~moc_mehanizma)

ggsave("NMAR_sim500.png")



