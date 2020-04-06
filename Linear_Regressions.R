#Feb/4/2020
#Exploring relationship between envt factors and sound ecology 
install.packages("mefa")
install.packages("plotly")
#Libraries
library(tidyverse)
library(tidyselect)
library(ggplot2)
library(soundecology)
library(tuneR)
library(seewave)
library(nlme)
library(dplyr)
library(mefa)
library(plotly)


setwd("D:/RA-withSangermano/Miles_and_Shreena/CSV")

# import CSV
DF_evt<- read.csv('D:/RA-withSangermano/Miles_and_Shreena/CSV/NDVI_NDBI_PERBUFFER.csv')
ACI<- read.csv('D:/RA-withSangermano/Miles_and_Shreena/CSV/Revised_ACI.csv')
ADI <- read.csv('D:/RA-withSangermano/Miles_and_Shreena/CSV/Revised_ADI.csv')
NDSI <- read.csv('D:/RA-withSangermano/Miles_and_Shreena/CSV/Revised_NDSI.csv')
AEI<- read.csv('D:/RA-withSangermano/Miles_and_Shreena/CSV/Revised_AEI.csv')
BI<- read.csv('D:/RA-withSangermano/Miles_and_Shreena/CSV/Revised_BI.csv')

#reorder DF_evt to match order of batch files

DF_evt <- DF_evt %>% arrange(SITE)

#------------------------------------------------------------------------------------------
#Acoustic data cleanup
#create new data frame with only acoustic index 
ACI_select<- ACI$LEFT_CHANNEL
ADI_select<- ADI$LEFT_CHANNEL
AEI_select <- AEI$LEFT_CHANNEL
BI_select <- BI$LEFT_CHANNEL
filenames <- NDSI$X
NDSI_select<- NDSI$ndsi_left
BIO_select<- NDSI$biophony_left
ANT_select <- NDSI$anthrophony_left

#ADD NDSI, Biophony, Antrophony 

#create Acoustic dataframe
DF_sounds_sub<- data.frame(filenames, NDSI_select, BIO_select, ANT_select)


DF_sounds <- data.frame(filenames, NDSI_select, BIO_select, ANT_select, ACI_select, ADI_select, AEI_select, BI_select)

#select by site
names <- c('BMB', 'BP', 'CC', 'FR', 'L_', 'PM', 'RB', 'W_','WM', 'EL', 'LM')

names


BMB<- 'BMB'
dat_BMB <- subset(DF_sounds, grepl(BMB, filenames))

BP <- 'BP'
dat_BP <- subset(DF_sounds, grepl(BP, filenames))

CC <- 'CC'
dat_CC <- subset(DF_sounds, grepl(CC, filenames))

FR <- 'FR'
dat_FR <- subset(DF_sounds, grepl(FR, filenames))


#Subset for 'L_', 'PM', 'RB', 'W_'

L_ <- 'L_'
dat_L <- subset(DF_sounds, grepl(L_, filenames))

PM <- 'PM'
dat_PM <- subset(DF_sounds, grepl(PM, filenames))

RB <- 'RB'
dat_RB <- subset(DF_sounds, grepl(RB, filenames))

W_<- 'W_'
dat_W <- subset(DF_sounds, grepl(W_, filenames))

#Subset for  WM', 'EL', 'LM'

WM <- 'WM'
dat_WM <- subset(DF_sounds, grepl(WM, filenames))

EL <- 'EL'
dat_EL <- subset(DF_sounds, grepl(EL, filenames))

LM <- 'LM'
dat_LM <- subset(DF_sounds, grepl(LM, filenames))


for (i in length(df_sounds)){
  if (names == names[11]){
    dat_LM <- subset(df_sounds, grepl(names[11], filenames))
  }else {
    dat_BMB <- subset(df_sounds, grepl(names[1], filenames)) 
  }else{
    dat_BP <- subset(df_sounds, grepl(names[2], filenames))
  }else{
    dat_PM <- subset(df_sounds, grepl(names[6], filenames))
  }else{
    dat_RB <- subset(df_sounds, grepl(names[7], filenames))
  }else {
    dat_CC <- subset(df_sounds, grepl(names[3], filenames))
  }else{
    dat_EL <- subset(df_sounds, grepl(names[10], filenames))
  }else{
    dat_WM <- subset(df_sounds, grepl(names[9], filenames))
  }else{
    dat_W <- subset(df_sounds, grepl(names[8], filenames))
  }else{
    dat_FR <- subset(df_sounds, grepl(names[4], filenames))
  }else{
    dat_L <- subset(df_sounds, grepl(names[5], filenames))
  }
}

#remove duplicates of eagle lake in dat_L 
dat_L <- dat_L[!grepl("EL", dat_L$filenames),]






# # these didnt work in loop
# BP <- 'BP'
# dat_BP <- subset(DF_sounds, grepl(BP, filenames))
# 
# LM<- 'LM'
# dat_LM <- subset(DF_sounds, grepl(LM, filenames))
# 
# BMB<- 'BMB'
# dat_BMB <- subset(DF_sounds, grepl(BMB, filenames))

#mean acoustic index per site

# dat_BMB %>% mutate_all(summarize(., mean(.))

# mean_BMB <- dat_BMB %>%  summarise_all(funs(mean))
# mean_EL <- dat_EL %>%  summarise_all(funs(mean))
# mean_BP <- dat_BP %>%  summarise_all(funs(mean))
# mean_CC <- dat_CC %>%  summarise_all(funs(mean))
# mean_FR <- dat_FR %>%  summarise_all(funs(mean))
# mean_L <- dat_L %>%  summarise_all(funs(mean))
# mean_PM <- dat_PM %>%  summarise_all(funs(mean))
# mean_RB <- dat_RB %>%  summarise_all(funs(mean))
# mean_W <- dat_W %>%  summarise_all(funs(mean))
# mean_WM <- dat_WM %>%  summarise_all(funs(mean))
# mean_LM <- dat_LM %>%  summarise_all(funs(mean))

mean_BMB <- sapply(dat_BMB, mean)
mean_BP <- sapply(dat_BP, mean)
mean_CC <- sapply(dat_CC, mean)
mean_FR <- sapply(dat_FR, mean)
mean_L <- sapply(dat_L, mean)
mean_PM <- sapply(dat_PM, mean)
mean_RB <- sapply(dat_RB, mean)
mean_W <- sapply(dat_W, mean)
mean_WM <- sapply(dat_WM, mean)
mean_EL <- sapply(dat_EL, mean)
mean_LM <- sapply(dat_LM, mean)

#data frame
DF_sounds_mean <- data.frame(mean_BMB, mean_BP, mean_CC, mean_EL, mean_FR, mean_W, mean_LM, mean_L, mean_PM, mean_RB, mean_WM)

#flip row column
DF_sound_mean_transpose <- as.data.frame(t(as.matrix(DF_sounds_mean)))
#---------------------------------------------------------------------------------------------
#Enviornmental Factors
#DATA FRAME per buffer
m500<- select(filter(DF_evt, Buffer_Meter == 500),c(SITE, MEAN_NDVI, MEAN_NDBI, Buffer_Meter))
m1000<- select(filter(DF_evt, Buffer_Meter == 1000),c(SITE, MEAN_NDVI, MEAN_NDBI, Buffer_Meter))
m1500<- select(filter(DF_evt, Buffer_Meter == 1500),c(SITE, MEAN_NDVI, MEAN_NDBI, Buffer_Meter))
m2000<- select(filter(DF_evt, Buffer_Meter == 2000),c(SITE, MEAN_NDVI, MEAN_NDBI, Buffer_Meter))
m2500<- select(filter(DF_evt, Buffer_Meter == 2500),c(SITE, MEAN_NDVI, MEAN_NDBI, Buffer_Meter))
m3000<- select(filter(DF_evt, Buffer_Meter == 3000),c(SITE, MEAN_NDVI, MEAN_NDBI, Buffer_Meter))


unique(DF_evt$SITE)
#Assign site
DF_filesnames2 <- DF_sound_mean_transpose %>% mutate(filenames = unique(DF_evt$SITE)) %>%  rename(`Site Name` = filenames)
DF_filesnames2 <- DF_filesnames2 %>% arrange(`Site Name`)


write.csv(DF_filesnames2, file = "DF_FullTable.csv")



#repeated values within to make room for NDVI & NDBI data
DF_FullTable <-  rep(DF_filesnames2, 6)

#Rename Site Name to SITE
DF_FullTable <- DF_FullTable %>% rename( SITE = Site.Name)

DF_FullTable <- DF_FullTable %>% arrange(SITE)

write.csv(DF_FullTable, file = "DF_FullTable.csv")
# 
# DF_FullTable2 <- read.csv("DF_FullTable.csv")


# #Clean DF_evt before join
# rownames(DF_evt) = 1:dim(DF_evt)[1]




# #Join DF_evt and DF_Fulltable together to create a whole dataset which includes the buffer values and the sound indices values. 
# DF_FullTable <- DF_FullTable %>%  arrange(SITE)
# DF_FullTable 
# 
# 
# DF_Left_Join <- left_join(x = DF_FullTable, y = DF_evt)
# DF_Left_Join
# rownames(DF_Left_Join) = 1:dim(DF_Left_Join)[1]
# 
# 
# 
# 
# 
# DF_Inner_Join <- inner_join(x = DF_FullTable, y = DF_evt)


# #cbind ndbi & ndvi data with Buffers and the DF_filenames data
DF_FullTable
DF_all_cbind <- bind_cols(DF_FullTable, DF_evt$MEAN_NDVI)

DF_all_cbind <- cbind(DF_FullTable, DF_evt$MEAN_NDVI, DF_evt$MEAN_NDBI, DF_evt$Buffer_Meter)
DF_all_cbind

#renaming the columns of the newly combined data 

# DF_all_cbind <- DF_all_cbind %>% rename( MEAN_NDVI = `DF_evt$MEAN_NDVI`, MEAN_NDBI = `DF_evt$MEAN_NDBI`,  Buffer = `DF_evt$Buffer_Meter`)
# DF_Left_Join <- DF_Left_Join %>% rename(Buffer = `Buffer_Meter`)

# #renaming the renaming because I was dumb
# names(DF_all)[10] <- "Mean_NDBI"
# names(DF_all)

#writing csv for the newly created dataframe

write.table(DF_all_cbind, file = "all_indices_updated_join.csv", append = TRUE, quote = FALSE, sep = ",",
            na = 'NA', dec = ".", row.names = T, col.names = NA)

write.csv(DF_all_cbind, file = "ALL_INDICES_ALL_DATA_CBIND_AGAIN_AGAIN.csv")


setwd("D:/RA-withSangermano/Miles_and_Shreena/CSV")
# DF<- read.csv("ALL_INDICES.csv")

#Your Choice whether to read in the data from a csv or use the made datafram
DF <- DF_all_cbind
DF <- read.csv("DF_Fulltable_CBIND.csv")


#In the following code we use lmlist to run the regressions given we have data points taken at different buffers. 
#In order to prevent grouping of the data for the calculation of the standard error, deviation, t statistics, and pvalues, 
#we use pool: an optional logical value indicating whether a pooled estimate of the residual standard error should be used 
#in calculations of standard deviations or standard errors for summaries.

#ACI
lm_ACI_NDVI<- lmList(ACI_select ~ MEAN_NDVI | Buffer, data = DF, pool = FALSE)
ACI_NDVI_lm <- summary(lm_ACI_NDVI)$r.squared 

lm_ACI_NDVI



lm_ACI_NDBI<- lmList(ACI_select ~ MEAN_NDBI | Buffer, data = DF, pool = FALSE)
ACI_NDBI_lm <- summary(lm_ACI_NDBI)$r.squared

ACI_NDVI_lm

#ADI
lm_ADI_NDVI<- lmList(ADI_select ~ MEAN_NDVI | Buffer, data = DF, pool = FALSE)
ADI_NDVI_lm <- summary(lm_ADI_NDVI)$r.squared 

lm_ADI_NDVI


lm_ADI_NDBI<- lmList(ADI_select ~ MEAN_NDBI | Buffer, data = DF, pool = FALSE)
ADI_NDBI_lm <- summary(lm_ADI_NDBI)$r.squared

ADI_NDVI_lm


#AEI

lm_AEI_NDVI<- lmList(AEI_select ~ MEAN_NDVI | Buffer, data = DF, pool = FALSE)
summary(lm_AEI_NDVI)

AEI_NDVI_lm <- summary(lm_AEI_NDVI)$r.squared 

lm_AEI_NDVI


lm_AEI_NDBI<- lmList(AEI_select ~ MEAN_NDBI | Buffer, data = DF, pool = FALSE)
AEI_NDBI_lm <- summary(lm_AEI_NDBI)$r.squared

AEI_NDBI_lm

plot(lm_AEI_NDVI, resid(., type = "pool") ~ fitted(.) | Buffer, abline = 0, id = 0.05)





#BI

lm_BI_NDVI<- lmList(BI_select ~ MEAN_NDVI | Buffer, data = DF, pool = FALSE)
BI_NDVI_lm <- summary(lm_BI_NDVI)$r.squared 

lm_BI_NDVI


lm_BI_NDBI<- lmList(BI_select ~ MEAN_NDBI | Buffer, data = DF, pool = FALSE)
BI_NDBI_lm <- summary(lm_BI_NDBI)$r.squared

BI_NDBI_lm


#plot(m1$residuals ~ mlb11$at_bats)



#----------------------------------------------------------------------------------------------------------
#NDSI
lm_NDSI_NDVI <- lmList(NDSI_select ~ MEAN_NDVI | Buffer, data = DF, pool = FALSE)
NDSI_NDVI_lm <- summary(lm_NDSI_NDVI)$r.squared

lm_NDSI_NDVI
NDSI_NDVI_lm

cor(x = DF$NDSI_select, y = DF$MEAN_NDVI)

#Obtain numbers for NDVI at Buffer 2000
cor_2000_NDVI <- lm_NDSI_NDVI[["2000"]][["model"]][["MEAN_NDVI"]]
cor_2000_NDSI <- lm_NDSI_NDVI[["2000"]][["model"]][["NDSI_select"]]

cor(x = cor_2000_NDSI, y = cor_2000_NDVI)

lm_NDSI_NDBI<- lmList(NDSI_select ~ MEAN_NDBI | Buffer, data= DF, pool = FALSE)
NDSI_NDBI_lm<-summary(lm_NDSI_NDBI)$r.squared

cor(x = DF$NDSI_select, y = DF$MEAN_NDBI)


#Obtain numbers for NDBI at Buffer 2000
cor_2000_NDBI <- lm_NDSI_NDBI[["2000"]][["model"]][["MEAN_NDBI"]]
cor(x = cor_2000_NDSI, y = cor_2000_NDBI)


#---------------------------------------------------------------------------------------------------------
#Biophony

lm_NDSI_NDBIO_NDVI<- lmList(BIO_select ~ MEAN_NDVI | Buffer, data= DF, pool = FALSE)
BIO_NDVI_lm<-summary(lm_NDSI_NDBIO_NDVI)$r.squared

lm_NDSI_NDBIO_NDVI


cor(x = DF$BIO_select, y = DF$MEAN_NDVI)


#Obtain data for NDBIO at Buffer 2000
cor_2000_NDBIO <- lm_NDSI_NDBIO_NDVI[["2000"]][["model"]][["BIO_select"]]

cor(x = cor_2000_NDBIO, y = cor_2000_NDVI)


lm_NDSI_NDBIO_NDBI <- lmList(BIO_select ~ MEAN_NDBI | Buffer, data= DF, pool = FALSE)
BIO_NDBI_lm<-summary(lm_NDSI_NDBIO_NDBI)$r.squared

lm_NDSI_NDBIO_NDBI

cor(x = DF$BIO_select, y = DF$MEAN_NDBI)

#Correlation between NDBIO and NDBI at 2000 BUffer

cor(x = cor_2000_NDBIO, y = cor_2000_NDBI)

#-----------------------------------------------------------------------------------------------------------------
#Anthrophony

lm_ANT_NDVI<- lmList(ANT_select ~ MEAN_NDVI | Buffer, data= DF, pool = FALSE)
ANT_NDVI_lm <- summary(lm_ANT_NDVI)$r.squared

lm_ANT_NDVI

cor(x = DF$ANT_select, y = DF$MEAN_NDVI)


#Obtain data for ANT at Buffer 2000
cor_2000_ANT <- lm_ANT_NDVI[["2000"]][["model"]][["ANT_select"]]
cor(x = cor_2000_ANT, y = cor_2000_NDVI)



lm_ANT_NDBI<- lmList(ANT_select ~ MEAN_NDBI | Buffer, data= DF, pool = FALSE)
ANT_NDBI_lm<-summary(lm_ANT_NDBI)$r.squared

lm_ANT_NDBI

cor(x = DF$ANT_select, y = DF$MEAN_NDBI)

#Correlation between ANT and NDBI at 2500 BUffer

cor(x = cor_2000_ANT, y = cor_2000_NDBI)



buffername <- c(500, 1000,1500,2000,2500,3000)

linear_rsquared <- data.frame(buffername, ACI_NDVI_lm, ACI_NDBI_lm, ADI_NDVI_lm, ADI_NDBI_lm, AEI_NDVI_lm, AEI_NDBI_lm, BI_NDVI_lm, BI_NDBI_lm, NDSI_NDVI_lm, NDSI_NDBI_lm, BIO_NDVI_lm, BIO_NDBI_lm, ANT_NDVI_lm, ANT_NDBI_lm)


#-------------------------------------------------------------------------------------------------------------------------
#Graphs


#graph of R^2 for NDVI


RSquaredPlot_NDVI <- ggplot(data = linear_rsquared) + 
  geom_line(aes(x=linear_rsquared$buffername, y=linear_rsquared$ACI_NDVI_lm, color = 'ACI')) +
  geom_line(aes(x=linear_rsquared$buffername, y=linear_rsquared$ADI_NDVI_lm, color = 'ADI')) +
  geom_line(aes(x=linear_rsquared$buffername, y=linear_rsquared$AEI_NDVI_lm, color = 'AEI')) +
  geom_line(aes(x=linear_rsquared$buffername, y=linear_rsquared$BI_NDVI_lm, color = 'BI')) +
  geom_line(aes(x=linear_rsquared$buffername, y=linear_rsquared$NDSI_NDVI_lm, color = 'NDSI'))+
  geom_line(aes(x=linear_rsquared$buffername, y=linear_rsquared$BIO_NDVI_lm, color = 'BIOPHONY'))+
  geom_line(aes(x=linear_rsquared$buffername, y=linear_rsquared$ANT_NDVI_lm, color = 'ANTHRO')) +  
  labs(title = "Mean NDVI ~ R squared of metrics",x ="Focal Distance (meters)", y = "R squared of metrics") 

ggplotly(RSquaredPlot_NDVI)



#graph of R^2 for NDBI

RSquaredPlot_NDBI <- ggplot(data = linear_rsquared) + 
  geom_line (aes(x=linear_rsquared$buffername, y=linear_rsquared$ACI_NDBI_lm, color = 'ACI'))+
  geom_line (aes(x=linear_rsquared$buffername, y=linear_rsquared$ADI_NDBI_lm, color = 'ADI')) +
  geom_line (aes(x=linear_rsquared$buffername, y=linear_rsquared$AEI_NDBI_lm, color = 'AEI')) +
  geom_line (aes(x=linear_rsquared$buffername, y=linear_rsquared$BI_NDBI_lm, color = 'BI')) +
  geom_line (aes(x=linear_rsquared$buffername, y=linear_rsquared$NDSI_NDBI_lm, color = 'NDSI'))+
  geom_line (aes(x=linear_rsquared$buffername, y=linear_rsquared$BIO_NDBI_lm, color = 'BIOPHONY'))+
  geom_line (aes(x=linear_rsquared$buffername, y=linear_rsquared$ANT_NDBI_lm, color = 'ANT')) + scale_x_log10() + 
  labs(title = "Mean NDBI ~ R squared of metrics",x ="Focal Distance (meters)", y = "R squared of metrics") 


#interactive GGPlot NDBI
ggplotly(RSquaredPlot_NDBI)


#graph of R^2 using facet wrap by buffer


#-------------------------------------------------------------------------------------------------------------------------
#Graphs for NDVI vs Indices


#Graphs of metric vs NDVI/AEI

ggplot(data = DF, aes (x = MEAN_NDVI, y = AEI_select, group = Buffer, color = factor(Buffer))) +
  # geom_line(data = fortify(fit), aes(x = MEAN_NDVI, y = .fitted)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point(alpha=0) + 
  geom_point(aes(color = factor(Buffer)))+
  theme(legend.position = "right") +
  labs(y="AEI", x = "NDVI") 



ggplot(data = DF, aes(x = MEAN_NDVI, y = AEI_select, group=Buffer, color = factor(Buffer))) +
  # geom_line(data = fortify(fit), aes(x = MEAN_NDVI, y = .fitted)) +
  geom_smooth(method = "lm") +
  geom_point(alpha=0) + 
  geom_point(aes(color = factor(Buffer)))+
  geom_line(data = fortify(fit), aes(x = MEAN_NDVI, y = .fitted)) +
  theme(legend.position = "right") +
  labs(y="AEI", x = "NDVI") + geom_abline()


#Graphs of metric vs NDVI/ACI

ggplot(data = DF, aes(x = MEAN_NDVI, y = ACI_select, group=Buffer, color = factor(Buffer))) +
  geom_smooth(method = "lm", se=F) +
  geom_point(alpha=0) + 
  geom_point(aes(color = factor(Buffer)))+
  theme(legend.position = "right") +
  labs(y="ACI", x = "NDVI") + geom_abline()

#NDVI vs Biophony (BIO_SELECT)
ggplot(data = DF, aes (x = MEAN_NDVI, y = BIO_select, group=Buffer, color = factor(Buffer))) +
  geom_smooth(method = "lm", se=F) +
  geom_point(alpha=0) + 
  geom_point(aes(color = factor(Buffer)))+
  theme(legend.position = "right") +
  labs(y="BIO_select", x = "NDVI")

#NDVI vs Anthrophony (ANT_SELECT)
ggplot(data = DF, aes (x = MEAN_NDVI, y = ANT_select, group=Buffer, color = factor(Buffer))) +
  geom_smooth(method = "lm", se=F) +
  geom_point(alpha=0) + 
  geom_point(aes(color = factor(Buffer)))+
  theme(legend.position = "right") +  
  labs(y="ANT_select", x = "NDVI")


#NDVI vs NDSI Facets wrap
corr_2000_NDSI <- ggplot(data = DF, aes (x = MEAN_NDVI, y = NDSI_select, group=Buffer, color = factor(Buffer))) +
  geom_smooth(method = "lm", se=F) +
  geom_point(alpha=0) + 
  geom_point(aes(color = factor(Buffer)))+
  theme(legend.position = "right") +
  labs(y="NDSI", x = "NDVI") +
  facet_wrap("Buffer")



lm_NDSI_NDBIO_NDVI[["2000"]][["model"]]


ggscatter(my_data, x = "mpg", y = "wt", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Miles/(US) gallon", ylab = "Weight (1000 lbs)")


#NDVI vs NDSI Non-facets warp

ggplot(data = DF, aes (x = MEAN_NDVI, y = NDSI_select, group=Buffer, color = factor(Buffer))) +
  geom_smooth(method = "lm", se=F) +
  geom_point(alpha=0) + 
  geom_point(aes(color = factor(Buffer)))+
  theme(legend.position = "right") +
  labs(y="NDSI", x = "NDVI")


#-------------------------------------------------------------------------------------------------------------------------
#Graphs for NDBI vs Indices


#graph of metric vs NDBI/ACI
#NDVI
ggplot(data = DF, aes (x = MEAN_NDBI, y = ACI_select, group=Buffer, color = factor(Buffer))) +
  geom_smooth(method = "lm", se=F) +
  geom_point(alpha=0) + 
  geom_point(aes(color = factor(Buffer)))+
  theme(legend.position = "right") +
  labs(y="ACI", x = "NDBI")

#Graphs of metric vs NDSI/NDBI

#NDBI vs Biophony (BIO_SELECT)
ggplot(data = DF, aes (x = MEAN_NDBI, y = BIO_select, group=Buffer, color = factor(Buffer))) +
  geom_smooth(method = "lm", se=F) +
  geom_point(alpha=0) + 
  geom_point(aes(color = factor(Buffer)))+
  theme(legend.position = "right") +
  labs(y="BIO_select", x = "NDBI")

#NDBI vs Anthrophony (ANT_SELECT)
ggplot(data = DF, aes (x = MEAN_NDBI, y = ANT_select, group=Buffer, color = factor(Buffer))) +
  geom_smooth(method = "lm", se=F) +
  geom_point(alpha=0) + 
  geom_point(aes(color = factor(Buffer)))+
  theme(legend.position = "right") +  
  labs(y="ANT_select", x = "NDBI")


#NDBI vs NDSI Facets Wrap
ggplot(data = DF, aes (x = MEAN_NDBI, y = NDSI_select, group=Buffer, color = factor(Buffer))) +
  geom_smooth(method = "lm", se=F) +
  geom_point(alpha=0) + 
  geom_point(aes(color = factor(Buffer)))+
  theme(legend.position = "right") +
  labs(y="NDSI", x = "NDBI") +
  facet_wrap("Buffer")

#NDBI vs NDSI Non-facets warp

ggplot(data = DF, aes (x = MEAN_NDBI, y = NDSI_select, group=Buffer, color = factor(Buffer))) +
  geom_smooth(method = "lm", se=F) +
  geom_point(alpha=0) + 
  geom_point(aes(color = factor(Buffer)))+
  theme(legend.position = "right") +
  labs(y="NDSI", x = "NDBI")


# 
# #examine coefficients matrix from m1_Summary list
# m1_Summary$coefficients
# 
# #Extract pvalue from m1_Summary$coefficients
# my.p <- m1_Summary$coefficients[2, 4]
# 
# m1_Summary$coefficients[[1]]
# m1_Summary$coefficients[[2]]
# 
# coef(lm(MEAN_NDVI ~ AEI_select, data = DF))
# 
# + geom_abline(intercept = 37, slope = -5)



