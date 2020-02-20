#Libraries
library(tidyverse)
library(tidyselect)
library(ggplot2)
setwd('D:/RA-withSangermano/CSV')

# import CSV
df_evt<- read.csv('D:/RA-withSangermano/CSV/NDVI_NDBI_PERBUFFER.csv')
ACI<- read.csv("Revised_ACI.csv")
ADI <- read.csv("Revised_ACI.csv" )
NDSI <- read.csv( "Revised_NDSI.csv")
AEI<- read.csv("Revised_AEI.csv")
BI<- read.csv("Revised_BI.csv")

#------------------------------------------------------------------------------------------
#Acoustic data cleanup
#create new data frame with only acoustic index 
ACI_select<- ACI$LEFT_CHANNEL
ADI_select<- ADI$LEFT_CHANNEL
AEI_select<- AEI$LEFT_CHANNEL
BI_select<- BI$LEFT_CHANNEL
filenames<- NDSI$X
NDSI_select<- NDSI$ndsi_left
BIO_select<- NDSI$biophony_left
ANT_select <- NDSI$anthrophony_left


#create Acoustic dataframe
df_sounds<- data.frame(filenames, NDSI_select, BIO_select, ANT_select, ADI_select,AEI_select, BI_select)

#select by site
names <- c('BMB', 'BP', 'CC', 'FR', 'L_', 'PM', 'RB', 'W_','WM', 'EL', 'LM')

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
dat_L<- dat_L[!grepl("EL", dat_L$filenames),]
# these didnt work in loop
BP <- 'BP'
dat_BP <- subset(df_sounds, grepl(BP, filenames))

LM<- 'LM'
dat_LM <- subset(df_sounds, grepl(LM, filenames))
BMB<- 'BMB'
dat_BMB <- subset(df_sounds, grepl(BMB, filenames))

#mean acoustic index per site
mean_BMB <-sapply(dat_BMB, mean)
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
df_sounds_mean<- data.frame(mean_LM, mean_BMB,mean_BP, mean_PM, mean_RB, mean_CC, mean_EL, mean_WM, mean_W, mean_FR, mean_L )
#filp row column
df_sound_mean_transpose <- as.data.frame(t(as.matrix(df_sounds_mean)))
#---------------------------------------------------------------------------------------------
#Enviornmental Factors
#DATA FRAME per buffer
m500<- select(filter(df_evt, Buffer_Meter == 500),c(SITE, MEAN_NDVI, MEAN_NDBI))
m1000<- select(filter(df_evt, Buffer_Meter == 1000),c(SITE, MEAN_NDVI, MEAN_NDBI))
m1500<- select(filter(df_evt, Buffer_Meter == 1500),c(SITE, MEAN_NDVI, MEAN_NDBI))
m2000<- select(filter(df_evt, Buffer_Meter == 2000),c(SITE, MEAN_NDVI, MEAN_NDBI))
m2500<- select(filter(df_evt, Buffer_Meter == 2500),c(SITE, MEAN_NDVI, MEAN_NDBI))
m3000<- select(filter(df_evt, Buffer_Meter == 3000),c(SITE, MEAN_NDVI, MEAN_NDBI))

df_all <-cbind(df_sound_mean_transpose, m500, m1000, m1500, m2000, m2500, m3000)
write.table(df_all, file = "Shreena_2_20_20.csv", append = TRUE, quote = FALSE, sep = ",",
            na = 'NA', dec = ".", row.names = T, col.names = NA)
write.csv(df_all, file = "Shreena_2_20")
#I fixed the rows and columns CSV in EXCEL--> in common folder

#-------------------------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(nlme)

setwd("D:/RA-withSangermano/CSV")
df<- read.csv("SP.csv")

#ACI
lm_ACI_NDVI<- lmList(ACI ~ MEAN_NDVI | Buffer, data=df)
ACI_NDVI_lm<- summary(lm_ACI_NDVI)$r.squared 

lm_ACI_NDBI<- lmList(ACI_select ~ MEAN_NDBI | Buffer, data=df)
ACI_NDBI_lm<- summary(lm_ACI_NDBI)$r.squared

#--------------------------------------------------------------------------------------------------------
#BI
lm_BI_NDVI<- lmList(BI_select ~ MEAN_NDVI | Buffer, data=df)
BI_NDVI_lm<-summary(lm_BI_NDVI)$r.squared 

lm_BI_NDBI<- lmList(BI ~ MEAN_NDBI | Buffer, data=df)
BI_NDBI_lm<- summary(lm_BI_NDBI)$r.squared
#--------------------------------------------------------------------------------------------------------
# ADI
lm_ADI_NDVI<- lmList(ADI ~ MEAN_NDVI | Buffer, data=df)
ADI_NDVI_lm<- summary(lm_ADI_NDVI)$r.squared 

lm_ADI_NDBI<- lmList(ADI ~ MEAN_NDBI | Buffer, data=df)
ADI_NDBI_lm<- summary(lm_ADI_NDBI)$r.squared

#------------------------------------------------------------------------------------------------------
#AEI #results same as BI-- need to figure out why
lm_AEI_NDVI<- lmList(AEI ~ MEAN_NDVI | Buffer, data=df)
summary(lm_AEI_NDVI)$r.squared

#----------------------------------------------------------------------------------------------------------
#NDSI
lm_NDSI_NDVI<- lmList(NDSI_select ~ MEAN_NDVI | Buffer, data=df)
NDSI_NDVI_lm<-summary(lm_NDSI_NDVI)$r.squared

lm_NDSI_NDBI<- lmList(NDSI ~ MEAN_NDBI | Buffer, data=df)
NDSI_NDBI_lm<-summary(lm_NDSI_NDBI)$r.squared
#---------------------------------------------------------------------------------------------------------
#Biophony
lm_NDSI_NDBIO_NDVI<- lmList(BIO_select ~ MEAN_NDVI | Buffer, data=df)
BIO_NDVI_lm<-summary(lm_NDSI_NDBIO_NDVI)$r.squared

lm_NDSI_NDBIO_NDBI<- lmList(BIO ~ MEAN_NDBI | Buffer, data=df)
BIO_NDBI_lm<-summary(lm_NDSI_NDBIO_NDBI)$r.squared

lm_NDSI_NDANT_NDVI<- lmList(ANT_select ~ MEAN_NDVI | Buffer, data=df)
ANT_NDVI_lm<-summary(lm_NDSI_NDANT_NDVI)$r.squared

lm_NDSI_NDBIO_NDBI<- lmList(BIO ~ MEAN_NDBI | Buffer, data=df)
BIO_NDBI_lm<-summary(lm_NDSI_NDBIO_NDBI)$r.squared

#-----------------------------------------------------------------------------------------------------------------
buffername <- c(500, 1000,1500,2000,2500,3000)
linear_rsquared <- data.frame(buffername,  NDSI_NDVI_lm, BIO_NDVI_lm,ANT_NDVI_lm)
#-------------------------------------------------------------------------------------------------------------------------
#Graphs
#graph of R^2
install.packages('digest')
ggplot(data = linear_rsquared) + 
  #geom_line (aes(x=linear_rsquared$buffername, y=linear_rsquared$ACI_NDVI_lm, color = 'ACI'))+
  #geom_line (aes(x=linear_rsquared$buffername, y=linear_rsquared$BI_NDVI_lm, color = 'BI'))+
 # geom_line (aes(x=linear_rsquared$buffername, y=linear_rsquared$ADI_NDVI_lm, color = 'ADI'))+
  geom_line (aes(x=linear_rsquared$buffername, y=linear_rsquared$NDSI_NDVI_lm, color = 'NDSI'))+
  geom_line (aes(x=linear_rsquared$buffername, y=linear_rsquared$BIO_NDVI_lm, color = 'BIOPHONY'))+
  geom_line (aes(x=linear_rsquared$buffername, y=linear_rsquared$ANT_NDVI_lm, color = 'ANTHRO'))+
  labs(title = "Mean NDVI ~ R squared of metrics",x ="Focal Distance (meters)", y = "R squared of metrics")

#graph of metric vs NDVI/NDBI

#ACI

#NDVI
ggplot(data = df, 
       aes (x = MEAN_NDVI, y = ACI, group=Buffer, color = factor(Buffer))) +
  geom_smooth(method = "lm", se=F) +
  geom_point(alpha=0) + 
  geom_point(aes(color = factor(Buffer)))+
  theme(legend.position = "right") +
  labs(y="ACI", x = "NDVI")


#graph of NDSI
#NDVI
ggplot(data = df, aes (x = MEAN_NDVI, y = NDSI,group=Buffer, color = factor(Buffer))) +
  geom_smooth(method = "lm", se=F) +
  geom_point(alpha=0) + 
  geom_point(aes(color = factor(Buffer)))+
  theme(legend.position = "right") +
  labs(y="NDSI", x = "NDVI")



#graph of BI
#NDVI
ggplot(data = df, 
       aes (x = MEAN_NDVI, y = BI,group=Buffer, color = factor(Buffer))) +
  geom_smooth(method = "lm", se=F) +
  geom_point(alpha=0) + 
  geom_point(aes(color = factor(Buffer)))+
  theme(legend.position = "right") +
  labs(y="BI", x = "NDVI")

#Biophony
ggplot(data = df, 
       aes (x = MEAN_NDVI, y = BIO,group=Buffer, color = factor(Buffer))) +
  geom_smooth(method = "lm", se=F) +
  geom_point(alpha=0) + 
  geom_point(aes(color = factor(Buffer)))+
  theme(legend.position = "right") +
  labs(y="BIOPHONY", x = "NDVI")


#Antrophony
ggplot(data = df, 
       aes (x = MEAN_NDVI, y = ANT,group=Buffer, color = factor(Buffer))) +
  geom_smooth(method = "lm", se=F) +
  geom_point(alpha=0) + 
  geom_point(aes(color = factor(Buffer)))+
  theme(legend.position = "right") +
  labs(y="ANT", x = "NDVI")
