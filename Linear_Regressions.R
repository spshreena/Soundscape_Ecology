#Feb/4/2020
#Exploring relationship between envt factors and sound ecology 

#Libraries
library(tidyverse)
library(tidyselect)
library(ggplot2)
library(soundecology)
library(tuneR)
library(seewave)

# import CSV
df_evt<- read.csv('D:/RA-withSangermano/CSV/NDVI_NDBI_PERBUFFER.csv')
ACI<- read.csv('D:/RA-withSangermano/CSV/ACI_BMB_2000-12000.csv')
ADI <- read.csv('D:/RA-withSangermano/CSV/acoustic_diversity1.csv')
NDSI <- read.csv('D:/RA-withSangermano/CSV/NDSI_bio_ant.csv')
AEI<- read.csv('D:/RA-withSangermano/CSV/soundfile_AEI_clean.csv')
BI<- read.csv('D:/RA-withSangermano/CSV/soundfile_BI_removed_clean.csv')

#------------------------------------------------------------------------------------------
#Acoustic data cleanup
#create new data frame with only acoustic index 
ACI_select<- ACI$LEFT_CHANNEL
ADI_select<- ADI$LEFT_CHANNEL
AEI_select<- AEI$aei_left
BI_select<- BI$left_area
filenames<- ACI$FILENAME
NDSI_select<- NDSI$ndsi_left
BIO_select<- NDSI$biophony_left
ANT_select <- NDSI$anthrophony_left

#ADD NDSI, Biophony, Antrophony 

#create Acoustic dataframe
df_sounds<- data.frame(filenames, ACI_select, ADI_select, AEI_select, BI_select, NDSI_select, BIO_select, ANT_select)

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
write.table(df_all, file = "all_indices_updated.csv", append = TRUE, quote = FALSE, sep = ",",
            na = 'NA', dec = ".", row.names = T, col.names = NA)
write.csv(df_all, file = "ALL_INDICES.csv")
#I fixed the rows and columns CSV in EXCEL--> in common folder
