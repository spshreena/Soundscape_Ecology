#Feb/4/2020
#Exploring relationship between envt factors and sound ecology 

#Libraries
library(tidyverse)
library(tidyselect)
library(soundecology)
library(tuneR)
library(seewave)

# import CSV
df_evt<- read.csv('D:/RA-withSangermano/SpatialData/NDVI_NDBI_PERBUFFER.csv')
ACI<- read.csv('D:/RA-withSangermano/SpatialData/ACI_BMB_2000-12000.csv')
ADI <- read.csv('D:/RA-withSangermano/SpatialData/acoustic_diversity1.csv')
NDSI_BMB <- read.csv('D:/RA-withSangermano/SpatialData/NDSI_BMB_default.csv')
#AEI<- read.csv('D:/RA-withSangermano/SpatialData/soundfile_AEI_clean.csv')
BI<- read.csv('D:/RA-withSangermano/SpatialData/soundfile_BI_removed_clean.csv')

#------------------------------------------------------------------------------------------
#Acoustic data cleanup
#create new data frame with only acoustic index 
ACI_select<- ACI$LEFT_CHANNEL
ADI_select<- ADI$LEFT_CHANNEL
NDSI_BMB_select<- NDSI_BMB$LEFT_CHANNEL
BI_select<- BI$left_area
filenames<- ACI$FILENAME
#ADD AEI 
#ADD NDSI, Biophony, Antrophony 
#add missing points
#create Acoustic dataframe
df_sounds<- data.frame(filenames, ACI_select, ADI_select, NDSI_BMB_select, BI_select )

BP <- 'BP'
dat_BP <- subset(df_sounds, grepl(BP, filenames))

#select by site
names <- c('BMB', 'BP', 'CC', 'FR', 'L', 'PM', 'RB', 'W_','WM')

for (i in length (df_sounds)){
  if (names == names[1]){
    dat_BMB <- subset(df_sounds, grepl(names[1], filenames))
  }else {
    dat_BP <- subset(df_sounds, grepl(names[2], filenames)) 
  }else {
    dat_CC <- subset(df_sounds, grepl(names[3], filenames))
  }else {
    dat_FR <- subset(df_sounds, grepl(names[4], filenames))
  }else {
    dat_L <- subset(df_sounds, grepl(names[5], filenames))
  }else {
    dat_PM <- subset(df_sounds, grepl(names[6], filenames))
  }else{
    dat_RB <- subset(df_sounds, grepl(names[7], filenames))
  }else{
    dat_W <- subset(df_sounds, grepl(names[8], filenames))
  }else{
    dat_WM <- subset(df_sounds, grepl(names[9], filenames))
  }
}
#mean acoustic index per site
mean_BMB <- sapply(dat_BMB, mean)
mean_BP <- sapply(dat_BP, mean)
mean_CC <- sapply(dat_CC, mean)
mean_FR <- sapply(dat_FR, mean)
mean_L <- sapply(dat_L, mean)
mean_PM <- sapply(dat_PM, mean)
mean_RB <- sapply(dat_RB, mean)
mean_W <- sapply(dat_W, mean)
mean_WM <- sapply(dat_WM, mean)

#data frame
df_sound_mean<- data.frame(mean_BMB, mean_BP, mean_CC, mean_FR, mean_L, mean_PM, mean_RB, mean_W, mean_WM )
#---------------------------------------------------------------------------------------------
