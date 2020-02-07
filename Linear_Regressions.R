df_evt<- read.csv('D:/RA-withSangermano/CSV/NDVI_NDBI_PERBUFFER.csv')
ACI<- read.csv('D:/RA-withSangermano/CSV/ACI_BMB_2000-12000.csv')
ADI <- read.csv('D:/RA-withSangermano/CSV/acoustic_diversity1.csv')
#NDSI_BMB <- read.csv('D:/RA-withSangermano/CSV/NDSI_BMB_default.csv')
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

#ADD NDSI, Biophony, Antrophony 

#create Acoustic dataframe
df_sounds<- data.frame(filenames, ACI_select, ADI_select, AEI_select, BI_select )

BP <- 'BP'
dat_BP <- subset(df_sounds, grepl(BP, filenames))

#select by site
names <- c('BMB', 'BP', 'CC', 'FR', 'L_', 'PM', 'RB', 'W_','WM', 'EL', 'LM')

for (i in length (df_sounds)){
  if (names == names[1]){
    dat_BMB <- subset(df_sounds, grepl(names[1], filenames))
  }else {
    dat_BP <- subset(df_sounds, grepl(names[2], filenames)) 
  }else{
    dat_CC <- subset(df_sounds, grepl(names[3], filenames))
  }else{
    dat_FR <- subset(df_sounds, grepl(names[4], filenames))
  }else{
    dat_L <- subset(df_sounds, grepl(names[5], filenames))
  }else {
    dat_PM <- subset(df_sounds, grepl(names[6], filenames))
  }else{
    dat_RB <- subset(df_sounds, grepl(names[7], filenames))
  }else{
    dat_W <- subset(df_sounds, grepl(names[8], filenames))
  }else{
    dat_WM <- subset(df_sounds, grepl(names[9], filenames))
  }else{
    dat_EL <- subset(df_sounds, grepl(names[10], filenames))
  }else{
    dat_LM <- subset(df_sounds, grepl(names[11], filenames))
  }
}

#NEED TO FIX dat_EL it has double values************************************************
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
#filp row column
df_sound_mean_transpose <- as.data.frame(t(as.matrix(df_sound_mean)))
#---------------------------------------------------------------------------------------------
#Enviornmental Factors
#DATA FRAME per buffer
m500<- select(filter(df_evt, Buffer_Meter == 500),c(SITE, MEAN_NDVI, MEAN_NDBI))
m1000<- select(filter(df_evt, Buffer_Meter == 1000),c(SITE, MEAN_NDVI, MEAN_NDBI))
m1500<- select(filter(df_evt, Buffer_Meter == 1500),c(SITE, MEAN_NDVI, MEAN_NDBI))
m2000<- select(filter(df_evt, Buffer_Meter == 2000),c(SITE, MEAN_NDVI, MEAN_NDBI))
m2500<- select(filter(df_evt, Buffer_Meter == 2500),c(SITE, MEAN_NDVI, MEAN_NDBI))
m3000<- select(filter(df_evt, Buffer_Meter == 3000),c(SITE, MEAN_NDVI, MEAN_NDBI))
#--------------------------------------------------------------------------------------------------
#Linear Regressions 


