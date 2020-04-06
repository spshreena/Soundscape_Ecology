install.packages("soundecology")
install.packages("seewave")
install.packages("tuneR")
install.packages("tidyverse")



library(tidyverse)
library(soundecology)
library(tuneR)
library(seewave)


#https://cran.r-project.org/web/packages/soundecology/vignettes/intro.html
#https://marce10.github.io/2019/01/12/phylo_spectro_function.html
## https://cran.r-project.org/web/packages/soundecology/soundecology.pdf

setwd("D:/RA-withSangermano/Miles_and_Shreena")


#create single plot spectrogram from wav
sound_raster(wavfile = "CC_20190702_060000.wav",
             max_freq=10000)
## End Plot Spectrogram


#Calcualte Spectrogram for all files in folder
#read all wavs as vector
allwavs1 <- list.files(pattern = "\\.wav$")

#loop through files
for (i in 1:length(allwavs1)) {
  sound_raster(wavfile = allwavs1[i], max_freq=10000)
}
## End Plot Spectrogram for all files


##Sets working directory to location of missing datasets from Flor
setwd("D:/RA-withSangermano/Miles_and_Shreena")
dir()
#remaining code calculates NDSI/Anthrophony/Biophony for missing sites 

## calculate indices independently using waveobjects  
#Load single file as a waveobject object called soundfile
soundfile <- readWave("CC_20190702_060000.wav", from = 0, to = 60, units = "seconds")

#Run the function on this waveobject and save the results in a new variable called "soundfile.aci"
soundfile.NDSI <- ndsi(soundfile, fft_w = 512, anthro_min = 1000, anthro_max = 2000,
                       bio_min = 2000, bio_max = 11000)
#Print the NDSI value for the left channel of the wav file, stored in soundfile.aci
print(soundfile.NDSI$anthrophony_left)
print(soundfile.NDSI$biophony_left)
#Save all results as csv
write.csv(soundfile.NDSI,'soundfile.ndsi.csv', append = TRUE)
## End Calc indices


# Loop calculate NDSI indices for all files in folder
#read all wavs as vector
allwavs3 <- list.files(pattern = "\\.wav$")

#create initial outputfile


#loop through files
#If the code doesn't run correctly(repeats the same file over and over again, check the code below and confirm that the file names are consistent throughout)
for (i in 1:length(allwavs3)){
  
  #Load all files from vector as a waveobject called soundfile_es
  soundfile_es <- readWave(allwavs3[i], from = 0, to = 60, units = "seconds")
  #Calcualte index -- for other indices replace this line of code with notes below
  soundfile_NDSI_extrasites <- ndsi(soundfile_es, fft_w = 512, anthro_min = 1000, anthro_max = 2000,
                         bio_min = 2000, bio_max = 11000)
  #write index as csv
  write.table(soundfile_NDSI_extrasites, file = 'soundfile_NDSI_extrasites.csv', append = TRUE, quote = FALSE, sep = ",",
              na = "NA", dec = ".", row.names = allwavs3[i], col.names = NA)
  
}
#remove extra col titles

setwd("D:/RA-withSangermano/Miles_and_Shreena")
dat_extra <- read.csv("soundfile_NDSI_extrasites.csv")
toDelete <- seq(1, nrow(dat_extra), 2)
data_extra <- dat_extra[ toDelete ,]

write.table(data_extra, file = 'soundfile_NDSI_es_clean.csv', append = TRUE, quote = FALSE, sep = ",",
            na = "NA", dec = ".", row.names = T, col.names = NA)

## delete columns with NA
cols.dont.want <- c("ndsi_right", "biophony_right", "anthrophony_right") # if you want to remove multiple columns

data_ES <- data_extra[, !names(data_extra) %in% cols.dont.want, drop = F]

#Fix the rownames in the dataset using 
#1:the dimensions of the dataset(dim(data))[1]

rownames(data_ES) = 1:dim(data_ES)[1]

#Write csv using cleaned data 
write.table(data_ES, file = 'soundfile_NDSI_es_NA_removed_clean.csv', append = TRUE, quote = FALSE, sep = ",",
            na = "NA", dec = ".", row.names = T, col.names = NA)


#Fix the rownames in the dataset using 
#1:the dimensions of the dataset(dim(data))[1]

rownames(data_ES) = 1:dim(data_ES)[1]

# Combine the NDSI dataset with the new soundfile_NDSI_extrasites
soundfile_NDSI_ALLSITES <- rbind(cleaned_Data_NDSI, data_ES)

# Write csv file for cleaned NDSI with missing sites 
write.csv(soundfile_NDSI_ALLSITES, file = "Cleaned_NDSI_AllSites.csv")

















#Acoustic Complexity Index - ACI
#soundfile.ACI <- acoustic_complexity(soundfile, fft_w = 512, anthro_min = 1000, anthro_max = 2000,
#bio_min=2000, bio_max = 11000)


# Loop calculate ASI indices for all files in folder
#read all wavs as vector
allwavs2 <- list.files(pattern = "\\.wav$")

#create initial outputfile

#loop through files
for (i in 1:length(allwavs2)){
  
  #Load all files from vector as a waveobject called soundfile
  soundfile <- readWave(allwavs2[i], from = 0, to = 60, units = "seconds")
  #Calcualte index -- for other indices replace this line of code with notes below
  soundfile_ACI <- acoustic_complexity(soundfile, fft_w = 512, min_freq=2000, max_freq = 11000)
  #write index as csv
  write.table(soundfile_ACI, file = 'soundfile_ACI.csv', append = TRUE, quote = FALSE, sep = ",",
              na = "NA", dec = ".", row.names = allwavs2[i], col.names = NA)
}
#remove extra col titles

dat2 <- read.csv("soundfile_ACI.csv")
toDelete <- seq(1, nrow(dat2), 2)
dataACI<- dat2[ toDelete ,]

write.table(data, file = 'soundfile_ACIsp1_clean.csv', append = TRUE, quote = FALSE, sep = ",",
            na = "NA", dec = ".", row.names = T, col.names = NA)

## delete columns with NA
cols.dont.want <- c("aci_right", "biophony_right", "anthrophony_right") # if you want to remove multiple columns

data2 <- dataACI[, ! names(dataACI) %in% cols.dont.want, drop = F]
write.table(dataACI, file = 'soundfile_ACI_NA_removed_clean.csv', append = TRUE, quote = FALSE, sep = ",",
            na = "NA", dec = ".", row.names = T, col.names = NA)


#Fix the rownames in the dataset using 
#1:the dimensions of the dataset(dim(data))[1]

rownames(dataACI) = 1:dim(dataACI)[1]





# 2- Bioacoustic Index - BI      
#  soundfile.BI <- bioacoustic_index(soundfile, fft_w = 512, min_freq=2000, max_freq = 8000)



# Loop calculates BI indices for all files in folder

#create initial outputfile

#loop through files

allwavs2 <- list.files(pattern = "\\.wav$")

for (i in 1:length(allwavs2)){
  
  #Load all files from vector as a waveobject called soundfile
  soundfile <- readWave(allwavs2[i], from = 0, to = 60, units = "seconds")
  #Calcualte index -- for other indices replace this line of code with notes below
  soundfile_BI <- bioacoustic_index(soundfile, fft_w = 512, min_freq=2000, max_freq = 8000)
  #write index as csv
  write.table(soundfile_BI, file = 'soundfile_BI.csv', append = TRUE, quote = FALSE, sep = ",",
              na = "NA", dec = ".", row.names = allwavs2[i], col.names = NA)
}


#remove extra row titles

dat3 <- read.csv("soundfile_BI.csv")
toDelete <- seq(1, nrow(dat3), 2)
dataBI<- dat3[toDelete,]

write.table(dataBI, file = 'soundfile_BI_clean.csv', append = TRUE, quote = FALSE, sep = ",",
            na = "NA", dec = ".", row.names = T, col.names = NA)

## delete columns with NA
cols.dont.want <- c("right_area") # if you want to remove multiple columns

dataBI_Cleaned <- dataBI[, !names(dataBI) %in% cols.dont.want, drop = F]
write.table(data3, file = 'soundfile_BI_removed_clean.csv', append = TRUE, quote = FALSE, sep = ",",
            na = "NA", dec = ".", row.names = T, col.names = NA)


#Fix the rownames in the dataset using 
#1:the dimensions of the dataset(dim(data))[1]

rownames(dataBI_Cleaned) = 1:dim(dataBI_Cleaned)[1]






# 3- Acoustic Diversity Index - ADI
# acoustic_diversity(soundfile, max_freq = 10000, db_threshold = -50, freq_step = 1000, shannon = TRUE)


# Loop calculates BI indices for all files in folder

#create initial outputfile

#loop through files


allwavs2 <- list.files(pattern = "\\.wav$")


for (i in 1:length(allwavs2)){
  
  #Load all files from vector as a waveobject called soundfile
  soundfile <- readWave(allwavs2[i], from = 0, to = 60, units = "seconds")
  #Calcualte index -- for other indices replace this line of code with notes below
  soundfile_ADI <- acoustic_diversity(soundfile, max_freq = 10000, db_threshold = -50, freq_step = 1000, shannon = TRUE)
  #write index as csv
  
  write.table(soundfile_ADI, file = 'soundfile_ADI.csv', append = TRUE, quote = FALSE, sep = ",",
              na = "NA", dec = ".", row.names = allwavs2[i], col.names = NA)
}


#remove extra row titles

datADI <- read.csv("soundfile_ADI.csv")
toDelete <- seq(1, nrow(datADI), 2)
dataADI<- datADI[toDelete,]

write.table(dataADI, file = 'soundfile_ADI_clean.csv', append = TRUE, quote = FALSE, sep = ",",
            na = "NA", dec = ".", row.names = T, col.names = NA)

## delete columns with NA
cols.dont.want <- c("right_area") # if you want to remove multiple columns

dataADI_Cleaned <- dataADI[, !names(dataADI) %in% cols.dont.want, drop = F]
write.table(dataADI, file = 'soundfile_ADI_removed_clean.csv', append = TRUE, quote = FALSE, sep = ",",
            na = "NA", dec = ".", row.names = T, col.names = NA)


#Fix the rownames in the dataset using 
#1:the dimensions of the dataset(dim(data))[1]

rownames(dataADI_Cleaned) = 1:dim(dataADI_Cleaned)[1]





# 4- Acoustic Evenness - AEI
#acoustic_evenness(soundfile, max_freq = 10000, db_threshold = -50, freq_step = 1000)


allwavs2 <- list.files(pattern = "\\.wav$")


for (i in 1:length(allwavs2)){
  
  #Load all files from vector as a waveobject called soundfile
  soundfile <- readWave(allwavs2[i], from = 0, to = 60, units = "seconds")
  #Calcualte index -- for other indices replace this line of code with notes below
  soundfile_AEI <- acoustic_evenness(soundfile, max_freq = 10000, db_threshold = -50, freq_step = 1000)
  #write index as csv
  
  write.table(soundfile_AEI, file = 'soundfile_AEI.csv', append = TRUE, quote = FALSE, sep = ",",
              na = "NA", dec = ".", row.names = allwavs2[i], col.names = NA)
}


#remove extra row titles

datAEI <- read.csv("soundfile_AEI.csv")
toDelete <- seq(1, nrow(datAEI), 2)
dataAEI<- datAEI[toDelete,]

write.table(dataAEI, file = 'soundfile_AEI_clean.csv', append = TRUE, quote = FALSE, sep = ",",
            na = "NA", dec = ".", row.names = T, col.names = NA)

## delete columns with NA
cols.dont.want <- c("aei_right") # if you want to remove multiple columns

dataAEI_Cleaned <- dataAEI[, !names(dataADI) %in% cols.dont.want, drop = F]
write.table(dataAEI, file = 'soundfile_AEI_removed_clean.csv', append = TRUE, quote = FALSE, sep = ",",
            na = "NA", dec = ".", row.names = T, col.names = NA)


#Fix the rownames in the dataset using 
#1:the dimensions of the dataset(dim(data))[1]

rownames(dataAEI_Cleaned) = 1:dim(dataAEI_Cleaned)[1]





#Subsetting the data list of NDSI sites by site of recording

data_BMB_NDSI <- soundfile_ndsi_clean[c(1:99),]
data_BP_NDSI <- soundfile_ndsi_clean[c(100:189),]
data_CC_NDSI <- soundfile_ndsi_clean[c(190:279),]
data_FR_NDSI <- soundfile_ndsi_clean[c(280:351),]
data_L_NDSI <- soundfile_ndsi_clean[c(352:423),]
data_PM_NDSI <- soundfile_ndsi_clean[c(424:512),]
data_RB_NDSI <- soundfile_ndsi_clean[c(513:602),]
data_W_NDSI <- soundfile_ndsi_clean[c(603:674),]
data_WM_NDSI <- soundfile_ndsi_clean[c(675:755),]


#Subsetting the data list of ACI sites by site of recording

data_BMB_ACI <- soundfile_ACI_clean[c(1:99),]
data_BP_ACI <- soundfile_ACI_clean[c(100:189),]
data_CC_ACI <- soundfile_ACI_clean[c(190:279),]
data_FR_ACI <- soundfile_ACI_clean[c(280:351),]
data_L_ACI <- soundfile_ACI_clean[c(352:423),]
data_PM_ACI <- soundfile_ACI_clean[c(424:512),]
data_RB_ACI <- soundfile_ACI_clean[c(513:602),]
data_W_ACI <- soundfile_ACI_clean[c(603:674),]
data_WM_ACI <- soundfile_ACI_clean[c(675:755),]


#Subsetting the data list of ADI sites by site of recording

data_BMB_ADI <- soundfile_ADI_clean[c(1:99),]
data_BP_ADI <- soundfile_ADI_clean[c(100:189),]
data_CC_ADI <- soundfile_ADI_clean[c(190:279),]
data_FR_ADI <- soundfile_ADI_clean[c(280:351),]
data_L_ADI <- soundfile_ADI_clean[c(352:423),]
data_PM_ADI <- soundfile_ADI_clean[c(424:512),]
data_RB_ADI <- soundfile_ADI_clean[c(513:602),]
data_W_ADI <- soundfile_ADI_clean[c(603:674),]
data_WM_ADI <- soundfile_ADI_clean[c(675:755),]

#Subsetting the data list of AEI sites by site of recording

data_BMB_AEI <- soundfile_AEI_clean[c(1:99),]
data_BP_AEI <- soundfile_AEI_clean[c(100:189),]
data_CC_AEI <- soundfile_AEI_clean[c(190:279),]
data_FR_AEI <- soundfile_AEI_clean[c(280:351),]
data_L_AEI <- soundfile_AEI_clean[c(352:423),]
data_PM_AEI <- soundfile_AEI_clean[c(424:512),]
data_RB_AEI <- soundfile_AEI_clean[c(513:602),]
data_W_AEI <- soundfile_AEI_clean[c(603:674),]
data_WM_AEI <- soundfile_AEI_clean[c(675:755),]

#Subsetting the data list of BI sites by site of recording

data_BMB_BI <- soundfile_BI_clean[c(1:99),]
data_BP_BI <- soundfile_BI_clean[c(100:189),]
data_CC_BI <- soundfile_BI_clean[c(190:279),]
data_FR_BI <- soundfile_BI_clean[c(280:351),]
data_L_BI <- soundfile_BI_clean[c(352:423),]
data_PM_BI <- soundfile_BI_clean[c(424:512),]
data_RB_BI <- soundfile_BI_clean[c(513:602),]
data_W_BI <- soundfile_BI_clean[c(603:674),]
data_WM_BI <- soundfile_BI_clean[c(675:755),]




#convert dataframes to numeric


#Code below mass converts every column in the data to factor but removes the title data in the process
# data_NDSI <-mutate_if(data[, 2:4], is.factor, ~ as.numeric(as.character(.x)))
# 
# data_NDSI 
# 
# 
# indx <- sapply(data[, 2:4], is.factor)
# data[indx] <- lapply(data[indx], function(x) as.numeric(as.character(x)))

dataAEI[,'aei_left'] <- as.numeric(as.character(dataAEI[,'aei_left']))

cleaned_Data_AEI <- dataAEI
cleaned_Data_NDSI <- soundfile_ndsi_clean
cleaned_Data_BI <- soundfile_BI_removed_clean


#Note- Indices

# 1- Acoustic Complexity Index - ACI
#soundfile.ACI <- acoustic_complexity(soundfile, fft_w = 512, min_freq=2000, max_freq = 11000)
# 2- Bioacoustic Index - BI      
#  soundfile.BI <- bioacoustic_index(soundfile, fft_w = 512, min_freq=2000, max_freq = 8000)
# 3- Acoustic Diversity Index - ADI
# acoustic_diversity(soundfile, max_freq = 10000, db_threshold = -50, freq_step = 1000, shannon = TRUE)
# 4- Acoustic Evenness - AEI
#acoustic_evenness(soundfile, max_freq = 10000, db_threshold = -50, freq_step = 1000)





##Run Batch Indices Example -- these indices do not save complete output table
## https://cran.r-project.org/web/packages/soundecology/soundecology.pdf

multiple_sounds(directory = "D:/RA-withSangermano/MA-Acoustic-DATA/4to6am",
                resultfile = "D:/RA-withSangermano/MA-Acoustic-DATA/Revised_AEI.csv",
                soundindex = "acoustic_evenness",  max_freq = 10000, db_threshold = -50, freq_step = 1000)

multiple_sounds(directory = "D:/RA-withSangermano/MA-Acoustic-DATA/4to6am",
                resultfile = "D:/RA-withSangermano/MA-Acoustic-DATA/Revised_ACI.csv",
                soundindex = "acoustic_complexity",  fft_w = 1024, max_freq = 12000, min_freq = 2000)

multiple_sounds(directory = "D:/RA-withSangermano/MA-Acoustic-DATA/4to6am",
                resultfile = "D:/RA-withSangermano/MA-Acoustic-DATA/4to6am/Revised_ADI.csv",
                soundindex = "acoustic_diversity",  from = 0, to = 60, units = "seconds", max_freq = 10000, db_threshold = -50,
                freq_step = 1000, shannon = TRUE)

multiple_sounds(directory = "D:/RA-withSangermano/MA-Acoustic-DATA/4to6am",
                resultfile = "D:/RA-withSangermano/MA-Acoustic-DATA/Revised_BI.csv",
                soundindex = "bioacoustic_index",  from = 0, to = 60, units = "seconds", min_freq= 2000, max_freq = 10000, fft_w = 512)

multiple_sounds(directory = "D:/RA-withSangermano/MA-Acoustic-DATA/4to6am",
                resultfile = "D:/RA-withSangermano/MA-Acoustic-DATA/Entropy_index.csv",
                soundindex = "H",  from = 0, to = 60, units = "seconds")
# Multiple SOunds  

# df_evt<- read.csv('D:/RA-withSangermano/Miles_and_Shreena/CSV/NDVI_NDBI_PERBUFFER.csv')
ACI<- read.csv('D:/RA-withSangermano/Miles_and_Shreena/CSV/ACI_BMB_2000-12000.csv')
ADI <- read.csv('D:/RA-withSangermano/Miles_and_Shreena/CSV/acoustic_diversity1.csv')
NDSI <- read.csv('D:/RA-withSangermano/Miles_and_Shreena/CSV/Cleaned_NDSI_AllSites.csv')
AEI<- read.csv('D:/RA-withSangermano/Miles_and_Shreena/CSV/soundfile_AEI_clean.csv')
BI<- read.csv('D:/RA-withSangermano/Miles_and_Shreena/CSV/soundfile_BI_removed_clean.csv')
