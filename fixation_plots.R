#File: fixation_plots.R
#Programmer: Stephen A. Gonzalez
#Date Created: 4/29/2022
#Purpose: Plot Fixation data

# ------------------- Load packages and Work space Setup --------------------------------
library(ggplot2)
library(patchwork)
library(dplyr)
library(reshape)
library(tidyverse)

library(groundhog)
groundhog.day <- "2022-04-27"
pkgs <- c('ggplot2',
          'patchwork',
          'dplyr',
          'reshape',
          "tidyverse")

groundhog.library(pkgs,groundhog.day)

rm(list = ls()) #Clear Workspace

all.neg <- function(x) -1*abs(x)

#Get SIDs
setwd("~/Desktop/graph-examples/data")
SIDs <- list.files()
SIDs[-(which(SIDs == "bad"))] #Remove 'Bad' File Folder
SIDs <- as.numeric(SIDs) #Change to Numeric
SIDs <- na.omit(SIDs) #Remove NA's
SIDs <- as.data.frame(SIDs) #Convert to Dataframe
SIDs <- SIDs[order(SIDs),] #Order Data

#======================================================================================


# ---------------------------------- Set up Bins and datasets ---------------------------------------
#First Fixation Duration Vs. EV L - R
firstFixBins <- seq(0,6,1)

# WIN Condition
all_firstFix_binCount_win <- matrix(-1,length(firstFixBins)-1,length(SIDs))
all_Avg_fixDur_win <- matrix(-1,length(firstFixBins)-1,length(SIDs))

# LOSS Condition
all_firstFix_binCount_loss <- matrix(-1,length(firstFixBins)-1,length(SIDs))
all_Avg_fixDur_loss <- matrix(-1,length(firstFixBins)-1,length(SIDs))


# Probability First Fix Best
# WIN Condition
all_firstFixBest_binCount_win <- matrix(-1,length(firstFixBins)-1,length(SIDs))
all_p_firstFixBest_win <- matrix(-1,length(firstFixBins)-1,length(SIDs))

# LOSS Condition
all_firstFixBest_binCount_loss <- matrix(-1,length(firstFixBins)-1,length(SIDs))
all_p_firstFixBest_loss <- matrix(-1,length(firstFixBins)-1,length(SIDs))


# Fixation Duration Vs. Fixation Type
all_Avg_fixDurType <- matrix(-1,4, length(SIDs))


# Net Fixation Duration
netFixBins <- seq(-10,10,2)

all_Avg_NetFixDur_win <- matrix(-1,length(netFixBins)-1,length(SIDs))
all_Avg_NetFixDur_loss <- matrix(-1,length(netFixBins)-1,length(SIDs))
#======================================================================================


# ---------------------------------- Data Set up ---------------------------------------
for (j in 1:length(SIDs)){
  #Read in Choice Data
  setwd('~/Desktop/graph-examples/data')
  data.choice_win <- read.csv(paste(SIDs[j], "/choice_", SIDs[j], "_win.csv", sep = ""))
  data.choice_loss <- read.csv(paste(SIDs[j], "/choice_", SIDs[j], "_loss.csv", sep = ""))
  
  #Read in Fixation Data
  data.eye_win <- read.csv(paste(SIDs[j],"/cleanFix_", SIDs[j], "_win.csv", sep = ""))
  data.eye_loss <- read.csv(paste(SIDs[j],"/cleanFix_", SIDs[j], "_loss.csv", sep = ""))
  
  ##########################
  #### FIRST FIXATION ####
  ##########################
  #### First Fix WIN ####
  # Win Best - Worst option
  data.choice_win$EV_BW_diff = abs(data.choice_win$p_left - data.choice_win$p_right)
  
  #Filter first Fixation for each trial
  #data.firstFix_win <- filter(data.eye_win, fix_num == 1)
  temp2 <- filter(data.eye_win, fix_num == 1)
  
  #Re-format dataframe for Subjects who don't look at either Target Stim
  temp <- data.frame(numeric(200),numeric(200),numeric(200),numeric(200),numeric(200),numeric(200),numeric(200),numeric(200))
  if (length(temp2$trial_number) < length(data.choice_win$trial_number)){
    for (ii in temp2$trial_number){
      row_counter <- which(temp2$trial_number == ii)
      temp[ii,] <- temp2[row_counter,]
    }
    colnames(temp) <- colnames(temp2)
    data.firstFix_win <- temp
    data.firstFix_win$subject_ID <- rep(SIDs[j],200)
    data.firstFix_win$trial_number <- seq(1,200,1)
  } else {
    #Filter first Fixation for each trial
    data.firstFix_win <- filter(data.eye_win, fix_num == 1)
  }

  
  #Assign Expected Values to fist fixation data set
  data.firstFix_win$EV_BW_diff <- data.choice_win$EV_BW_diff
  
  
  #Set up Data Frames
  firstFix_binCount_win <- rep(-1, length(firstFixBins)-1)
  Avg_fixDur_win <- rep(-1, length(firstFixBins)-1)
  
  
  ### Bin first Fix WIN ###
  for(i in 1:(length(firstFixBins)-1)){
    counter = i+1
    # Count the number of instances in each bin
    if(firstFixBins[counter]==firstFixBins[length(firstFixBins)]){
      df_curBin = filter(data.firstFix_win, EV_BW_diff >= firstFixBins[i] & EV_BW_diff <= firstFixBins[counter])
    } else {
      df_curBin = filter(data.firstFix_win, EV_BW_diff >= firstFixBins[i] & EV_BW_diff < firstFixBins[counter])
    }
    firstFix_binCount_win[i] <- length(df_curBin$subject_ID)
    Avg_fixDur_win[i] <- mean(df_curBin$fix_dur)
  }
  all_firstFix_binCount_win[,j] <- firstFix_binCount_win
  all_Avg_fixDur_win[,j] <- Avg_fixDur_win
  
  
  #### First Fix LOSS ####
  # loss EV_L - EV_R
  data.choice_loss$EV_BW_diff = all.neg(data.choice_loss$p_left) - all.neg(data.choice_loss$p_right)
  
  #Filter first Fixation for each trial
  #data.firstFix_loss <- filter(data.eye_loss, fix_num == 1)
  temp2 <- filter(data.eye_loss, fix_num == 1)
  
  #Re-format dataframe for Subjects who don't look at either Target Stim
  temp <- data.frame(numeric(200),numeric(200),numeric(200),numeric(200),numeric(200),numeric(200),numeric(200),numeric(200))
  if (length(temp2$trial_number) < length(data.choice_loss$trial_number)){
    for (ii in temp2$trial_number){
      row_counter <- which(temp2$trial_number == ii)
      temp[ii,] <- temp2[row_counter,]
    }
    colnames(temp) <- colnames(temp2)
    data.firstFix_loss <- temp
    data.firstFix_loss$subject_ID <- rep(SIDs[j],200)
    data.firstFix_loss$trial_number <- seq(1,200,1)
  } else {
    #Filter first Fixation for each trial
    data.firstFix_loss <- filter(data.eye_loss, fix_num == 1)
  }
  
  #Assign Expected Values to fist fixation data set
  data.firstFix_loss$EV_BW_diff <- data.choice_loss$EV_BW_diff
  
  
  #Set up Data Frames
  firstFix_binCount_loss <- rep(-1, length(firstFixBins)-1)
  Avg_fixDur_loss <- rep(-1, length(firstFixBins)-1)
  
  
  ### Bin first Fix LOSS ###
  for(i in 1:(length(firstFixBins)-1)){
    counter = i+1
    # Count the number of instances in each bin
    if(firstFixBins[counter]==firstFixBins[length(firstFixBins)]){
      df_curBin = filter(data.firstFix_loss, EV_BW_diff >= firstFixBins[i] & EV_BW_diff <= firstFixBins[counter])
    } else {
      df_curBin = filter(data.firstFix_loss, EV_BW_diff >= firstFixBins[i] & EV_BW_diff < firstFixBins[counter])
    }
    firstFix_binCount_loss[i] <- length(df_curBin$subject_ID)
    Avg_fixDur_loss[i] <- mean(df_curBin$fix_dur)
  }
  all_firstFix_binCount_loss[,j] <- firstFix_binCount_loss
  all_Avg_fixDur_loss[,j] <- Avg_fixDur_loss
  
  
  
  ####################################################
  #### Probability First Fixation Best ####
  ####################################################
  #### Prob First Fix Best WIN ####
  # Win Best - Worst option
  data.choice_win$EV_BW_diff = data.choice_win$p_left - data.choice_win$p_right
  
  #Filter first Fixation for each trial
  #data.firstFix_win <- filter(data.eye_win, fix_num == 1)
  temp2 <- filter(data.eye_win, fix_num == 1)
  
  #Re-format dataframe for Subjects who don't look at either Target Stim
  temp <- data.frame(numeric(200),numeric(200),numeric(200),numeric(200),numeric(200),numeric(200),numeric(200),numeric(200))
  if (length(temp2$trial_number) < length(data.choice_win$trial_number)){
    for (ii in temp2$trial_number){
      row_counter <- which(temp2$trial_number == ii)
      temp[ii,] <- temp2[row_counter,]
    }
    colnames(temp) <- colnames(temp2)
    data.firstFix_win <- temp
    data.firstFix_win$subject_ID <- rep(SIDs[j],200)
    data.firstFix_win$trial_number <- seq(1,200,1)
  } else {
    #Filter first Fixation for each trial
    data.firstFix_win <- filter(data.eye_win, fix_num == 1)
  }
  
  #Assign Expected Values to fist fixation data set
  data.firstFix_win$EV_BW_diff <- data.choice_win$EV_BW_diff
  
  
  #Set up Data Frames
  firstFixBest_binCount_win <- rep(-1, length(firstFixBins)-1)
  p_firstFixBest_win <- rep(-1, length(firstFixBins)-1)
  
  # Bin Prob First Fix Best WIN
  for(i in 2:(length(firstFixBins))){
    df_curBin <- filter(data.firstFix_win, abs(EV_BW_diff) == firstFixBins[i])
    
    for(k in 1:(length(df_curBin$subject_ID))){
      if(df_curBin$EV_BW_diff[k] < 0){
        if(df_curBin$location[k]==1){
          df_curBin$bestChoice[k] <- 0 
          
        } else if(df_curBin$location[k]==2){
          df_curBin$bestChoice[k] <- 1 
        }
      } else if (df_curBin$EV_BW_diff[k] > 0){
        if(df_curBin$location[k]==1){
          df_curBin$bestChoice[k] <- 1 
          
        } else if(df_curBin$location[k]==2){
          df_curBin$bestChoice[k] <- 0 
        }
      }
    }
    firstFixBest_binCount_win[i-1] <- length(df_curBin$bestChoice)
    p_firstFixBest_win[i-1] <- sum(df_curBin$bestChoice)/length(df_curBin$bestChoice)
  }
  all_firstFixBest_binCount_win[,j] <- firstFixBest_binCount_win
  all_p_firstFixBest_win[,j] <- p_firstFixBest_win
  
  
  #### Prob First Fix Best LOSS ####
  # Win Best - Worst option
  data.choice_loss$EV_BW_diff = all.neg(data.choice_loss$p_left) - all.neg(data.choice_loss$p_right)
  
  #Filter first Fixation for each trial
  #data.firstFix_loss <- filter(data.eye_loss, fix_num == 1)
  temp2 <- filter(data.eye_loss, fix_num == 1)
  
  #Re-format dataframe for Subjects who don't look at either Target Stim
  temp <- data.frame(numeric(200),numeric(200),numeric(200),numeric(200),numeric(200),numeric(200),numeric(200),numeric(200))
  if (length(temp2$trial_number) < length(data.choice_loss$trial_number)){
    for (ii in temp2$trial_number){
      row_counter <- which(temp2$trial_number == ii)
      temp[ii,] <- temp2[row_counter,]
    }
    colnames(temp) <- colnames(temp2)
    data.firstFix_loss <- temp
    data.firstFix_loss$subject_ID <- rep(SIDs[j],200)
    data.firstFix_loss$trial_number <- seq(1,200,1)
  } else {
    #Filter first Fixation for each trial
    data.firstFix_loss <- filter(data.eye_loss, fix_num == 1)
  }
  
  #Assign Expected Values to fist fixation data set
  data.firstFix_loss$EV_BW_diff <- data.choice_loss$EV_BW_diff
  
  
  #Set up Data Frames
  firstFixBest_binCount_loss <- rep(-1, length(firstFixBins)-1)
  p_firstFixBest_loss <- rep(-1, length(firstFixBins)-1)
  
  # Bin Prob First Fix Best LOSS
  for(i in 2:(length(firstFixBins))){
    df_curBin <- filter(data.firstFix_loss, abs(EV_BW_diff) == firstFixBins[i])
    
    #Assign if First Fix is Best Chosen
    for(k in 1:(length(df_curBin$subject_ID))){
      if(df_curBin$EV_BW_diff[k] < 0){
        if(df_curBin$location[k]==1){
          df_curBin$bestChoice[k] <- 0 
          
        } else if(df_curBin$location[k]==2){
          df_curBin$bestChoice[k] <- 1 
        }
      } else if (df_curBin$EV_BW_diff[k] > 0){
        if(df_curBin$location[k]==1){
          df_curBin$bestChoice[k] <- 1 
          
        } else if(df_curBin$location[k]==2){
          df_curBin$bestChoice[k] <- 0 
        }
      }
    }
    firstFixBest_binCount_loss[i-1] <- length(df_curBin$bestChoice)
    p_firstFixBest_loss[i-1] <- sum(df_curBin$bestChoice)/length(df_curBin$bestChoice)
  }
  all_firstFixBest_binCount_loss[,j] <- firstFixBest_binCount_loss
  all_p_firstFixBest_loss[,j] <- p_firstFixBest_loss
  
  
  ####################################################
  #### Fixation Duration Vs. Fixation Type ####
  ####################################################
  #Filter FIRST and LAST Fixation for each trial
  data.firstFix_win <- filter(data.eye_win, fix_num == 1)
  data.lastFix_win <- filter(data.eye_win, fix_num_rev == 0)
  data.firstFix_loss <- filter(data.eye_loss, fix_num ==1)
  data.lastFix_loss <- filter(data.eye_loss, fix_num_rev ==0)
  
  #Average
  all_Avg_fixDurType[1,j] <- mean(data.firstFix_win$fix_dur)
  all_Avg_fixDurType[2,j] <- mean(data.lastFix_win$fix_dur)
  all_Avg_fixDurType[3,j] <- mean(data.firstFix_loss$fix_dur)
  all_Avg_fixDurType[4,j] <- mean(data.lastFix_loss$fix_dur)
  
  
  #####################################
  #### Net Fixation Duration ####
  #####################################
  #### Net Fix WIN ####
  # Left - Right
  data.choice_win$EV_LR_diff = data.choice_win$p_left - data.choice_win$p_right
  
  # Filter Left/Right Location
  fixLeft_win <- filter(data.eye_win, location == 1)
  fixRight_win <- filter(data.eye_win, location == 2)
  
  #Assign Net Fixation Duration to choice Data
  for(i in 1:length(data.choice_win$trial_number)){
    trialSubset_L <- filter(fixLeft_win, trial_number == i)
    trialSubset_R <- filter(fixRight_win, trial_number == i)
    
    data.choice_win$NetFixDur[i] <- sum(trialSubset_L$fix_dur) - sum(trialSubset_R$fix_dur)
  }
  
  #Set up Data Frames
  Avg_NetFixDur_win <- rep(-1, length(netFixBins)-1)
  
  # Bin
  for(i in 1:(length(netFixBins)-1)){
    counter = i+1
    # Count the number of instances in each bin
    if(netFixBins[counter]==netFixBins[length(netFixBins)]){
      df_curBin = filter(data.choice_win, EV_LR_diff >= netFixBins[i] & EV_LR_diff <= netFixBins[counter])
    } else {
      df_curBin = filter(data.choice_win, EV_LR_diff >= netFixBins[i] & EV_LR_diff < netFixBins[counter])
    }
    Avg_NetFixDur_win[i] <- mean(df_curBin$NetFixDur)
  }
  all_Avg_NetFixDur_win[,j] <- Avg_NetFixDur_win
  
  
  #### Net Fix LOSS ####
  # Left - Right
  data.choice_loss$EV_LR_diff = all.neg(data.choice_loss$p_left) - all.neg(data.choice_loss$p_right)
  
  # Filter Left/Right Location
  fixLeft_loss <- filter(data.eye_loss, location == 1)
  fixRight_loss <- filter(data.eye_loss, location == 2)
  
  #Assign Net Fixation Duration to choice Data
  for(i in 1:length(data.choice_win$trial_number)){
    trialSubset_L <- filter(fixLeft_loss, trial_number == i)
    trialSubset_R <- filter(fixRight_loss, trial_number == i)
    
    data.choice_loss$NetFixDur[i] <- sum(trialSubset_L$fix_dur) - sum(trialSubset_R$fix_dur)
  }
  
  #Set up Data Frames
  Avg_NetFixDur_loss <- rep(-1, length(netFixBins)-1)
  
  # Bin
  for(i in 1:(length(netFixBins)-1)){
    counter = i+1
    # Count the number of instances in each bin
    if(netFixBins[counter]==netFixBins[length(netFixBins)]){
      df_curBin = filter(data.choice_loss, EV_LR_diff >= netFixBins[i] & EV_LR_diff <= netFixBins[counter])
    } else {
      df_curBin = filter(data.choice_loss, EV_LR_diff >= netFixBins[i] & EV_LR_diff < netFixBins[counter])
    }
    Avg_NetFixDur_loss[i] <- mean(df_curBin$NetFixDur)
  }
  all_Avg_NetFixDur_loss[,j] <- Avg_NetFixDur_loss
}
#======================================================================================





# ---------------------------------- Plots ----------------------------------------
setwd('~/Desktop/graph-examples')
### Avg First Fixation Duration ###
df_firstFixDur <- data.frame()

#WIN
# Calculate Mean and SD & cbind them together
df_firstFixDur_win <- cbind(firstFixBins[-7], apply(all_Avg_fixDur_win, 1, mean), apply(all_Avg_fixDur_win, 1, sd))
colnames(df_firstFixDur_win) <- c("Bin", "mean", "sd") # Label columns
df_firstFixDur_win <- as.data.frame(df_firstFixDur_win) # Coerce into Data frame
df_firstFixDur_win$condition <- rep("Win",length(df_firstFixDur_win$Bin)) # Add a condition column

#LOSS
df_firstFixDur_loss <- cbind(firstFixBins[-7], apply(all_Avg_fixDur_loss, 1, mean), apply(all_Avg_fixDur_loss, 1, sd))
colnames(df_firstFixDur_loss) <- c("Bin", "mean", "sd")
df_firstFixDur_loss <- as.data.frame(df_firstFixDur_loss)
df_firstFixDur_loss$condition <- rep("Loss",length(df_firstFixDur_loss$Bin))

#Combine WIN/LOSS Data sets
df_firstFixDur <- rbind(df_firstFixDur_win, df_firstFixDur_loss)


ggplot(df_firstFixDur, aes(x=Bin, y=mean, color=condition)) +
  geom_line(size=.7) +
  geom_point() +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2, size=.7) +
  labs(title="First Fix Duration", x="Best - Worst (choice)", y="First Fix. Duration (ms)") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

#Save out Plot
plot_save <- paste("Fixation_plots/firstFixDur_plot.png", sep = "")
ggsave("Fixation_plots/firstFixDur_plot.png", width = 2000, height = 1300, units = "px")



#### Probability First Fix Best ####
df_pFirstFixBest <- data.frame()

#WIN
# Calculate Mean and SD & cbind them together
df_pFirstFixBest_win <- cbind(firstFixBins[-1], apply(all_p_firstFixBest_win,1,mean), apply(all_p_firstFixBest_win,1,sd))
colnames(df_pFirstFixBest_win) <- c("Bin", "mean", "sd")
df_pFirstFixBest_win <- as.data.frame(df_pFirstFixBest_win)
df_pFirstFixBest_win$condition <- rep("Win",length(df_pFirstFixBest_win$Bin))

#LOSS
df_pFirstFixBest_loss <- cbind(firstFixBins[-1], apply(all_p_firstFixBest_loss,1,mean), apply(all_p_firstFixBest_loss,1,sd))
colnames(df_pFirstFixBest_loss) <- c("Bin", "mean", "sd")
df_pFirstFixBest_loss <- as.data.frame(df_pFirstFixBest_loss)
df_pFirstFixBest_loss$condition <- rep("Loss",length(df_pFirstFixBest_loss$Bin))

#Combine WIN/LOSS Data sets
df_pFirstFixBest <- rbind(df_pFirstFixBest_win, df_pFirstFixBest_loss)

ggplot(df_pFirstFixBest, aes(x=Bin, y=mean, color=condition)) + 
  geom_line(size=.7) +
  geom_point() +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2, size=.7) +
  labs(title="Probability First Fix is Best", x="Best - Worst (choice)", y="P (First Fix Best)") +
  scale_x_continuous(breaks = firstFixBins[-1]) +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

#Save out Plot
ggsave("Fixation_plots/pFirstFixBest_plot.png", width = 2000, height = 1300, units = "px")


#### Fixation Duration Vs. Fixation Type ####
df_fixDurType <- cbind(apply(all_Avg_fixDurType,1,mean), apply(all_Avg_fixDurType,1,sd))
df_fixDurType <- as.data.frame(df_fixDurType)
colnames(df_fixDurType) <- c("mean","sd")
df_fixDurType$fixType <- c('First','Last','First','Last')
df_fixDurType$fixType <- as.factor(df_fixDurType$fixType)

df_fixDurType$condition <- c('Win','Win','Loss','Loss')
df_fixDurType$condition <- as.factor(df_fixDurType$condition)

ggplot(df_fixDurType, aes(x=fixType, y=mean, fill=condition)) +
  geom_bar(stat='identity', position = position_dodge()) +
  geom_linerange(aes(x=fixType, ymin=mean-sd, ymax=mean+sd), size=.7, position=position_dodge(.9)) +
  labs(title="Fixation Duration", x="Fixation Type", y="Fixation Duration (ms)") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

#Save out Plot
ggsave("Fixation_plots/fixDurType_plot.png", width = 2000, height = 1300, units = "px")


#### Net Fixation ####
df_NetFixDur <- data.frame()

#WIN
df_NetFixDur_win <- cbind(netFixBins[-11], apply(all_Avg_NetFixDur_win, 1, mean), apply(all_Avg_NetFixDur_win, 1, sd))
colnames(df_NetFixDur_win) <- c("Bin", "mean", "sd") # Label columns
df_NetFixDur_win <- as.data.frame(df_NetFixDur_win) # Coerce into Data frame
df_NetFixDur_win$condition <- rep("Win", length(df_NetFixDur_win$Bin))

#LOSS
df_NetFixDur_loss <- cbind(netFixBins[-11], apply(all_Avg_NetFixDur_loss, 1, mean), apply(all_Avg_NetFixDur_loss, 1, sd))
colnames(df_NetFixDur_loss) <- c("Bin", "mean", "sd") # Label columns
df_NetFixDur_loss <- as.data.frame(df_NetFixDur_loss) # Coerce into Data frame
df_NetFixDur_loss$condition <- rep("Loss", length(df_NetFixDur_loss$Bin))

#Combine WIN/LOSS Data sets
df_NetFixDur <- rbind(df_NetFixDur_win, df_NetFixDur_loss)

ggplot(df_NetFixDur, aes(x=Bin, y=mean, color=condition)) +
  geom_line(size=.7) +
  geom_point() +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2, size=.7) +
  labs(title="Net Fix Duration", x="Left - Right (EV)", y="Net Fix. Duration (L-R, ms)") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5)) #+ 
#geom_hline(yintercept = 0, color="grey") + geom_vline(xintercept = 0, color="grey")

#Save out Plot
ggsave("Fixation_plots/NetFixDur.png", width = 2000, height = 1300, units = "px")
#======================================================================================
