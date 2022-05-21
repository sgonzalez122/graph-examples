#File: Psychometric_plots.R
#Programmer: Stephen A. Gonzalez
#Date Created: 4/13/2022
#Purpose: Plot behaviral data

# ------------------- Load packages and Work space Setup --------------------------------
#library(ggplot2)
#library(patchwork)
#library(dplyr)
#library(reshape)
#library(tidyverse)

library(groundhog)
groundhog.day <- "2022-04-27"
pkgs <- c("ggplot2",
          "patchwork",
          "dplyr",
          "reshape",
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


# Difference vs % Choice LEFT
# Set up Bins
EV_LR_dif_Bins = seq(-10,10,2)

# WIN
all_numInBin_win = matrix(-1,length(EV_LR_dif_Bins)-1,length(SIDs))
all_Avg_choice_left_win = matrix(-1,length(EV_LR_dif_Bins)-1,length(SIDs))
all_num_LEFT_chosen_win = matrix(-1,length(EV_LR_dif_Bins)-1,length(SIDs))

# LOSS
all_numInBin_loss = matrix(-1,length(EV_LR_dif_Bins)-1,length(SIDs))
all_Avg_choice_left_loss = matrix(-1,length(EV_LR_dif_Bins)-1,length(SIDs))
all_num_LEFT_chosen_loss = matrix(-1,length(EV_LR_dif_Bins)-1,length(SIDs))

# Difference vs RT 
# Set up Bins
EV_LR_dif_Bins_RT = seq(-10,10,2)

# WIN
all_numInBin_RT_win = matrix(-1,length(EV_LR_dif_Bins_RT)-1,length(SIDs))
all_mean_RT_win = matrix(-1,length(EV_LR_dif_Bins_RT)-1,length(SIDs))
all_sd_RT_win = matrix(-1,length(EV_LR_dif_Bins_RT)-1,length(SIDs))

# LOSS
all_numInBin_RT_loss = matrix(-1,length(EV_LR_dif_Bins_RT)-1,length(SIDs))
all_mean_RT_loss = matrix(-1,length(EV_LR_dif_Bins_RT)-1,length(SIDs))
all_sd_RT_loss = matrix(-1,length(EV_LR_dif_Bins_RT)-1,length(SIDs))

# Diference vs % Chose Last Seen
# Set up Bins
EV_Last_Seen_Bins = seq(-10,10,2)
# WIN
all_numInBin_ChoseLastSeen_win <- matrix(-1,length(EV_Last_Seen_Bins)-1,length(SIDs))
all_mean_ChoseLastSeen_win <- matrix(-1,length(EV_Last_Seen_Bins)-1,length(SIDs))
all_num_ChoseLastSeen_win <- matrix(-1,length(EV_Last_Seen_Bins)-1,length(SIDs))

# LOSS
all_numInBin_ChoseLastSeen_loss <- matrix(-1,length(EV_Last_Seen_Bins)-1,length(SIDs))
all_mean_ChoseLastSeen_loss <- matrix(-1,length(EV_Last_Seen_Bins)-1,length(SIDs))
all_num_ChoseLastSeen_loss <- matrix(-1,length(EV_Last_Seen_Bins)-1,length(SIDs))

#======================================================================================


# ---------------------------------- Data Set up ---------------------------------------
# Name: 
# Note: 
# --------------------------------------------------------------------------------------
for (j in 1:length(SIDs)){
  #Read in Choice Data
  setwd("~/Desktop/graph-examples/data")
  data.choice_win <- read.csv(paste(SIDs[j], "/choice_", SIDs[j], "_win.csv", sep = ""))
  data.choice_loss <- read.csv(paste(SIDs[j], "/choice_", SIDs[j], "_loss.csv", sep = ""))
  
  #Read in Eye Data
  data.eye_win <- read.csv(paste(SIDs[j],"/raw_fixations_", SIDs[j], "_win.csv", sep = ""))
  data.eye_loss <- read.csv(paste(SIDs[j],"/raw_fixations_", SIDs[j], "_loss.csv", sep = ""))
  
  
  
  #### WIN ####
  # WIN EV_L - EV_R
  win_EV_L = data.choice_win$p_left                     #* .01
  win_EV_R = data.choice_win$p_right                    #* .01
  data.choice_win$EV_LR_diff = win_EV_L - win_EV_R
  
  # Set up dataframes
  numInBin_win = rep(-1, length(EV_LR_dif_Bins)-1)
  Avg_choice_left_win = rep(-1, length(EV_LR_dif_Bins)-1)
  num_LEFT_chosen_win = rep(-1, length(EV_LR_dif_Bins)-1)
  
  # Bin % Choice LEFT WIN
  for (i in 1:(length(EV_LR_dif_Bins)-1)){
    counter = i+1
    # Count the number of instances in each bin
    if(EV_LR_dif_Bins[counter]==EV_LR_dif_Bins[length(EV_LR_dif_Bins)]){
      df_curBin = filter(data.choice_win, EV_LR_diff >= EV_LR_dif_Bins[i] & EV_LR_diff <= EV_LR_dif_Bins[counter])
    } else {
      df_curBin = filter(data.choice_win, EV_LR_diff >= EV_LR_dif_Bins[i] & EV_LR_diff < EV_LR_dif_Bins[counter])
    }
    numInBin_win[i] = length(df_curBin$subject_ID)
    Avg_choice_left_win[i] = mean(df_curBin$choice == "left")
    num_LEFT_chosen_win[i] = sum(df_curBin$choice == "left")
  }
  all_numInBin_win[,j] = numInBin_win
  all_Avg_choice_left_win[,j] = Avg_choice_left_win
  all_num_LEFT_chosen_win[,j] = num_LEFT_chosen_win
  
  
  ### RT ###
  # Set up dataframe
  numInBin_RT_win = rep(-1, length(EV_LR_dif_Bins_RT)-1)
  mean_RT_win = rep(-1, length(EV_LR_dif_Bins_RT)-1)
  sd_RT_win = rep(-1, length(EV_LR_dif_Bins_RT)-1)
  
  # Bin RT WIN
  for (i in 1:(length(EV_LR_dif_Bins_RT)-1)){
    counter = i+1
    # Count the number of instances in each bin
    if(EV_LR_dif_Bins_RT[counter]==EV_LR_dif_Bins_RT[length(EV_LR_dif_Bins_RT)]){
      df_curBin = filter(data.choice_win, EV_LR_diff>= EV_LR_dif_Bins_RT[i] & EV_LR_diff <= EV_LR_dif_Bins_RT[counter])
    } else {
      df_curBin = filter(data.choice_win, EV_LR_diff>=EV_LR_dif_Bins_RT[i] & EV_LR_diff < EV_LR_dif_Bins_RT[counter])
    }
    numInBin_RT_win[i] = length(df_curBin$subject_ID)
    mean_RT_win[i] = mean(df_curBin$RT)
    sd_RT_win[i] = sd(df_curBin$RT)
  }
  all_numInBin_RT_win[,j] = numInBin_RT_win
  all_mean_RT_win[,j] = mean_RT_win
  all_sd_RT_win[,j] = sd_RT_win
  
  
  ### % Choice last seen WIN ###
  # Set up dataframe
  numInBin_ChoseLastSeen_win = rep(-1, length(EV_Last_Seen_Bins)-1)
  mean_ChoseLastSeen_win = rep(-1, length(EV_Last_Seen_Bins)-1)
  num_ChoseLastSeen_win = rep(-1, length(EV_Last_Seen_Bins)-1)
  
  # Extract Last item seen WIN
  last_item_seen_win <- data.frame()
  for (i in 1:length(data.choice_win$trial_number)){
    k <- toString(i) 
    trial_subset <- subset(data.eye_win,trial_number==k) # Subset each trial
    
    if ((any(trial_subset$ROI == 'left') == FALSE) & (any(trial_subset$ROI == 'right') == FALSE)){ #If subject doesn't look at either Item
      data.choice_win$last_item_seen[i] <- trial_subset$ROI[length(trial_subset$ROI)]
    } else {
      subset_filtered <- filter(trial_subset, ROI == "left" | ROI == "right") # Filter out L/R choice data
      data.choice_win$last_item_seen[i] <- subset_filtered$ROI[length(subset_filtered$ROI)] # assign the last item seen
    }
  }
  
  data.choice_win$chose_last_item_seen <- data.choice_win$choice == data.choice_win$last_item_seen # Assign chose_last_item_seen variable
  data.choice_win$chose_last_item_seen <- as.integer(data.choice_win$chose_last_item_seen) # Convert to Integer
  
  # Bin % Choice last seen
  for (i in 1:(length(EV_Last_Seen_Bins)-1)){
    counter = i+1
    # Count the number of instances in each bin
    if(EV_Last_Seen_Bins[counter]==EV_Last_Seen_Bins[length(EV_Last_Seen_Bins)]){
      df_curBin = filter(data.choice_win, EV_LR_diff >= EV_Last_Seen_Bins[i] & EV_LR_diff <= EV_Last_Seen_Bins[counter])
    } else {
      df_curBin = filter(data.choice_win, EV_LR_diff >= EV_Last_Seen_Bins[i] & EV_LR_diff < EV_Last_Seen_Bins[counter])
    }
    numInBin_ChoseLastSeen_win[i] <- length(df_curBin$subject_ID)
    mean_ChoseLastSeen_win[i] <- mean(df_curBin$chose_last_item_seen == 1)
    num_ChoseLastSeen_win[i] <- sum(df_curBin$chose_last_item_seen == 1)
  }
  all_numInBin_ChoseLastSeen_win[,j] <- numInBin_ChoseLastSeen_win
  all_mean_ChoseLastSeen_win[,j] <- mean_ChoseLastSeen_win
  all_num_ChoseLastSeen_win[,j] <- num_ChoseLastSeen_win
  
  
  #### LOSS ####
  # LOSS EV_L - EV_R
  loss_EV_L = data.choice_loss$p_left                   #* .01
  loss_EV_R =  data.choice_loss$p_right                 #* .01
  data.choice_loss$EV_LR_diff = all.neg(loss_EV_L) - all.neg(loss_EV_R)
  
  # Set up data frames
  numInBin_loss = rep(-1, length(EV_LR_dif_Bins)-1)
  Avg_choice_left_loss = rep(-1, length(EV_LR_dif_Bins)-1)
  num_LEFT_chosen_loss = rep(-1, length(EV_LR_dif_Bins)-1)
  
  # % Choice LEFT LOSS
  for (i in 1:(length(EV_LR_dif_Bins)-1)){
    counter = i+1
    # Count the number of instances in each bin
    if(EV_LR_dif_Bins[counter]==EV_LR_dif_Bins[length(EV_LR_dif_Bins)]){
      df_curBin = filter(data.choice_loss, EV_LR_diff >= EV_LR_dif_Bins[i] & EV_LR_diff <= EV_LR_dif_Bins[counter])
    } else {
      df_curBin = filter(data.choice_loss, EV_LR_diff >= EV_LR_dif_Bins[i] & EV_LR_diff < EV_LR_dif_Bins[counter])
    }
    numInBin_loss[i] = length(df_curBin$subject_ID)
    Avg_choice_left_loss[i] = mean(df_curBin$choice == "left")
    num_LEFT_chosen_loss[i] = sum(df_curBin$choice == "left")
  }
  all_numInBin_loss[,j] = numInBin_loss
  all_Avg_choice_left_loss[,j] = Avg_choice_left_loss
  all_num_LEFT_chosen_loss[,j] = num_LEFT_chosen_loss
  
  ### RT ###
  # Set up data frames
  numInBin_RT_loss = rep(-1, length(EV_LR_dif_Bins_RT)-1)
  mean_RT_loss = rep(-1, length(EV_LR_dif_Bins_RT)-1)
  sd_RT_loss = rep(-1, length(EV_LR_dif_Bins_RT)-1)
  
  # RT LOSS
  for (i in 1:(length(EV_LR_dif_Bins_RT)-1)){
    counter = i+1
    # Count the number of instances in each bin
    if(EV_LR_dif_Bins_RT[counter]==EV_LR_dif_Bins_RT[length(EV_LR_dif_Bins_RT)]){
      df_curBin = filter(data.choice_loss, EV_LR_diff>= EV_LR_dif_Bins_RT[i] & EV_LR_diff <= EV_LR_dif_Bins_RT[counter])
    } else {
      df_curBin = filter(data.choice_loss, EV_LR_diff>=EV_LR_dif_Bins_RT[i] & EV_LR_diff < EV_LR_dif_Bins_RT[counter])
    }
    numInBin_RT_loss[i] = length(df_curBin$subject_ID)
    mean_RT_loss[i] = mean(df_curBin$RT)
    sd_RT_loss[i] = sd(df_curBin$RT)
  }
  all_numInBin_RT_loss[,j] = numInBin_RT_loss
  all_mean_RT_loss[,j] = mean_RT_loss
  all_sd_RT_loss[,j] = sd_RT_loss
  
  
  # % Choice last seen LOSS
  # Set up dataframe
  numInBin_ChoseLastSeen_loss = rep(-1, length(EV_Last_Seen_Bins)-1)
  mean_ChoseLastSeen_loss = rep(-1, length(EV_Last_Seen_Bins)-1)
  num_ChoseLastSeen_loss = rep(-1, length(EV_Last_Seen_Bins)-1)
  
  #Extract Last item seen LOSS
  for (i in 1:length(data.choice_win$trial_number)){
    k <- toString(i) 
    trial_subset <- subset(data.eye_loss,trial_number==k) #Subset each trial
    
    if ((any(trial_subset$ROI == 'left') == FALSE) & (any(trial_subset$ROI == 'right') == FALSE)){ #If subject doesn't look at either Item
      data.choice_loss$last_item_seen[i] <- trial_subset$ROI[length(trial_subset$ROI)]
    } else {
      subset_filtered <- filter(trial_subset, ROI == "left" | ROI == "right") #Filter out L/R choice data
      data.choice_loss$last_item_seen[i] <- subset_filtered$ROI[length(subset_filtered$ROI)] #assign the last item seen
    }
  }
  
  data.choice_loss$chose_last_item_seen <- data.choice_loss$choice == data.choice_loss$last_item_seen # Assign chose_last_item_seen variable
  data.choice_loss$chose_last_item_seen <- as.integer(data.choice_loss$chose_last_item_seen) # Convert to Integer
  
  # Bin % Choice last seen LOSS
  for (i in 1:(length(EV_Last_Seen_Bins)-1)){
    counter = i+1
    # Count the number of instances in each bin
    if(EV_Last_Seen_Bins[counter]==EV_Last_Seen_Bins[length(EV_Last_Seen_Bins)]){
      df_curBin = filter(data.choice_loss, EV_LR_diff >= EV_Last_Seen_Bins[i] & EV_LR_diff <= EV_Last_Seen_Bins[counter])
    } else {
      df_curBin = filter(data.choice_loss, EV_LR_diff >= EV_Last_Seen_Bins[i] & EV_LR_diff < EV_Last_Seen_Bins[counter])
    }
    numInBin_ChoseLastSeen_loss[i] <- length(df_curBin$subject_ID)
    mean_ChoseLastSeen_loss[i] <- mean(df_curBin$chose_last_item_seen == 1)
    num_ChoseLastSeen_loss[i] <- sum(df_curBin$chose_last_item_seen == 1)
  }
  all_numInBin_ChoseLastSeen_loss[,j] <- numInBin_ChoseLastSeen_loss
  all_mean_ChoseLastSeen_loss[,j] <- mean_ChoseLastSeen_loss
  all_num_ChoseLastSeen_loss[,j] <- num_ChoseLastSeen_loss
}



#==================================================================================================


# ---------------------------------- WIN Plots  ----------------------------------------
# Name: Display Plots
# Note:
# --------------------------------------------------------------------------------------
setwd('~/Desktop/graph-examples/Psychometric_plots')

# Difference from left and right EV WIN
df_Pchoice_left_win = data.frame(x = EV_LR_dif_Bins[1:length(EV_LR_dif_Bins)-1],y = all_Avg_choice_left_win)
colnames(df_Pchoice_left_win) <- c("x",SIDs)

df_Pchoice_left_win.melted = melt(df_Pchoice_left_win, id = "x")

#All Raw Data Binned
ggplot(data = df_Pchoice_left_win.melted, aes(x = x, y = value, color = variable)) + geom_point() + geom_line() +
  labs(title="WIN Choice Curves", x="EV[left] - EV[right]", y="% Left Chosen") +
  scale_color_discrete(name="Subject") +  theme_classic()  +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
#Save Plot
ggsave("WIN_Choice_Curves.png", width = 2000, height = 1300, units = "px")


#Averaged
ggplot(df_Pchoice_left_win.melted, aes(x,value)) +
  stat_summary(fun = mean, geom = "point") +                #Basic Graph
  stat_summary(fun = mean, geom = "line", aes(group = 1)) + #Add a line
  labs(title="Avg WIN Choice Curves", x="EV[left] - EV[right]", y="% Left Chosen") +
  theme_classic()  + theme(plot.title = element_text(hjust = 0.5))
#Save Plot
ggsave("WIN_Choice_Curves_Avg.png", width = 2000, height = 1300, units = "px")


# Difference vs RT (Bin) WIN
df_RT_win <- data.frame()
for(i in 1:length(SIDs)){
  SID_vec <- rep(SIDs[i],length(EV_LR_dif_Bins_RT)-1)
  temp <- cbind(SID_vec,EV_LR_dif_Bins_RT[-11],all_mean_RT_win[,i],all_sd_RT_win[,i])
  df_RT_win <- rbind(df_RT_win,temp)
}
colnames(df_RT_win) <- c("SID", "Bin", "mean_RT", "sd_RT")
df_RT_win$SID <- as.factor(df_RT_win$SID)

#All Raw Data Binned
ggplot(df_RT_win, aes(x=Bin, y=mean_RT, color=SID)) +
  geom_line() +
  geom_point() + 
  geom_errorbar(aes(ymin=mean_RT-sd_RT, ymax=mean_RT+sd_RT), width=.2) + 
  labs(title="WIN RT Curves", x="EV[left] - EV[right]", y="Reaction Time (ms)") +
  scale_color_discrete(name="Subject") +  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") 
#Save Plot
ggsave("WIN_RT_Curves.png", width = 2000, height = 1300, units = "px")

#Averaged
ggplot(df_RT_win, aes(Bin,mean_RT)) +
  stat_summary(fun = mean, geom = "point") +                #Basic Graph
  stat_summary(fun = mean, geom = "line", aes(group = 1)) + #Add a line
  labs(title="Avg WIN RT Curves", x="EV[left] - EV[right]", y="Reaction Time (ms)") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult=1), geom = "errorbar", width = 0.2) +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5)) 
#Save Plot
ggsave("WIN_RT_Curves_Avg.png", width = 2000, height = 1300, units = "px")


# Diference vs % Chose Last Seen
df_lastSeenWin = data.frame(x = EV_Last_Seen_Bins[1:length(EV_Last_Seen_Bins)-1],y = all_mean_ChoseLastSeen_win)
colnames(df_lastSeenWin) <- c("x",SIDs)


df_lastSeenWin.melted = melt(df_lastSeenWin, id = "x")

#All Raw Data Binned
ggplot(data = df_lastSeenWin.melted, aes(x = x, y = value, color = variable)) + geom_point() + geom_line() +
  labs(title="Last Seen Curves WIN", x="EV[left] - EV[right]", y="% Chose Last Seen") +
  scale_color_discrete(name="Subject") +  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
#Save Plot
ggsave("WIN_Last_Seen_Curves.png", width = 2000, height = 1300, units = "px")

#Averaged
ggplot(df_lastSeenWin.melted, aes(x,value)) +
  stat_summary(fun = mean, geom = "point") +                #Basic Graph
  stat_summary(fun = mean, geom = "line", aes(group = 1)) + #Add a line
  labs(title="Avg Last Seen Curves WIN", x="EV[left] - EV[right]", y="% Chose Last Seen") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5)) 
#Save Plot
ggsave("WIN_Last_Seen_Curves_Avg.png", width = 2000, height = 1300, units = "px")
#==================================================================================================


# ---------------------------------- LOSS Plots  ----------------------------------------
# Name: Display Plots
# Note:
# --------------------------------------------------------------------------------------
# Difference from left and right EV LOSS
df_Pchoice_left_loss = data.frame(x = EV_LR_dif_Bins[1:length(EV_LR_dif_Bins)-1],y = all_Avg_choice_left_loss)
colnames(df_Pchoice_left_loss) <- c("x",SIDs)


df_Pchoice_left_loss.melted = melt(df_Pchoice_left_loss, id = "x")

#All Raw Data Binned
ggplot(data = df_Pchoice_left_loss.melted, aes(x = x, y = value, color = variable)) + geom_point() + geom_line() +
  labs(title="LOSS Choice Curves", x="EV[left] - EV[right]", y="% Left Chosen") +
  scale_color_discrete(name="Subject") +  theme_classic()  +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
#Save Plot
ggsave("LOSS_Choice_Curves.png", width = 2000, height = 1300, units = "px")

#Averaged
ggplot(df_Pchoice_left_loss.melted, aes(x,value)) +
  stat_summary(fun = mean, geom = "point") +                #Basic Graph
  stat_summary(fun = mean, geom = "line", aes(group = 1)) + #Add a line
  labs(title="Avg LOSS Choice Curves", x="EV[left] - EV[right]", y="% Left Chosen") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5)) 
#Save Plot
ggsave("LOSS_Choice_Curves_Avg.png", width = 2000, height = 1300, units = "px")


# Difference vs RT (Bin) LOSS
df_RT_loss <- data.frame()
for(i in 1:length(SIDs)){
  SID_vec <- rep(SIDs[i],length(EV_LR_dif_Bins_RT)-1)
  temp <- cbind(SID_vec,EV_LR_dif_Bins_RT[-11],all_mean_RT_loss[,i],all_sd_RT_loss[,i])
  df_RT_loss <- rbind(df_RT_loss,temp)
}
colnames(df_RT_loss) <- c("SID", "Bin", "mean_RT", "sd_RT")
df_RT_loss$SID <- as.factor(df_RT_loss$SID)

#All Raw Data Binned
ggplot(df_RT_loss, aes(x=Bin, y=mean_RT, color=SID)) +
  geom_line() +
  geom_point() + 
  geom_errorbar(aes(ymin=mean_RT-sd_RT, ymax=mean_RT+sd_RT), width=.2) + 
  labs(title="LOSS RT Curves", x="EV[left] - EV[right]", y="Reaction Time (ms)") +
  scale_color_discrete(name="Subject") +  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
#Save Plot
ggsave("LOSS_RT_Curves.png", width = 2000, height = 1300, units = "px")

#Averaged
ggplot(df_RT_loss, aes(Bin,mean_RT)) +
  stat_summary(fun = mean, geom = "point") +                #Basic Graph
  stat_summary(fun = mean, geom = "line", aes(group = 1)) + #Add a line
  labs(title="Avg LOSS RT Curves", x="EV[left] - EV[right]", y="Reaction Time (ms)") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult=1), geom = "errorbar", width = 0.2) +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5)) 
#Save Plot
ggsave("LOSS_RT_Curves_Avg.png", width = 2000, height = 1300, units = "px")


# Diference vs % Chose Last Seen
df_lastSeenLoss = data.frame(x = EV_Last_Seen_Bins[1:length(EV_Last_Seen_Bins)-1],y = all_mean_ChoseLastSeen_loss)
colnames(df_lastSeenLoss) <- c("x",SIDs)

df_lastSeenLoss.melted = melt(df_lastSeenLoss, id = "x")

#All Raw Data Binned
ggplot(data = df_lastSeenLoss.melted, aes(x = x, y = value, color = variable)) + geom_point() + geom_line() +
  labs(title="Avg Last Seen Curves LOSS", x="EV[left] - EV[right]", y="% Chose Last Seen") +
  scale_color_discrete(name="Subject") +  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
#Save Plot
ggsave("LOSS_Last_Seen_Curves.png", width = 2000, height = 1300, units = "px")

#Averaged
ggplot(df_lastSeenLoss.melted, aes(x,value)) +
  stat_summary(fun = mean, geom = "point") +                #Basic Graph
  stat_summary(fun = mean, geom = "line", aes(group = 1)) + #Add a line
  labs(title="Avg Last Seen Curves LOSS", x="EV[left] - EV[right]", y="% Chose Last Seen") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5)) 
#Save Plot
ggsave("LOSS_Last_Seen_Curves_Avg.png", width = 2000, height = 1300, units = "px")
#==================================================================================================