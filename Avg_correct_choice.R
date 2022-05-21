#File: Avg_correct_Choice.R
#Programmer: Stephen A. Gonzalez
#Date Created: 03/08/2022
#Purpose: Plot QC criteria and thresholds


# ------------------- Load packages and Work space Setup --------------------------------
library(ggplot2)
library(patchwork)
library(dplyr)
library(reshape)
library(tidyverse)
rm(list = ls()) #Clear Workspace

SID <- 1
SIDs <- c(1,2,3,4)

#Get SIDs
setwd("~/Desktop/graph-examples/data")
SIDs <- list.files()
SIDs[-(which(SIDs == "bad"))] #Remove 'Bad' File Folder
SIDs <- as.numeric(SIDs) #Change to Numeric
SIDs <- na.omit(SIDs) #Remove NA's
SIDs <- as.data.frame(SIDs) #Convert to Dataframe
SIDs <- SIDs[order(SIDs),] #Order Data

all_percentCorrectChoice <- matrix(-1, 2, length(SIDs))
colnames(all_percentCorrectChoice) <- SIDs

all_percentIncorrectChoice <- matrix(-1,2, length(SIDs))
colnames(all_percentIncorrectChoice) <- SIDs

for(j in 1:length(SIDs)){
  setwd("~/Desktop/graph-examples/data")
  data.choice_win <- read.csv(paste(SIDs[j], "/choice_", SIDs[j], "_win.csv", sep = ""))
  data.choice_loss <- read.csv(paste(SIDs[j], "/choice_", SIDs[j], "_loss.csv", sep = ""))
  
  
  ##########################
        #### WIN ####
  ##########################
  # Correct Option
  for(k in 1:length(data.choice_win$trial_number)){
    if(data.choice_win$p_left[k] > data.choice_win$p_right[k]){
      data.choice_win$correctOption[k] <- 'left'
      
    } else if(data.choice_win$p_left[k] < data.choice_win$p_right[k]){
      data.choice_win$correctOption[k] <- 'right'
      
    } else if(data.choice_win$p_left[k] == data.choice_win$p_right[k]){
      data.choice_win$correctOption[k] <- data.choice_win$choice[k]
    }
  }
  
  #Make a column on whether or not the choice made was correct or not
  data.choice_win$choiceCorrect <- data.choice_win$choice == data.choice_win$correctOption
  
  #Percent Correct Choice
  all_percentCorrectChoice[1,j] <- (sum(data.choice_win$choiceCorrect)/length(data.choice_win$trial_number))*100
  
  #Percent Incorrect Choice
  all_percentIncorrectChoice[1,j] <- ((length(data.choice_win$trial_number) - sum(data.choice_win$choiceCorrect))/length(data.choice_win$trial_number))*100
  
  
  ##########################
        #### LOSS ####
  ##########################
  # Correct Option
  for(k in 1:length(data.choice_loss$trial_number)){
    if(data.choice_loss$p_left[k] < data.choice_loss$p_right[k]){
      data.choice_loss$correctOption[k] <- 'left'
      
    } else if(data.choice_loss$p_left[k] > data.choice_loss$p_right[k]){
      data.choice_loss$correctOption[k] <- 'right'
      
    } else if(data.choice_loss$p_left[k] == data.choice_loss$p_right[k]){
      data.choice_loss$correctOption[k] <- data.choice_loss$choice[k]
    }
  }
  
  #Make a column on whether or not the choice made was correct or not
  data.choice_loss$choiceCorrect <- data.choice_loss$choice == data.choice_loss$correctOption
  
  #Percent Correct Choice
  all_percentCorrectChoice[2,j] <- (sum(data.choice_loss$choiceCorrect)/length(data.choice_loss$trial_number))*100
  
  #Percent Incorrect Choice
  all_percentIncorrectChoice[2,j] <- ((length(data.choice_loss$trial_number) - sum(data.choice_loss$choiceCorrect))/length(data.choice_loss$trial_number))*100
}

# Percent Correct 
#Make data frame
all_percentCorrectChoice <- data.frame(all_percentCorrectChoice) #Convert to dataframe
all_percentCorrectChoice <- cbind(c(1,2),all_percentCorrectChoice) #cbind categories. Need to do this in two steps because data turns into character otherwise...
colnames(all_percentCorrectChoice) <- c("block_type", SIDs)
all_percentCorrectChoice$block_type <- factor(all_percentCorrectChoice$block_type, levels = c(1,2), labels = c("Win","Loss"))

df_correct_melted <- melt(all_percentCorrectChoice, id = "block_type")

#Color by Block_type
p1 <- ggplot(data = df_correct_melted, aes(x = block_type, y = value, color = block_type)) + geom_point(alpha = 0.5) + ylim(0,100) + 
        stat_summary(fun = "mean", geom = "point", size = 5) +
        geom_hline(yintercept = 50, linetype = 2) + theme_classic() +
        labs(title="Average Correct Choice", x="Block Type", y="% Correct") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"),axis.title = element_text(face = "bold"),axis.text = element_text(face = "bold")) + 
        scale_color_manual(values = c("#00BFC4","#F8766D"), name = "Block Type")


# Percent Incorrect
#Make data frame
all_percentIncorrectChoice <- data.frame(all_percentIncorrectChoice)
all_percentIncorrectChoice <- cbind(c(1,2),all_percentIncorrectChoice)
colnames(all_percentIncorrectChoice) <- c("block_type", SIDs)
all_percentIncorrectChoice$block_type <- factor(all_percentIncorrectChoice$block_type, levels = c(1,2), labels = c("Win", "Loss"))

df_incorrect_melted <- melt(all_percentIncorrectChoice, id = "block_type")


p2 <- ggplot(data = df_incorrect_melted, aes(x = block_type, y = value, color = block_type)) + geom_point(alpha = 0.5) + ylim(0,100) + 
        stat_summary(fun = "mean", geom = "point", size = 5) + 
        geom_hline(yintercept = 50, linetype = 2) + theme_classic() +
        labs(title="Average Incorrect Choice", x="Block Type", y="% Incorrect") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"),axis.title = element_text(face = "bold"),axis.text = element_text(face = "bold")) + 
        scale_color_manual(values = c("#00668F","#910C00"), name = "Block Type") 


# Correct/Incorrect Data together
#Data prep
df_correct_melted$corr <- rep("Correct", length(df_correct_melted$block_type))
df_incorrect_melted$corr <- rep("Incorrect", length(df_incorrect_melted$block_type))
df_melted <- rbind(df_correct_melted,df_incorrect_melted)


#ggplot(data = df_correct_melted, aes(x = block_type, y = value, color = block_type)) + geom_point(alpha = 0.5) + ylim(0,100) + 
#  stat_summary(fun = "mean", data = df_correct_melted, geom = "point", size = 5) +
#  scale_color_manual(values = c("#00BFC4","#F8766D")) +
#  geom_point(data = df_incorrect_melted, alpha = 0.5, aes(x = block_type, y = value, color = block_type)) +
#  stat_summary(fun = "mean", data = df_incorrect_melted, geom = "point", size = 5) +
#  scale_color_manual(values = c("#00BFC4","#F8766D","#00668F","#910C00")) 

#
df <- tidyr::unite(df_melted,"block_corr",block_type, corr, remove=F)

p3 <- ggplot(data = df, aes(x = block_type, y = value, color = block_corr)) + geom_point(alpha = 0.5) + ylim(0,100) + 
        stat_summary(fun = "mean", data = df, geom = "point", size = 5) +
        geom_hline(yintercept = 50, linetype = 2) + theme_classic() +
        labs(title="Average Correct/Incorrect Choice", x="Block Type", 
             y="% Incorrect                                                               % Correct") +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"), 
          axis.title = element_text(face = "bold"),
          axis.text = element_text(face = "bold"),
          legend.position = "none") + 
        scale_color_manual(values = c("#F8766D","#91342d","#00BFC4","#21a0ab"))  
                                     #(Light Red, Dark Red,  Light Blue, Dark Blue)

#Save out plots
setwd("~/Desktop/graph-examples/Avg_correct_choice_plots")

#Avg Correct Choices
ggsave('Avg_corr_choice.png', plot = p1, width = 2600, height = 2000, units = 'px')   

#Avg Incorrect Choices
ggsave('Avg_InCorr_choice.png', plot = p2, width = 2600, height = 2000, units = 'px')   

#Avg Correct/Incorrect Choices
ggsave('Avg_corr-InCorr_choice.png', plot = p3, width = 2600, height = 2000, units = 'px')   