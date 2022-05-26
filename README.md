# graph-examples
- The scripts in this repository will generate graphs for behavioral and Eye-Fixation data from the aDDM Lottery Task <br>
   (https://github.com/sgonzalez122/aDDM_win_loss_lottery). 
   
- I am unable to provide data for the script, but the output can be viewed in the "Plot" folders available in .png, .pdf. and .ppx format

# Description's
#### Psychometric Plots
The Psychometric plots break down into 3 categories 
- % Chose Left: Plots the percentage a subject choice the left option for a give Expected value. 
- RT Curves: Plots the reaction time (miliseconds) for a given Expected value. 
- % Chose Last seen: Plots the percentage the subject chose the last stimulus item they were viewing

NOTE: The Expected value is the Left Stimulu Probability minus the Right Stimulu Probability. <br>
NOTE: Each category has plots for 2 blocks (WIN/LOSS) and for both Raw data and Averaged data

#### Psychometric markdown
The Markdown file will plot the same plots as the psychometric_plots.R file, except instead of ploting everything together
onto one plot the markdown file will plot an individual plot for each participant. This makes it easier when explor the data for outliers 

#### Fixation Plots
- First Fix Duration: Plots the duration of the first fixation for the best and worst choice options for both the WIN and LOSS Blocks
- Fixation Duration: Plots the durationof the First and Last Fixation duration for both the WIN and LOSS Blocks
- Net Fix Duration: Plots the Net Fixation Duration for the Left and Right stimulus for each expected value for both blocks
- Probability First Fix Best: Plots the Probability that the first fixation is the best option for the best and worst choice options for both blocks. 

#### Average Correct Choice Plots
