# Just a script to subset the fitness data to what we need for these analyses and merge in competition variable.

load('~/Dropbox/KRSP/Process_Data/Processed_Data/all+ID.RData')
#load('~/Dropbox/KRSP/Process_Data/Processed_Data/fn.RData')

behav.data <- subset(all.data, select=c(ID, Sex, Grid, Year, julian, Trial.id,
 Obs, docil, HE.year, Study, front, attack.rate, back, ln.attack.latency, 
 ln.approach.latency, hole.rate, jump.rate, chew, still, hang, groom ,walk, 
 fecal, trial.life, trial.year))

names(behav.data)[names(behav.data) == "HE.year"] <- "handlevent.year"
names(behav.data)[names(behav.data) == "Trial.id"] <- "trial.id"



save(behav.data, file="data/krsp-data/behavior.RData")