require("ggplot2")
require("reshape2")

n_runs <- 10
n_patients_baseline <- rep(0,n_runs)
n_patients_short_baseline <- rep(0,n_runs)
n_patients_medium_baseline <- rep(0,n_runs)
n_patients_long_baseline <- rep(0,n_runs)
n_bed_days_standard_ward_baseline <-rep(0,n_runs)
n_bed_days_ltac_ward_baseline <- rep(1,n_runs)
  
n_patients_s1 <- rep(0,n_runs)
n_patients_short_s1 <- rep(0,n_runs)
n_patients_medium_s1 <- rep(0,n_runs)
n_patients_long_s1 <- rep(0,n_runs)
n_bed_days_standard_ward_s1 <-rep(0,n_runs)
n_bed_days_ltac_ward_s1 <- rep(0,n_runs)

col_while_in_standard_ward_baseline <- rep(0,n_runs)
col_while_in_standard_ward_s1 <- rep(0,n_runs)
col_while_in_ltac_ward_baseline <- rep(0,n_runs)
col_while_in_ltac_ward_s1 <- rep(0,n_runs)

col_and_short_stay_baseline <- rep(0,n_runs)
col_and_medium_stay_baseline <- rep(0,n_runs)
col_and_long_stay_baseline <- rep(0,n_runs)

col_and_short_stay_s1 <- rep(0,n_runs)
col_and_medium_stay_s1 <- rep(0,n_runs)
col_and_long_stay_s1 <- rep(0,n_runs)


#evaluate baseline simulations
for (k in 1:n_runs){

ptLog <- read.csv(paste("outfiles_simulations/ptLog_baseline_",k,".csv",sep=""))
wardLog <- read.csv(paste("outfiles_simulations/wardLog_baseline_",k,".csv",sep=""))
simulation_values <- read.csv(paste("outfiles_simulations/simulation_values_baseline_",k,".csv",sep=""))

n_standard_wards <- simulation_values$n_standard_wards
n_ltac_wards <- simulation_values$n_ltac_wards
beds_per_standard_ward <- simulation_values$beds_per_standard_ward
beds_per_ltac_ward <- simulation_values$beds_per_ltac_ward
n_days <- simulation_values$n_days
mean_length_of_stay <- simulation_values$mean_length_of_stay

n_patients_baseline[k] <- nrow(ptLog)
n_bed_days_standard_ward_baseline[k] <- sum(pmin(ptLog$discharge,ptLog$transfer_to_ltac,na.rm = T) - ptLog$admission +1)
col_while_in_standard_ward_baseline[k] <- sum(ptLog$colonisation >= ptLog$admission & ptLog$colonisation <= ptLog$discharge
                                              & (is.na(ptLog$transfer_to_ltac) | ptLog$colonisation <= ptLog$transfer_to_ltac))

n_patients_short_baseline[k] <- sum(ptLog$discharge - ptLog$admission <= 2)
n_patients_medium_baseline[k] <- sum(ptLog$discharge - ptLog$admission > 2 & ptLog$discharge - ptLog$admission <= 7)
n_patients_long_baseline[k] <- sum( ptLog$discharge - ptLog$admission > 7)

col_and_short_stay_baseline[k] <- sum(ptLog$colonisation >= ptLog$admission & ptLog$colonisation <= ptLog$discharge & ptLog$discharge - ptLog$admission <=2)
col_and_medium_stay_baseline[k] <- sum(ptLog$colonisation >= ptLog$admission & ptLog$colonisation <= ptLog$discharge & ptLog$discharge - ptLog$admission >2 &ptLog$discharge - ptLog$admission <=7 )
col_and_long_stay_baseline[k] <- sum(ptLog$colonisation >= ptLog$admission & ptLog$colonisation <= ptLog$discharge & ptLog$discharge - ptLog$admission >7)
}

#evaluate s1 simulations
for (k in 1:n_runs){
  
  ptLog <- read.csv(paste("outfiles_simulations/ptLog_s1_",k,".csv",sep=""))
  wardLog <- read.csv(paste("outfiles_simulations/wardLog_s1_",k,".csv",sep=""))
  simulation_values <- read.csv(paste("outfiles_simulations/simulation_values_s1_",k,".csv",sep=""))
  
  n_standard_wards <- simulation_values$n_standard_wards
  n_ltac_wards <- simulation_values$n_ltac_wards
  beds_per_standard_ward <- simulation_values$beds_per_standard_ward
  beds_per_ltac_ward <- simulation_values$beds_per_ltac_ward
  n_days <- simulation_values$n_days
  mean_length_of_stay <- simulation_values$mean_length_of_stay
  
  n_patients_s1[k] <- nrow(ptLog)
  n_bed_days_standard_ward_s1[k] <- sum(pmin(ptLog$discharge,ptLog$transfer_to_ltac,na.rm = T) - ptLog$admission +1)
  n_bed_days_ltac_ward_s1[k] <-  sum(ptLog$discharge - ptLog$transfer_to_ltac,na.rm = T)
  col_while_in_standard_ward_s1[k] <- sum(ptLog$colonisation >= ptLog$admission & ptLog$colonisation <= ptLog$discharge
                                          & (is.na(ptLog$transfer_to_ltac) | ptLog$colonisation <= ptLog$transfer_to_ltac))
  
  col_while_in_ltac_ward_s1[k] <- sum(is.na(ptLog$ltac_ward)==FALSE & ptLog$colonisation > ptLog$transfer_to_ltac & +
                                            ptLog$discharge >= ptLog$colonisation)
  
  n_patients_short_s1[k] <- sum(ptLog$discharge - ptLog$admission <= 2)
  n_patients_medium_s1[k] <- sum(ptLog$discharge - ptLog$admission > 2 & ptLog$discharge - ptLog$admission <= 7)
  n_patients_long_s1[k] <- sum( ptLog$discharge - ptLog$admission > 7)
  
  col_and_short_stay_s1[k] <- sum(ptLog$colonisation >= ptLog$admission & ptLog$colonisation <= ptLog$discharge & ptLog$discharge - ptLog$admission <=2)
  col_and_medium_stay_s1[k] <- sum(ptLog$colonisation >= ptLog$admission & ptLog$colonisation <= ptLog$discharge & ptLog$discharge - ptLog$admission >2 &ptLog$discharge - ptLog$admission <=7 )
  col_and_long_stay_s1[k] <- sum(ptLog$colonisation >= ptLog$admission & ptLog$colonisation <= ptLog$discharge & ptLog$discharge - ptLog$admission >7)
  
}






#plot proportion of all patients acquiring colonisation while in a standard/ltac/any ward
scenarios <- rep(c("baseline","scenario 1"),each=(n_runs)*3)
subgroup <- rep(c("standard","ltac","total","standard","ltac","total"),each=n_runs)

proportion <- c(col_while_in_standard_ward_baseline/n_patients_baseline,col_while_in_ltac_ward_baseline/n_patients_baseline,
                (col_while_in_standard_ward_baseline+col_while_in_ltac_ward_baseline)/n_patients_baseline,
                col_while_in_standard_ward_s1/n_patients_s1, col_while_in_ltac_ward_s1/n_patients_s1,
                (col_while_in_standard_ward_s1+ col_while_in_ltac_ward_s1)/n_patients_s1)

data <- data.frame(scenarios, subgroup, proportion)

# draw the histogram with the specified number of bins
#hist(as.numeric(x), col = 'darkgray', border = 'white')
ggplot(data, aes(x=scenarios, y=proportion, fill=subgroup)) + 
  geom_boxplot() + ylab("number of acquisitions per ward / total number of patients") + ggtitle("Proportion of patients acquiring infection")





#plot acquisitions per bed day
scenarios <- rep(c("baseline","scenario 1"),each=(n_runs)*3)
subgroup <- rep(c("standard","ltac","total","standard","ltac","total"),each=n_runs)

proportion <- c(col_while_in_standard_ward_baseline/n_bed_days_standard_ward_baseline,col_while_in_ltac_ward_baseline/n_bed_days_ltac_ward_baseline,
                (col_while_in_standard_ward_baseline+col_while_in_ltac_ward_baseline)/(n_bed_days_standard_ward_baseline+n_bed_days_ltac_ward_baseline),
                col_while_in_standard_ward_s1/n_bed_days_standard_ward_s1, col_while_in_ltac_ward_s1/n_bed_days_ltac_ward_s1,
                (col_while_in_standard_ward_s1+ col_while_in_ltac_ward_s1)/(n_bed_days_standard_ward_s1+n_bed_days_ltac_ward_s1))

data <- data.frame(scenarios, subgroup, proportion)

# draw the histogram with the specified number of bins
#hist(as.numeric(x), col = 'darkgray', border = 'white')
ggplot(data, aes(x=scenarios, y=proportion, fill=subgroup)) + 
  geom_boxplot() + ylab("number of acquisitions per ward / bed days per ward ") + ggtitle("acquisitions per bed day")



#plot acquisitions per patient in the different groups

scenarios <- rep(c("baseline","scenario 1"),each=(n_runs)*3)
subgroup <- rep(c("0 - 3","4 - 7","8+","0 - 3","4 - 7","8+"),each=n_runs)

proportion <- c(col_and_short_stay_baseline/n_patients_short_baseline,col_and_medium_stay_baseline/n_patients_medium_baseline,
                col_and_long_stay_baseline/n_patients_long_baseline,
                col_and_short_stay_s1/n_patients_short_s1, col_and_medium_stay_s1/n_patients_medium_s1,
                col_and_long_stay_s1/n_patients_long_s1)

data <- data.frame(scenarios, subgroup, proportion)

# draw the histogram with the specified number of bins
#hist(as.numeric(x), col = 'darkgray', border = 'white')
ggplot(data, aes(x=scenarios, y=proportion, fill=subgroup)) + 
  geom_boxplot() + ylab("number of acquisitions per group / patients per group ") + ggtitle("acquisitions by length of stay")





