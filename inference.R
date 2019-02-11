setwd("~/workspace/Ltac_simulations")

source("simulation.R")

n_runs <- 10
n_days <- 300
#baseline

n_standard_wards <- 7
n_ltac_wards <- 0
beds_per_standard_ward <- 15
beds_per_ltac_ward <- 15

mean_length_of_stay <- 4
proportion_colonised_on_admission <- 0.1
within_ward_transmission_rate <- 0.02

for (run in 1:n_runs){

simulation_values <- data.frame(n_standard_wards,n_ltac_wards,beds_per_standard_ward,beds_per_ltac_ward,n_days,mean_length_of_stay,proportion_colonised_on_admission)
colnames(simulation_values) <- c("n_standard_wards","n_ltac_wards","beds_per_standard_ward","beds_per_ltac_ward","n_days","mean_length_of_stay","proportion_colonised_on_admission")

write.csv(simulation_values,paste("outfiles_simulations/simulation_values_baseline_",run,".csv",sep = ""))

output<- run_simulations(n_standard_wards, n_ltac_wards, beds_per_standard_ward, beds_per_ltac_ward, 
                n_days, mean_length_of_stay,proportion_colonised_on_admission, within_ward_transmission_rate)

write.csv(output[1],paste("outfiles_simulations/ptLog_baseline_",run,".csv",sep=""),row.names = FALSE)
write.csv(output[2],paste("outfiles_simulations/wardLog_baseline_",run,".csv",sep=""),row.names = FALSE)

}

#scenario 1


n_standard_wards <- 6
n_ltac_wards <- 1
beds_per_standard_ward <- 15
beds_per_ltac_ward <- 15
mean_length_of_stay <- 4
proportion_colonised_on_admission <- 0.1
within_ward_transmission_rate <- 0.02

for (run in 1:n_runs){
  
  simulation_values <- data.frame(n_standard_wards,n_ltac_wards,beds_per_standard_ward,beds_per_ltac_ward,n_days,mean_length_of_stay,proportion_colonised_on_admission)
  colnames(simulation_values) <- c("n_standard_wards","n_ltac_wards","beds_per_standard_ward","beds_per_ltac_ward","n_days","mean_length_of_stay","proportion_colonised_on_admission")
  
  write.csv(simulation_values,paste("outfiles_simulations/simulation_values_s1_",run,".csv",sep = ""))
  
  output<- run_simulations(n_standard_wards, n_ltac_wards, beds_per_standard_ward, beds_per_ltac_ward, 
                           n_days, mean_length_of_stay,proportion_colonised_on_admission, within_ward_transmission_rate)
  
  write.csv(output[1],paste("outfiles_simulations/ptLog_s1_",run,".csv",sep=""),row.names = FALSE)
  write.csv(output[2],paste("outfiles_simulations/wardLog_s1_",run,".csv",sep=""),row.names = FALSE)
  
}





