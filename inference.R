setwd("~/workspace/Ltac_simulations")

source("simulation.R")


n_standard_wards <- 5
n_ltac_wards <- 2
beds_per_standard_ward <- 20
beds_per_ltac_ward <- 10
n_days <- 500
mean_length_of_stay <- 5 #to do: this is not the mean length of stay, change to meaningful number


simulation_values <- data.frame(n_standard_wards,n_ltac_wards,beds_per_standard_ward,beds_per_ltac_ward,n_days,mean_length_of_stay)
colnames(simulation_values) <- c("n_standard_wards","n_ltac_wards","beds_per_standard_ward","beds_per_ltac_ward","n_days","mean_length_of_stay")

write.csv(simulation_values,"outfiles_simulations/simulation_values.csv")

discharge_prevalence <- run_simulations(n_standard_wards, n_ltac_wards, beds_per_standard_ward, beds_per_ltac_ward, 
                            n_days, mean_length_of_stay)








