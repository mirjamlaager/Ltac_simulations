setwd("~/workspace/Ltac_simulations")

source("simulation.R")


n_standard_wards <- 5
n_ltac_wards <- 2
beds_per_standard_ward <- 7
beds_per_ltac_ward <- 4
n_days <- 50
mean_length_of_stay <- 4
proportion_colonised_on_admission <- 0.1
within_ward_transmission_rate <- 0.02

simulation_values <- data.frame(n_standard_wards,n_ltac_wards,beds_per_standard_ward,beds_per_ltac_ward,n_days,mean_length_of_stay,proportion_colonised_on_admission)
colnames(simulation_values) <- c("n_standard_wards","n_ltac_wards","beds_per_standard_ward","beds_per_ltac_ward","n_days","mean_length_of_stay","proportion_colonised_on_admission")

write.csv(simulation_values,"outfiles_simulations/simulation_values.csv")

simulation_output <- run_simulations(n_standard_wards, n_ltac_wards, beds_per_standard_ward, beds_per_ltac_ward, 
                            n_days, mean_length_of_stay,proportion_colonised_on_admission, within_ward_transmission_rate)

admission_prevalence <- simulation_output[1]
discharge_prevalence <- simulation_output[2]





