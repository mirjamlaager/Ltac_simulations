require("ggplot2")
require("reshape2")
ptLog <- read.csv("outfiles_simulations/ptLog.csv")
wardLog <- read.csv("outfiles_simulations/wardLog.csv")
simulation_values <- read.csv("outfiles_simulations/simulation_values.csv")
  
n_standard_wards <- simulation_values$n_standard_wards
n_ltac_wards <- simulation_values$n_ltac_wards
beds_per_standard_ward <- simulation_values$beds_per_standard_ward
beds_per_ltac_ward <- simulation_values$beds_per_ltac_ward
n_days <- simulation_values$n_days
mean_length_of_stay <- simulation_values$mean_length_of_stay





#plot standard ward occupancy over time
standard_ward_occupancy <- data.frame(matrix(NA, nrow = n_days, ncol = n_standard_wards+1))
colnames(standard_ward_occupancy) <- c("time",1:n_standard_wards)
standard_ward_occupancy[,1] <- 1:n_days

for (day in 1:n_days){
  for (ward in 1:n_standard_wards){
    standard_ward_occupancy[day,ward+1] <- sum(ptLog$standard_ward == ward & ptLog$admission <= day & ptLog$discharge>=day)
  }
}


melted_standard_ward_occupancy <- melt(standard_ward_occupancy,id.vars = "time")
colnames(melted_standard_ward_occupancy) <- c("time","ward","n_patients")
c1 <- ggplot(melted_standard_ward_occupancy, aes(x=time,y=n_patients,color = ward))
c1 <- c1+ geom_step()
c1 <- c1 + labs(title = "all standard wards")
print(c1)

for (ward in 1:n_standard_wards){
  melted_standard_ward_occupancy <- melt(standard_ward_occupancy[,c(1,ward+1)],id.vars = "time")
  colnames(melted_standard_ward_occupancy) <- c("time","ward","n_patients")
  c1 <- ggplot(melted_standard_ward_occupancy, aes(x=time,y=n_patients))
  c1 <- c1+ geom_step() 
  c1 <- c1 + labs(title = paste("patients in standard ward",ward))
  print(c1)
}



#plot ltac ward occupancy over time

n_days <- 50
ltac_ward_occupancy <- data.frame(matrix(NA, nrow = n_days, ncol = n_ltac_wards+1))
colnames(ltac_ward_occupancy) <- c("time",1:n_ltac_wards)
ltac_ward_occupancy[,1] <- 1:n_days

for (day in 1:n_days){
  k <- 2
  for (ward in (n_standard_wards + 1):(n_standard_wards + n_ltac_wards)){
    ltac_ward_occupancy[day,k] <- sum(ptLog$ltac_ward == ward & ptLog$transfer_to_ltac <= day & ptLog$discharge>=day, na.rm=TRUE)
    k <- k + 1
  }
}


melted_ltac_ward_occupancy <- melt(ltac_ward_occupancy,id.vars = "time")
colnames(melted_ltac_ward_occupancy) <- c("time","ward","n_patients")
c1 <- ggplot(melted_ltac_ward_occupancy, aes(x=time,y=n_patients,color = ward))
c1 <- c1+ geom_step()
c1 <- c1 + labs(title = "all ltac wards")
print(c1)

for (ward in 1:n_ltac_wards){
  melted_ltac_ward_occupancy <- melt(ltac_ward_occupancy[,c(1,ward+1)],id.vars = "time")
  colnames(melted_ltac_ward_occupancy) <- c("time","ward","n_patients")
  c1 <- ggplot(melted_ltac_ward_occupancy, aes(x=time,y=n_patients))
  c1 <- c1+ geom_step() 
  c1 <- c1 + labs(title = paste("patients in ltac ward",ward))
  print(c1)
}




#plot length of stay distribution
length_of_stays <- ptLog$discharge - ptLog$admission
c1 <- ggplot() + aes(length_of_stays)+ geom_histogram(binwidth = 1, color="white", fill ="black") + theme_light()
c1 









