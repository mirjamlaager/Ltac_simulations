


#to do: Does it make sense to round the results of gamma to the nearest integer
#to do: what happens to patients who have been admitted and discharged on the same day?
#if avoid: think about what that means for the mean length of stay

run_simulations <- function(n_standard_wards, n_ltac_wards, beds_per_standard_ward, beds_per_ltac_ward, 
                            n_days, mean_length_of_stay)
  
{
  #parameters of the distribution of patient admissions
  patient_admission_shape <- 2
  patient_admission_rate <- 1
  
  patient_length_of_stay_mean <- log(mean_length_of_stay)
  patient_length_of_stay_variance <- 1
  

  
  ####
  ## initialise ward log
  ####
  
  #Each row is a bed. The bed occupancy is either 0 if the bed is empty or a patient number if the bed
  #is occupied. For empty beds the bed_time_to_event denotes the number of days until a new patient
  #will be added to that bed. For occupied beds the bed_time_to_event denots the number of days until a
  #patient will be discharged from the hospital (transfers to ltac are not included in time to event).

  n_beds <- n_standard_wards*beds_per_standard_ward + n_ltac_wards*beds_per_ltac_ward
  wardLog <- data.frame(matrix(nrow = n_beds, ncol = 4))
  colnames(wardLog) <- c("ward_number","ward_type","bed_occupancy","bed_time_to_event")
  wardLog$ward_type <- c(rep("s",n_standard_wards*beds_per_standard_ward),rep("ltac",n_ltac_wards*beds_per_ltac_ward))
  wardLog$ward_number <- c(rep(c(1:n_standard_wards),each=beds_per_standard_ward),rep(c((n_standard_wards+1):(n_standard_wards+n_ltac_wards)),each=beds_per_ltac_ward)) 
  wardLog$bed_occupancy <- 0
  wardLog$bed_time_to_event <- round(rgamma(n_beds,patient_admission_shape,patient_admission_rate))
   
  ### 
  #initialise patient log
  ###
  
  ptLog <- data.frame(matrix(nrow = 0, ncol = 6))
  colnames(ptLog) <- c("standard_ward","ltac_ward","admission", "colonisation", "transfer_to_ltac", "discharge")
  n_patients <- 0
 
  
  ###
  #update ward log
  ###
  for (day in 1:n_days){
  
  
  #update the ltac wards
  #discharge patients from ltac
  #refill beds in ltac according to ltac waiting list
  #update that patients bed in the standard ward
  #update that patients ptLog entry
  #remove patients from latac waiting list


  
  #update the standard wards
  
  for (ward in 1:n_standard_wards){
    #discharge patients 
    index <- which(wardLog$ward_number == ward & wardLog$bed_occupancy != 0 & wardLog$bed_time_to_event == 0)
    if (length(index) > 0){
    wardLog[index,]$bed_occupancy <- 0
    wardLog[index,]$bed_time_to_event <- round(rgamma(length(index),patient_admission_shape,patient_admission_rate))
    }
    
    #admit patients
    index <- which(wardLog$ward_number == ward & wardLog$bed_occupancy == 0 & wardLog$bed_time_to_event == 0)
    if (length(index) > 0){
    for (k in 1:length(index)){
      #create a new patient
      n_patients <- n_patients + 1
      length_of_stay <- round(rlnorm(1,patient_length_of_stay_mean,patient_length_of_stay_variance))
      ptLog[n_patients,] <- c(ward,NA,day,NA,NA,day+length_of_stay)
      
      #update bed
      wardLog[index[k],]$bed_occupancy <- n_patients
      wardLog[index[k],]$bed_time_to_event <- length_of_stay
      
    } 
    }
  }
  
  wardLog$bed_time_to_event <- wardLog$bed_time_to_event - 1
  }
  
  write.csv(ptLog,"outfiles_simulations/ptLog")
  write.csv(wardLog,"outfiles_simulations/wardLog")
  
  
  
  prevalence_at_discharge <- 0

  return(prevalence_at_discharge)
}