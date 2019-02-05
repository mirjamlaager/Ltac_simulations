#Does it make sense to round the results of gamma to the nearest integer?
#convert mean length of stay to meaningful number

run_simulations <- function(n_standard_wards, n_ltac_wards, beds_per_standard_ward, beds_per_ltac_ward, 
                            n_days, mean_length_of_stay){

  #parameters of the distribution of patient admissions
  patient_admission_shape <- 1
  patient_admission_rate <- 1
  
  patient_length_of_stay_mean <- log(mean_length_of_stay)
  patient_length_of_stay_variance <- 1
  
  #after a number of days in the standard ward, patients are assigned to the latac waiting list
  #unless they number of days to discharge is small
  ltac_assignment_length_of_stay_cutoff <- 7 
  ltac_assignment_days_to_discharge_cutoff <- 2
  ltac_waiting_list <- vector()
  
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
  if (n_ltac_wards > 0){wardLog$ward_number <- c(rep(c(1:n_standard_wards),each=beds_per_standard_ward),rep(c((n_standard_wards+1):(n_standard_wards+n_ltac_wards)),each=beds_per_ltac_ward))
  } else {wardLog$ward_number <- rep(c(1:n_standard_wards),each=beds_per_standard_ward)}
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
  
  #update ltac wards
  if (n_ltac_wards > 0){
  for (ward in (n_standard_wards+1):(n_standard_wards+n_ltac_wards)){
    
    #discharge patients from ltac
    index <- which(wardLog$ward_number == ward & wardLog$bed_occupancy != 0 & wardLog$bed_time_to_event == 0)
    if (length(index) > 0){
      wardLog[index,]$bed_occupancy <- 0
      wardLog[index,]$bed_time_to_event <- round(rgamma(length(index),patient_admission_shape,patient_admission_rate))
    }
    
    #admit patients
    index <- which(wardLog$ward_number == ward & wardLog$bed_occupancy == 0 & wardLog$bed_time_to_event == 0)
    if (length(index) > 0){
      for (k in index){
        if (length(ltac_waiting_list) > 0){
          
        #sample a patient from waiting list
        if (length(ltac_waiting_list) == 1){pt <- ltac_waiting_list[1]}
        else {pt <- sample(ltac_waiting_list,1)}

        #update bed in standard ward
        wardLog[wardLog$bed_occupancy == pt,]$bed_time_to_event <- round(rgamma(1,patient_admission_shape,patient_admission_rate))
        wardLog[wardLog$bed_occupancy == pt,]$bed_occupancy <- 0
        
        #update bed in ltac ward
        wardLog[k,]$bed_occupancy <- pt
        wardLog[k,]$bed_time_to_event <- ptLog[pt,]$discharge - day 
        
        #update patient
        ptLog[pt,]$ltac_ward <- ward
        ptLog[pt,]$transfer_to_ltac <- day
        
        #update waiting list
        ltac_waiting_list <- ltac_waiting_list[ltac_waiting_list != pt]
        }
        
        else {wardLog[k,]$bed_time_to_event <- 1}
      }
    }
  }
  }
  
  #update standard wards
  ltac_waiting_list <- vector()
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
      length_of_stay <- round(rlnorm(1,patient_length_of_stay_mean,patient_length_of_stay_variance)) + 2
      ptLog[n_patients,] <- c(ward, NA, day+1, NA, NA, day + length_of_stay - 1)
      
      #update bed
      wardLog[index[k],]$bed_occupancy <- n_patients
      wardLog[index[k],]$bed_time_to_event <- length_of_stay - 1
      
    } 
    }
    
  
  }
  
  #assign patients to ltac waiting list
  ltac_waiting_list <- which(is.na(ptLog$ltac_ward) == TRUE & 
                               (day - ptLog$admission) > ltac_assignment_length_of_stay_cutoff-1 &
                               (ptLog$discharge - day) > ltac_assignment_days_to_discharge_cutoff)

  wardLog$bed_time_to_event <- wardLog$bed_time_to_event - 1
  
  #some sanity checks
  if (sum(is.na(ptLog$ltac_ward) & ptLog$admission<= day & ptLog$discharge >= day) > n_standard_wards*beds_per_standard_ward){print("Multiple patients allocated to the same bed.")
    print(day)
    print(ptLog)
    print(wardLog)}
  if (sum(duplicated(wardLog[wardLog$bed_occupancy > 0,]$bed_occupancy)) > 0){print("Multiple beds allocated to the same patient.")
    print(day)
    print(ptLog)
    print(wardLog)}
  if (length(which(wardLog$bed_time_to_event < 0)) > 0){print("Negative event times.")
    print(day)
    print(ptLog)
    print(wardLog)}
  }
  
  write.csv(ptLog,"outfiles_simulations/ptLog.csv",row.names = FALSE)
  write.csv(wardLog,"outfiles_simulations/wardLog.csv",row.names = FALSE)
  
  
  
  prevalence_at_discharge <- 0

  return(prevalence_at_discharge)
}