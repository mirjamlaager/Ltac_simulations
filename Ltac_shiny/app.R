library(shiny)
library(shinythemes)
source("run_simulations_shiny.R")
library(ggplot2)

ui <- fluidPage(theme = shinytheme("journal"),
                
  
                title = "Long Time Acute Care Simulations",
  
titlePanel("Long Time Acute Care Simulations"),

                  fluidRow(
                    column(3,
                           h3("Baseline Scenario"),
                           p("This is the baseline scenario.")
                    ),
                    
                    column(3,
                           h3("Scenario 1"),
                           p("This is scenario 1.")
                    )
                    
                  ),
                  
                  fluidRow(
                    column(3,
                           h4("Ward Setup Baseline"),
                           sliderInput("n_standard_wards_baseline",
                                       "Number of standard wards:",
                                       min = 1,max = 10,value = 5),
                           
                           sliderInput("beds_per_standard_ward_baseline",
                                       "Number of beds per standard ward:",
                                       min = 1,max = 20,value = 15),
                          
                          sliderInput("n_ltac_wards_baseline",
                                "Number of ltac wards:",
                                min = 0,max = 5,value = 0),
                          
                          sliderInput("beds_per_ltac_ward_baseline",
                                      "Number of beds per ltac ward:",
                                      min = 1,max = 20,value = 5),
                          sliderInput("mean_length_of_stay_baseline",
                                      "Mean length of stay:",
                                      min = 1,max = 14,value = 4)
                            ),
                    
                    column(3,
                           h4("Ward Setup Scenario 1"),
                           
                           sliderInput("n_standard_wards_s1",
                                       "Number of standard wards:",
                                       min = 1,max = 10,value = 5),
                           
                           sliderInput("beds_per_standard_ward_s1",
                                       "Number of beds per standard ward:",
                                       min = 1,max = 20,value = 15),
                           
                           sliderInput("n_ltac_wards_s1",
                                       "Number of ltac wards:",
                                       min = 0,max = 5,value = 2),
                           
                           sliderInput("beds_per_ltac_ward_s1",
                                       "Number of beds per ltac ward:",
                                       min = 1,max = 20,value = 5),
                           sliderInput("mean_length_of_stay_s1",
                                       "Mean length of stay:",
                                       min = 1,max = 14,value = 4)
                    ),
                    
                    column(6,
                           plotOutput("distPlot")
                    )
                  ),
                   
                           
                    fluidRow(
                      column(3,
                             h4("Transmission Setup Baseline"),
                             sliderInput("within_ward_transmission_rate_baseline",
                                         "within ward transmission rate:",
                                         min = 0,max = 0.2,value = 0.02,step=0.001),
                             
                             sliderInput("importation_probability_baseline",
                                         "proportion positive on admission:",
                                         min = 0,max = 1,value = 0.1,step=0.01)
                             ),
                      
                      column(3,
                             h4("Transmission Setup Scenario 1"),
                             sliderInput("within_ward_transmission_rate_s1",
                                         "within ward transmission rate:",
                                         min = 0,max = 0.2,value = 0.02,step=0.001),
                             
                             sliderInput("importation_probability_s1",
                                         "proportion positive on admission:",
                                         min = 0,max = 1,value = 0.1,step=0.01)
                      )
                      ),
                  
                  
        fluidRow(
          column(10,
                h3("Simulation Setup"),
                sliderInput("n_days",
                            "Number of days:",
                            min = 1,max = 500,value = 50,step=10),
                sliderInput("n_runs",
                            "Number of simulation runs:",
                            min = 1,max = 100,value = 20),
                
                actionButton("goButton", "Run Simulations"))
                
        )

  
                  )
                
                  
              
         
                


# Define server logic required to draw a histogram
server <- function(input, output) {

  output$distPlot <- renderPlot({
    if (input$goButton == 0)
      return()

    input$goButton
    
    isolate({
    col_on_admission_baseline <- rep(0,input$n_runs)
    col_while_in_standard_ward_baseline <- rep(0,input$n_runs)
    col_while_in_ltac_ward_baseline <- rep(0,input$n_runs)
    col_on_discharge_baseline <- rep(0,input$n_runs)
    n_patients_baseline <- rep(0,input$n_runs)
    
    col_on_admission_s1 <- rep(0,input$n_runs)
    col_while_in_standard_ward_s1<- rep(0,input$n_runs)
    col_while_in_ltac_ward_s1 <- rep(0,input$n_runs)
    col_on_discharge_s1 <- rep(0,input$n_runs)
    n_patients_s1 <- rep(0,input$n_runs)
    
    withProgress(message = 'Running Baseline', value = 0, {
      
    for (run in 1:input$n_runs){
      
    simulation_output <- run_simulations_shiny(input$n_standard_wards_baseline, input$n_ltac_wards_baseline, 
                                               input$beds_per_standard_ward_baseline, input$beds_per_ltac_ward_baseline, 
                                               input$n_days, input$mean_length_of_stay_baseline,input$importation_probability_baseline,
                                                input$within_ward_transmission_rate_baseline)
    
    col_on_admission_baseline[run] <- simulation_output[1]
    col_while_in_standard_ward_baseline[run] <- simulation_output[2]
    col_while_in_ltac_ward_baseline[run] <- simulation_output[3]
    col_on_discharge_baseline[run] <- simulation_output[4]
    n_patients_baseline[run] <- simulation_output[5]
    
    incProgress(1/input$n_runs, detail = paste("Simulation", run))
    }
      col_on_admission_baseline <- as.numeric(col_on_admission_baseline)
      col_while_in_standard_ward_baseline <- as.numeric(col_while_in_standard_ward_baseline)
      col_while_in_ltac_ward_baseline <- as.numeric(col_while_in_ltac_ward_baseline)
      col_on_discharge_baseline <- as.numeric(col_on_discharge_baseline)
      n_patients_baseline <- as.numeric(n_patients_baseline)
    })
    
    
    withProgress(message = 'Running Scenario 1', value = 0, {
      
      for (run in 1:input$n_runs){
        
        simulation_output <- run_simulations_shiny(input$n_standard_wards_s1, input$n_ltac_wards_s1, 
                                                   input$beds_per_standard_ward_s1, input$beds_per_ltac_ward_s1, 
                                                   input$n_days, input$mean_length_of_stay_s1,input$importation_probability_s1,
                                                   input$within_ward_transmission_rate_s1)
   
        col_on_admission_s1[run] <- simulation_output[1]
        col_while_in_standard_ward_s1[run] <- simulation_output[2]
        col_while_in_ltac_ward_s1[run] <- simulation_output[3]
        col_on_discharge_s1[run] <- simulation_output[4]
        n_patients_s1[run] <- simulation_output[5]
        
        incProgress(1/input$n_runs, detail = paste("Simulation", run))
      }
      col_on_admission_s1 <- as.numeric(col_on_admission_s1)
      col_while_in_standard_ward_s1 <- as.numeric(col_while_in_standard_ward_s1)
      col_while_in_ltac_ward_s1 <- as.numeric(col_while_in_ltac_ward_s1)
      col_on_discharge_s1 <- as.numeric(col_on_discharge_s1)
      n_patients_s1 <- as.numeric(n_patients_s1)
      
    })
    
    #prepare dataframe for plotting
    scenarios <- rep(c("baseline","scenario 1"),each=(input$n_runs)*3)
    subgroup <- rep(c("standard","ltac","total","standard","ltac","total"),each=input$n_runs)
    
    proportion <- c(col_while_in_standard_ward_baseline/n_patients_baseline,col_while_in_ltac_ward_baseline/n_patients_baseline,
                    (col_while_in_standard_ward_baseline+col_while_in_ltac_ward_baseline)/n_patients_baseline,
                    col_while_in_standard_ward_s1/n_patients_s1, col_while_in_ltac_ward_s1/n_patients_s1,
                    (col_while_in_standard_ward_s1+ col_while_in_ltac_ward_s1)/n_patients_s1)
    
    data <- data.frame(scenarios, subgroup, proportion)
    
    # draw the histogram with the specified number of bins
    #hist(as.numeric(x), col = 'darkgray', border = 'white')
    ggplot(data, aes(x=scenarios, y=proportion, fill=subgroup)) + 
      geom_boxplot() + ylab("proportion of patients") + ggtitle("Acquisitions")
    
  })
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

