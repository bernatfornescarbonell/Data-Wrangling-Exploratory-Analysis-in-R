library(shiny)
library(tidyverse)
library(dplyr)
library(tidyr)
library(rvest)
library(readxl)
library(stringr)
library(broom)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(viridis)
library(choroplethr)
library(choroplethrMaps)
library(shiny)
library(plyr)
library(faraway)

state_salary <- read_excel("state_May2015_dl.xlsx", na="", col_names = TRUE, col_types = NULL)


state_salary1 <- select(state_salary, ST, STATE, OCC_CODE,  OCC_TITLE, OCC_GROUP, TOT_EMP, LOC_Q, H_MEAN, A_MEAN, H_MEDIAN, A_MEDIAN)

colnames(state_salary1) <- c("ST.code", "State", "OCC.Code",  "Job.title", "Group", "Emp.count", "LOC.Q", "H.mean.wage", "A.mean.wage", "H.median.wage", "A.median.wage")


state_salary1$LOC.Q <- fround(as.numeric(state_salary1$LOC.Q), 2)
state_salary1$H.mean.wage <- fround(as.numeric(state_salary1$H.mean.wage), 2)
state_salary1$A.mean.wage <- fround(as.numeric(state_salary1$A.mean.wage), 2)
state_salary1$H.median.wage <- fround(as.numeric(state_salary1$H.median.wage), 2)
state_salary1$A.median.wage <- fround(as.numeric(state_salary1$A.median.wage), 2)

state_salary1 <- state_salary1 %>% mutate(code2 = substr(state_salary1$OCC.Code, 1, 2), major = "")

state_salary_major <- state_salary1 %>% filter(Group=="major") 


state_salary1$major <- ifelse(state_salary1$code2 == "11", c("Management"), state_salary1$major)
state_salary1$major <- ifelse(state_salary1$code2 == "13", c("Business and Financial Operations"), state_salary1$major)  
state_salary1$major <- ifelse(state_salary1$code2 == "15", c("Computer and Mathematical"), state_salary1$major)
state_salary1$major <- ifelse(state_salary1$code2 == "17", c("Architecture and Engineering Occupations"), state_salary1$major)  
state_salary1$major <- ifelse(state_salary1$code2 == "19", c("Life, Physical, and Social Science"), state_salary1$major)  
state_salary1$major <- ifelse(state_salary1$code2 == "21", c("Community and Social Service"), state_salary1$major)
state_salary1$major <- ifelse(state_salary1$code2 == "23", c("Legal"), state_salary1$major)
state_salary1$major <- ifelse(state_salary1$code2 == "25", c("Education, Training, and Library"), state_salary1$major)
state_salary1$major <- ifelse(state_salary1$code2 == "27", c("Arts, Design, Entertainment, Sports, and Media"), state_salary1$major)
state_salary1$major <- ifelse(state_salary1$code2 == "29", c("Healthcare Practitioners"), state_salary1$major)
state_salary1$major <- ifelse(state_salary1$code2 == "31", c("Healthcare Support"), state_salary1$major)
state_salary1$major <- ifelse(state_salary1$code2 == "33", c("Protective Service"), state_salary1$major)
state_salary1$major <- ifelse(state_salary1$code2 == "35", c("Food Preparation and Serving"), state_salary1$major)
state_salary1$major <- ifelse(state_salary1$code2 == "37", c("Building and Grounds Cleaning and Maintenance"), state_salary1$major)
state_salary1$major <- ifelse(state_salary1$code2 == "39", c("Personal Care and Service"), state_salary1$major)
state_salary1$major <- ifelse(state_salary1$code2 == "41", c("Sales and Related"), state_salary1$major)
state_salary1$major <- ifelse(state_salary1$code2 == "43", c("Office and Administrative Support"), state_salary1$major)
state_salary1$major <- ifelse(state_salary1$code2 == "45", c("Farming, Fishing, and Forestry"), state_salary1$major)
state_salary1$major <- ifelse(state_salary1$code2 == "47", c("Construction and Extraction"), state_salary1$major)
state_salary1$major <- ifelse(state_salary1$code2 == "49", c("Installation, Maintenance, and Repair"), state_salary1$major)
state_salary1$major <- ifelse(state_salary1$code2 == "51", c("Production"), state_salary1$major)
state_salary1$major <- ifelse(state_salary1$code2 == "53", c("Transportation and Material Moving"), state_salary1$major)


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  reduced_df <- reactive({
      state_salary1 %>% filter(Group=='major' & major==input$var)
    
  })
  
  data1 <- reactive({
    switch(input$var2,
           "Annual Wage" = reduced_df()$Emp.count,
           "Location Quotient" = reduced_df()$Emp.count)
    
  })
  
   hist_title <- reactive({
    switch(input$var2,
          "Annual Wage" = "Total Employment count (Numbers), by State & Occupation",
          "Location Quotient" =  "Mean Annual Wage (USD), by State & Occupation")
  
  })
  
   choro_title <- reactive({
     switch(input$var2,
            "Annual Wage" =  "United States - Annual Mean Salary, 2015 by State & Occupation",
            "Location Quotient" =  "United States - Location Quotient, 2015 by Occupation")
     
   })
   
   choro_legend <- reactive({
     switch(input$var2,
            "Annual Wage" =  "USD",
            "Location Quotient" =  "Loc Quotient")
     
   })
  
 
  output$main_plot <- renderPlot({
 
    ggplot(reduced_df(), aes(reduced_df()$State, data1())) + geom_bar(aes(fill=data1()), stat = "identity", width = 0.6, fill = "lightgreen") + geom_text(label = data1()) + coord_flip() + ggtitle(hist_title()) + xlab(NULL) + ylab(NULL) 
  })  
 
  output$results <- renderPlot({
    
      if (input$var2 == "Annual Wage") {  
        State_salary_comp_choro <- reduced_df() %>% filter(State != "Guam" & State != "Puerto Rico" & State != "Virgin Islands") %>% select(State, A.mean.wage)
        State_salary_comp_choro$A.mean.wage <- as.numeric(State_salary_comp_choro$A.mean.wage)
        
    
    } else {
      State_salary_comp_choro <- reduced_df() %>% filter(State != "Guam" & State != "Puerto Rico" & State != "Virgin Islands") %>% select(State, LOC.Q)
      State_salary_comp_choro$LOC.Q <- as.numeric(State_salary_comp_choro$LOC.Q)
    
        }
    
    
        colnames(State_salary_comp_choro) <- c("region", "value")
        State_salary_comp_choro$region <- str_to_lower(State_salary_comp_choro$region)
        state_choropleth(State_salary_comp_choro, 
        title  = choro_title(), legend = choro_legend(), num_colors = 1)
  })
  })
  
 

