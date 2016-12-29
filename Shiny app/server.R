
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

state_salary2 <- state_salary1 %>% filter(Group=='total')

## load and clean Crime data file
crime_file <- read_excel("US_Crime_2015.xls", skip=3, na="", col_names = TRUE, col_types = NULL)

## Add 'Area' value to each row

prevarea <- crime_file$Area    #Moved Area column data into a character vector


for(i in 2:nrow(crime_file))  # FOR loop to fill Area column value correctly whereever its blank or NA in the dataset
{ 
  if (is.na(prevarea[i])) {
    prevarea[i] <- prevarea[i-1]  # when area value blank, fill it from previous row area value
  } 
}

Crime_file1 <- data.frame(prevarea, crime_file)   

Crime_file1 <- Crime_file1 %>% filter(Crime_file1$Year == "2015.000000")  # select rows only for  2015


colnames(Crime_file1) <- c("State", "area", "year", "population", "violent.crime", "violent.crime.rate", "murder", "murder.rate", "rape", "rape.rate", "rape.legacy", "rape.legacy.rate", "robbery", "robbery.rate", "assault", "assault.rate", "property.crime", "property.crime.rate", "burglary", "burglary.rate", "larceny.theft", "larceny.theft.rate", "vehicle.theft", "vehicle.theft.rate","a","b")

Crime_file1$State <- tolower(Crime_file1$State) 

state_list <- data.frame(state_salary2$ST.code, state_salary2$State)  # Use state_salary2 dataframe to build state_list dataframe that will have 2 columns , state name and state code. 

colnames(state_list) <- c("state.code", "state")
state_list$state <- str_trim(tolower(state_list$state))  


Crime_file1$State <- str_trim(gsub(",","",str_trim(str_replace_all(Crime_file1$State,"[0-9]+",""))))    #clean Crime_file1 column to remove numeric digits from state name  


crime_file3 <- left_join(Crime_file1, state_list, by=c("State" = "state"))  # join crime table and state list table on state name

crime_file4 <- crime_file3 %>% filter(!is.na(state.code))    # select rows that have valid value for state code
crime_file4$State <- str_trim(crime_file4$State)
crime_file4$year <- fround(as.numeric(crime_file4$year), 0)   # remove decimals/zeros from column Year
crime_file4$population <- as.numeric(fround(as.numeric(crime_file4$population), 0)) # remove decimals/zeros from column population
crime_file4$violent.crime <- as.numeric(fround(as.numeric(crime_file4$violent.crime), 2))
crime_file4$violent.crime.rate <- as.numeric(fround(as.numeric(crime_file4$violent.crime.rate), 2))
crime_file4$murder <- as.numeric(fround(as.numeric(crime_file4$murder), 2))
crime_file4$murder.rate <- as.numeric(fround(as.numeric(crime_file4$murder.rate), 2))
crime_file4$rape <- as.numeric(fround(as.numeric(crime_file4$rape), 2))
crime_file4$rape.rate <- as.numeric(fround(as.numeric(crime_file4$rape.rate), 2))
crime_file4$rape.legacy <- as.numeric(fround(as.numeric(crime_file4$rape.legacy), 2))
crime_file4$rape.legacy.rate <- as.numeric(fround(as.numeric(crime_file4$rape.legacy.rate), 2))
crime_file4$robbery <- as.numeric(fround(as.numeric(crime_file4$robbery), 2))
crime_file4$robbery.rate <- as.numeric(fround(as.numeric(crime_file4$robbery.rate), 2))
crime_file4$assault <- as.numeric(fround(as.numeric(crime_file4$assault), 2))
crime_file4$assault.rate <- as.numeric(fround(as.numeric(crime_file4$assault.rate), 2))
crime_file4$property.crime <- as.numeric(fround(as.numeric(crime_file4$property.crime), 2))
crime_file4$property.crime.rate <- as.numeric(fround(as.numeric(crime_file4$property.crime.rate), 2))
crime_file4$burglary <- as.numeric(fround(as.numeric(crime_file4$burglary), 2))
crime_file4$burglary.rate <- as.numeric(fround(as.numeric(crime_file4$burglary.rate), 2))
crime_file4$larceny.theft <- as.numeric(fround(as.numeric(crime_file4$larceny.theft), 2))
crime_file4$larceny.theft.rate <- as.numeric(fround(as.numeric(crime_file4$larceny.theft.rate), 2))
crime_file4$vehicle.theft <- as.numeric(fround(as.numeric(crime_file4$vehicle.theft), 2))
crime_file4$vehicle.theft.rate <- as.numeric(fround(as.numeric(crime_file4$vehicle.theft.rate), 2))

crime_file4$Total <- rowSums(crime_file4[,c(5,7,9,13,15,17,19,21,23)])   # Total the crime count in a new column 'Total'

crime_file4$Total.rate <- rowSums(crime_file4[,c(6,8,10,14,16,18,20,22,24)])   # Total the "crime rate per 100,000"   in a new column 'Total.rate'

### Crime data cleaning end ##########

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
    
    ggplot(reduced_df(), aes(reduced_df()$State, data1())) + geom_bar(aes(fill=data1()), stat = "identity", width = 0.6, fill = "lightgreen") + geom_text(label = data1()) + coord_flip() + ggtitle(hist_title()) + xlab(NULL) + ylab(NULL) + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
      
      
      
      
      
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
                     title  = choro_title(), legend = choro_legend(), num_colors = 9)
  })
  
  output$results1 <- renderPlot({
    
    if (input$var3 == "Total Crime Rate - all category types") {  
      State_crime_comp_choro <- crime_file4 %>% select(State, Total.rate)
      State_crime_comp_choro$Total.rate <- as.numeric(State_crime_comp_choro$Total.rate)}
  
    if (input$var3 == "Violent Crime") {  
          State_crime_comp_choro <- crime_file4 %>% select(State, violent.crime.rate)
          State_crime_comp_choro$violent.crime.rate <- as.numeric(State_crime_comp_choro$violent.crime.rate)}
      
     
    if (input$var3 == "Murder") {  
      State_crime_comp_choro <- crime_file4 %>% select(State, murder.rate)
      State_crime_comp_choro$murder.rate <- as.numeric(State_crime_comp_choro$murder.rate)}
    
    if (input$var3 == "Rape") {  
      State_crime_comp_choro <- crime_file4 %>% select(State, rape.rate)
      State_crime_comp_choro$rape.rate <- as.numeric(State_crime_comp_choro$rape.rate)}
    
    if (input$var3 == "Robbery") {  
      State_crime_comp_choro <- crime_file4 %>% select(State, robbery.rate)
      State_crime_comp_choro$robbery.rate <- as.numeric(State_crime_comp_choro$robbery.rate)}
    
    if (input$var3 == "Assault") {  
      State_crime_comp_choro <- crime_file4 %>% select(State, assault.rate)
      State_crime_comp_choro$assault.rate <- as.numeric(State_crime_comp_choro$assault.rate)}
    
    if (input$var3 == "Property Crime") {  
      State_crime_comp_choro <- crime_file4 %>% select(State, property.crime.rate)
      State_crime_comp_choro$property.crime.rate <- as.numeric(State_crime_comp_choro$property.crime.rate)}
    
    if (input$var3 == "Burglary") {  
      State_crime_comp_choro <- crime_file4 %>% select(State, burglary.rate)
      State_crime_comp_choro$burglary.rate <- as.numeric(State_crime_comp_choro$burglary.rate)}
    
    if (input$var3 == "Larceny Theft") {  
      State_crime_comp_choro <- crime_file4 %>% select(State, larceny.theft.rate)
      State_crime_comp_choro$larceny.theft.rate <- as.numeric(State_crime_comp_choro$larceny.theft.rate)}
    
    if (input$var3 == "Vehicle Theft") {  
      State_crime_comp_choro <- crime_file4 %>% select(State, vehicle.theft.rate)
      State_crime_comp_choro$vehicle.theft.rate <- as.numeric(State_crime_comp_choro$vehicle.theft.rate)}
    
    colnames(State_crime_comp_choro) <- c("region", "value")
    State_crime_comp_choro$region <- str_to_lower(State_crime_comp_choro$region)
    state_choropleth(State_crime_comp_choro, 
                     title  = "United States Crime Rate 2015", legend = "Rate per 100,000", num_colors = 9)
  })
})





