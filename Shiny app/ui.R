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

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("United States National Occupational Employment and Wage Estimates 2015 / US Crime Rate 2015 "),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create demographic maps with
               information from the 2015 US National occupational employment and wage estimates & 2015 US Crime data"),
      
      selectInput("var",
                  label = "Choose a Data Item to display",
                  choices = c("Management",
                              "Business and Financial Operations",
                              "Computer and Mathematical",
                              "Architecture and Engineering Occupations",
                              "Life, Physical, and Social Science",
                              "Community and Social Service",
                              "Legal",
                              "Education, Training, and Library",
                              "Arts, Design, Entertainment, Sports, and Media",
                              "Healthcare Practitioners",
                              "Healthcare Support",
                              "Protective Service",
                              "Food Preparation and Serving",
                              "Building and Grounds Cleaning and Maintenance",
                              "Personal Care and Service",
                              "Sales and Related",
                              "Office and Administrative Support",
                              "Farming, Fishing, and Forestry",
                              "Construction and Extraction",
                              "Installation, Maintenance, and Repair",
                              "Production",
                              "Transportation and Material Moving"),
                  selected = "Computer & Mathematical Occupations"),
      
      selectInput("var2",
                  label = "Choose the Choropleth Map type",
                  choices = c("Annual Wage", "Location Quotient"),
                  selected = "Annual Wage"),
      
      selectInput("var3",
                  label = "Choose the crime type",
                  choices = c("Total Crime Rate - all category types","Violent Crime", "Murder", "Rape", "Robbery", "Assault", "Property Crime", "Burglary","Larceny Theft", "Vehicle Theft"),
                  selected = "Total Crime Rate - all category types")
      
      ),
    
    
    
    mainPanel(
      plotOutput("main_plot"),
      plotOutput("results"),
      plotOutput("results1") 
      
      
    )
  )
))
