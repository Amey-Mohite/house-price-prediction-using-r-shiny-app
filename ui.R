library(shiny)
library(shinydashboard)
library(maps)
library(dplyr)
library(leaflet)
library(shinycssloaders)
library(shinythemes)
library(datadigest)
library(rio)
library(DT)
library(stargazer)
library(shinyWidgets)


data <- read.csv("portland_housing1.csv")
drop_cols <- c()
for (i in colnames(data)){
  if (sum(is.na(data[[i]])) > 4000){
    drop_cols <- c(drop_cols, i)
  }
}
data <- data[ , !(names(data) %in% drop_cols)]

numeric_col <- c()
categorical_col <- c()

for (i in colnames(data)){
  if(class(data[[i]]) == 'character' || class(data[[i]]) == 'logical' )
  {categorical_col <- c(categorical_col,i)}
  else{
    numeric_col <- c(numeric_col,i)
  }
}

num_na_cols <- c()

for(i in numeric_col){
  if(sum(is.na(data[[i]]))>0){
    num_na_cols <- c(num_na_cols,i)
  }
}


cat_na_col <- c()

for(i in categorical_col){
  if(sum(is.na(data[[i]]))>0){
    cat_na_col <- c(cat_na_col,i)
  }
}





dashboardPage(
  
  dashboardHeader(title = "House Price Prediction - Portland Data", dropdownMenuOutput("msgOutput")),
  
  dashboardSidebar(
    sliderInput(
      "Slider1",
      label = h3("Train - Test Split %"),
      min = 0,
      max = 100,
      value = 75
    ),
    status = "warning",
    textOutput("cntTrain"),
    textOutput("cntTest"),
    br()
    
  ),
  dashboardBody(
    fluidPage(
      tags$head(
        tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
      body {
        background-color: black;
        color: black;
      }
      h3 {
        font-family: 'Times New Roman', sans-serif;
        color: white;
      }
      #tabset1{
        background-color: orange;
      }
      .navbar navbar-static-top{
      background-color: orange;
      }
      
     /* logo */
    .skin-blue .main-header .logo {
                          background-color: orange;
                          }

    /* logo when hovered */
    .skin-blue .main-header .logo:hover {
                          background-color: orange;
                          }

    /* navbar (rest of the header) */
    .skin-blue .main-header .navbar {
                          background-color: orange;
                          }        

    /* main sidebar */
    .skin-blue .main-sidebar {
                          background-color: orange;
                          }

      "))
      ),
      box(
        selectInput(
          "X",
          label = "Select Independent variables:",
          choices = names(data),
          multiple = TRUE,
          selected = names(data)
        ),
        solidHeader = TRUE,
        width = "7",
        status = "warning",
        title = "X variable",
      ),
      box(
        selectInput("y", label = "Select target Variable:", choices = names(data)),
        solidHeader = TRUE,
        width = "3",
        status = "warning",
        title = "Y variable"
      )
      
    ),
    
    fluidPage(  
      
      tabBox(
        id = "tabset1",
        height = "1000px",
        width = 12,
        
        
        tabPanel("Data",
                 box(withSpinner(DTOutput(
                   "Data"
                 )), width = 12)),
        
        tabPanel("Data Processing",
                 box(
                   selectInput("var", 
                               label = "Numerical Variables having NA values",
                               choices = num_na_cols,
                               multiple = TRUE,
                               selected = num_na_cols),
                   
                  actionButton("action", "Replace with the Mean Value",class = "btn-warning"),
                  br(),
                  textOutput("selected_var"),
                  textOutput("new_output"), 
                 ),
                 box(
                   selectInput("var_cat", 
                               label = "Categorical Variables having NA values",
                               choices = cat_na_col,
                               multiple = TRUE,
                               selected = cat_na_col),
                   
                   actionButton("action1", "Replace with the Mode Value",class = "btn-warning"),
                   br(),
                   textOutput("selected_var1"),
                   textOutput("new_output1"), 
                 ),
                
                
                 width = 12),
    
        tabPanel(
          "Data Summary",
          box(withSpinner(verbatimTextOutput("Summ")), width = 6),
          box(withSpinner(verbatimTextOutput("Summ_old")), width = 6)
        ),
        tabPanel(
          "Model",
          box(
            withSpinner(verbatimTextOutput("Model")),
            width = 6,
            title = "Model Summary"
          ),
          box(
            withSpinner(verbatimTextOutput("ImpVar")),
            width = 5,
            title = "Variable Importance"
          )
        ),
        tabPanel(
          "Prediction",
          box(withSpinner(plotOutput("Prediction")), width = 6, title = "Best Fit Line"),
          box(withSpinner(plotOutput("residualPlots")), width = 6, title = "Diagnostic Plots")
        )
      )
    )
  )
)
