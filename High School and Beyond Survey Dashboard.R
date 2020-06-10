#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(ggplot2)
library(dplyr)
library(openintro)
library(DT)
library(shinythemes)
data(hsb2)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cerulean"),
  
   # Application title
   titlePanel("High School and Beyond Survey Data"),
   h3 ("-Anustha Shrestha"),
   # Sidebar with a slider input for number of bins 
   
   sidebarLayout(
      sidebarPanel(
       wellPanel(
         h4 (strong ("Univariate Plots")),   
         
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30),
         
         selectInput(inputId = "subject",
                     label = "Select subject for univariate analysis:",
                     choices = c("math","read", "write", "science", "socst", "gender", "ses", "race", "schtyp", "prog"),
                     selected = "math")
                     
        
       ), 
       
      wellPanel(
      h4 (strong("Bivariate Plots and Simple Linear Regresssion")),   
         selectInput(inputId = "y",
                     label = "Y-axis:",
                     choices = c("math", "read", "write", "science", "socst"),
                     selected = "math"),
         
         selectInput(inputId = "x",
                     label = "X-axis:",
                     choices = c("math", "read", "write", "science", "socst", "gender", "ses", "race", "schtyp", "prog"),
                     selected = "science")
      
      ),
      
      wellPanel(
        # Show data table
        checkboxInput(inputId = "show_lmOutput",
                      label = "Show Linear Model Output",
                      value = TRUE)
      ),
      
      
      # Built with Shiny by RStudio
      br(), br(),
      h5("Built with", em("Shiny"), "by Anustha Shrestha.")
  
      ),
   
      
      # Show all outputs in tabs
      mainPanel(
        #Tab: Plot
        tabsetPanel(type = "tabs",
         tabPanel(title = "Plots",
         plotOutput("DisplayHist"),
         plotOutput("ScatterPlot"),
         verbatimTextOutput(outputId = "lmoutput")
         ),
        
        #Tab: Data
        tabPanel(title="Data",
                 DT::dataTableOutput(outputId = "hsb2table")),
        
        #Tab:Dictionary
        tabPanel(title = "Dictionary",
                 br(),
                 h2("High School and Beyond Survey"),
                 br(),
                 h4("Description"),
                 br(),
                 "The dataset includes 200 observations sampled randomly from the National Center of Educaitonal Statistics' High School and Beyond Survey.",
                 br(),br(),
                 
                 h4("Source"),
                 br(),
                 "The dataset hsb2 has been directly downloaded from", em("R package, "), strong(em("openintro")),
                 br(),
                 "The original source of this dataset is", strong("UCLA Academic Technology Services."),
                 br(), br(),
                 
                 h4("Variables"),
                 br(),
                 "These are the variables in the dataset:",
                 br(),br(),
                 
                 strong("id"),  "  : Student ID", br(),br(),
                 strong("gender"),"  : Student's gender, with levels female and male", br(), br(),
                 strong("race"),"  : Student's race, with levels african american, asian, hispanic, and white", br(), br(),
                 strong("ses"), "  : Socio economic status of student's family, with levels low, middle, and high", br(), br(),
                 strong("schtyp"), ": Type of school, with levels public and private", br(), br(),
                 strong("prog"), ": Type of program, with levels general, academic, and vocational", br(), br(),
                 strong("read"), ": Standardized reading score", br(), br(),
                 strong("write"), ": Standardized writing score", br(), br(),
                 strong("math"), ": Standardized mathematics score", br(), br(),
                 strong("science"), ": Standardized science score", br(), br(),
                 strong("socst"), ": Standardized social studies score", br(), br()))
      )
   )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$DisplayHist <- renderPlot(
     if(input$subject == "math"|| input$subject == "read"|| input$subject== "write"|| input$subject == "science"|| input$subject == "socst"){
      # generate bins based on input$bins from ui.R
     Score    <- hsb2[,input$subject]
      bins <- seq(min(Score), max(Score), length.out = input$bins + 1)
      label <- (input$subject)
      
      # draw the histogram with the specified number of bins
      hist(Score, breaks = bins, col = 'steelblue', border = 'white', xlab = label, main = paste("Histogram of ", label))
     }
      else {
     ggplot (data= hsb2, aes_string(x=input$subject))+geom_bar(color = "steelblue", fill = "steelblue")
     }
    )
   
  #draw a scatterplot
    output$ScatterPlot <- renderPlot({
      if (input$x == "math" || input$x =="read" || input$x == "write" || input$x == "science" || input$x == "socst")
        ggplot(data = hsb2, aes_string(x = input$x, y = input$y)) +
        geom_point()+ geom_smooth(method = "lm", color = "steelblue", se=TRUE)
      
      else 
      ggplot(data=hsb2, aes_string(x=input$x, y=input$y,fill = input$x))+geom_boxplot() 
      }
    )
  
    # Create regression output
    output$lmoutput <- renderPrint(
      if (input$show_lmOutput) {
      x <- hsb2 %>% pull(input$x)
      y <- hsb2 %>% pull(input$y)
      print(summary(lm(y ~ x, data = hsb2)), digits = 3, signif.stars = FALSE)
    })
    
    # Print data table if checked
    output$hsb2table <- DT::renderDataTable(
    {
        DT::datatable(data = hsb2, 
                      options = list(pageLength = 10), 
                      rownames = FALSE)
      }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)

