library(shiny)
library(dplyr)
library(tibble)
library(ggplot2)
library(googlesheets4)

url <- 'https://docs.google.com/spreadsheets/d/1cXYAd8REWA_Qylx9KehqHwFmYPnaNRhBRTZakcG0reQ/edit#gid=1873886035'


teams <- c('All teams', 'Bichette Happens', 'Capital of Vermont', 'ChiefChompChamp44', "Civil Wright's", 'Drunken Ueckers',
            "Fightin' Hellfish", 'Fluke of Nature', 'Lindor Truffles', 'Orange the New Black', "Rhys's Pieces",
            'The Beach Bombers', 'The Big Red Machine', 'Three True Outcomes', 'Trust the Prospects', 'Waverider',
            'We Got Da Runs')


# Define UI for app that draws a histogram ----
ui <- navbarPage("Dynasty Baseball",
                 tabPanel("Summary Statistics",       
                          selectInput(inputId = "dataset", label = "Dataset", choices = unique(all_teams$Team)),
                          conditionalPanel( condition = "output.nrows",
                                            checkboxInput("headonly", "Only use first 1000 rows")),
                          mainPanel(
                          tabsetPanel(id = 'Measures',
                                      tabPanel("Cap Cost", plotOutput("capPlot")),
                                      tabPanel("Position Depth", plotOutput("posPlot")),
                                      tabPanel("Database", DT::dataTableOutput("mytable3")))
                 )
                 ),
                 tabPanel(title = "Rules",
                          fluidRow(
                            column(5, column(4,img(src = "my.image.png", height = 400, width = 300))))),
                 tabPanel("Communication"),
                 navbarMenu("More",
                            tabPanel("Helpful Links"),
                            tabPanel("Awards"))
              
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  # choose columns to display
  output$capPlot <- renderPlot({
    choice <- subset(all_teams, Team == input$dataset)
    ggplot({choice}, aes(x = reorder(Name, -Cap.Cost), Cap.Cost)) +
             geom_bar(stat = 'identity')
  }, width = 1536, height = 768)
  
  # sorted columns are colored now because CSS are attached to them  ####THIS NEEDS FIXING
  output$posPlot <- renderPlot({
    choice <- subset(merged_all, Team == input$dataset)
    ggplot(choice, aes(x = reorder(Position, -Number.of.Players), y = Number.of.Players))+
      geom_bar(stat = 'identity')
  }, width = 1536, height = 768)
  
  # customize the length drop-down menu; display 5 rows per page by default
  output$mytable3 <- DT::renderDataTable({
    choice <- subset(merged_all, Team == input$dataset)
    DT::datatable(choice, options = list(lengthMenu = c(3), pageLength = 30))
  })
}
shinyApp(ui = ui, server = server)
runGitHub( "dynasty_baseball_dashboard", "adamjst")
