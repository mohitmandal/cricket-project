library(shiny)
library(shinydashboard)
source("plots.R")

ui <- navbarPage(
    "The Future of Cricket",
    tabPanel("Background",
             h5("How have top cricketers performed in T20i since their debut?"),
             p("For this analysis, I look at the performances of Virat Kohli, and Rohit Sharmsa.")
    ),
    tabPanel("Analysis",
             h3("Statistics about batsman"),
             selectInput("batsman",
                         "Select a batsman",
                         c("Virat Kohli", "Rohit Sharma")
             ),
             tabBox(
                 title = NULL, width = 12,
                 
                 # The id lets us use input$tabset1 on the server to find the current tab
                 
                 id = "tabset1", height = "250px",
                 tabPanel("Runs", plotOutput("runs_plot"),
                          p("Test")),
                 tabPanel("Strike Rate", plotOutput("SR_plot"))
             )),
    tabPanel("Model", 
             h3("Projections of Future Performance")
    ),
    tabPanel("About", 
             h3("Project interests"),
             p("Migration, cricket, labor"),
             h3("About Me"),
             p("My name is Mohit Mandal and I am a PhD student in the Department of Anthropology at 
             Harvard  University. 
             You can reach me at mandal@g.harvard.edu."),
             h3("Data sources"),
             p(tags$a(href="https://stats.espncricinfo.com/ci/engine/stats/index.html", "ESPNCricinfo Statsguru")),
             p(tags$a(href="https://github.com/mohitmandal/cricket-project", "Link"), "to repo")))

server <- function(input, output) {
    output$runs_plot <- renderPlot(
        if (input$batsman == "Virat Kohli") {
            kohli_runs_plot
        } else {
            sharma_runs_plot
        }
    )
    output$SR_plot <- renderPlot(
            if (input$batsman == "Virat Kohli") {
                kohli_SR_plot
            } else {
                sharma_SR_plot
            }    
            )
}

# Run the application 
shinyApp(ui = ui, server = server)
