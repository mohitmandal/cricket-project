library(shiny)
library(shinydashboard)
source("plots.R")

ui <- navbarPage(
    "The Future of Cricket",
    tabPanel("Background",
             h5("How have Indian cricketers performed in T20 cricket since its inception in the early 2000s?"),
             p("Twenty-20, or T20, is the newest format of cricket, which was officially sanctioned by the International Council of Cricket (ICC) in 2004, with the first international T20 taking place between the England and New Zealand women's teams."),
             p("In a Twenty20 game, the two teams have a single innings each, which is restricted to a maximum of 20 overs. Since its inception, T20 cricket has soared in popularity, especially with the rise of competitive franchise leagues such as the Indian Premier League. The game is fast-paced and often relies upon batsmen scoring runs at a quick run-rate, as compared to the 50-over format."),
             p("In this analysis, I seek to understand the evolution of T20 cricket in the past 50 years through a number of parameters.")
    ),
    tabPanel("Batsman analysis",
             h4("How have top Indian players fared since their debut?"),
             selectInput("batsman",
                         "Select a batsman",
                         c("Virat Kohli", "Rohit Sharma", "Shikhar Dhawan", "KL Rahul")
             ),
             tabBox(
                 title = NULL, width = 12,
                 id = "tabset1", height = "250px",
                 tabPanel("Runs", plotOutput("runs_plot"),
                          p(" ")),
                 tabPanel("Strike Rate", plotOutput("SR_plot"))
             )),
    tabPanel("Predicting Future Performance", 
             h3("What is India's best opening combination?"),
             p("In 2021 and 2022, there are two T20 World Cups scheduled. Based on their performances to-date, can we calculate what India's best opening combination would be?"),
             plotOutput("plot_1"),
             p("Second plot"),
             plotOutput("plot_2")
    ),
    tabPanel("About",
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
        } 
        else if (input$batsman == "Rohit Sharma") {
            sharma_runs_plot
        } 
        else if (input$batsman == "Shikhar Dhawan") {
            dhawan_runs_plot
        } 
        else {
            klrahul_runs_plot
        }
    )
    output$SR_plot <- renderPlot(
            if (input$batsman == "Virat Kohli") {
                kohli_SR_plot
            } 
            else if (input$batsman == "Rohit Sharma") {
                sharma_SR_plot
            } 
            else if (input$batsman == "Shikhar Dhawan") {
                dhawan_SR_plot
            } 
            else {
                klrahul_SR_plot
            }    
            )
    output$plot_1 <- renderPlot(plot_1)
    output$plot_2 <- renderPlot(plot_2)
}

# Run the application 
shinyApp(ui = ui, server = server)
