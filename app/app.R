library(shiny)
library(shinydashboard)
source("plots.R")

ui <- navbarPage(
    "The Future of Cricket",
    tabPanel("Background",
             h5("How have Indian cricketers performed in T20 cricket since its inception in the early 2000s?"),
             p("Twenty-20, or T20, is the newest format of cricket, which was officially sanctioned by the International Council of Cricket (ICC) in 2004, with the first international men's T20 taking place between Australia and New Zealand in 2005."),
             p("In a Twenty20 game, two teams have a single innings each, which is restricted to a maximum of 20 overs (or 120 balls). Since its inception, T20 cricket has soared in popularity, as is observable from the chart below, especially with the rise of competitive franchise leagues such as the Indian Premier League. The game is fast-paced and often relies upon batsmen scoring runs at a quick run-rate, as compared to the 50-over (ODI, i.e. One Day International) format."),
             p("In this analysis, I seek to understand the evolution of T20 cricket in the past 15 years through a number of parameters."),
             
             # br() is a useful function. It adds white space to the page.
             
             br(),
             plotOutput("plot_games")
    ),
    tabPanel("Batsman analysis",
             h4("How have top Indian players fared since their debut?"),
             selectInput("batsman",
                         "Select a batsman",
                         c("Virat Kohli", "Rohit Sharma", "Shikhar Dhawan", "KL Rahul")
             ),
             
             # tabBox() is a neat way of displaying multiple panels. I do not
             # need to add each plot separately. Instead, I have included an
             # if-else chain in the server section below, which displays the
             # relevant plot depending on the selection by the user.
             
             tabBox(
                 title = NULL, width = 12,
                 id = "tabset1", height = "250px",
                 tabPanel("Runs", plotOutput("runs_plot"),
                          p(" ")),
                 tabPanel("Strike Rate", plotOutput("SR_plot"))
             )),
    tabPanel("Predicting Future Performance", 
             h3("What is India's best opening combination?"),
             p("In 2021 and 2022, there are two T20 World Cups scheduled. Based on their performances to-date, can we evaluate India's best openers? What is India's best opening combination?"),
             br(),
             plotOutput("plot_1"),
             br(),
             p(" "),
             plotOutput("plot_2")
    ),
    tabPanel("About",
             h3("About Me"),
             p("My name is Mohit Mandal and I am a PhD student in the Department of Anthropology at 
             Harvard  University. 
             You can reach me at mandal@g.harvard.edu."),
             h3("About this Project"),
             p("I have always avidly followed cricket, and cricket has always been a numbers-obsessed sport. So, it made natural sense that I would combine my love for cricket with my newfound data analysis skills to glean new insights about the sport."),
             h3("Data sources"),
             p(tags$a(href="https://stats.espncricinfo.com/ci/engine/stats/index.html", "ESPNCricinfo"), "provided all of the data of this project. Specifically, I am heavily indebted to the cricketr package developed by Tinniam V Ganesh, which made it exceedingly easy to scrape data for players and teams from ESPNCricinfo."),
             p("You can find my Github repository", tags$a(href="https://github.com/mohitmandal/cricket-project", "here."))))

server <- function(input, output) {
    
    # These code chunks takes the input selection from the user and returns the
    # relevant plot. It's a tidy way of generating the plots on the cloud
    # without needing to save them as images beforehand.
    
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
    output$plot_games <- renderPlot(plot_games)
}

# Run the application 
shinyApp(ui = ui, server = server)
