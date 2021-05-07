library(shiny)
library(shinydashboard)
source("plots.R")

ui <- navbarPage(
    "The Future of Cricket",
    tabPanel("Background",
             h5("How have Indian cricketers performed in T20 cricket since its inception in the early 2000s?"),
             p("Twenty-20, or T20, is the newest format of cricket, which was officially sanctioned by the International Council of Cricket (ICC) in 2004, with the first international men's T20 taking place between Australia and New Zealand in 2005."),
             p("In a Twenty20 game, two teams have a single innings each, which is restricted to a maximum of 20 overs (or 120 balls) or 10 wickets (or outs), whichever elapses first. The team that wins is the team that scores a minimum of 1 run more than the opposition."),
             p("Since its inception, T20 cricket has soared in popularity, as is observable from the chart below, especially with the rise of competitive franchise leagues such as the Indian Premier League. The game is fast-paced and often relies upon batsmen scoring runs at a quick strike-rate, as compared to the 50-over (ODI, i.e. One Day International) format."),
             p("In this analysis, I seek to understand the evolution of T20 cricket in the past 15 years through a number of parameters."),
             
             # br() is a useful function. It adds white space to the page.
             
             br(),
             plotOutput("plot_games")
    ),
    tabPanel("Batsman analysis",
             h4("How have top Indian players fared since their debut?"),
             p("In this analysis, I examine runs scored by a batsman over the course of their entire career and their strike rate. Strike rate (SR) is calculated as runs scored per 100 balls faced, and provides a measure of the pace at which a batsman scores runs."),
             p("The increasing tendency of strike-rate indicates that each batsman has become more aggressive over the course of their careers, with the notable exception of KL Rahul, who started out with a high strike-rate (around 150) and has maintained it since."),
             br(),
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
             h4("What is India's best opening combination?"),
             p("In 2021 and 2022, there are two T20 World Cups scheduled. Based on their performances to-date, can we evaluate India's best openers? What is India's best opening combination?"),
             p("These models project average runs scored as a function of balls faced and batting position."),
             br(),
             plotOutput("plot_1"),
             br(),
             p("The plot above predicts that KL Rahul and Rohit Sharma are India's best opening batsman based on prior performance. While the batsman are within a close range of values, between 51 to 54 runs scored, a 3 run difference in average is not insignificant. T20 games are often decided by 1 run or two."),
             br(),
             plotOutput("plot_2"),
             br(),
             p("This plot projects that India should opt for the opening combination of Rahul and Sharma for their best chance of success in the upcoming World Cups. As we can see, the opening combinations are again within a close range of values (from 103 to 106), which is actually indicative of how well these four batsman have performed in relation to one another."),
             br(),
             br(),
    ),
    tabPanel("About",
             h4("About Me"),
             p("My name is Mohit Mandal, I am a PhD student in the Department of Anthropology at 
             Harvard  University."),
             p("You can reach me at mandal@g.harvard.edu."),
             h4("About this Project"),
             p("I have always followed cricket, and cricket is very much a numbers-obsessed sport. So, it was natural that I would combine my love for cricket with my newfound data analysis skills to glean new insights about the sport."),
             h4("Data sources"),
             p(tags$a(href="https://stats.espncricinfo.com/ci/engine/stats/index.html", "ESPNCricinfo"), "provided all of the data on players and teams. Specifically, I am indebted to the", tags$a(href="https://cran.r-project.org/web/packages/cricketr/index.html", "cricketr"), "package developed by Tinniam V Ganesh, which made it exceedingly easy to scrape this data from ESPNCricinfo."),
             p("You can find my Github repository", tags$a(href="https://github.com/mohitmandal/cricket-project", "here."))))

server <- function(input, output) {
    
    # These code chunks takes the input selection from the user and returns the
    # relevant plot. It's a tidy way of generating the plots on the cloud
    # without needing to save them as images beforehand. The advantage of this
    # method is that the code scrapes and analyses the data each time, meaning
    # that the plots should automatically update after each new cricket game.
    
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
