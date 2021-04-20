library(shiny)

ui <- navbarPage(
    "The Future of Cricket",
    tabPanel("Background",
             h5("How have top cricketers maintained their performance since their debut?"),
             p("For this analysis, I look at the performances of Virat Kohli, Babar Azam, and Rohit Sharma."),
    ),
    tabPanel("Analysis",
             h3("Data processing"),
             p(plotOutput("kohli_ODI_plot"))
    ),
    tabPanel("Model", 
             h3("Projections of Future Performance"),
    ),
    tabPanel("About", 
             h3("Project interests"),
             p("Migration, cricket, labor"),
             h3("About Me"),
             p("My name is Mohit Mandal and I am a PhD student in the Department of Anthropology at 
             Harvard  University. 
             You can reach me at mandal@g.harvard.edu."),
             h3("Data sources"),
             p(tags$a(href="https://www.un.org/development/desa/pd/content/international-migrant-stock", "International Migrant Stock 2020"), ": disaggregated by age, sex, destination and origin country"),
             p(tags$a(href="https://github.com/mohitmandal/finalproject", "Link"), "to repo")))

server <- function(input, output) {
    output$kohli_ODI_plot <- renderImage({
        list(src = 'kohli_ODI_plot.png',
             width = 700,
             height = 600)
    }, deleteFile = FALSE)
    output$plot_income <- renderImage({
        list(src = 'plot_income.png',
             width = 500,
             height = 500)
    }, deleteFile = FALSE)
}


# Run the application 
shinyApp(ui = ui, server = server)
