library(shiny)
library(bslib)
library(shinydashboard)


source("../tree_datastructure.R")
source("../greedy.R")

ui <- dashboardPage(
        dashboardHeader(title = "Random Forests"),
        dashboardSidebar(sidebarMenu(
            menuItem("Greedy", tabName = "greedy", icon = icon("tree")),
            menuItem("Pruning", tabName = "pruning", icon = icon("scissors"))
        )),
        dashboardBody(
            tags$head(
                tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
            ),
            tabItems(
                tabItem(tabName = "greedy",
                        fluidRow(width=12, align="center",
                            
                            box(
                                title = "Settings",
                                collapsible = T,
                                width=12,
                                sliderInput("splits", "Number of splits:", 1, 10, 3)
                            ),
                            
                                   box(title="Greedy Regression", width=12,
                            column(width=12, align="center", 
                                   plotOutput("greedyPlot", width="750px"))
                                       )
                        )
                ),
                tabItem(tabName = "pruning",
                        fluidRow(
                            h2("HELLO")
                        )
                )
            )
        )
    )

# Define server logic required to draw a histogram ----
server <- function(input, output) {
    
    # Histogram of the Old Faithful Geyser Data ----
    # with requested number of bins
    # This expression that generates a histogram is wrapped in a call
    # to renderPlot to indicate that:
    #
    # 1. It is "reactive" and therefore should be automatically
    #    re-executed when inputs (input$bins) change
    # 2. Its output type is a plot
    output$greedyPlot <- renderPlot({
        
        set.seed(123)
        n <- 150
        sigma <- 0.1
        X <- runif(n, 0, 1)
        epsilon <- rnorm(n, 0, sigma)
        Y <- sin(2*pi*X) + epsilon
        
        testtree <- greedy(
            matrix(X, ncol=1),
            matrix(Y, ncol=1),
            input$splits
        )
        
        
        x <- seq(0,1,length.out=100)
        y <- sin(2*pi*x)
        testtree$plot_data()
        lines(x, y, col="red")
        
    })
    
}

shinyApp(ui = ui, server = server)
