library(shiny)
library(bslib)
library(shinydashboard)
library(shinycssloaders)
library(randomForests)

options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

source("../tree_datastructure.R")
source("../greedy.R")

ui <- dashboardPage(
        dashboardHeader(title = "Random Forests"),
        dashboardSidebar(sidebarMenu(
            menuItem("Greedy Regression", tabName = "greedy", icon = icon("wave-square")),
            menuItem("Greedy Classification", tabName = "cl", icon = icon("folder-tree")),
            # menuItem("Pruning", tabName = "pruning", icon = icon("scissors")),
            menuItem("Random Forests", tabName = "rf", icon = icon("tree"))
        )),
        dashboardBody(
            tags$head(
                tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
            ),
            tabItems(
                tabItem(tabName = "greedy",
                            fluidRow(
                                box(
                                    title = "Settings",
                                    collapsible = T,
                                    width=6,
                                    sliderInput("splits", "Number of splits:", 1, 10, 3)
                                ),
                                box(
                                    title = "Explanation",
                                    collapsible = T,
                                    width=6,
                                    p("A greedy algorithm is a heuristic algorithm that makes locally optimal decisions. In this case the, algorithmn builds a decision tree with k levels. This means that on the inital level, the data is bipartitioned. Then, on every level, all exiting partitions are bipartitioned. Each leaf contains a prediction constant, which will be the regression prediction for all values that fall into the partition it corresponds to. Each split improves the prediction accurary for the training data, but can lead to overfitting.")
                                ),
                            ),
                        fluidRow(width=12, align="center",
                            
                                   box(title="Greedy Regression", width=12,
                            column(width=12, align="center", 
                                   plotOutput("greedyPlot", width="750px"))
                                       )
                        )
                ),
                tabItem(tabName = "cl",
                              fluidRow(
                                # box(
                                #    title = "Explanation",
                                #    collapsible = T,
                                #    width=6,
                                #    p("Overfitting means creating a model that matches (memorizes) the training set so closely that the model fails to make correct predictions on new data. An overfit model is analogous to an invention that performs well in the lab but is worthless in the real world.")
                                #),
                            ),
                        fluidRow(width=12, align="center",
                                   box(title="Classification", width=12,
                            column(width=12, align="center", 
                                   withSpinner(plotOutput("classifiedPlot", width="750px")),type=2),
                           p(align="right", "May take a while to load... ")
                                       ),
                        ),
                ),
                tabItem(tabName = "rf",
                        
                        fluidRow(width=12, align="center",
                            box(width=12,title="Random Forest Results",
                            withSpinner(verbatimTextOutput("rfText"), type=1),
                            )
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
    
    output$classifiedPlot <- renderPlot({
        
        set.seed(123)
        n <- 400
        X1 <- runif(n, 0, 1)
        X2 <- runif(n, -1, 1)
        Y <- as.integer((X2 > sin(2*pi*X1)) + (X1 > 0.5)) + 1
        
        X <- matrix(c(X1, X2), ncol=2)
        Y <- matrix(Y, ncol=1)
        testtree <- greedy(X,Y, classification_tree = TRUE)
        
        testtree$plot_data()
        
        
    })
    
    output$rfText <- renderText({
    
    iristest <- as.matrix(iris[, 1:4])
    
    random_rows <- sample(1:nrow(iristest), 70)
    
    greedy_tree <- greedy(
        iristest[random_rows, 1:3, drop=FALSE],
        iristest[random_rows, 4, drop=FALSE],
    )
    
    forest_function <- random_forest(iristest[random_rows, 1:3, drop=FALSE],
                                     iristest[random_rows, 4, drop=FALSE])
    
    print("testing tree: ")
    for (i in 1:10) {
        random_rows <- sample(1:nrow(iristest), 100)
        greedy_matrix <- matrix(
            c(iristest[random_rows, ],
              sapply(random_rows, \(x) greedy_tree$decide(iristest[x, 1:3]))
            ),
            ncol = 5
        )
        
        forest_matrix <- matrix(
            c(iristest[random_rows, ],
              sapply(random_rows, \(x) forest_function(iristest[x, 1:3]))
            ),
            ncol = 5
        )
}
        greedy_error <- sum(abs(greedy_matrix[, 4] - greedy_matrix[, 5]))/nrow(greedy_matrix)
        forest_error <- sum(abs(forest_matrix[, 4] - forest_matrix[, 5]))/nrow(forest_matrix)
        paste0("Average error with greedy: ", greedy_error, "\n", "Average error with random Forest: ", forest_error, "\n")
    })
    
    
    
}

shinyApp(ui = ui, server = server)
