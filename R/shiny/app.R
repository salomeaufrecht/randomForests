library(shiny)
library(bslib)

# Define UI for app that draws a histogram ----
ui <- page_sidebar(
    # App title ----
    title = "Random Forests",
    # Sidebar panel for inputs ----
    sidebar = sidebar(
        # Input: Slider for the number of bins ----
        sliderInput(
            inputId = "splits",
            label = "Number of splits:",
            min = 1,
            max = 10,
            value = 5
        )
    ),
    # Output: Histogram ----
    plotOutput(outputId = "distPlot")
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
    output$distPlot <- renderPlot({
        
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
