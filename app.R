#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

ui <- fluidPage(
    h1("Plot click demo"),
    plotOutput("plot", click = "plot_click"),
    actionButton("reset", "Reset"),
    actionButton("sample", "Sample")
)

server <- function(input, output, session) {
    
    v <- reactiveValues(
        click1  = NULL,
        vals    = NULL
    )
    
    s <- reactiveValues(
        go = 0
    )
    
    # Handle clicks on the plot
    observeEvent(input$plot_click, {
        if (is.null(v$click1)) {
            # We don't have a first click, so this is the first click
            v$click1 <- input$plot_click;
            v$vals   <- NULL
        } else {
            # We already had a first click, so this is the second click.
            v$vals <- c(v$click1$x, v$click1$y, input$plot_click$x,
                        input$plot_click$y);
            # And clear the first click so the next click starts a new
            # range.
            v$click1 <- NULL
        }
    })
    
    observeEvent(input$reset, {
        # Reset both the range and the first click, if any.
        v$vals   <- NULL
        v$click1 <- NULL
        s$go     <- 0
    })
    
    observeEvent(input$sample, {
        s$go <- 1;
    })
    
    output$plot <- renderPlot({
        if(s$go == 0 | is.null(v$vals[1])){
            plot(x = 0, y = 0, type = "n", xlim = c(0, 100), ylim = c(0, 100));
            if (!is.null(v$click1$x)){
                points(x = v$click1$x, y = v$click1$y, pch = 20, col = "red", 
                       cex = 3)
            }
            if (!is.null(v$vals[1])){
                xvals <- c(v$vals[1], v$vals[3]);
                yvals <- c(v$vals[2], v$vals[4]);
                points(x = xvals, y = yvals, pch = 20, col = "red", cex = 3)
                points(x = xvals, y = yvals, type = "l", col = "red", lwd = 3)
            }
        }
        if(s$go == 1 & !is.null(v$vals[1])){
            plot(x = 0, y = 0, type = "n", xlim = c(0, 100), ylim = c(0, 100));
            xvals <- c(v$vals[1], v$vals[3]);
            yvals <- c(v$vals[2], v$vals[4]);
            points(x = xvals, y = yvals, pch = 20, col = "red", cex = 3)
        }
    })
    
}

shinyApp(ui, server)
