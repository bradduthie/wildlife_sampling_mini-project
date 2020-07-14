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
    
    l <- reactiveValues(
        x = 0,
        y = 0
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
        l$x      <- 0
        l$y      <- 0
    })
    
    observeEvent(input$sample, {
        s$go <- s$go + 1;
    })
    
    output$plot <- renderPlot({
        if(s$go == 0 | is.null(v$vals[1])){
            par(mar = c(0, 0, 0, 0), bg = "#E69F00");
            plot(x = 0, y = 0, type = "n", xlim = c(0, 100), ylim = c(0, 100),
                 xaxt = "n", yaxt = "n");
            if (!is.null(v$click1$x)){
                points(x = v$click1$x, y = v$click1$y, pch = 20, 
                       col = "#56B4E9", 
                       cex = 3)
            }
            if (!is.null(v$vals[1])){
                xvals <- c(v$vals[1], v$vals[3]);
                yvals <- c(v$vals[2], v$vals[4]);
                points(x = xvals, y = yvals, pch = 20, col = "#56B4E9", 
                       cex = 3)
                points(x = xvals, y = yvals, type = "l", col = "#56B4E9", 
                       lwd = 3)
                l$x <- seq(from = xvals[1], to = xvals[2], length = 20);
                l$y <- seq(from = yvals[1], to = yvals[2], length = 20);
            }
        }
        if(s$go == 1 & !is.null(v$vals[1])){
            par(mar = c(0, 0, 0, 0), bg = "#E69F00");
            plot(x = 0, y = 0, type = "n", xlim = c(0, 100), ylim = c(0, 100),
                 xaxt = "n", yaxt = "n");
            xvals <- c(v$vals[1], v$vals[3]);
            yvals <- c(v$vals[2], v$vals[4]);
            points(x = xvals, y = yvals, pch = 20, col = "#56B4E9", cex = 3)
            points(x = xvals, y = yvals, type = "l", col = "#56B4E9", lwd = 3)
            l$x <- seq(from = xvals[1], to = xvals[2], length = 20);
            l$y <- seq(from = yvals[1], to = yvals[2], length = 20);
            
            
        

            points(x = l$x[1], y = l$y[1], pch = 20, col = "#009E73", cex = 4);
        }
        if(s$go == 2 & !is.null(v$vals[1])){
            par(mar = c(0, 0, 0, 0), bg = "#E69F00");
            plot(x = 0, y = 0, type = "n", xlim = c(0, 100), ylim = c(0, 100),
                 xaxt = "n", yaxt = "n");
            xvals <- c(v$vals[1], v$vals[3]);
            yvals <- c(v$vals[2], v$vals[4]);
            points(x = xvals, y = yvals, pch = 20, col = "#56B4E9", cex = 3)
            points(x = xvals, y = yvals, type = "l", col = "#56B4E9", lwd = 3)
            l$x <- seq(from = xvals[1], to = xvals[2], length = 20);
            l$y <- seq(from = yvals[1], to = yvals[2], length = 20);
            
            
            
        
            points(x = l$x[1:2], y = l$y[1:2], pch = 20, col = "#009E73", 
                   type = "b", cex = 4);
            points(x = c(v$vals[1], l$x[2]), y = c(v$vals[2], l$y[2]), 
                   col = "#009E73", type = "l", lwd = 5);
        }
        if(s$go == 3 & !is.null(v$vals[1])){
            par(mar = c(0, 0, 0, 0), bg = "#E69F00");
            plot(x = 0, y = 0, type = "n", xlim = c(0, 100), ylim = c(0, 100),
                 xaxt = "n", yaxt = "n");
            xvals <- c(v$vals[1], v$vals[3]);
            yvals <- c(v$vals[2], v$vals[4]);
            points(x = xvals, y = yvals, pch = 20, col = "#56B4E9", cex = 3)
            points(x = xvals, y = yvals, type = "l", col = "#56B4E9", lwd = 3)
            l$x <- seq(from = xvals[1], to = xvals[2], length = 20);
            l$y <- seq(from = yvals[1], to = yvals[2], length = 20);
            
            
            
    
            points(x = l$x[1:3], y = l$y[1:3], pch = 20, col = "#009E73", 
                   type = "b", cex = 4);
            points(x = c(v$vals[1], l$x[3]), y = c(v$vals[2], l$y[3]), 
                   col = "#009E73", type = "l", lwd = 5);
        }
        if(s$go == 4 & !is.null(v$vals[1])){
            par(mar = c(0, 0, 0, 0), bg = "#E69F00");
            plot(x = 0, y = 0, type = "n", xlim = c(0, 100), ylim = c(0, 100),
                 xaxt = "n", yaxt = "n");
            xvals <- c(v$vals[1], v$vals[3]);
            yvals <- c(v$vals[2], v$vals[4]);
            points(x = xvals, y = yvals, pch = 20, col = "#56B4E9", cex = 3)
            points(x = xvals, y = yvals, type = "l", col = "#56B4E9", lwd = 3)
            l$x <- seq(from = xvals[1], to = xvals[2], length = 20);
            l$y <- seq(from = yvals[1], to = yvals[2], length = 20);
            
            
   
            points(x = l$x[1:4], y = l$y[1:4], pch = 20, col = "#009E73", 
                   type = "b", cex = 4);
            points(x = c(v$vals[1], l$x[4]), y = c(v$vals[2], l$y[4]), 
                   col = "#009E73", type = "l", lwd = 5);
        }
        if(s$go == 5 & !is.null(v$vals[1])){
            par(mar = c(0, 0, 0, 0), bg = "#E69F00");
            plot(x = 0, y = 0, type = "n", xlim = c(0, 100), ylim = c(0, 100),
                 xaxt = "n", yaxt = "n");
            xvals <- c(v$vals[1], v$vals[3]);
            yvals <- c(v$vals[2], v$vals[4]);
            points(x = xvals, y = yvals, pch = 20, col = "#56B4E9", cex = 3)
            points(x = xvals, y = yvals, type = "l", col = "#56B4E9", lwd = 3)
            l$x <- seq(from = xvals[1], to = xvals[2], length = 20);
            l$y <- seq(from = yvals[1], to = yvals[2], length = 20);
            
            
            

            points(x = l$x[1:5], y = l$y[1:5], pch = 20, col = "#009E73", 
                   type = "b", cex = 4);
            points(x = c(v$vals[1], l$x[5]), y = c(v$vals[2], l$y[5]), 
                   col = "#009E73", type = "l", lwd = 5);
        }
    })
    
}

collect_data <- function(v){
    steps <- 20;
    plot(x = 0, y = 0, type = "n", xlim = c(0, 100), ylim = c(0, 100));
    xvals <- c(v$vals[1], v$vals[3]);
    yvals <- c(v$vals[2], v$vals[4]);
    points(x = xvals, y = yvals, pch = 20, col = "#56B4E9", cex = 3)
    xsteps <- seq(from = xvals[1], to = xvals[2], length = steps);
    ysteps <- seq(from = yvals[1], to = yvals[2], length = steps);
    
    step <- 1;
    while(step <= steps){
        points(x = c(xvals[1], xsteps[step]), y = c(yvals[1], ysteps[step]),
               type = "l", col = "#56B4E9", lwd = 3);
        step <- step + 1;
        
        date_time <- Sys.time()
        while( (as.numeric(Sys.time()) - as.numeric(date_time)) < 0.05){
            
        }
        
    }
}

shinyApp(ui, server)





