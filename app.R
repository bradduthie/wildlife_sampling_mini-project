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
    h2("http://bradduthie.github.io/wildlife_sampling_mini-project"),
    plotOutput("plot", click = "plot_click", height = "700px"),
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
        s$seed   <- NULL;
    })
    
    observeEvent(input$sample, {
        s$go <- s$go + 1;
    })
    
    output$plot <- renderPlot({
        if(s$go == 0 | is.null(v$vals[1])){
            s$seed   <- sample(x = 1:9999, size = 1);
            sim_seed <- s$seed;
            par(mar = c(0, 0, 0, 0), bg = "#CCCC99");
            plot(x = 0, y = 0, type = "n", xlim = c(0, 100), ylim = c(-10, 100),
                 xaxt = "n", yaxt = "n", asp = 1);
            polygon(x = c(0, 0, 100, 100), y = c(0, 100, 100, 0), 
                    border = 1, lty = "dotted");
            lines(x = c(0, 0), y = c(10, 85), lwd = 5, col = "#CCCC99");
            lines(x = c(100, 100), y = c(10, 85), lwd = 5, col = "#CCCC99");
            m1 <- paste("You are in field location number: ", sim_seed);
            m2 <- "Write this number down, then click anywhere in"
            m3 <- "the field to set where your transect starts."
            m4 <- "Click again to set where your transect ends,"
            m5 <- "and start sampling by clicking 'Sample' once."
            m6 <- "Keep your transect inside the dotted box! Click to start."
            text(x = 50, y = 75, labels = m1, cex = 2);
            text(x = 50, y = 60, labels = m2, cex = 2);
            text(x = 50, y = 50, labels = m3, cex = 2);
            text(x = 50, y = 40, labels = m4, cex = 2);
            text(x = 50, y = 30, labels = m5, cex = 2);
            text(x = 50, y = 15, labels = m6, cex = 2, col = "red");
            if (!is.null(v$click1$x)){
                par(mar = c(0, 0, 0, 0), bg = "#CCCC99");
                plot(x = 0, y = 0, type = "n", xlim = c(0, 100), 
                     ylim = c(-10, 100), xaxt = "n", yaxt = "n", asp = 1);
                polygon(x = c(0, 0, 100, 100), y = c(0, 100, 100, 0), 
                        border = 1, lty = "dotted");
                points(x = v$click1$x, y = v$click1$y, pch = 20, 
                       col = "#D55E00", 
                       cex = 3)
            }
            if (!is.null(v$vals[1])){
                par(mar = c(0, 0, 0, 0), bg = "#CCCC99");
                plot(x = 0, y = 0, type = "n", xlim = c(0, 100), 
                     ylim = c(-10, 100), xaxt = "n", yaxt = "n" , asp = 1);
                polygon(x = c(0, 0, 100, 100), y = c(0, 100, 100, 0), 
                        border = 1, lty = "dotted");
                xvals <- c(v$vals[1], v$vals[3]);
                yvals <- c(v$vals[2], v$vals[4]);
                points(x = xvals, y = yvals, col = "#D55E00", cex = 3,
                       pch = 20);
                points(x = xvals, y = yvals, type = "l", col = "#D55E00", 
                       lwd = 3)
                l$x <- seq(from = xvals[1], to = xvals[2], length = 20);
                l$y <- seq(from = yvals[1], to = yvals[2], length = 20);
            }
        }
        if(s$go == 1 & !is.null(v$vals[1])){
            set.seed(s$seed);
            tlocs_x <- sample(x = 1:100, size = 1000, replace = TRUE);
            tlocs_y <- sample(x = 1:100, size = 1000, replace = TRUE);
            par(mar = c(0, 0, 0, 0), bg = "#CCCC99");
            plot(x = 0, y = 0, type = "n", xlim = c(0, 100), ylim = c(-10, 100),
                 xaxt = "n", yaxt = "n", asp = 1);
            polygon(x = c(0, 0, 100, 100), y = c(0, 100, 100, 0), border = 1,
                    lty = "dotted");
            xvals <- c(v$vals[1], v$vals[3]);
            yvals <- c(v$vals[2], v$vals[4]);
            points(x = xvals, y = yvals, pch = 20, col = "#D55E00", cex = 3)
            points(x = xvals, y = yvals, type = "l", col = "#D55E00", lwd = 3)
            l$x <- seq(from = xvals[1], to = xvals[2], length = 20);
            l$y <- seq(from = yvals[1], to = yvals[2], length = 20);
            points(x = l$x[1:2], y = l$y[1:2], pch = 20, col = "#0072B2", 
                   type = "b", cex = 4);
            points(x = c(v$vals[1], l$x[2]), y = c(v$vals[2], l$y[2]), 
                   col = "#0072B2", type = "l", lwd = 5);
            if(!is.null(tlocs_x)){
                found <- search_tsct(l$x[1], l$x[2], l$y[1], l$y[2], 
                                     tlocs_x, tlocs_y);
                if(!is.null(found)){
                    points(x = found[,1], y = found[,2], col = "green", 
                           pch = 20);
                    text(x = 10, y = -5, labels = "Distances:", cex = 2);
                    found_vec <- sort(found[,3]);
                    for(i in 1:dim(found)[1]){
                        found_val <- round(found_vec[i], digits = 1);
                        found_prt <- paste(found_val, ",", sep = "");
                        text(x = 17 + (8 * i), y = -5.5, labels = found_prt, 
                             cex = 2);
                    }
                }else{
                    text(x = 10, y = -5, labels = "Distances: (none)", cex = 2);
                }
            }
        }
        if(s$go == 2 & !is.null(v$vals[1])){
            set.seed(s$seed);
            tlocs_x <- sample(x = 1:100, size = 1000, replace = TRUE);
            tlocs_y <- sample(x = 1:100, size = 1000, replace = TRUE);
            par(mar = c(0, 0, 0, 0), bg = "#CCCC99");
            plot(x = 0, y = 0, type = "n", xlim = c(0, 100), ylim = c(-10, 100),
                 xaxt = "n", yaxt = "n", asp = 1);
            polygon(x = c(0, 0, 100, 100), y = c(0, 100, 100, 0), border = 1,
                    lty = "dotted");
            xvals <- c(v$vals[1], v$vals[3]);
            yvals <- c(v$vals[2], v$vals[4]);
            points(x = xvals, y = yvals, pch = 20, col = "#D55E00", cex = 3)
            points(x = xvals, y = yvals, type = "l", col = "#D55E00", lwd = 3)
            l$x <- seq(from = xvals[1], to = xvals[2], length = 20);
            l$y <- seq(from = yvals[1], to = yvals[2], length = 20);
            points(x = l$x[1:3], y = l$y[1:3], pch = 20, col = "#0072B2", 
                   type = "b", cex = 4);
            points(x = c(v$vals[1], l$x[3]), y = c(v$vals[2], l$y[3]), 
                   col = "#0072B2", type = "l", lwd = 5);
            if(!is.null(tlocs_x)){
                found <- search_tsct(l$x[2], l$x[3], l$y[2], l$y[3], 
                                     tlocs_x, tlocs_y);
                if(!is.null(found)){
                    points(x = found[,1], y = found[,2], col = "green", 
                           pch = 20);
                    text(x = 10, y = -5, labels = "Distances:", cex = 2);
                    found_vec <- sort(found[,3]);
                    for(i in 1:dim(found)[1]){
                        found_val <- round(found_vec[i], digits = 1);
                        found_prt <- paste(found_val, ",", sep = "");
                        text(x = 17 + (8 * i), y = -5.5, labels = found_prt, 
                             cex = 2);
                    }
                }else{
                    text(x = 10, y = -5, labels = "Distances: (none)", cex = 2);
                }
            }
        }
        if(s$go == 3 & !is.null(v$vals[1])){
            set.seed(s$seed);
            tlocs_x <- sample(x = 1:100, size = 1000, replace = TRUE);
            tlocs_y <- sample(x = 1:100, size = 1000, replace = TRUE);
            par(mar = c(0, 0, 0, 0), bg = "#CCCC99");
            plot(x = 0, y = 0, type = "n", xlim = c(0, 100), ylim = c(-10, 100),
                 xaxt = "n", yaxt = "n", asp = 1);
            polygon(x = c(0, 0, 100, 100), y = c(0, 100, 100, 0), border = 1,
                    lty = "dotted");
            xvals <- c(v$vals[1], v$vals[3]);
            yvals <- c(v$vals[2], v$vals[4]);
            points(x = xvals, y = yvals, pch = 20, col = "#D55E00", cex = 3)
            points(x = xvals, y = yvals, type = "l", col = "#D55E00", lwd = 3)
            l$x <- seq(from = xvals[1], to = xvals[2], length = 20);
            l$y <- seq(from = yvals[1], to = yvals[2], length = 20);
            points(x = l$x[1:4], y = l$y[1:4], pch = 20, col = "#0072B2", 
                   type = "b", cex = 4);
            points(x = c(v$vals[1], l$x[4]), y = c(v$vals[2], l$y[4]), 
                   col = "#0072B2", type = "l", lwd = 5);
            if(!is.null(tlocs_x)){
                found <- search_tsct(l$x[3], l$x[4], l$y[3], l$y[4], 
                                     tlocs_x, tlocs_y);
                if(!is.null(found)){
                    points(x = found[,1], y = found[,2], col = "green", 
                           pch = 20);
                    text(x = 10, y = -5, labels = "Distances:", cex = 2);
                    found_vec <- sort(found[,3]);
                    for(i in 1:dim(found)[1]){
                        found_val <- round(found_vec[i], digits = 1);
                        found_prt <- paste(found_val, ",", sep = "");
                        text(x = 17 + (8 * i), y = -5.5, labels = found_prt, 
                             cex = 2);
                    }
                }else{
                    text(x = 10, y = -5, labels = "Distances: (none)", cex = 2);
                }
            }
        }
        if(s$go == 4 & !is.null(v$vals[1])){
            set.seed(s$seed);
            tlocs_x <- sample(x = 1:100, size = 1000, replace = TRUE);
            tlocs_y <- sample(x = 1:100, size = 1000, replace = TRUE);
            par(mar = c(0, 0, 0, 0), bg = "#CCCC99");
            plot(x = 0, y = 0, type = "n", xlim = c(0, 100), ylim = c(-10, 100),
                 xaxt = "n", yaxt = "n", asp = 1);
            polygon(x = c(0, 0, 100, 100), y = c(0, 100, 100, 0), border = 1,
                    lty = "dotted");
            xvals <- c(v$vals[1], v$vals[3]);
            yvals <- c(v$vals[2], v$vals[4]);
            points(x = xvals, y = yvals, pch = 20, col = "#D55E00", cex = 3)
            points(x = xvals, y = yvals, type = "l", col = "#D55E00", lwd = 3)
            l$x <- seq(from = xvals[1], to = xvals[2], length = 20);
            l$y <- seq(from = yvals[1], to = yvals[2], length = 20);
            points(x = l$x[1:5], y = l$y[1:5], pch = 20, col = "#0072B2", 
                   type = "b", cex = 4);
            points(x = c(v$vals[1], l$x[5]), y = c(v$vals[2], l$y[5]), 
                   col = "#0072B2", type = "l", lwd = 5);
            if(!is.null(tlocs_x)){
                found <- search_tsct(l$x[4], l$x[5], l$y[4], l$y[5], 
                                     tlocs_x, tlocs_y);
                if(!is.null(found)){
                    points(x = found[,1], y = found[,2], col = "green", 
                           pch = 20);
                    text(x = 10, y = -5, labels = "Distances:", cex = 2);
                    found_vec <- sort(found[,3]);
                    for(i in 1:dim(found)[1]){
                        found_val <- round(found_vec[i], digits = 1);
                        found_prt <- paste(found_val, ",", sep = "");
                        text(x = 17 + (8 * i), y = -5.5, labels = found_prt, 
                             cex = 2);
                    }
                }else{
                    text(x = 10, y = -5, labels = "Distances: (none)", cex = 2);
                }
            }
        }
        if(s$go == 5 & !is.null(v$vals[1])){
            set.seed(s$seed);
            tlocs_x <- sample(x = 1:100, size = 1000, replace = TRUE);
            tlocs_y <- sample(x = 1:100, size = 1000, replace = TRUE);
            par(mar = c(0, 0, 0, 0), bg = "#CCCC99");
            plot(x = 0, y = 0, type = "n", xlim = c(0, 100), ylim = c(-10, 100),
                 xaxt = "n", yaxt = "n", asp = 1);
            polygon(x = c(0, 0, 100, 100), y = c(0, 100, 100, 0), border = 1,
                    lty = "dotted");
            xvals <- c(v$vals[1], v$vals[3]);
            yvals <- c(v$vals[2], v$vals[4]);
            points(x = xvals, y = yvals, pch = 20, col = "#D55E00", cex = 3)
            points(x = xvals, y = yvals, type = "l", col = "#D55E00", lwd = 3)
            l$x <- seq(from = xvals[1], to = xvals[2], length = 20);
            l$y <- seq(from = yvals[1], to = yvals[2], length = 20);
            points(x = l$x[1:6], y = l$y[1:6], pch = 20, col = "#0072B2", 
                   type = "b", cex = 4);
            points(x = c(v$vals[1], l$x[6]), y = c(v$vals[2], l$y[6]), 
                   col = "#0072B2", type = "l", lwd = 5);
            if(!is.null(tlocs_x)){
                found <- search_tsct(l$x[5], l$x[6], l$y[5], l$y[6], 
                                     tlocs_x, tlocs_y);
                if(!is.null(found)){
                    points(x = found[,1], y = found[,2], col = "green", 
                           pch = 20);
                    text(x = 10, y = -5, labels = "Distances:", cex = 2);
                    found_vec <- sort(found[,3]);
                    for(i in 1:dim(found)[1]){
                        found_val <- round(found_vec[i], digits = 1);
                        found_prt <- paste(found_val, ",", sep = "");
                        text(x = 17 + (8 * i), y = -5.5, labels = found_prt, 
                             cex = 2);
                    }
                }else{
                    text(x = 10, y = -5, labels = "Distances: (none)", cex = 2);
                }
            }
        }
        if(s$go == 6 & !is.null(v$vals[1])){
            set.seed(s$seed);
            tlocs_x <- sample(x = 1:100, size = 1000, replace = TRUE);
            tlocs_y <- sample(x = 1:100, size = 1000, replace = TRUE);
            par(mar = c(0, 0, 0, 0), bg = "#CCCC99");
            plot(x = 0, y = 0, type = "n", xlim = c(0, 100), ylim = c(-10, 100),
                 xaxt = "n", yaxt = "n", asp = 1);
            polygon(x = c(0, 0, 100, 100), y = c(0, 100, 100, 0), border = 1,
                    lty = "dotted");
            xvals <- c(v$vals[1], v$vals[3]);
            yvals <- c(v$vals[2], v$vals[4]);
            points(x = xvals, y = yvals, pch = 20, col = "#D55E00", cex = 3)
            points(x = xvals, y = yvals, type = "l", col = "#D55E00", lwd = 3)
            l$x <- seq(from = xvals[1], to = xvals[2], length = 20);
            l$y <- seq(from = yvals[1], to = yvals[2], length = 20);
            points(x = l$x[1:7], y = l$y[1:7], pch = 20, col = "#0072B2", 
                   type = "b", cex = 4);
            points(x = c(v$vals[1], l$x[7]), y = c(v$vals[2], l$y[7]), 
                   col = "#0072B2", type = "l", lwd = 5);
            if(!is.null(tlocs_x)){
                found <- search_tsct(l$x[6], l$x[7], l$y[6], l$y[7], 
                                     tlocs_x, tlocs_y);
                if(!is.null(found)){
                    points(x = found[,1], y = found[,2], col = "green", 
                           pch = 20);
                    text(x = 10, y = -5, labels = "Distances:", cex = 2);
                    found_vec <- sort(found[,3]);
                    for(i in 1:dim(found)[1]){
                        found_val <- round(found_vec[i], digits = 1);
                        found_prt <- paste(found_val, ",", sep = "");
                        text(x = 17 + (8 * i), y = -5.5, labels = found_prt, 
                             cex = 2);
                    }
                }else{
                    text(x = 10, y = -5, labels = "Distances: (none)", cex = 2);
                }
            }
        }
        if(s$go == 7 & !is.null(v$vals[1])){
            set.seed(s$seed);
            tlocs_x <- sample(x = 1:100, size = 1000, replace = TRUE);
            tlocs_y <- sample(x = 1:100, size = 1000, replace = TRUE);
            par(mar = c(0, 0, 0, 0), bg = "#CCCC99");
            plot(x = 0, y = 0, type = "n", xlim = c(0, 100), ylim = c(-10, 100),
                 xaxt = "n", yaxt = "n", asp = 1);
            polygon(x = c(0, 0, 100, 100), y = c(0, 100, 100, 0), border = 1,
                    lty = "dotted");
            xvals <- c(v$vals[1], v$vals[3]);
            yvals <- c(v$vals[2], v$vals[4]);
            points(x = xvals, y = yvals, pch = 20, col = "#D55E00", cex = 3)
            points(x = xvals, y = yvals, type = "l", col = "#D55E00", lwd = 3)
            l$x <- seq(from = xvals[1], to = xvals[2], length = 20);
            l$y <- seq(from = yvals[1], to = yvals[2], length = 20);
            points(x = l$x[1:8], y = l$y[1:8], pch = 20, col = "#0072B2", 
                   type = "b", cex = 4);
            points(x = c(v$vals[1], l$x[8]), y = c(v$vals[2], l$y[8]), 
                   col = "#0072B2", type = "l", lwd = 5);
            if(!is.null(tlocs_x)){
                found <- search_tsct(l$x[7], l$x[8], l$y[7], l$y[8], 
                                     tlocs_x, tlocs_y);
                if(!is.null(found)){
                    points(x = found[,1], y = found[,2], col = "green", 
                           pch = 20);
                    text(x = 10, y = -5, labels = "Distances:", cex = 2);
                    found_vec <- sort(found[,3]);
                    for(i in 1:dim(found)[1]){
                        found_val <- round(found_vec[i], digits = 1);
                        found_prt <- paste(found_val, ",", sep = "");
                        text(x = 17 + (8 * i), y = -5.5, labels = found_prt, 
                             cex = 2);
                    }
                }else{
                    text(x = 10, y = -5, labels = "Distances: (none)", cex = 2);
                }
            }
        }
        if(s$go == 8 & !is.null(v$vals[1])){
            set.seed(s$seed);
            tlocs_x <- sample(x = 1:100, size = 1000, replace = TRUE);
            tlocs_y <- sample(x = 1:100, size = 1000, replace = TRUE);
            par(mar = c(0, 0, 0, 0), bg = "#CCCC99");
            plot(x = 0, y = 0, type = "n", xlim = c(0, 100), ylim = c(-10, 100),
                 xaxt = "n", yaxt = "n", asp = 1);
            polygon(x = c(0, 0, 100, 100), y = c(0, 100, 100, 0), border = 1,
                    lty = "dotted");
            xvals <- c(v$vals[1], v$vals[3]);
            yvals <- c(v$vals[2], v$vals[4]);
            points(x = xvals, y = yvals, pch = 20, col = "#D55E00", cex = 3)
            points(x = xvals, y = yvals, type = "l", col = "#D55E00", lwd = 3)
            l$x <- seq(from = xvals[1], to = xvals[2], length = 20);
            l$y <- seq(from = yvals[1], to = yvals[2], length = 20);
            points(x = l$x[1:9], y = l$y[1:9], pch = 20, col = "#0072B2", 
                   type = "b", cex = 4);
            points(x = c(v$vals[1], l$x[9]), y = c(v$vals[2], l$y[9]), 
                   col = "#0072B2", type = "l", lwd = 5);
            if(!is.null(tlocs_x)){
                found <- search_tsct(l$x[8], l$x[9], l$y[8], l$y[9], 
                                     tlocs_x, tlocs_y);
                if(!is.null(found)){
                    points(x = found[,1], y = found[,2], col = "green", 
                           pch = 20);
                    text(x = 10, y = -5, labels = "Distances:", cex = 2);
                    found_vec <- sort(found[,3]);
                    for(i in 1:dim(found)[1]){
                        found_val <- round(found_vec[i], digits = 1);
                        found_prt <- paste(found_val, ",", sep = "");
                        text(x = 17 + (8 * i), y = -5.5, labels = found_prt, 
                             cex = 2);
                    }
                }else{
                    text(x = 10, y = -5, labels = "Distances: (none)", cex = 2);
                }
            }
        }
        if(s$go == 9 & !is.null(v$vals[1])){
            set.seed(s$seed);
            tlocs_x <- sample(x = 1:100, size = 1000, replace = TRUE);
            tlocs_y <- sample(x = 1:100, size = 1000, replace = TRUE);
            par(mar = c(0, 0, 0, 0), bg = "#CCCC99");
            plot(x = 0, y = 0, type = "n", xlim = c(0, 100), ylim = c(-10, 100),
                 xaxt = "n", yaxt = "n", asp = 1);
            polygon(x = c(0, 0, 100, 100), y = c(0, 100, 100, 0), border = 1,
                    lty = "dotted");
            xvals <- c(v$vals[1], v$vals[3]);
            yvals <- c(v$vals[2], v$vals[4]);
            points(x = xvals, y = yvals, pch = 20, col = "#D55E00", cex = 3)
            points(x = xvals, y = yvals, type = "l", col = "#D55E00", lwd = 3)
            l$x <- seq(from = xvals[1], to = xvals[2], length = 20);
            l$y <- seq(from = yvals[1], to = yvals[2], length = 20);
            points(x = l$x[1:10], y = l$y[1:10], pch = 20, col = "#0072B2", 
                   type = "b", cex = 4);
            points(x = c(v$vals[1], l$x[10]), y = c(v$vals[2], l$y[10]), 
                   col = "#0072B2", type = "l", lwd = 5);
            if(!is.null(tlocs_x)){
                found <- search_tsct(l$x[9], l$x[10], l$y[9], l$y[10], 
                                     tlocs_x, tlocs_y);
                if(!is.null(found)){
                    points(x = found[,1], y = found[,2], col = "green", 
                           pch = 20);
                    text(x = 10, y = -5, labels = "Distances:", cex = 2);
                    found_vec <- sort(found[,3]);
                    for(i in 1:dim(found)[1]){
                        found_val <- round(found_vec[i], digits = 1);
                        found_prt <- paste(found_val, ",", sep = "");
                        text(x = 17 + (8 * i), y = -5.5, labels = found_prt, 
                             cex = 2);
                    }
                }else{
                    text(x = 10, y = -5, labels = "Distances: (none)", cex = 2);
                }
            }
        }
        if(s$go == 10 & !is.null(v$vals[1])){
            set.seed(s$seed);
            tlocs_x <- sample(x = 1:100, size = 1000, replace = TRUE);
            tlocs_y <- sample(x = 1:100, size = 1000, replace = TRUE);
            par(mar = c(0, 0, 0, 0), bg = "#CCCC99");
            plot(x = 0, y = 0, type = "n", xlim = c(0, 100), ylim = c(-10, 100),
                 xaxt = "n", yaxt = "n", asp = 1);
            polygon(x = c(0, 0, 100, 100), y = c(0, 100, 100, 0), border = 1,
                    lty = "dotted");
            xvals <- c(v$vals[1], v$vals[3]);
            yvals <- c(v$vals[2], v$vals[4]);
            points(x = xvals, y = yvals, pch = 20, col = "#D55E00", cex = 3)
            points(x = xvals, y = yvals, type = "l", col = "#D55E00", lwd = 3)
            l$x <- seq(from = xvals[1], to = xvals[2], length = 20);
            l$y <- seq(from = yvals[1], to = yvals[2], length = 20);
            points(x = l$x[1:11], y = l$y[1:11], pch = 20, col = "#0072B2", 
                   type = "b", cex = 4);
            points(x = c(v$vals[1], l$x[11]), y = c(v$vals[2], l$y[11]), 
                   col = "#0072B2", type = "l", lwd = 5);
            if(!is.null(tlocs_x)){
                found <- search_tsct(l$x[10], l$x[11], l$y[10], l$y[11], 
                                     tlocs_x, tlocs_y);
                if(!is.null(found)){
                    points(x = found[,1], y = found[,2], col = "green", 
                           pch = 20);
                    text(x = 10, y = -5, labels = "Distances:", cex = 2);
                    found_vec <- sort(found[,3]);
                    for(i in 1:dim(found)[1]){
                        found_val <- round(found_vec[i], digits = 1);
                        found_prt <- paste(found_val, ",", sep = "");
                        text(x = 17 + (8 * i), y = -5.5, labels = found_prt, 
                             cex = 2);
                    }
                }else{
                    text(x = 10, y = -5, labels = "Distances: (none)", cex = 2);
                }
            }
        }
        if(s$go == 11 & !is.null(v$vals[1])){
            set.seed(s$seed);
            tlocs_x <- sample(x = 1:100, size = 1000, replace = TRUE);
            tlocs_y <- sample(x = 1:100, size = 1000, replace = TRUE);
            par(mar = c(0, 0, 0, 0), bg = "#CCCC99");
            plot(x = 0, y = 0, type = "n", xlim = c(0, 100), ylim = c(-10, 100),
                 xaxt = "n", yaxt = "n", asp = 1);
            polygon(x = c(0, 0, 100, 100), y = c(0, 100, 100, 0), border = 1,
                    lty = "dotted");
            xvals <- c(v$vals[1], v$vals[3]);
            yvals <- c(v$vals[2], v$vals[4]);
            points(x = xvals, y = yvals, pch = 20, col = "#D55E00", cex = 3)
            points(x = xvals, y = yvals, type = "l", col = "#D55E00", lwd = 3)
            l$x <- seq(from = xvals[1], to = xvals[2], length = 20);
            l$y <- seq(from = yvals[1], to = yvals[2], length = 20);
            points(x = l$x[1:12], y = l$y[1:12], pch = 20, col = "#0072B2", 
                   type = "b", cex = 4);
            points(x = c(v$vals[1], l$x[12]), y = c(v$vals[2], l$y[12]), 
                   col = "#0072B2", type = "l", lwd = 5);
            if(!is.null(tlocs_x)){
                found <- search_tsct(l$x[11], l$x[12], l$y[11], l$y[12], 
                                     tlocs_x, tlocs_y);
                if(!is.null(found)){
                    points(x = found[,1], y = found[,2], col = "green", 
                           pch = 20);
                    text(x = 10, y = -5, labels = "Distances:", cex = 2);
                    found_vec <- sort(found[,3]);
                    for(i in 1:dim(found)[1]){
                        found_val <- round(found_vec[i], digits = 1);
                        found_prt <- paste(found_val, ",", sep = "");
                        text(x = 17 + (8 * i), y = -5.5, labels = found_prt, 
                             cex = 2);
                    }
                }else{
                    text(x = 10, y = -5, labels = "Distances: (none)", cex = 2);
                }
            }
        }
        if(s$go == 12 & !is.null(v$vals[1])){
            set.seed(s$seed);
            tlocs_x <- sample(x = 1:100, size = 1000, replace = TRUE);
            tlocs_y <- sample(x = 1:100, size = 1000, replace = TRUE);
            par(mar = c(0, 0, 0, 0), bg = "#CCCC99");
            plot(x = 0, y = 0, type = "n", xlim = c(0, 100), ylim = c(-10, 100),
                 xaxt = "n", yaxt = "n", asp = 1);
            polygon(x = c(0, 0, 100, 100), y = c(0, 100, 100, 0), border = 1,
                    lty = "dotted");
            xvals <- c(v$vals[1], v$vals[3]);
            yvals <- c(v$vals[2], v$vals[4]);
            points(x = xvals, y = yvals, pch = 20, col = "#D55E00", cex = 3)
            points(x = xvals, y = yvals, type = "l", col = "#D55E00", lwd = 3)
            l$x <- seq(from = xvals[1], to = xvals[2], length = 20);
            l$y <- seq(from = yvals[1], to = yvals[2], length = 20);
            points(x = l$x[1:13], y = l$y[1:13], pch = 20, col = "#0072B2", 
                   type = "b", cex = 4);
            points(x = c(v$vals[1], l$x[13]), y = c(v$vals[2], l$y[13]), 
                   col = "#0072B2", type = "l", lwd = 5);
            if(!is.null(tlocs_x)){
                found <- search_tsct(l$x[12], l$x[13], l$y[12], l$y[13], 
                                     tlocs_x, tlocs_y);
                if(!is.null(found)){
                    points(x = found[,1], y = found[,2], col = "green", 
                           pch = 20);
                    text(x = 10, y = -5, labels = "Distances:", cex = 2);
                    found_vec <- sort(found[,3]);
                    for(i in 1:dim(found)[1]){
                        found_val <- round(found_vec[i], digits = 1);
                        found_prt <- paste(found_val, ",", sep = "");
                        text(x = 17 + (8 * i), y = -5.5, labels = found_prt, 
                             cex = 2);
                    }
                }else{
                    text(x = 10, y = -5, labels = "Distances: (none)", cex = 2);
                }
            }
        }
        if(s$go == 13 & !is.null(v$vals[1])){
            set.seed(s$seed);
            tlocs_x <- sample(x = 1:100, size = 1000, replace = TRUE);
            tlocs_y <- sample(x = 1:100, size = 1000, replace = TRUE);
            par(mar = c(0, 0, 0, 0), bg = "#CCCC99");
            plot(x = 0, y = 0, type = "n", xlim = c(0, 100), ylim = c(-10, 100),
                 xaxt = "n", yaxt = "n", asp = 1);
            polygon(x = c(0, 0, 100, 100), y = c(0, 100, 100, 0), border = 1,
                    lty = "dotted");
            xvals <- c(v$vals[1], v$vals[3]);
            yvals <- c(v$vals[2], v$vals[4]);
            points(x = xvals, y = yvals, pch = 20, col = "#D55E00", cex = 3)
            points(x = xvals, y = yvals, type = "l", col = "#D55E00", lwd = 3)
            l$x <- seq(from = xvals[1], to = xvals[2], length = 20);
            l$y <- seq(from = yvals[1], to = yvals[2], length = 20);
            points(x = l$x[1:14], y = l$y[1:14], pch = 20, col = "#0072B2", 
                   type = "b", cex = 4);
            points(x = c(v$vals[1], l$x[14]), y = c(v$vals[2], l$y[14]), 
                   col = "#0072B2", type = "l", lwd = 5);
            if(!is.null(tlocs_x)){
                found <- search_tsct(l$x[13], l$x[14], l$y[13], l$y[14], 
                                     tlocs_x, tlocs_y);
                if(!is.null(found)){
                    points(x = found[,1], y = found[,2], col = "green", 
                           pch = 20);
                    text(x = 10, y = -5, labels = "Distances:", cex = 2);
                    found_vec <- sort(found[,3]);
                    for(i in 1:dim(found)[1]){
                        found_val <- round(found_vec[i], digits = 1);
                        found_prt <- paste(found_val, ",", sep = "");
                        text(x = 17 + (8 * i), y = -5.5, labels = found_prt, 
                             cex = 2);
                    }
                }else{
                    text(x = 10, y = -5, labels = "Distances: (none)", cex = 2);
                }
            }
        }
        if(s$go == 14 & !is.null(v$vals[1])){
            set.seed(s$seed);
            tlocs_x <- sample(x = 1:100, size = 1000, replace = TRUE);
            tlocs_y <- sample(x = 1:100, size = 1000, replace = TRUE);
            par(mar = c(0, 0, 0, 0), bg = "#CCCC99");
            plot(x = 0, y = 0, type = "n", xlim = c(0, 100), ylim = c(-10, 100),
                 xaxt = "n", yaxt = "n", asp = 1);
            polygon(x = c(0, 0, 100, 100), y = c(0, 100, 100, 0), border = 1,
                    lty = "dotted");
            xvals <- c(v$vals[1], v$vals[3]);
            yvals <- c(v$vals[2], v$vals[4]);
            points(x = xvals, y = yvals, pch = 20, col = "#D55E00", cex = 3)
            points(x = xvals, y = yvals, type = "l", col = "#D55E00", lwd = 3)
            l$x <- seq(from = xvals[1], to = xvals[2], length = 20);
            l$y <- seq(from = yvals[1], to = yvals[2], length = 20);
            points(x = l$x[1:15], y = l$y[1:15], pch = 20, col = "#0072B2", 
                   type = "b", cex = 4);
            points(x = c(v$vals[1], l$x[15]), y = c(v$vals[2], l$y[15]), 
                   col = "#0072B2", type = "l", lwd = 5);
            if(!is.null(tlocs_x)){
                found <- search_tsct(l$x[14], l$x[15], l$y[14], l$y[15], 
                                     tlocs_x, tlocs_y);
                if(!is.null(found)){
                    points(x = found[,1], y = found[,2], col = "green", 
                           pch = 20);
                    text(x = 10, y = -5, labels = "Distances:", cex = 2);
                    found_vec <- sort(found[,3]);
                    for(i in 1:dim(found)[1]){
                        found_val <- round(found_vec[i], digits = 1);
                        found_prt <- paste(found_val, ",", sep = "");
                        text(x = 17 + (8 * i), y = -5.5, labels = found_prt, 
                             cex = 2);
                    }
                }else{
                    text(x = 10, y = -5, labels = "Distances: (none)", cex = 2);
                }
            }
        }
        if(s$go == 15 & !is.null(v$vals[1])){
            set.seed(s$seed);
            tlocs_x <- sample(x = 1:100, size = 1000, replace = TRUE);
            tlocs_y <- sample(x = 1:100, size = 1000, replace = TRUE);
            par(mar = c(0, 0, 0, 0), bg = "#CCCC99");
            plot(x = 0, y = 0, type = "n", xlim = c(0, 100), ylim = c(-10, 100),
                 xaxt = "n", yaxt = "n", asp = 1);
            polygon(x = c(0, 0, 100, 100), y = c(0, 100, 100, 0), border = 1,
                    lty = "dotted");
            xvals <- c(v$vals[1], v$vals[3]);
            yvals <- c(v$vals[2], v$vals[4]);
            points(x = xvals, y = yvals, pch = 20, col = "#D55E00", cex = 3)
            points(x = xvals, y = yvals, type = "l", col = "#D55E00", lwd = 3)
            l$x <- seq(from = xvals[1], to = xvals[2], length = 20);
            l$y <- seq(from = yvals[1], to = yvals[2], length = 20);
            points(x = l$x[1:16], y = l$y[1:16], pch = 20, col = "#0072B2", 
                   type = "b", cex = 4);
            points(x = c(v$vals[1], l$x[16]), y = c(v$vals[2], l$y[16]), 
                   col = "#0072B2", type = "l", lwd = 5);
            if(!is.null(tlocs_x)){
                found <- search_tsct(l$x[15], l$x[16], l$y[15], l$y[16], 
                                     tlocs_x, tlocs_y);
                if(!is.null(found)){
                    points(x = found[,1], y = found[,2], col = "green", 
                           pch = 20);
                    text(x = 10, y = -5, labels = "Distances:", cex = 2);
                    found_vec <- sort(found[,3]);
                    for(i in 1:dim(found)[1]){
                        found_val <- round(found_vec[i], digits = 1);
                        found_prt <- paste(found_val, ",", sep = "");
                        text(x = 17 + (8 * i), y = -5.5, labels = found_prt, 
                             cex = 2);
                    }
                }else{
                    text(x = 10, y = -5, labels = "Distances: (none)", cex = 2);
                }
            }
        }
        if(s$go == 16 & !is.null(v$vals[1])){
            set.seed(s$seed);
            tlocs_x <- sample(x = 1:100, size = 1000, replace = TRUE);
            tlocs_y <- sample(x = 1:100, size = 1000, replace = TRUE);
            par(mar = c(0, 0, 0, 0), bg = "#CCCC99");
            plot(x = 0, y = 0, type = "n", xlim = c(0, 100), ylim = c(-10, 100),
                 xaxt = "n", yaxt = "n", asp = 1);
            polygon(x = c(0, 0, 100, 100), y = c(0, 100, 100, 0), border = 1,
                    lty = "dotted");
            xvals <- c(v$vals[1], v$vals[3]);
            yvals <- c(v$vals[2], v$vals[4]);
            points(x = xvals, y = yvals, pch = 20, col = "#D55E00", cex = 3)
            points(x = xvals, y = yvals, type = "l", col = "#D55E00", lwd = 3)
            l$x <- seq(from = xvals[1], to = xvals[2], length = 20);
            l$y <- seq(from = yvals[1], to = yvals[2], length = 20);
            points(x = l$x[1:17], y = l$y[1:17], pch = 20, col = "#0072B2", 
                   type = "b", cex = 4);
            points(x = c(v$vals[1], l$x[17]), y = c(v$vals[2], l$y[17]), 
                   col = "#0072B2", type = "l", lwd = 5);
            if(!is.null(tlocs_x)){
                found <- search_tsct(l$x[16], l$x[17], l$y[16], l$y[17], 
                                     tlocs_x, tlocs_y);
                if(!is.null(found)){
                    points(x = found[,1], y = found[,2], col = "green", 
                           pch = 20);
                    text(x = 10, y = -5, labels = "Distances:", cex = 2);
                    found_vec <- sort(found[,3]);
                    for(i in 1:dim(found)[1]){
                        found_val <- round(found_vec[i], digits = 1);
                        found_prt <- paste(found_val, ",", sep = "");
                        text(x = 17 + (8 * i), y = -5.5, labels = found_prt, 
                             cex = 2);
                    }
                }else{
                    text(x = 10, y = -5, labels = "Distances: (none)", cex = 2);
                }
            }
        }
        if(s$go == 17 & !is.null(v$vals[1])){
            set.seed(s$seed);
            tlocs_x <- sample(x = 1:100, size = 1000, replace = TRUE);
            tlocs_y <- sample(x = 1:100, size = 1000, replace = TRUE);
            par(mar = c(0, 0, 0, 0), bg = "#CCCC99");
            plot(x = 0, y = 0, type = "n", xlim = c(0, 100), ylim = c(-10, 100),
                 xaxt = "n", yaxt = "n", asp = 1);
            polygon(x = c(0, 0, 100, 100), y = c(0, 100, 100, 0), border = 1,
                    lty = "dotted");
            xvals <- c(v$vals[1], v$vals[3]);
            yvals <- c(v$vals[2], v$vals[4]);
            points(x = xvals, y = yvals, pch = 20, col = "#D55E00", cex = 3)
            points(x = xvals, y = yvals, type = "l", col = "#D55E00", lwd = 3)
            l$x <- seq(from = xvals[1], to = xvals[2], length = 20);
            l$y <- seq(from = yvals[1], to = yvals[2], length = 20);
            points(x = l$x[1:18], y = l$y[1:18], pch = 20, col = "#0072B2", 
                   type = "b", cex = 4);
            points(x = c(v$vals[1], l$x[18]), y = c(v$vals[2], l$y[18]), 
                   col = "#0072B2", type = "l", lwd = 5);
            if(!is.null(tlocs_x)){
                found <- search_tsct(l$x[17], l$x[18], l$y[17], l$y[18], 
                                     tlocs_x, tlocs_y);
                if(!is.null(found)){
                    points(x = found[,1], y = found[,2], col = "green", 
                           pch = 20);
                    text(x = 10, y = -5, labels = "Distances:", cex = 2);
                    found_vec <- sort(found[,3]);
                    for(i in 1:dim(found)[1]){
                        found_val <- round(found_vec[i], digits = 1);
                        found_prt <- paste(found_val, ",", sep = "");
                        text(x = 17 + (8 * i), y = -5.5, labels = found_prt, 
                             cex = 2);
                    }
                }else{
                    text(x = 10, y = -5, labels = "Distances: (none)", cex = 2);
                }
            }
        }
        if(s$go == 18 & !is.null(v$vals[1])){
            set.seed(s$seed);
            tlocs_x <- sample(x = 1:100, size = 1000, replace = TRUE);
            tlocs_y <- sample(x = 1:100, size = 1000, replace = TRUE);
            par(mar = c(0, 0, 0, 0), bg = "#CCCC99");
            plot(x = 0, y = 0, type = "n", xlim = c(0, 100), ylim = c(-10, 100),
                 xaxt = "n", yaxt = "n", asp = 1);
            polygon(x = c(0, 0, 100, 100), y = c(0, 100, 100, 0), border = 1,
                    lty = "dotted");
            xvals <- c(v$vals[1], v$vals[3]);
            yvals <- c(v$vals[2], v$vals[4]);
            points(x = xvals, y = yvals, pch = 20, col = "#D55E00", cex = 3)
            points(x = xvals, y = yvals, type = "l", col = "#D55E00", lwd = 3)
            l$x <- seq(from = xvals[1], to = xvals[2], length = 20);
            l$y <- seq(from = yvals[1], to = yvals[2], length = 20);
            points(x = l$x[1:19], y = l$y[1:19], pch = 20, col = "#0072B2", 
                   type = "b", cex = 4);
            points(x = c(v$vals[1], l$x[19]), y = c(v$vals[2], l$y[19]), 
                   col = "#0072B2", type = "l", lwd = 5);
            if(!is.null(tlocs_x)){
                found <- search_tsct(l$x[18], l$x[19], l$y[18], l$y[19], 
                                     tlocs_x, tlocs_y);
                if(!is.null(found)){
                    points(x = found[,1], y = found[,2], col = "green", 
                           pch = 20);
                    text(x = 10, y = -5, labels = "Distances:", cex = 2);
                    found_vec <- sort(found[,3]);
                    for(i in 1:dim(found)[1]){
                        found_val <- round(found_vec[i], digits = 1);
                        found_prt <- paste(found_val, ",", sep = "");
                        text(x = 17 + (8 * i), y = -5.5, labels = found_prt, 
                             cex = 2);
                    }
                }else{
                    text(x = 10, y = -5, labels = "Distances: (none)", cex = 2);
                }
            }
        }
        if(s$go == 19 & !is.null(v$vals[1])){
            set.seed(s$seed);
            tlocs_x <- sample(x = 1:100, size = 1000, replace = TRUE);
            tlocs_y <- sample(x = 1:100, size = 1000, replace = TRUE);
            par(mar = c(0, 0, 0, 0), bg = "#CCCC99");
            plot(x = 0, y = 0, type = "n", xlim = c(0, 100), ylim = c(-10, 100),
                 xaxt = "n", yaxt = "n", asp = 1);
            polygon(x = c(0, 0, 100, 100), y = c(0, 100, 100, 0), border = 1,
                    lty = "dotted");
            xvals <- c(v$vals[1], v$vals[3]);
            yvals <- c(v$vals[2], v$vals[4]);
            points(x = xvals, y = yvals, pch = 20, col = "#D55E00", cex = 3)
            points(x = xvals, y = yvals, type = "l", col = "#D55E00", lwd = 3)
            l$x <- seq(from = xvals[1], to = xvals[2], length = 20);
            l$y <- seq(from = yvals[1], to = yvals[2], length = 20);
            points(x = l$x[1:19], y = l$y[1:19], pch = 20, col = "#0072B2", 
                   type = "b", cex = 4);
            points(x = c(v$vals[1], l$x[20]), y = c(v$vals[2], l$y[20]), 
                   col = "#0072B2", type = "l", lwd = 5);
            if(!is.null(tlocs_x)){
                found <- search_tsct(l$x[19], l$x[20], l$y[19], l$y[20], 
                                     tlocs_x, tlocs_y);
                if(!is.null(found)){
                    points(x = found[,1], y = found[,2], col = "green", 
                           pch = 20);
                    text(x = 10, y = -5, labels = "Distances:", cex = 2);
                    found_vec <- sort(found[,3]);
                    for(i in 1:dim(found)[1]){
                        found_val <- round(found_vec[i], digits = 1);
                        found_prt <- paste(found_val, ",", sep = "");
                        text(x = 17 + (8 * i), y = -5.5, labels = found_prt, 
                             cex = 2);
                    }
                }else{
                    text(x = 10, y = -5, labels = "Distances: (none)", cex = 2);
                }
            }
        }
        if(s$go == 20 & !is.null(v$vals[1])){
            par(mar = c(0, 0, 0, 0), bg = "#CCCC99");
            plot(x = 0, y = 0, type = "n", xlim = c(0, 100), ylim = c(-10, 100),
                 xaxt = "n", yaxt = "n", asp = 1);
            lines(x = c(0, 0), y = c(10, 85), lwd = 5, col = "#CCCC99");
            lines(x = c(100, 100), y = c(10, 85), lwd = 5, col = "#CCCC99");
            m1 <- paste("You have now finished sampling distances");
            m2 <- paste("The start of your transect was: X = ", 
                        round(v$vals[1], digits = 1), ", Y = ", 
                        round(v$vals[2], digits = 1));
            m3 <- paste("The end of your transect was: X = ", 
                        round(v$vals[3], digits = 1), ", Y = ", 
                        round(v$vals[4], digits = 1));
            m4 <- "Record these numbers for later use."
            m5 <- "Repeat this exercise until you have completed 3 transects,"
            m6 <- "then continue to the data analsis portion of the project."
            m7 <- "http://bradduthie.github.io/wildlife_sampling_mini-project"
            text(x = 50, y = 75, labels = m1, cex = 2);
            text(x = 50, y = 60, labels = m2, cex = 2);
            text(x = 50, y = 50, labels = m3, cex = 2);
            text(x = 50, y = 40, labels = m4, cex = 2);
            text(x = 50, y = 30, labels = m5, cex = 2);
            text(x = 50, y = 20, labels = m6, cex = 2);
            text(x = 50, y = 10, labels = m7, cex = 2);
        }
    })
    
}

point_inter_dist <- function(x1, x2, y1, y2, xval, yval){
    slp <- (y1 - y2) / (x1 - x2); # slope
    icp <- y1 - (slp * x1);       # intercept
    if(slp == Inf | slp == -Inf){
        p_dist  <- abs(xval - x1);
        x_inter <- x1;
        y_inter <- yval;
        return(list(p_dist = p_dist, x_inter = x_inter, y_inter = y_inter));
    }
    if(slp == 0){
        p_dist  <- abs(yval - y1);
        x_inter <- xval;
        y_inter <- y1;
        return(list(p_dist = p_dist, x_inter = x_inter, y_inter = y_inter));
    }
    isl     <- (-1 * slp^-1);
    p_int   <- yval - (isl * xval);
    x_inter <- (p_int - icp) / (slp - isl);
    y_inter <- icp + x_inter * slp;
    p_dist  <- sqrt((xval - x_inter)^2 + (yval - y_inter)^2);
    return(list(p_dist = p_dist, x_inter = x_inter, y_inter = y_inter));
}

search_tsct <- function(x1, x2, y1, y2, tx, ty){
    found <- NULL;
    for(i in 1:length(tx)){
        pts  <- point_inter_dist(x1, x2, y1, y2, 
                                 tx[i], ty[i]);
        if(pts$p_dist <= 10){
            eyes <- rbinom(n = 1, size = 1, prob = exp(-0.5 * pts$p_dist));
        }else{
            eyes <- 0;
        }
        rdix <- sqrt((x1 - x2)*(x1 - x2));
        rd1x <- sqrt((pts$x_inter - x1)*(pts$x_inter - x1));
        rd2x <- sqrt((pts$x_inter - x2)*(pts$x_inter - x2));
        rng1 <- (rdix > rd1x & rdix > rd2x);
        rdiy <- sqrt((y1 - y2)*(y1 - y2));
        rd1y <- sqrt((pts$y_inter - y1)*(pts$y_inter - y1));
        rd2y <- sqrt((pts$y_inter - y2)*(pts$y_inter - y2));
        rng2 <- (rdiy > rd1y & rdiy > rd2y);
        if(eyes == TRUE & rng1 == TRUE & rng2 == TRUE){
            found <- rbind(found, c(tx[i], ty[i], pts$p_dist));
        }
    }
    return(found);
}

shinyApp(ui, server)





