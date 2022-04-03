## Author: Taylor Falk
## tfalk@bu.edu
## BU BF591
## Assignment 7

# Welcome to R Shiny. All that glitters is not gold.
library(shiny)
library(ggplot2)
library(colourpicker) # you might need to install this.


# Define UI for application that draws a histogram
ui <- fluidPage(
    h1("BF591 Assignment 7: Working with RShiny"),
    h6("To use this application, download the CSV deseq_res.csv from the data directory of this app's repository."),
    
    sidebarPanel(
        fileInput("input_data", "Load differential expression results", accept = ".csv"), 

        p("A volcano plot can be generated with log_2 fold-change on the x-axis and p-adjusted on the y-axis."),
        br(),
        
        radioButtons("x_name", "Choose the column for the x-axis", 
                     c("baseMean", "log2FoldChange", "lfcSE", "stat", "pvalue", "padj")),
        
        radioButtons("y_name", "Choose the column for the y-axis", 
                     c("baseMean", "log2FoldChange", "lfcSE", "stat", "pvalue", "padj")),
        
        colourInput("color_1", "Base Point Color", "C47DF0"), 
        colourInput("color_2", "Highlight Point Color", "#57E657"),
        
        sliderInput("slider", "Select the magnitude of the p adjusted coloring:", min = -300, max = 0, value = 30), 
        
        submitButton("Plot", icon=NULL, width = 300),
        
        plotOutput(outputId = "volcano"),
        tableOutput(outputId = "table")
        
    )

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    #' load_Data
    #'
    #' @details Okay this one is a little weird but bear with me here. This is 
    #' still a "function", but it will take no arguments. The `reactive({})` bit 
    #' says "if any of my inputs (as in, input$...) are changed, run me again". 
    #' This is useful when a user clicks a new button or loads a new file. In 
    #' our case, look for the uploaded file's datapath argument and load it with 
    #' read.csv. Return this data frame in the normal return() style.
    load_data <- reactive({
        
        read.csv(input$input_data$datapath, header = TRUE)
        
        return()
    })
    
    #' Volcano plot
    #'
    #' @param input_data The loaded data frame.
    #' @param x_name The column name to plot on the x-axis
    #' @param y_name The column name to plot on the y-axis
    #' @param slider A negative integer value representing the magnitude of
    #' p-adjusted values to color. Most of our data will be between -1 and -300.
    #' @param color_1 One of the colors for the points.
    #' @param color_2 The other colors for the points. Hexadecimal strings: "#CDC4B5"
    #'
    #' @return A ggplot object of a volcano plot
    #' @details I bet you're tired of these plots by now. Me too, don't worry.
    #' This is _just_ a normal function. No reactivity, no bells, no whistles. 
    #' Write a normal volcano plot using geom_point, and integrate all the above 
    #' values into it as shown in the example app. The testing script will treat 
    #' this as a normal function.
    #' 
    #' !!sym() may be required to access column names in ggplot aes().
    #'
    #' @examples volcano_plot(df, "log2fc", "padj", -100, "blue", "taupe")
    volcano_plot <-function(input_data, x_name, y_name, slider, color_1, color_2) {
        
        #The goal here is to develop another column named "sorting". In this column, 
        #we will originally have "NA", but based on the input of the slider value, 
        #the volcano plot should display whether it is above the user's threshold
        #or not. 
        input_data$sorting <- "NA"
        input_data$sorting[input_data$padj < 1E(slider)] <- "TRUE"
        input_data$sorting[input_data$padj > 1E(slider)] <- "FALSE"
        
        #The two color points are going to be based on what the user chooses
        #These colors will then be assigned to either "TRUE" or "FALSE"
        point_colors = c(color_1, color_2)
        names(point_colors) <- c("TRUE", "FALSE")
        
        volcano_friend <- 
            ggplot(input_data, aes(x = x_name, y = y_name, col = sorting)) +
            geom_point() +
            xlab(x_name)+
            ylab(y_name)+
            #The points of the colors will be based on the information above
            scale_color_manual(values = point_colors) +
            theme_bw()+
            theme(legend.position = "bottom") +
            theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5))
        
        return(volcano_friend)
    }
    #' Draw and filter table
    #'
    #' @param input_data Data frame loaded by load_data()
    #' @param slider Negative number, typically from the slider input.
    #'
    #' @return Data frame filtered to p-adjusted values that are less than 
    #' 1 * 10^slider, columns for p-value and p-adjusted value have more digits 
    #' displayed.
    #' @details Same as above, this function is a standard R function. Tests will 
    #' evaluate it normally. Not only does this function filter the data frame to 
    #' rows that are above the slider magnitude, it should also change the format 
    #' of the p-value columns to display more digits. This is so that it looks 
    #' better when displayed on the web page. I would suggest the function 
    #' `formatC()`
    #'
    #' @examples draw_table(deseq_df, -210)
    #'    X  baseMean     log2FC     lfcSE      stat       pvalue         padj
    #'gene1 11690.780   9.852926 0.2644650  37.25607 8.45125e-304 1.54472e-299
    #'gene2  3550.435  -6.183714 0.1792708 -34.49369 9.97262e-261 9.11398e-257
    
    draw_table <- function(input_data, slider) {
        #This is done just to make sure that that input_data is in a tibble or
        #dataframe format 
        slider_tibble <- as.tibble(input_data)
        format(slider_tibble)
        
        #Here we are taking the tibble we just made and then we want to 
        #specifically target out the padj values that are less than 1E(slider)
        filtered_tible <- slider_tibble %>%
            filter(padj < 1E(slider)) %>%
        return()
    }
    
    #' These outputs aren't really functions, so they don't get a full skeleton, 
    #' but use the renderPlot() and renderTabel() functions to return() a plot 
    #' or table object, and those will be displayed in your application.
    output$volcano <- renderPlot({
        return(volcano_friend)
    
    })
    
    # Same here, just return the table as you want to see it in the web page
    output$table <- renderPlot({
        return(filtered_table)
        
    })
}

# Run the application
shinyApp(ui = ui, server = server)
