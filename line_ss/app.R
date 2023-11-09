#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(MASS)
library(reactable)
library(BH)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Sums of Squares Visualization"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("intercept",
                        "Intercept:",
                        value = 0),
            numericInput("slope",
                         "Slope:",
                         value = 1),
            radioButtons("view_ss",
                          "View Sums of Squares",
                          choices = list(
                              "Normal View" = 1,
                              "View Residuals" = 2,
                              "View Sums of Squares" = 3),
                         selected = 1),
            h3("Data Simulation"),
            numericInput("mean_x",
                         "Mean of X:",
                         value = 5),
            numericInput("mean_y",
                         "Mean of Y:",
                         value = 5),
            numericInput("corr_xy",
                         "Correlation",
                         value = 0.5),
            numericInput("n_sim",
                         "Number of observations",
                         value = 10),
            h3("Axis Settings"),
            numericInput("x_min",
                         "Minimum X:",
                         value = 0),
            numericInput("x_max",
                         "Maximum X",
                         value = 10),
            numericInput("y_min",
                         "Minimum Y",
                         value = 0),
            numericInput("y_max",
                         "Maximum Y",
                         value = 10)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("plotted"),
           textOutput("text"),
        )
    )
)

sig <- matrix(c(1, 0.5, 0.5, 1), nrow = 2, ncol = 2)

sim_data <- as_tibble(mvrnorm(n = 10, 
                      mu = c(X = 0, Y = 0),
                      Sigma = sig)) %>% 
        mutate(pred = 0 + 0.5*X,
               diff = sqrt((Y - pred)^2))
sim_data

sum((sim_data$Y-sim_data$pred)^2)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
sl <- reactive({input$slope})
int <- reactive({input$intercept})

## Correlation matrix
my_sigma <- reactive({
    matrix(c(1, input$corr_xy, input$corr_xy, 1), nrow = 2, ncol = 2)
})

## Simulate data
sim_data <- reactive({
    set.seed(999)

    data <- as_tibble(mvrnorm(n = input$n_sim, 
                mu = c(X = input$mean_x, Y = input$mean_y),
                Sigma = my_sigma())) %>% 
        mutate(pred = int() + sl()*X,
               diff = sqrt((Y - pred)^2))
    return(data)
})

## Calculate Sum of Squares
ss <- reactive({
    round(sum((sim_data()$Y-sim_data()$pred)^2), 2)
})

output$text <- renderText({
    paste("Sums of Squares =", ss())
})

## Render reference plot
output$plotted <- renderPlot({
    if(input$view_ss == 2) {
        my_plot <- ggplot(sim_data(), aes(x = X, y = Y)) +
            geom_point() +
            geom_abline(slope = sl(), intercept = int()) +
            xlim(input$x_min, input$x_max) +
            ylim(input$y_min, input$y_max) +
            theme_minimal()
        
        for (i in 1:input$n_sim) {
            my_plot <- my_plot +
                geom_segment(aes_string(x = sim_data()$X[i], xend = sim_data()$X[i],
                                     y = sim_data()$Y[i], yend = sim_data()$pred[i]),
                          fill=alpha("grey",0),
                          color = "red")
        }
        return(my_plot)
    }
    
    if(input$view_ss == 3) {
        my_plot <- ggplot(sim_data(), aes(x = X, y = Y)) +
            geom_point() +
            geom_abline(slope = sl(), intercept = int()) +
            xlim(input$x_min, input$x_max) +
            ylim(input$y_min, input$y_max) +
            theme_minimal()
        
        for (i in 1:input$n_sim) {
            my_plot <- my_plot +
                geom_rect(aes_string(xmin = sim_data()$X[i], xmax = sim_data()$X[i] - sim_data()$diff[i],
                                     ymin = sim_data()$Y[i], ymax = sim_data()$pred[i]),
                          fill=alpha("grey",0),
                          color = "red")
        }
        return(my_plot)
    }
    else {
        ggplot(sim_data(), aes(x = X, y = Y)) +
            geom_point() +
            geom_abline(slope = sl(), intercept = int()) +
            xlim(input$x_min, input$x_max) +
            ylim(input$y_min, input$y_max) +
            theme_minimal()
    }
})


# ## Create Sum of Squares Plot
# plot_squares <- reactive({
#     my_plot <- ggplot(sim_data(), aes(x = X, y = Y)) +
#         geom_point() +
#         geom_abline(slope = sl(), intercept = int()) +
#         coord_equal() +
#         xlim(0, max(X)) +
#         ylim(0, max(Y))
# 
#     for (i in 1:input$n_sim) {
#         my_plot <- my_plot +
#             geom_rect(aes_string(xmin = sim_data()$X[i], xmax = sim_data()$X[i] - sim_data()$diff[i],
#                                  ymin = sim_data()$Y[i], ymax = sim_data()$pred[i]),
#                       fill=alpha("grey",0),
#                       color = "red")
#     }
#         return(my_plot)
# })


# output$plot_sq <- renderPlot({plot_squares()})

# Output data table for reference
# output$table <- renderReactable({
#     reactable(sim_data())
# })


}
# Run the application 
shinyApp(ui = ui, server = server)
