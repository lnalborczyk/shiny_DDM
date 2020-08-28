##############################################################################
# --------------- Shiny App - An interactive visualisation of the DDM ------ #
##############################################################################

library(shinythemes)
library(shinyhelper)
library(hrbrthemes)
library(tidyverse)
library(RWiener)
library(shiny)

################################################################################
############################# USER INTERFACE ###################################
################################################################################

ui <- shinyUI(
    navbarPage(
        title = "An interactive visualisation of the drift diffusion model",
        # choose a theme
        # themeSelector(),
        # setting a theme
        theme = shinytheme("sandstone"),
        
        ##############################################################################
        # -------------------------- UI: 4DM --------------------------------------- #
        ##############################################################################
        
        tabPanel(
            title = "4DDM",
            fluidPage(
                
                fluidRow(h3("Simulating response time distributions") ),
                fluidRow(h4("The code underlying this model can be found at https://github.com/lnalborczyk/shiny_DDM.") ),
                
                fluidRow(
                    sidebarLayout(
                        sidebarPanel(
                            id = "sidebar",
                            width = 4,
                            sliderInput(
                                inputId = "nobs",
                                label = "Select a number of observations",
                                value = 100,
                                min = 50,
                                max = 500,
                                step = 1,
                                width = "500px"
                                ),
                            sliderInput(
                                inputId = "alpha",
                                label = "Select a value for the boundary separation",
                                value = 2,
                                min = 0,
                                max = 10,
                                step = 0.1,
                                width = "500px"
                                ),
                            sliderInput(
                                inputId = "beta",
                                label = "Select a value for the starting point (bias)",
                                value = 0.5,
                                min = 0,
                                max = 1,
                                step = 0.1,
                                width = "500px"
                                ),
                            sliderInput(
                                inputId = "delta",
                                label = "Select a value for the drift rate",
                                value = 0.5,
                                min = -10,
                                max = 10,
                                step = 0.1,
                                width = "500px"
                                ),
                            sliderInput(
                                inputId = "tau",
                                label = "Select a value for the non-decision time",
                                value = 0.3,
                                min = 0,
                                max = 10,
                                step = 0.1,
                                width = "500px"
                                ),
                            actionButton(
                                inputId = "refresh",
                                label = "Generate data",
                                width = "100%"
                                )
                            ),
                        mainPanel(
                            id = "MainPanel",
                            width = 8,
                            plotOutput("BFdist.plot", width = "100%", height = "600px")
                            )
                        )
                    )
                ),
            
            # footer
            hr(),
            HTML(
                paste(
                    "Written by <a href='https://www.barelysignificant.com'>
                    Ladislas Nalborczyk</a>. Last update: August 28th, 2020"
                    )
                )
            
            ) # ends panel 4DDM
        
        )
    ) # end UI

################################################################################
################################# SERVER #######################################
################################################################################

server <- function (input, output) {
    
    ##############################################################
    # ---------------------- Server: 4DDM ---------------------- #
    ##############################################################
    
    output$BFdist.plot <-
        renderPlot({
            input$refresh
            
                df <- rwiener(
                    n = as.numeric(input$nobs),
                    alpha = as.numeric(input$alpha),
                    beta = as.numeric(input$beta),
                    delta = as.numeric(input$delta),
                    tau = as.numeric(input$tau)
                    )
                
                df %>%
                    ggplot(aes(x = q) ) +
                    geom_density(
                        data = . %>% filter(resp == "upper"),
                        aes(y = ..density..),
                        colour = "steelblue", fill = "steelblue",
                        outline.type = "upper", alpha = 0.8, adjust = 1, trim = TRUE
                        ) +
                    geom_density(
                        data = . %>% filter(resp == "lower"),
                        aes(y = -..density..), colour = "orangered", fill = "orangered",
                        outline.type = "upper", alpha = 0.8, adjust = 1, trim = TRUE
                        ) +
                    geom_segment(
                        aes(x = 0, xend = as.numeric(input$tau), y = 0, yend = 0),
                        arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
                        size = 0.5
                        ) +
                    annotate(
                        geom = "text",
                        x = 0, y = 0, hjust = 0, vjust = -1, size = 3,
                        label = "non-decision time"
                        ) +
                    theme_ipsum_rc() +
                    labs(x = "Reaction time (in seconds)", y = "") +
                    xlim(0, NA)
            })
    
}

# running the application 
shinyApp(ui = ui, server = server)
