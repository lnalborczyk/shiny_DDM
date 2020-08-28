##############################################################################
# --------------- Shiny App - An interactive visualisation of the DDM ------ #
##############################################################################

library(shinythemes)
library(shinyhelper)
library(hrbrthemes)
library(tidyverse)
library(shiny)

source("DDM_simulation.R")

################################################################################
############################# USER INTERFACE ###################################
################################################################################

ui <- shinyUI(
    navbarPage(
        title = "An interactive visualisation of the drift diffusion model",
        # choose a theme
        # themeSelector(),
        # set a theme
        theme = shinytheme("sandstone"),
        
        ##################################################################################
        # --------------------- UI: Instructions Panel --------------------------------- #
        ##################################################################################
        
        # tabPanel(
        #     title = "Instructions",
        #     # includes html file containing the instructions
        #     # includeMarkdown("instructions.md"),
        #     includeHTML("instructions.html"),
        #     # includes a footer
        #     hr(),
        #     HTML(
        #         paste(
        #             "Written by <a href='https://www.barelysignificant.com'>
        #             Ladislas Nalborczyk</a>. Last update: April 26th, 2019"
        #             )
        #         )
        #     ),
        
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
            
            ) # end panel 4DDM
        
        )
    ) # end UI

################################################################################
################################# SERVER #######################################
################################################################################

server <- function (input, output) {
    
    ##################################################################################
    # ---------------------- Server: scripts generation ---------------------------- #
    ##################################################################################
    
    # Uses 'helpfiles' directory by default
    # observe_helpers(withMathJax = TRUE)
    
    #####################################################################
    # ---------------------- Server: Bias factor ---------------------- #
    #####################################################################
    
    output$BFdist.plot <-
        renderPlot({
            input$refresh
                ddm(
                    nobs = as.numeric(input$nobs),
                    alpha = as.numeric(input$alpha),
                    beta = as.numeric(input$beta),
                    delta = as.numeric(input$delta),
                    tau = as.numeric(input$tau)
                    )
                # biasfactor(
                #     nsims = input$nsims, nsamples = input$nsamples, origin = 0,
                #     drift = input$drift, bfsd = 1, criterion = 10,
                #     originsd = 0, driftsd = 0, prior = input$prior,
                #     priorsd = 0
                #     ) %>%
                #     ggplot(aes(x = obs, y = bf, group = sim) ) +
                #     geom_hline(yintercept = 0, linetype = 2) +
                #     geom_line(alpha = 0.25) +
                #     theme_ipsum_rc(base_size = 14) +
                #     labs(
                #         title = "Simulating the evolution of SBF trajectories",
                #         subtitle = paste(
                #             "For an effect of", input$drift,
                #             "and a bias of", input$prior
                #             ),
                #         x = "Number of observations", y = "log(BF)"
                #         )
            })
    
}

# run the application 
shinyApp(ui = ui, server = server)
