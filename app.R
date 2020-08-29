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
        
        #####################################################################
        # -------------------------- UI: 4DM -------------------------------#
        #####################################################################
        
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
                            # sliderInput(
                            #     inputId = "nobs",
                            #     label = "Select a number of observations",
                            #     value = 100,
                            #     min = 50,
                            #     max = 500,
                            #     step = 10,
                            #     width = "500px"
                            #     ),
                            sliderInput(
                                inputId = "alpha",
                                label = "Select a value for the boundary separation",
                                value = 2,
                                min = 1,
                                max = 3,
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
                                value = 0,
                                min = -5,
                                max = 5,
                                step = 0.1,
                                width = "500px"
                                ),
                            sliderInput(
                                inputId = "tau",
                                label = "Select a value for the non-decision time",
                                value = 1,
                                min = 0,
                                max = 2,
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
                    Ladislas Nalborczyk</a>. Last update: August 30th, 2020"
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
            
            # nobs = 1e2; alpha = 2; beta = 0.5; delta = 0.5; tau = 0.3;
            # df <- rwiener(n = nobs, alpha = alpha, tau = tau, beta = beta, delta = delta)
            
                df <- rwiener(
                    # n = as.numeric(input$nobs),
                    n = 1e3,
                    alpha = as.numeric(input$alpha),
                    beta = as.numeric(input$beta),
                    delta = as.numeric(input$delta),
                    tau = as.numeric(input$tau)
                    )
                
                alpha <- as.numeric(input$alpha)
                beta <- as.numeric(input$beta)
                delta <- as.numeric(input$delta)
                tau <- as.numeric(input$tau)
                
                # compute densities
                ud <- density(df$q[df$resp == "upper"], cut = 0)
                ld <- density(df$q[df$resp == "lower"], cut = 0)
                
                # rescale alpha between 0.5 and 1
                # vertical blank space between densities
                s <- (alpha / 10) * (1 - 0.5) + 0.5
                
                # horizontal position of the densities
                x <- c(ud$x[1], ud$x, ud$x[length(ud$x)], ld$x[1], ld$x, ld$x[length(ld$x)])
                
                # vertical position of the densities
                y <- c(s, ud$y + s, s, -s, -ld$y - s, -s)
                
                # standardised starting point
                lower_limit <- 0 - s / 2
                upper_limit <- 0 + s / 2
                beta_s <- lower_limit + beta * s
                
                # computes angle of drift rate
                drift_angle <- atan(delta) * 180 / pi
                
                # new data frame
                df2 <- data.frame(
                    x = x, y = y,
                    resp = rep(c("upper", "lower"), each =  length(ud$x) + 2)
                    )
                
                df2 %>%
                    ggplot(aes(x = x, y = y, fill = resp, color = resp) ) +
                    # plotting densities
                    geom_segment(
                        data = . %>% filter(resp == "upper"),
                        aes(x = 0, xend = max(x), y = min(y), yend = min(y) ),
                        color = "orangered"
                    ) +
                    geom_segment(
                        data = . %>% filter(resp == "lower"),
                        aes(x = 0, xend = max(x), y = max(y), yend = max(y) ),
                        color = "steelblue"
                    ) +
                    geom_polygon(alpha = 0.8) +
                    scale_fill_manual(
                        values = c("steelblue", "orangered"),
                        guide = guide_none()
                        ) +
                    scale_color_manual(
                        values = c("steelblue", "orangered"),
                        guide = guide_none()
                        ) +
                    # stimulus onset
                    geom_vline(xintercept = 0, lty = 2, col = "grey30") +
                    annotate(
                        geom = "text",
                        x = 0, y = max(y),
                        hjust = 1,
                        vjust = -1,
                        size = 4, angle = 90,
                        label = "stimulus onset",
                        color = "grey30"
                    ) +
                    # starting point
                    geom_hline(yintercept = beta_s, lty = 3, col = "purple") +
                    annotate(
                        geom = "label",
                        x = 0, y = beta_s,
                        hjust = 1,
                        vjust = 0.5,
                        size = 4,
                        label = "starting point",
                        colour = "purple"
                    ) +
                    # non-decision time
                    annotate(
                        geom = "rect",
                        xmin = 0, xmax = tau,
                        ymin = -Inf, ymax = Inf,
                        alpha = 0.3
                    ) +
                    # annotate(
                    #     geom = "label",
                    #     x = 0, y = 0,
                    #     hjust = 0.01, vjust = ifelse(delta < 0, -1, 1.5),
                    #     size = 4,
                    #     label = "non-decision time"
                    # ) +
                    # drift rate
                    geom_segment(
                        aes(
                            x = tau,
                            # xend = tau + 0.1,
                            xend = tau + 0.1 * max(df2$x),
                            y = beta_s,
                            # yend = log(delta / (1 - beta) ) + delta / 2
                            yend = beta_s + delta
                            ),
                        arrow = arrow(length = unit(0.2, "cm"), ends = "last", type = "closed"),
                        size = 0.5, colour = "darkgreen"
                        ) +
                    # geom_spoke(
                    #     # aes(x = tau, y = beta_s, angle = 90, radius = 1),
                    #     aes(x = tau, y = beta_s, angle = drift_angle, radius = 1),
                    #     arrow = arrow(length = unit(0.2, "cm"), ends = "last", type = "closed"),
                    #     color  = "green"
                    #     ) +
                    annotate(
                        geom = "label",
                        x = tau + 0.1,
                        # y =  log(beta / (1 - beta) ) + delta / 2,
                        y = 0.1^2 + delta,
                        hjust = -0.05, vjust = 0,
                        size = 4,
                        label = "drift rate",
                        color = "darkgreen"
                        ) +
                    # boundary separation
                    geom_segment(
                        aes(
                            x = mean(x), xend = mean(x),
                            y = -s, yend = s,
                        ),
                        arrow = arrow(
                            length = unit(0.2, "cm"),
                            ends = "both", type = "closed"
                            ),
                        size = 0.5, colour = "black"
                        ) +
                    annotate(
                        geom = "label",
                        x = mean(x), y = 0,
                        hjust = 0.5, vjust = 0.5,
                        size = 4,
                        label = "boundary separation",
                        color = "black"
                        ) +
                    # labelling distributions
                    geom_text(
                        data = . %>% filter(resp == "upper"),
                        aes(x = min(x), y = min(y) ),
                        hjust = 0, vjust = -0.5, size = 4,
                        label = "RT distribution for correct responses",
                        color = "white"
                        ) +
                    geom_text(
                        data = . %>% filter(resp == "lower"),
                        aes(x = min(x), y = max(y) ),
                        hjust = 0, vjust = 1.5, size = 4,
                        label = "RT distribution for incorrect responses",
                        color = "white"
                        ) +
                    # aesthetics
                    theme_ipsum_rc(base_size = 12) +
                    theme(
                        axis.text.y = element_blank(),
                        plot.margin = unit(c(1, 1, 1, 3), "cm")
                    ) +
                    labs(x = "Reaction time (in seconds)", y = "") +
                    coord_cartesian(
                        xlim = c(0, NA),
                        # ylim = c(-2, 2),
                        clip = "off"
                        )
                
            })
    
}

# running the application 
shinyApp(ui = ui, server = server)
