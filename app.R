#######################################################
# Shiny App - An interactive visualisation of the DDM #
# --------------------------------------------------- #
# Written by Ladislas Nalborczyk                      #
# Last updated on September 3, 2020                   #
#######################################################

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
                withMathJax(),
                fluidRow(h3("Simulating response time distributions") ),
                fluidRow(h4(HTML(
                "The code underlying this application can be found at
                <a href='https://github.com/lnalborczyk/shiny_DDM'>
                https://github.com/lnalborczyk/shiny_DDM</a>.
                NB: the non-decision time is represented by the shaded area."
                ) ) ),
                
                fluidRow(
                    sidebarLayout(
                        sidebarPanel(
                            id = "sidebar",
                            width = 4,
                            # height = "600px",
                            sliderInput(
                                inputId = "alpha",
                                label = "Select a value for the boundary separation (alpha)",
                                value = 2,
                                min = 1,
                                max = 3,
                                step = 0.1,
                                width = "500px"
                                ) %>%
                                helper(
                                    size = "m",
                                    colour = "black",
                                    type = "inline",
                                    title = "Boundary separation",
                                    content = "The boundary separation is the distance
                                    between the two decision bounds and can be interpreted
                                    as a measure of response caution, with high value
                                    meaning high caution (or speed-accuracy trade-off
                                    with high value meaning high accuracy).",
                                    fade = TRUE
                                    ),
                            sliderInput(
                                inputId = "beta",
                                label = "Select a value for the starting point (beta)",
                                value = 0.5,
                                min = 0,
                                max = 1,
                                step = 0.1,
                                width = "500px"
                                ) %>%
                                helper(
                                    size = "m",
                                    colour = "black",
                                    type = "inline",
                                    title = "Starting point",
                                    content = "The starting point (or bias) of the
                                    accumulation process is a measure of response
                                    bias towards one of the two response boundaries,
                                    reflecting the a priori expectation.",
                                    fade = TRUE
                                    ),
                            sliderInput(
                                inputId = "delta",
                                label = "Select a value for the drift rate (delta)",
                                value = 0,
                                min = -2,
                                max = 2,
                                step = 0.1,
                                width = "500px"
                                ) %>%
                                helper(
                                    size = "m",
                                    colour = "black",
                                    type = "inline",
                                    title = "Drift rate",
                                    content = "The drift rate is the average slope
                                    of the accumulation process towards the boundaries
                                    (i.e., it represents the average amount of evidence
                                    accumulated per unit of time). The larger the absolute
                                    value of the drift rate, the stronger the evidence for
                                    the corresponding response option (thus quantifying
                                    the 'ease of processing').",
                                    fade = TRUE
                                ),
                            sliderInput(
                                inputId = "tau",
                                label = "Select a value for the non-decision time (tau)",
                                value = 1,
                                min = 0,
                                max = 2,
                                step = 0.1,
                                width = "500px"
                                ) %>%
                                helper(
                                    size = "m",
                                    colour = "black",
                                    type = "inline",
                                    title = "Non-decision time",
                                    content = "The non-decision time captures all
                                    non-decisional processes such as stimulus encoding
                                    and (motor) response processes.",
                                    fade = TRUE
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
                    Ladislas Nalborczyk</a>. Last update: September 3, 2020"
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
    
    # displays helper text
    observe_helpers(withMathJax = TRUE)
    
    output$BFdist.plot <-
        renderPlot({
            
            # refresh the input
            input$refresh
            
            # extracts the user input as numeric values
            alpha <- as.numeric(input$alpha)
            beta <- as.numeric(input$beta)
            delta <- as.numeric(input$delta)
            tau <- as.numeric(input$tau)
            
            # defines the grid of possible RT values
            # rt_grid <- seq.int(from = 0, to = 15, length.out = 1e3)
            
            # computes the lower density
            # lower_dens <- dwiener(
            #     q = rt_grid,
            #     alpha = alpha, tau = tau, beta = beta, delta = delta,
            #     resp = "lower", give_log = FALSE
            #     )
            
            # computes the upper density
            # upper_dens <- dwiener(
            #     q = rt_grid,
            #     alpha = alpha, tau = tau, beta = beta, delta = delta,
            #     resp = "upper", give_log = FALSE
            #     )
            
            # generates some data using the RWiener package
            df <- rwiener(
                n = 1e3,
                alpha = alpha,
                beta = beta,
                delta = delta,
                tau = tau
                )
            
            # computes the number of upper and lower responses
            response_table <- table(df$resp) %>% data.frame
            
            n_upper <- response_table %>%
                filter(Var1 == "upper") %>%
                pull(Freq) %>%
                as.numeric
            
            n_lower <- response_table %>%
                filter(Var1 == "lower") %>%
                pull(Freq) %>%
                as.numeric
                
            # computes densities
            ud <- density(df$q[df$resp == "upper"], cut = 0)
            ld <- density(df$q[df$resp == "lower"], cut = 0)
            
            # rescales alpha between 0.5 and 1 to define the
            # vertical blank space between densities
            s <- (alpha / 10) * (1 - 0.5) + 0.5
            
            # horizontal position of the densities
            x <- c(
                ud$x[1], ud$x, ud$x[length(ud$x)],
                ld$x[1], ld$x, ld$x[length(ld$x)]
                )
            
            # vertical position of the densities
            y <- c(s, ud$y + s, s, -s, -ld$y - s, -s)
                
            # standardised starting point (relative to the standardised alpha)
            lower_limit <- 0 - s / 2
            upper_limit <- 0 + s / 2
            beta_s <- lower_limit + beta * s
            
            # computes the implied angle of the drift rate
            drift_angle <- atan(delta) * 180 / pi
            
            # new data frame
            df2 <- data.frame(
                x = x, y = y,
                resp = rep(c("upper", "lower"), each =  length(ud$x) + 2)
                )
                
            # gets maximum x-axis value
            max_x <- max(df2$x)
            
            # defines colors for lower and upper densities
            lower_color <- "#c72e29"
            upper_color <- "#016392"
                
            ############
            # plotting #
            ############
            
            df2 %>%
                ggplot(aes(x = x, y = y, fill = resp, color = resp) ) +
                # plotting densities
                geom_segment(
                    data = . %>% filter(resp == "upper"),
                    aes(x = 0, xend = max_x, y = min(y), yend = min(y) ),
                    color = upper_color
                    ) +
                geom_segment(
                    data = . %>% filter(resp == "lower"),
                    aes(x = 0, xend = max_x, y = max(y), yend = max(y) ),
                    color = lower_color
                    ) +
                geom_polygon(alpha = 0.8) +
                scale_fill_manual(
                    values = c(lower_color, upper_color),
                    guide = guide_none()
                    ) +
                scale_color_manual(
                    values = c(lower_color, upper_color),
                    guide = guide_none()
                    ) +
                # stimulus onset
                geom_vline(xintercept = 0, lty = 2, col = "grey30") +
                annotate(
                    geom = "text",
                    x = 0, y = max(y),
                    hjust = 1,
                    vjust = -1,
                    size = 5, angle = 90,
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
                    size = 5,
                    label = "starting point",
                    colour = "purple"
                    ) +
                # non-decision time
                annotate(
                    geom = "rect",
                    xmin = 0, xmax = tau,
                    ymin = -Inf, ymax = Inf,
                    alpha = 0.25
                    ) +
                # drift rate
                # geom_segment(
                #     aes(
                #         x = tau,
                #         # xend = tau + 0.1,
                #         xend = tau + 0.1 * max_x,
                #         y = beta_s,
                #         # yend = log(delta / (1 - beta) ) + delta / 2
                #         # yend = beta_s + delta
                #         yend = ifelse(beta_s + delta > s, s, beta_s + delta)
                #         ),
                #     arrow = arrow(
                #         length = unit(0.2, "cm"),
                #         ends = "last", type = "closed"
                #         ),
                #     size = 0.5, colour = "darkgreen"
                #     ) +
                geom_spoke(
                    aes(
                        x = tau, y = beta_s,
                        angle = drift_angle * (pi / 180),
                        radius = 0.5
                        ),
                    arrow = arrow(
                        length = unit(0.2, "cm"),
                        ends = "last", type = "closed"
                        ),
                    color  = "darkgreen"
                    ) +
                annotate(
                    geom = "label",
                    x = tau,
                    y = beta_s,
                    hjust = 1, vjust = 0.5,
                    size = 5,
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
                    size = 5,
                    label = "boundary separation",
                    color = "black"
                    ) +
                # labelling distributions
                annotate(
                    geom = "label",
                    x = min(df2$x[df2$resp == "upper"]),
                    y = min(df2$y[df2$resp == "upper"]),
                    hjust = 0, vjust = -0.5,
                    size = 5,
                    label = paste0(
                        "RT distribution for upper responses (",
                        (n_upper / 1e3) * 100, "% of trials)"
                        )
                    ) +
                annotate(
                    geom = "label",
                    x = min(df2$x[df2$resp == "lower"]),
                    y = max(df2$y[df2$resp == "lower"]),
                    hjust = 0, vjust = 1.5,
                    size = 5,
                    label = paste0(
                        "RT distribution for lower responses (",
                        (n_lower / 1e3) * 100, "% of trials)"
                        )
                    ) +
                # aesthetics
                theme_ipsum_rc(base_size = 14, axis_title_size = 14) +
                theme(
                    axis.text.y = element_blank(),
                    plot.margin = unit(c(1, 1, 1, 3), "cm")
                    ) +
                labs(x = "Response time (in seconds)", y = "") +
                # extends plotting area
                coord_cartesian(xlim = c(0, NA), clip = "off") +
                # adds a second axis on the top
                scale_x_continuous(sec.axis = sec_axis(trans = ~.) )
            
            })
    
}

# running the application 
shinyApp(ui = ui, server = server)

# nobs = 1e3; alpha = 2; beta = 0.5; delta = 0; tau = 1;
# df <- rwiener(n = nobs, alpha = alpha, tau = tau, beta = beta, delta = delta)
