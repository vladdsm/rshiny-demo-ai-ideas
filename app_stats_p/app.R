library(shiny)
library(ggplot2)
library(dplyr)
library(pwr)

ui <- fluidPage(
  titlePanel("Understanding Statistical Power"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      h4("Parameters"),
      
      sliderInput("effect_size",
                  "Effect Size (Cohen's d):",
                  min = 0.0, max = 1.5, value = 0.5, step = 0.05),
      
      sliderInput("n",
                  "Sample Size per Group (n):",
                  min = 5, max = 500, value = 30, step = 1),
      
      sliderInput("alpha",
                  "Significance Level (α):",
                  min = 0.01, max = 0.20, value = 0.05, step = 0.01),
      
      hr(),
      
      h4("Results"),
      verbatimTextOutput("power_output"),
      
      hr(),
      
      h4("Power Analysis (pwr package)"),
      p("Required n per group for target power:"),
      numericInput("target_power", "Target Power:", 
                   value = 0.80, min = 0.5, max = 0.99, step = 0.01),
      verbatimTextOutput("required_n"),
      
      hr(),
      
      p(em("The green area = Power (probability of correctly detecting a true effect)")),
      p(em("The blue areas = α (Type I error rate under H0)"))
    ),
    
    mainPanel(
      width = 9,
      plotOutput("power_plot", height = "550px"),
      
      hr(),
      
      fluidRow(
        column(6,
               h4("Key Concepts"),
               tags$ul(
                 tags$li(tags$b("H0 distribution:"), 
                         " What the test statistic looks like when there is NO effect."),
                 tags$li(tags$b("H1 distribution:"), 
                         " What the test statistic looks like when there IS a real effect."),
                 tags$li(tags$b("Blue regions (α):"), 
                         " Rejection regions — probability of falsely rejecting H0."),
                 tags$li(tags$b("Green region (Power):"), 
                         " Probability of correctly rejecting H0 when H1 is true.")
               )
        ),
        column(6,
               h4("Interpretation"),
               textOutput("interpretation"),
               br(),
               h4("Rule of Thumb"),
               p("A commonly recommended target power is ",
                 tags$b("0.80 – 0.90"),
                 ". If power is too low, your study may miss a real effect!"),
               p(tags$i("Use power analysis BEFORE collecting data to determine the required sample size."))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Compute critical value and power for two-sample t-test (normal approximation)
  calc <- reactive({
    d <- input$effect_size
    n <- input$n
    alpha <- input$alpha
    
    # Use normal approximation for visualization
    # Critical values (two-sided)
    z_crit <- qnorm(1 - alpha / 2)
    
    # Non-centrality parameter for two-sample t-test
    # For balanced design, SE of difference ~ sqrt(2/n)
    ncp <- d * sqrt(n / 2)
    
    # Power = P(Z > z_crit - ncp) + P(Z < -z_crit - ncp)
    power <- 1 - pnorm(z_crit - ncp) + pnorm(-z_crit - ncp)
    
    list(z_crit = z_crit, ncp = ncp, power = power,
         d = d, n = n, alpha = alpha)
  })
  
  output$power_output <- renderPrint({
    p <- calc()
    cat("Non-centrality parameter (δ):", round(p$ncp, 3), "\n")
    cat("Critical value (z):", round(p$z_crit, 3), "\n")
    cat("Power (1 - β):", round(p$power, 3), "\n")
    cat("Type II error (β):", round(1 - p$power, 3), "\n")
  })
  
  output$required_n <- renderPrint({
    tryCatch({
      # Two-sample t-test power calculation
      result <- pwr.t.test(d = max(input$effect_size, 0.01),
                           sig.level = input$alpha,
                           power = input$target_power,
                           type = "two.sample",
                           alternative = "two.sided")
      cat("n per group:", ceiling(result$n), "\n")
      cat("(Total N:", 2 * ceiling(result$n), ")")
    }, error = function(e) {
      cat("Cannot compute (effect size too small).")
    })
  })
  
  output$power_plot <- renderPlot({
    p <- calc()
    z_crit <- p$z_crit
    ncp <- p$ncp
    alpha <- p$alpha
    
    # x range
    x_min <- min(-4, -z_crit - 1, ncp - 4)
    x_max <- max(4, z_crit + 1, ncp + 4)
    x <- seq(x_min, x_max, length.out = 1000)
    
    # Densities
    h0 <- dnorm(x, mean = 0, sd = 1)
    h1 <- dnorm(x, mean = ncp, sd = 1)
    
    # Rejection region boundaries
    lower_crit <- -z_crit
    upper_crit <- z_crit
    
    # Build data frame for the rejection regions under H0 (blue)
    df_h0_rej_lower <- data.frame(
      x = x[x <= lower_crit],
      y = h0[x <= lower_crit]
    )
    df_h0_rej_upper <- data.frame(
      x = x[x >= upper_crit],
      y = h0[x >= upper_crit]
    )
    
    # Power region under H1 (green) — where we reject H0
    df_h1_power <- data.frame(
      x = c(x[x <= lower_crit], x[x >= upper_crit]),
      y = c(h1[x <= lower_crit], h1[x >= upper_crit])
    )
    
    # Type II error region under H1 (light red)
    df_h1_beta <- data.frame(
      x = x[x > lower_crit & x < upper_crit],
      y = h1[x > lower_crit & x < upper_crit]
    )
    
    df_lines <- data.frame(
      x = x,
      H0 = h0,
      H1 = h1
    )
    
    ggplot() +
      # Shade power (green) under H1
      geom_area(data = df_h1_power, aes(x = x, y = y),
                fill = "#2ecc71", alpha = 0.55) +
      # Shade β (light orange) under H1
      geom_area(data = df_h1_beta, aes(x = x, y = y),
                fill = "#f39c12", alpha = 0.25) +
      # Shade α (blue) under H0 — drawn on top
      geom_area(data = df_h0_rej_lower, aes(x = x, y = y),
                fill = "#3498db", alpha = 0.65) +
      geom_area(data = df_h0_rej_upper, aes(x = x, y = y),
                fill = "#3498db", alpha = 0.65) +
      # Distribution curves
      geom_line(data = df_lines, aes(x = x, y = H0, color = "H0 (no effect)"),
                size = 1.2) +
      geom_line(data = df_lines, aes(x = x, y = H1, color = "H1 (real effect)"),
                size = 1.2) +
      # Critical value lines
      geom_vline(xintercept = c(lower_crit, upper_crit),
                 linetype = "dashed", color = "grey30", size = 0.8) +
      # Annotations
      annotate("text", x = upper_crit + (x_max - upper_crit)/3,
               y = max(h0) * 0.85,
               label = sprintf("α/2 = %.3f", alpha/2),
               color = "#2471a3", fontface = "bold", size = 5) +
      annotate("text", x = lower_crit - (lower_crit - x_min)/3,
               y = max(h0) * 0.85,
               label = sprintf("α/2 = %.3f", alpha/2),
               color = "#2471a3", fontface = "bold", size = 5) +
      annotate("text", x = ncp, y = max(h1) * 0.9,
               label = sprintf("Power = %.2f", p$power),
               color = "#145a32", fontface = "bold", size = 6) +
      annotate("text", x = ncp, y = max(h1) * 0.35,
               label = sprintf("β = %.2f", 1 - p$power),
               color = "#7e5109", fontface = "bold", size = 5) +
      scale_color_manual(values = c("H0 (no effect)" = "#1a5276",
                                    "H1 (real effect)" = "#922b21"),
                         name = NULL) +
      labs(
        title = sprintf("Statistical Power Visualization  (d = %.2f, n = %d, α = %.2f)",
                        input$effect_size, input$n, input$alpha),
        subtitle = "Blue = significance level (α),  Green = power (1 − β),  Orange = β (Type II error)",
        x = "Test Statistic (e.g., z or t)",
        y = "Density"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "top",
        plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank()
      )
  })
  
  output$interpretation <- renderText({
    p <- calc()
    pw <- p$power
    if (pw < 0.5) {
      msg <- sprintf("Power = %.0f%%. Very LOW — you would miss a real effect most of the time!", pw * 100)
    } else if (pw < 0.8) {
      msg <- sprintf("Power = %.0f%%. Below the recommended 0.80 threshold — consider increasing sample size.", pw * 100)
    } else if (pw <= 0.9) {
      msg <- sprintf("Power = %.0f%%. Good! This is within the commonly recommended 0.80–0.90 range.", pw * 100)
    } else {
      msg <- sprintf("Power = %.0f%%. Excellent — very high chance of detecting a real effect.", pw * 100)
    }
    msg
  })
}

shinyApp(ui, server)