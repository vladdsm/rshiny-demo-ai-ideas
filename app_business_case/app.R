# =============================================================
# Cloud Product Comparison & Business Case Builder
# Author: V.Zhbanko
# Purpose: Allow sales managers to compare Microsoft Azure and
#          Amazon AWS GPU compute products, adjust cost parameters,
#          and generate a persuasive business case for customers.
#
# Stack: shiny + bslib + plotly + DT + dplyr + scales
# =============================================================

# --- 1. LOAD LIBRARIES ---------------------------------------
library(shiny)         # Core Shiny framework
library(bslib)         # Modern Bootstrap 5 theming
library(plotly)        # Interactive charts
library(DT)            # Interactive tables
library(dplyr)         # Data manipulation
library(tidyr)         # Data tidying
library(scales)        # Number formatting

# --- 2. MOCK DATA: GPU Compute Comparison --------------------
# Curated from publicly available Azure and AWS pricing pages.
# Focus: GPU-accelerated instances for AI/ML workloads.
# Prices are indicative USD/hour, on-demand, Linux, us-east-1
# and equivalent Azure regions. Users can adjust via sliders.

gpu_compare <- tibble::tribble(
  ~provider,   ~instance,           ~gpu_model,      ~gpu_count, ~vcpu, ~ram_gb, ~price_per_hr, ~network_gbps,
  "Azure",     "Standard_NC24ads_A100_v4", "NVIDIA A100",   1,       24,    220,     3.06,          100,
  "Azure",     "Standard_NC48ads_A100_v4","NVIDIA A100",   2,       48,    440,     6.12,          100,
  "Azure",     "Standard_NC96ads_A100_v4","NVIDIA A100",   4,       96,    880,     12.24,         100,
  "Azure",     "Standard_ND96asr_v4",     "NVIDIA A100",   8,       96,   900,     27.20,         160,
  "Azure",     "Standard_NV36ads_A10_v5", "NVIDIA A10",    1,       36,    440,     1.64,          50,
  "Azure",     "Standard_NV72ads_A10_v5", "NVIDIA A10",    2,       72,    880,     3.28,          50,
  "AWS",       "p4d.24xlarge",            "NVIDIA A100",   8,       96,  1152,     32.77,         400,
  "AWS",       "p4de.24xlarge",           "NVIDIA A100",   8,       96,  1152,     40.96,         400,
  "AWS",       "p3.2xlarge",              "NVIDIA V100",   1,        8,     61,     3.06,          10,
  "AWS",       "p3.8xlarge",              "NVIDIA V100",   4,       32,    244,     12.24,         32,
  "AWS",       "p3.16xlarge",             "NVIDIA V100",   8,       64,    488,     24.48,         25,
  "AWS",       "g5.2xlarge",              "NVIDIA A10G",   1,        8,     32,     1.21,          10,
  "AWS",       "g5.12xlarge",             "NVIDIA A10G",   4,       48,    192,     5.67,          40,
  "AWS",       "g5.48xlarge",             "NVIDIA A10G",   8,       96,    768,     16.29,         100
)

# --- 3. USER INTERFACE ---------------------------------------
ui <- page_sidebar(
  theme = bs_theme(
    version    = 5,
    bootswatch = "flatly",
    primary    = "#0078D4",         # Microsoft Azure blue
    secondary  = "#FF9900",         # AWS orange
    base_font  = font_google("Inter")
  ),
  title = tags$div(
    style = "display:flex; align-items:center; gap:10px;",
    tags$span("☁️"),
    tags$span("Cloud Product Comparison — Azure vs AWS")
  ),
  
  # ---------- Sidebar: global filters ----------
  sidebar = sidebar(
    width = 320,
    
    h5("1. Filter products"),
    selectInput("gpu_filter",
                "GPU model:",
                choices  = c("All", unique(gpu_compare$gpu_model)),
                selected = "All"),
    
    sliderInput("vcpu_range",
                "vCPU range:",
                min   = min(gpu_compare$vcpu),
                max   = max(gpu_compare$vcpu),
                value = c(min(gpu_compare$vcpu), max(gpu_compare$vcpu)),
                step  = 4),
    
    hr(),
    
    h5("2. Business case assumptions"),
    numericInput("hours_per_month",
                 "Hours used per month:",
                 value = 730, min = 1, max = 744, step = 10),
    
    numericInput("instance_count",
                 "Number of instances:",
                 value = 4, min = 1, max = 100, step = 1),
    
    selectInput("commitment",
                "Commitment term:",
                choices = c("On-demand" = "on_demand",
                            "1-year reserved" = "reserved_1y",
                            "3-year reserved" = "reserved_3y"),
                selected = "on_demand"),
    
    helpText("Reserved terms apply ~30% (1y) and ~50% (3y) discounts."),
    
    hr(),
    
    h5("3. Export"),
    downloadButton("dl_case", "Download business case (CSV)",
                   class = "btn-outline-primary w-100")
  ),
  
  # ---------- Main panel ----------
  navset_card_tab(
    id = "main_tabs",
    
    # ---- Tab 1: Overview / How to use ----
    nav_panel(
      "Overview",
      br(),
      layout_column_wrap(
        width = 1/2,
        value_box(
          title = "Products in view",
          value = textOutput("n_products"),
          showcase = bsicons::bs_icon("grid-3x3-gap")
        ),
        value_box(
          title = "Price gap (cheapest vs priciest)",
          value = textOutput("price_gap"),
          showcase = bsicons::bs_icon("currency-dollar")
        )
      ),
      br(),
      card(
        card_header("What this tool does"),
        card_body(
          tags$p("This app helps sales managers compare ",
                 tags$b("Microsoft Azure"), " and ", tags$b("Amazon AWS"),
                 " GPU compute offerings side by side, then build a ",
                 "customized business case for a customer."),
          tags$ul(
            tags$li(tags$b("Filter") , " by GPU model and vCPU count."),
            tags$li(tags$b("Compare") , " price per hour, price per vCPU, and ",
                    "monthly cost at your scale."),
            tags$li(tags$b("Adjust") , " commitment term to show savings ",
                    "from reserved capacity."),
            tags$li(tags$b("Drill down") , " by clicking any bar to see ",
                    "full specs for that instance.")
          ),
          tags$p(tags$b("How to use:"),
                 "Set your filters and assumptions on the left, then explore ",
                 "the Comparison, Cost Calculator, and Drill-down tabs.")
        )
      )
    ),
    
    # ---- Tab 2: Comparison charts ----
    nav_panel(
      "Comparison",
      br(),
      plotlyOutput("price_bar", height = "380px"),
      br(),
      plotlyOutput("value_scatter", height = "380px")
    ),
    
    # ---- Tab 3: Cost calculator ----
    nav_panel(
      "Cost Calculator",
      br(),
      DTOutput("cost_table"),
      br(),
      plotlyOutput("monthly_bar", height = "360px")
    ),
    
    # ---- Tab 4: Drill-down ----
    nav_panel(
      "Drill-down",
      br(),
      uiOutput("drill_header"),
      br(),
      DTOutput("drill_table")
    )
  )
)

# --- 4. SERVER LOGIC -----------------------------------------
server <- function(input, output, session) {
  
  # ---------- Reactive: filtered data ----------
  filtered_data <- reactive({
    df <- gpu_compare
    
    if (input$gpu_filter != "All") {
      df <- df %>% filter(gpu_model == input$gpu_filter)
    }
    
    df <- df %>%
      filter(vcpu >= input$vcpu_range[1],
             vcpu <= input$vcpu_range[2])
    
    # Apply commitment discount
    discount <- switch(input$commitment,
                       "on_demand"    = 0,
                       "reserved_1y"  = 0.30,
                       "reserved_3y"  = 0.50)
    
    df <- df %>%
      mutate(
        effective_price = price_per_hr * (1 - discount),
        monthly_cost    = effective_price * input$hours_per_month *
          input$instance_count,
        price_per_vcpu  = effective_price / vcpu
      )
    
    df
  })
  
  # ---------- Overview boxes ----------
  output$n_products <- renderText({
    nrow(filtered_data())
  })
  
  output$price_gap <- renderText({
    df <- filtered_data()
    if (nrow(df) < 2) return("—")
    gap <- max(df$effective_price) - min(df$effective_price)
    paste0("$", round(gap, 2), "/hr")
  })
  
  # ---------- Comparison: Price bar chart ----------
  output$price_bar <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) > 0)
    
    # Order by provider then price for a clean grouped view
    df <- df %>% arrange(provider, effective_price)
    
    plot_ly(
      df,
      x = ~reorder(paste(provider, instance, sep = " | "),
                   effective_price),
      y = ~effective_price,
      color = ~provider,
      colors = c("Azure" = "#0078D4", "AWS" = "#FF9900"),
      type = "bar",
      customdata = ~instance,
      hovertemplate = paste0(
        "<b>%{customdata}</b><br>",
        "Provider: %{color}<br>",
        "Effective price: $%{y:.4f}/hr<br>",
        "<extra></extra>"
      )
    ) %>%
      layout(
        title = list(text = "Effective price per hour (after discount)"),
        xaxis = list(title = "", tickangle = -35),
        yaxis = list(title = "USD / hour"),
        legend = list(orientation = "h", y = 1.1),
        margin = list(b = 140)
      )
  })
  
  # ---------- Comparison: Price vs performance scatter ----------
  output$value_scatter <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) > 0)
    
    plot_ly(
      df,
      x = ~vcpu,
      y = ~effective_price,
      size = ~ram_gb,
      color = ~provider,
      colors = c("Azure" = "#0078D4", "AWS" = "#FF9900"),
      text = ~instance,
      type = "scatter",
      mode = "markers",
      hovertemplate = paste0(
        "<b>%{text}</b><br>",
        "vCPU: %{x}<br>",
        "Price: $%{y:.4f}/hr<br>",
        "RAM: %{marker.size} GB<br>",
        "<extra></extra>"
      )
    ) %>%
      layout(
        title = list(text = "Price vs. vCPU (bubble size = RAM)"),
        xaxis = list(title = "vCPU"),
        yaxis = list(title = "Effective price (USD/hr)"),
        legend = list(orientation = "h", y = 1.1)
      )
  })
  
  # ---------- Cost table ----------
  output$cost_table <- renderDT({
    df <- filtered_data()
    req(nrow(df) > 0)
    
    display <- df %>%
      select(provider, instance, gpu_model, gpu_count, vcpu, ram_gb,
             effective_price, monthly_cost) %>%
      rename(
        Provider          = provider,
        Instance          = instance,
        GPU               = gpu_model,
        `GPU count`       = gpu_count,
        vCPU              = vcpu,
        `RAM (GB)`        = ram_gb,
        `Price/hr ($)`    = effective_price,
        `Monthly ($)`     = monthly_cost
      ) %>%
      mutate(
        `Price/hr ($)` = round(`Price/hr ($)`, 4),
        `Monthly ($)`  = round(`Monthly ($)`, 2)
      )
    
    datatable(
      display,
      selection = "single",
      rownames  = FALSE,
      options   = list(pageLength = 10, dom = "tip",
                       order = list(list(7, "asc")))
    ) %>%
      formatCurrency(c("Price/hr ($)", "Monthly ($)"),
                     currency = "$", digits = 2)
  })
  
  # ---------- Monthly cost bar ----------
  output$monthly_bar <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) > 0)
    
    df <- df %>% arrange(provider, monthly_cost)
    
    plot_ly(
      df,
      x = ~monthly_cost,
      y = ~reorder(paste(provider, instance, sep = " | "), monthly_cost),
      color = ~provider,
      colors = c("Azure" = "#0078D4", "AWS" = "#FF9900"),
      type = "bar",
      orientation = "h",
      hovertemplate = paste0(
        "<b>%{y}</b><br>",
        "Monthly: $%{x:,.2f}<br>",
        "<extra></extra>"
      )
    ) %>%
      layout(
        title = list(text = paste0(
          "Estimated monthly cost — ",
          input$instance_count, " instance(s) × ",
          input$hours_per_month, " hrs/month"
        )),
        xaxis = list(title = "USD / month"),
        yaxis = list(title = ""),
        legend = list(orientation = "h", y = 1.1),
        margin = list(l = 180)
      )
  })
  
  # ---------- Drill-down ----------
  output$drill_header <- renderUI({
    sel <- input$cost_table_rows_selected
    if (length(sel) == 0) {
      return(tags$p(style = "color:#888;",
                    "Select a row in the 'Cost Calculator' tab ",
                    "to see full specifications."))
    }
    df <- filtered_data()
    row <- df[sel, ]
    tags$div(
      style = "padding:14px; border-radius:10px; background:#f7fafc;",
      tags$h4(paste0(row$provider, " — ", row$instance)),
      tags$p(style = "color:#666;",
             paste0("GPU: ", row$gpu_model, " × ", row$gpu_count,
                    " | vCPU: ", row$vcpu,
                    " | RAM: ", row$ram_gb, " GB"))
    )
  })
  
  output$drill_table <- renderDT({
    sel <- input$cost_table_rows_selected
    req(length(sel) > 0)
    
    df <- filtered_data()
    row <- df[sel, ]
    
    detail <- data.frame(
      Specification = c("Provider", "Instance type", "GPU model",
                        "GPU count", "vCPU", "RAM (GB)",
                        "Network (Gbps)", "List price/hr ($)",
                        "Effective price/hr ($)",
                        "Monthly cost ($)"),
      Value = c(
        row$provider,
        row$instance,
        row$gpu_model,
        row$gpu_count,
        row$vcpu,
        row$ram_gb,
        row$network_gbps,
        round(row$price_per_hr, 4),
        round(row$effective_price, 4),
        round(row$monthly_cost, 2)
      )
    )
    
    datatable(detail, rownames = FALSE,
              options = list(dom = "t", pageLength = 20))
  })
  
  # ---------- Download business case ----------
  output$dl_case <- downloadHandler(
    filename = function() paste0("business_case_", Sys.Date(), ".csv"),
    content  = function(file) {
      df <- filtered_data() %>%
        select(provider, instance, gpu_model, gpu_count, vcpu, ram_gb,
               price_per_hr, effective_price, monthly_cost) %>%
        mutate(
          hours_per_month = input$hours_per_month,
          instance_count  = input$instance_count,
          commitment      = input$commitment
        )
      write.csv(df, file, row.names = FALSE)
    }
  )
}

# --- 5. RUN APP ----------------------------------------------
shinyApp(ui, server)