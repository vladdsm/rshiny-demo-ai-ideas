# =============================================================
# Industrial Equipment TCO Comparison — R Shiny App
# Author: [Your Name]
# Purpose: Help technical sales managers compare two VMCs
#          (Mazak VCN-530C vs Okuma GENOS M560-V) on 5-year TCO
#          and cost per productive hour.
#
# Stack: shiny + bslib + plotly + DT + dplyr + tidyr + scales
# =============================================================

# --- 1. LOAD LIBRARIES ---------------------------------------
library(shiny)
library(bslib)
library(plotly)
library(DT)
library(dplyr)
library(tidyr)
library(scales)

# --- 2. MOCK DATA: Machine Specifications --------------------
# Specs sourced from machinetoolindex.com and manufacturer sites.
# Price ranges: Mazak $123K-$150K; Okuma $140K-$190K.

machines <- tibble::tribble(
  ~machine,              ~manufacturer, ~x_travel, ~y_travel, ~z_travel,
  ~table_size,           ~spindle_rpm,  ~spindle_kw, ~tool_capacity,
  ~machine_weight_kg,    ~rapid_rate,   ~chip_to_chip, ~price_low, ~price_high,
  "VCN-530C",            "Mazak",       1050,      530,       510,
  "1300 x 550 mm",       12000,         22,          30,
  7500,                  42,            1.3,         123000,     150000,
  "GENOS M560-V",        "Okuma",       1050,      560,       460,
  "1300 x 560 mm",       15000,         22,          32,
  8000,                  40,            1.8,         140000,     190000
)

# --- 3. USER INTERFACE ---------------------------------------
ui <- page_sidebar(
  theme = bs_theme(
    version    = 5,
    bootswatch = "flatly",
    primary    = "#0d3b66",
    secondary  = "#e76f51",
    base_font  = font_google("Inter")
  ),
  title = tags$div(
    style = "display:flex; align-items:center; gap:10px;",
    tags$span("⚙️"), tags$span("Machine TCO Comparison")
  ),
  
  # ---------- Sidebar: Assumptions ----------
  sidebar = sidebar(
    width = 340,
    h5("Purchase Assumptions"),
    sliderInput("mazak_price", "Mazak price ($K):",
                min = 100, max = 200, value = 130, step = 5, post = "K"),
    sliderInput("okuma_price", "Okuma price ($K):",
                min = 120, max = 220, value = 165, step = 5, post = "K"),
    
    hr(),
    h5("Operating Assumptions"),
    sliderInput("hours_year", "Operating hours per year:",
                min = 1000, max = 6000, value = 4000, step = 100),
    sliderInput("utilization", "Utilization rate (%):",
                min = 30, max = 95, value = 70, step = 5, post = "%"),
    sliderInput("labor_rate", "Operator rate ($/hr):",
                min = 15, max = 60, value = 28, step = 1, pre = "$"),
    
    hr(),
    h5("Cost Assumptions"),
    numericInput("electricity", "Electricity ($/kWh):",
                 value = 0.12, min = 0.05, max = 0.30, step = 0.01),
    sliderInput("maintenance_pct", "Annual maintenance (% of price):",
                min = 2, max = 8, value = 4, step = 0.5, post = "%"),
    sliderInput("tooling_hr", "Tooling & consumables ($/hr):",
                min = 1, max = 30, value = 8, step = 1, pre = "$"),
    
    hr(),
    h5("Financial Assumptions"),
    sliderInput("discount_rate", "Discount rate (%):",
                min = 0, max = 15, value = 8, step = 0.5, post = "%"),
    sliderInput("residual_pct", "Residual value after 5 years (%):",
                min = 20, max = 60, value = 40, step = 5, post = "%")
  ),
  
  # ---------- Main Panel ----------
  navset_card_tab(
    id = "main_tabs",
    
    # ---- Tab 1: Executive Summary ----
    nav_panel(
      "Executive Summary",
      br(),
      uiOutput("summary_boxes"),
      br(),
      card(
        card_header("Recommendation"),
        card_body(
          uiOutput("recommendation_text")
        )
      )
    ),
    
    # ---- Tab 2: TCO Breakdown ----
    nav_panel(
      "TCO Breakdown",
      br(),
      plotlyOutput("tco_stacked", height = "400px"),
      br(),
      DTOutput("tco_table")
    ),
    
    # ---- Tab 3: Sensitivity Analysis ----
    nav_panel(
      "Sensitivity",
      br(),
      plotlyOutput("sensitivity_chart", height = "420px")
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
  
  # ---------- TCO Calculation ----------
  calculate_tco <- reactive({
    
    # Base parameters
    hours <- input$hours_year * (input$utilization / 100)
    years <- 5
    
    # Helper to compute TCO for one machine
    compute_machine <- function(name, price, rpm, tools, weight, chip_time) {
      
      # Annual energy consumption (kWh) = power (kW) * hours
      # Spindle power as proxy; add 20% for auxiliaries
      energy_kwh <- rpm / 1000 * 15 * hours  # scaled estimate
      energy_cost <- energy_kwh * input$electricity
      
      # Maintenance
      maint_cost <- price * (input$maintenance_pct / 100)
      
      # Tooling
      tooling_cost <- input$tooling_hr * hours
      
      # Labor
      labor_cost <- input$labor_rate * hours
      
      # Downtime: estimated 2% of hours * shop rate
      downtime_hours <- hours * 0.02
      downtime_cost <- downtime_hours * input$labor_rate * 1.5
      
      # Annual totals
      annual_opex <- energy_cost + maint_cost + tooling_cost +
        labor_cost + downtime_cost
      
      # 5-year discounted TCO
      total_opex <- sum(annual_opex / (1 + input$discount_rate/100)^(1:5))
      residual <- price * (input$residual_pct / 100) /
        (1 + input$discount_rate/100)^5
      
      tco <- price + total_opex - residual
      
      tibble(
        machine = name,
        purchase = price,
        energy = sum(energy_cost / (1 + input$discount_rate/100)^(1:5)),
        maintenance = sum(maint_cost / (1 + input$discount_rate/100)^(1:5)),
        tooling = sum(tooling_cost / (1 + input$discount_rate/100)^(1:5)),
        labor = sum(labor_cost / (1 + input$discount_rate/100)^(1:5)),
        downtime = sum(downtime_cost / (1 + input$discount_rate/100)^(1:5)),
        residual = -residual,
        total_tco = tco,
        annual_hours = hours,
        cost_per_hour = tco / (hours * years)
      )
    }
    
    bind_rows(
      compute_machine("Mazak VCN-530C", input$mazak_price * 1000,
                      machines$spindle_rpm[1], machines$tool_capacity[1],
                      machines$machine_weight_kg[1], machines$chip_to_chip[1]),
      compute_machine("Okuma GENOS M560-V", input$okuma_price * 1000,
                      machines$spindle_rpm[2], machines$tool_capacity[2],
                      machines$machine_weight_kg[2], machines$chip_to_chip[2])
    )
  })
  
  # ---------- Summary Boxes ----------
  output$summary_boxes <- renderUI({
    df <- calculate_tco()
    layout_column_wrap(
      width = 1/3,
      value_box(
        title = "Mazak 5-Year TCO",
        value = paste0("$", format(round(df$total_tco[1]/1000), big.mark = ","), "K"),
        showcase = bsicons::bs_icon("gear")
      ),
      value_box(
        title = "Okuma 5-Year TCO",
        value = paste0("$", format(round(df$total_tco[2]/1000), big.mark = ","), "K"),
        showcase = bsicons::bs_icon("gear-wide-connected"),
        theme = "secondary"
      ),
      value_box(
        title = "Cost per Productive Hour",
        value = paste0("$", round(df$cost_per_hour[1], 2), " vs $", round(df$cost_per_hour[2], 2)),
        showcase = bsicons::bs_icon("clock-history"),
        theme = ifelse(df$cost_per_hour[1] < df$cost_per_hour[2], "success", "warning")
      )
    )
  })
  
  # ---------- Recommendation ----------
  output$recommendation_text <- renderUI({
    df <- calculate_tco()
    cheaper <- ifelse(df$total_tco[1] < df$total_tco[2], "Mazak VCN-530C", "Okuma GENOS M560-V")
    diff <- abs(df$total_tco[1] - df$total_tco[2])
    
    tags$div(
      tags$h4(paste0("Based on your assumptions, the ", cheaper, " delivers lower 5-year TCO.")),
      tags$p(paste0("The estimated advantage is $", format(round(diff), big.mark = ","),
                    " over 5 years, or $", format(round(diff/5), big.mark = ","), " per year.")),
      tags$p(style = "color:#666;", "Adjust the assumptions on the left to see how the conclusion changes.")
    )
  })
  
  # ---------- TCO Stacked Bar ----------
  output$tco_stacked <- renderPlotly({
    df <- calculate_tco() %>%
      select(machine, purchase, energy, maintenance, tooling, labor, downtime, residual) %>%
      pivot_longer(-machine, names_to = "component", values_to = "cost")
    
    df$component <- factor(df$component,
                           levels = c("purchase", "energy", "maintenance", "tooling", "labor", "downtime", "residual"),
                           labels = c("Purchase", "Energy", "Maintenance", "Tooling", "Labor", "Downtime", "Residual (-)")
    )
    
    colors <- c("Purchase" = "#0d3b66", "Energy" = "#f4a261",
                "Maintenance" = "#e76f51", "Tooling" = "#2a9d8f",
                "Labor" = "#264653", "Downtime" = "#e9c46a",
                "Residual (-)" = "#a8dadc")
    
    plot_ly(df, x = ~machine, y = ~cost, color = ~component,
            colors = colors, type = "bar",
            hovertemplate = "%{color}: $%{y:,.0f}<extra></extra>") %>%
      layout(
        barmode = "relative",
        title = "5-Year TCO Components",
        yaxis = list(title = "USD", tickformat = "$,.0f"),
        xaxis = list(title = ""),
        legend = list(orientation = "h", y = -0.2),
        margin = list(b = 100)
      )
  })
  
  # ---------- TCO Table ----------
  output$tco_table <- renderDT({
    df <- calculate_tco() %>%
      mutate(
        Machine = machine,
        Purchase = dollar(purchase),
        Energy = dollar(energy),
        Maintenance = dollar(maintenance),
        Tooling = dollar(tooling),
        Labor = dollar(labor),
        Downtime = dollar(downtime),
        `Residual (-)` = dollar(residual),
        `Total TCO` = dollar(total_tco),
        `Cost / Hour` = dollar(cost_per_hour)
      ) %>%
      select(Machine, Purchase, Energy, Maintenance, Tooling, Labor,
             Downtime, `Residual (-)`, `Total TCO`, `Cost / Hour`)
    
    datatable(df, rownames = FALSE,
              options = list(pageLength = 5, dom = "t"))
  })
  
  # ---------- Sensitivity Chart ----------
  output$sensitivity_chart <- renderPlotly({
    df <- calculate_tco()
    
    # Vary utilization from 30% to 95%
    util_seq <- seq(30, 95, by = 5)
    
    sens <- lapply(util_seq, function(u) {
      hours <- input$hours_year * (u / 100)
      
      calc_one <- function(price, maint_pct, tooling_hr) {
        maint <- price * (maint_pct / 100)
        tooling <- tooling_hr * hours
        labor <- input$labor_rate * hours
        energy <- (hours * 20) * input$electricity
        downtime <- hours * 0.02 * input$labor_rate * 1.5
        annual <- maint + tooling + labor + energy + downtime
        total_op <- sum(annual / (1 + input$discount_rate/100)^(1:5))
        residual <- price * (input$residual_pct / 100) / (1 + input$discount_rate/100)^5
        (price + total_op - residual) / (hours * 5)
      }
      
      tibble(
        utilization = u,
        Mazak = calc_one(input$mazak_price * 1000, input$maintenance_pct, input$tooling_hr),
        Okuma = calc_one(input$okuma_price * 1000, input$maintenance_pct, input$tooling_hr)
      )
    }) %>% bind_rows()
    
    plot_ly(sens, x = ~utilization, y = ~Mazak, name = "Mazak",
            type = "scatter", mode = "lines+markers",
            line = list(color = "#0d3b66", width = 3)) %>%
      add_trace(y = ~Okuma, name = "Okuma",
                line = list(color = "#e76f51", width = 3)) %>%
      layout(
        title = "Cost per Productive Hour vs Utilization",
        xaxis = list(title = "Utilization Rate (%)"),
        yaxis = list(title = "Cost per Hour ($)", tickformat = "$,.2f"),
        hovermode = "x unified",
        legend = list(orientation = "h", y = 1.1)
      )
  })
  
  # ---------- Drill-down ----------
  output$drill_header <- renderUI({
    tags$div(
      style = "padding:14px; border-radius:10px; background:#f7fafc;",
      tags$h4("Machine Specifications"),
      tags$p(style = "color:#666;", "Full technical comparison of both machines.")
    )
  })
  
  output$drill_table <- renderDT({
    specs <- machines %>%
      select(machine, manufacturer, x_travel, y_travel, z_travel,
             table_size, spindle_rpm, spindle_kw, tool_capacity,
             machine_weight_kg, rapid_rate, chip_to_chip, price_low, price_high) %>%
      pivot_longer(-c(machine, manufacturer), names_to = "spec", values_to = "value") %>%
      pivot_wider(names_from = machine, values_from = value)
    
    specs$spec <- c("X Travel (mm)", "Y Travel (mm)", "Z Travel (mm)",
                    "Table Size", "Spindle Speed (RPM)", "Spindle Power (kW)",
                    "Tool Capacity", "Weight (kg)", "Rapid Rate (m/min)",
                    "Chip-to-Chip (sec)", "Price Low ($)", "Price High ($)")
    
    datatable(specs, rownames = FALSE,
              options = list(dom = "t", pageLength = 20))
  })
}

# --- 5. RUN APP ----------------------------------------------
shinyApp(ui, server)