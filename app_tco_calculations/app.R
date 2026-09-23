# =============================================================
# Industrial Equipment TCO Comparison — R Shiny App (v3)
# Author: [Your Name]
# Purpose: Compare Mazak VCN-530C vs Okuma GENOS M560-V on
#          5-year TCO. Accounts for motor efficiency differences
#          and design-driven maintenance savings.
#
# v3 changes:
#   - Common parameters moved to sidebar
#   - Distinctive parameters (price, efficiency, design) at top
#   - New "energy efficiency" slider per machine
#   - New energy gap visualization
# =============================================================

# --- 1. LOAD LIBRARIES ---------------------------------------
library(shiny)
library(bslib)
library(bsicons)
library(plotly)
library(DT)
library(dplyr)
library(tidyr)
library(scales)

# --- 2. MACHINE SPECIFICATIONS -------------------------------
machines <- tibble::tribble(
  ~machine,        ~manufacturer, ~x_travel, ~y_travel, ~z_travel,
  ~table_size,     ~spindle_rpm,  ~spindle_kw, ~tool_capacity,
  ~weight_kg,      ~rapid_rate,   ~chip_to_chip,
  ~price_low,      ~price_high,   ~design_note,
  "VCN-530C",      "Mazak",       1050,      530,       510,
  "1300 x 550 mm", 12000,         22,          30,
  7500,            42,            1.3,
  123000,          150000,        "C-frame design",
  "GENOS M560-V",  "Okuma",       1050,      560,       460,
  "1300 x 560 mm", 15000,         22,          32,
  9000,            40,            1.8,
  140000,          190000,        "Double-column design"
)

# --- 3. USER INTERFACE ---------------------------------------
ui <- page_fluid(
  theme = bs_theme(
    version    = 5,
    bootswatch = "flatly",
    primary    = "#0d3b66",
    secondary  = "#e76f51",
    base_font  = font_google("Inter")
  ),
  
  # ---------- Header ----------
  div(
    style = paste0("padding:12px 20px; ",
                   "background:linear-gradient(90deg,#0d3b66,#1d5fa8); ",
                   "color:#fff; border-radius:8px; margin-bottom:12px;"),
    h3(style = "margin:0;", "⚙️ Machine TCO Comparison — Mazak vs Okuma")
  ),
  
  # ---------- Layout: Sidebar (common) + Main (distinctive) ----------
  layout_sidebar(
    
    # =========================================================
    # SIDEBAR: Common parameters shared by both machines
    # =========================================================
    sidebar = sidebar(
      width = 300,
      open  = "open",
      
      h5("🔧 Common Parameters", style = "color:#0d3b66;"),
      
      tags$b("Operation"),
      sliderInput("hours_year", "Operating hours / year:",
                  min = 1000, max = 6000, value = 4000, step = 100),
      sliderInput("utilization", "Utilization rate (%):",
                  min = 30, max = 95, value = 70, step = 5, post = "%"),
      
      hr(),
      
      tags$b("Costs"),
      sliderInput("labor_rate", "Operator rate ($/hr):",
                  min = 15, max = 60, value = 28, step = 1, pre = "$"),
      sliderInput("maintenance_pct", "Base maintenance (% of price):",
                  min = 2, max = 8, value = 4, step = 0.5, post = "%"),
      sliderInput("tooling_hr", "Tooling & consumables ($/hr):",
                  min = 1, max = 30, value = 8, step = 1, pre = "$"),
      numericInput("electricity", "Electricity price ($/kWh):",
                   value = 0.12, min = 0.05, max = 0.30, step = 0.01),
      
      hr(),
      
      tags$b("Financial"),
      sliderInput("discount_rate", "Discount rate (%):",
                  min = 0, max = 15, value = 8, step = 0.5, post = "%"),
      sliderInput("residual_pct", "Residual value after 5 yrs (%):",
                  min = 20, max = 60, value = 40, step = 5, post = "%"),
      
      hr(),
      
      downloadButton("dl_report", "Download report (CSV)",
                     class = "btn-primary w-100")
    ),
    
    # =========================================================
    # MAIN PANEL
    # =========================================================
    div(
      
      # ---------- TOP: Distinctive parameters per machine ----------
      card(
        style = "margin-bottom:12px; border-top:4px solid #e76f51;",
        card_header(
          tags$div(
            style = "display:flex; align-items:center; gap:8px;",
            tags$span("🎯"),
            tags$b("Distinctive Parameters"),
            tags$span(style = "color:#888; font-size:12px;",
                      "(unique to each machine)")
          )
        ),
        card_body(
          layout_column_wrap(
            width = 1/2,
            gap  = "16px",
            
            # ---- Mazak card ----
            card(
              style = "border-left:5px solid #0d3b66; background:#f8faff;",
              card_body(
                h5("Mazak VCN-530C", style = "color:#0d3b66; margin-top:0;"),
                sliderInput("mazak_price", "Purchase price ($K):",
                            min = 100, max = 200, value = 130, step = 5,
                            post = "K"),
                sliderInput("mazak_power_kw", "Average power draw (kW):",
                            min = 10, max = 60, value = 35, step = 1,
                            post = " kW"),
                sliderInput("mazak_efficiency", "Motor efficiency (%):",
                            min = 70, max = 100, value = 88, step = 1,
                            post = "%"),
                sliderInput("mazak_maint_disc", "Design maintenance saving (%):",
                            min = 0, max = 40, value = 0, step = 1,
                            post = "%"),
                helpText(style = "font-size:11px; color:#666;",
                         "Design note: ", machines$design_note[1])
              )
            ),
            
            # ---- Okuma card ----
            card(
              style = "border-left:5px solid #e76f51; background:#fff8f5;",
              card_body(
                h5("Okuma GENOS M560-V", style = "color:#e76f51; margin-top:0;"),
                sliderInput("okuma_price", "Purchase price ($K):",
                            min = 120, max = 220, value = 165, step = 5,
                            post = "K"),
                sliderInput("okuma_power_kw", "Average power draw (kW):",
                            min = 10, max = 60, value = 30, step = 1,
                            post = " kW"),
                sliderInput("okuma_efficiency", "Motor efficiency (%):",
                            min = 70, max = 100, value = 94, step = 1,
                            post = "%"),
                sliderInput("okuma_maint_disc", "Design maintenance saving (%):",
                            min = 0, max = 40, value = 20, step = 1,
                            post = "%"),
                helpText(style = "font-size:11px; color:#666;",
                         "Design note: ", machines$design_note[2])
              )
            )
          ),
          
          # Live efficiency comparison strip
          uiOutput("efficiency_strip")
        )
      ),
      
      # ---------- Tabs ----------
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
            card_body(uiOutput("recommendation_text"))
          )
        ),
        
        # ---- Tab 2: TCO Breakdown ----
        nav_panel(
          "TCO Breakdown",
          br(),
          plotlyOutput("tco_stacked", height = "420px"),
          br(),
          DTOutput("tco_table")
        ),
        
        # ---- Tab 3: Sensitivity ----
        nav_panel(
          "Sensitivity",
          br(),
          plotlyOutput("sensitivity_chart", height = "400px"),
          br(),
          layout_column_wrap(
            width = 1/2,
            plotlyOutput("energy_gap", height = "320px"),
            plotlyOutput("maint_gap", height = "320px")
          )
        ),
        
        # ---- Tab 4: Specs & Drill-down ----
        nav_panel(
          "Specs & Drill-down",
          br(),
          uiOutput("drill_header"),
          br(),
          DTOutput("specs_table")
        )
      )
    )
  )
)

# --- 4. SERVER LOGIC -----------------------------------------
server <- function(input, output, session) {
  
  # ---------- Reactive: TCO calculation ----------
  calculate_tco <- reactive({
    
    hours    <- input$hours_year * (input$utilization / 100)
    years    <- 5
    dr       <- input$discount_rate / 100
    disc_sum <- sum(1 / (1 + dr)^(1:years))
    
    compute_machine <- function(name, price, power_kw, efficiency_pct,
                                maint_disc) {
      
      # --- Energy: power draw adjusted by motor efficiency ---
      # Effective consumption = nominal power / (efficiency/100)
      effective_kw <- power_kw / (efficiency_pct / 100)
      energy_annual <- effective_kw * hours * input$electricity
      
      # --- Maintenance: base % reduced by design advantage ---
      eff_maint_pct <- input$maintenance_pct * (1 - maint_disc / 100)
      maint_annual  <- price * (eff_maint_pct / 100)
      
      # --- Other annual costs ---
      tooling_annual  <- input$tooling_hr * hours
      labor_annual    <- input$labor_rate * hours
      downtime_annual <- hours * 0.02 * input$labor_rate * 1.5
      
      # --- Discounted totals ---
      energy_disc  <- energy_annual  * disc_sum
      maint_disc_v <- maint_annual   * disc_sum
      tooling_disc <- tooling_annual * disc_sum
      labor_disc   <- labor_annual   * disc_sum
      down_disc    <- downtime_annual * disc_sum
      
      residual <- price * (input$residual_pct / 100) / (1 + dr)^years
      
      tco <- price + energy_disc + maint_disc_v + tooling_disc +
        labor_disc + down_disc - residual
      
      tibble(
        machine        = name,
        purchase       = price,
        energy         = energy_disc,
        maintenance    = maint_disc_v,
        tooling        = tooling_disc,
        labor          = labor_disc,
        downtime       = down_disc,
        residual       = -residual,
        total_tco      = tco,
        annual_hours   = hours,
        cost_per_hr    = tco / (hours * years),
        effective_kw   = effective_kw,
        energy_annual  = energy_annual
      )
    }
    
    bind_rows(
      compute_machine("Mazak VCN-530C",
                      input$mazak_price * 1000,
                      input$mazak_power_kw,
                      input$mazak_efficiency,
                      input$mazak_maint_disc),
      compute_machine("Okuma GENOS M560-V",
                      input$okuma_price * 1000,
                      input$okuma_power_kw,
                      input$okuma_efficiency,
                      input$okuma_maint_disc)
    )
  })
  
  # ---------- Efficiency strip (live feedback under sliders) ----------
  output$efficiency_strip <- renderUI({
    df <- calculate_tco()
    mazak_kw  <- df$effective_kw[1]
    okuma_kw  <- df$effective_kw[2]
    delta_kw  <- mazak_kw - okuma_kw
    delta_pct <- (delta_kw / mazak_kw) * 100
    
    winner <- if (delta_kw > 0) "Okuma" else "Mazak"
    
    div(
      style = paste0("margin-top:10px; padding:10px 14px; ",
                     "border-radius:8px; background:#eef4fb; ",
                     "border-left:4px solid #1d5fa8;"),
      tags$b("Live efficiency comparison: "),
      paste0("Mazak ", round(mazak_kw, 1), " kW vs Okuma ",
             round(okuma_kw, 1), " kW effective — "),
      tags$b(style = "color:#0d3b66;",
             paste0(winner, " consumes ", round(abs(delta_pct), 1),
                    "% less power."))
    )
  })
  
  # ---------- Summary boxes ----------
  output$summary_boxes <- renderUI({
    df <- calculate_tco()
    layout_column_wrap(
      width = 1/3,
      value_box(
        title = "Mazak 5-Year TCO",
        value = paste0("$", format(round(df$total_tco[1]/1000), big.mark = ","), "K"),
        showcase = bs_icon("gear"),
        theme = "primary"
      ),
      value_box(
        title = "Okuma 5-Year TCO",
        value = paste0("$", format(round(df$total_tco[2]/1000), big.mark = ","), "K"),
        showcase = bs_icon("gear-wide-connected"),
        theme = "secondary"
      ),
      value_box(
        title = "Cost per Productive Hour",
        value = paste0("$", round(df$cost_per_hr[1], 2),
                       " vs $", round(df$cost_per_hr[2], 2)),
        showcase = bs_icon("clock-history"),
        theme = if (df$cost_per_hr[1] < df$cost_per_hr[2]) "success" else "warning"
      )
    )
  })
  
  # ---------- Recommendation ----------
  output$recommendation_text <- renderUI({
    df <- calculate_tco()
    cheaper <- if (df$total_tco[1] < df$total_tco[2]) "Mazak VCN-530C" else "Okuma GENOS M560-V"
    diff    <- abs(df$total_tco[1] - df$total_tco[2])
    energy_saving <- abs(df$energy[1] - df$energy[2])
    
    tags$div(
      tags$h4(paste0("📌 ", cheaper, " delivers the lower 5-year TCO.")),
      tags$p(paste0("Estimated advantage: $", format(round(diff), big.mark = ","),
                    " over 5 years ($", format(round(diff/5), big.mark = ","),
                    " per year).")),
      tags$p(paste0("Energy cost difference alone: $",
                    format(round(energy_saving), big.mark = ","),
                    " over 5 years — driven by motor efficiency and power draw.")),
      tags$p(style = "color:#666;",
             "Adjust power draw and efficiency sliders at the top to ",
             "reflect real-world motor performance.")
    )
  })
  
  # ---------- TCO stacked bar ----------
  output$tco_stacked <- renderPlotly({
    df <- calculate_tco() %>%
      select(machine, purchase, energy, maintenance, tooling,
             labor, downtime, residual) %>%
      pivot_longer(-machine, names_to = "component", values_to = "cost")
    
    df$component <- factor(df$component,
                           levels = c("purchase", "energy", "maintenance", "tooling",
                                      "labor", "downtime", "residual"),
                           labels = c("Purchase", "Energy", "Maintenance", "Tooling",
                                      "Labor", "Downtime", "Residual (-)")
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
        title   = "5-Year TCO Components",
        yaxis   = list(title = "USD", tickformat = "$,.0f"),
        xaxis   = list(title = ""),
        legend  = list(orientation = "h", y = -0.2),
        margin  = list(b = 100)
      )
  })
  
  # ---------- TCO table ----------
  output$tco_table <- renderDT({
    df <- calculate_tco() %>%
      transmute(
        Machine        = machine,
        Purchase       = dollar(purchase),
        Energy         = dollar(energy),
        Maintenance    = dollar(maintenance),
        Tooling        = dollar(tooling),
        Labor          = dollar(labor),
        Downtime       = dollar(downtime),
        `Residual (-)` = dollar(residual),
        `Total TCO`    = dollar(total_tco),
        `Cost / Hour`  = dollar(cost_per_hr)
      )
    
    datatable(df, rownames = FALSE,
              options = list(pageLength = 5, dom = "t"))
  })
  
  # ---------- Sensitivity chart ----------
  output$sensitivity_chart <- renderPlotly({
    util_seq <- seq(30, 95, by = 5)
    
    sens <- lapply(util_seq, function(u) {
      hours <- input$hours_year * (u / 100)
      dr    <- input$discount_rate / 100
      disc_sum <- sum(1 / (1 + dr)^(1:5))
      
      calc_one <- function(price, power_kw, efficiency, maint_disc) {
        eff_kw   <- power_kw / (efficiency / 100)
        energy   <- eff_kw * hours * input$electricity * disc_sum
        eff_pct  <- input$maintenance_pct * (1 - maint_disc / 100)
        maint    <- price * (eff_pct / 100) * disc_sum
        tooling  <- input$tooling_hr * hours * disc_sum
        labor    <- input$labor_rate * hours * disc_sum
        downtime <- hours * 0.02 * input$labor_rate * 1.5 * disc_sum
        residual <- price * (input$residual_pct / 100) / (1 + dr)^5
        (price + energy + maint + tooling + labor + downtime - residual) /
          (hours * 5)
      }
      
      tibble(
        utilization = u,
        Mazak = calc_one(input$mazak_price * 1000, input$mazak_power_kw,
                         input$mazak_efficiency, input$mazak_maint_disc),
        Okuma = calc_one(input$okuma_price * 1000, input$okuma_power_kw,
                         input$okuma_efficiency, input$okuma_maint_disc)
      )
    }) %>% bind_rows()
    
    plot_ly(sens, x = ~utilization, y = ~Mazak, name = "Mazak",
            type = "scatter", mode = "lines+markers",
            line = list(color = "#0d3b66", width = 3)) %>%
      add_trace(y = ~Okuma, name = "Okuma",
                line = list(color = "#e76f51", width = 3)) %>%
      layout(
        title   = "Cost per Productive Hour vs Utilization",
        xaxis   = list(title = "Utilization Rate (%)"),
        yaxis   = list(title = "Cost per Hour ($)", tickformat = "$,.2f"),
        hovermode = "x unified",
        legend  = list(orientation = "h", y = 1.1)
      )
  })
  
  # ---------- Energy gap ----------
  output$energy_gap <- renderPlotly({
    util_seq <- seq(30, 95, by = 5)
    dr <- input$discount_rate / 100
    disc_sum <- sum(1 / (1 + dr)^(1:5))
    
    gap <- lapply(util_seq, function(u) {
      hours <- input$hours_year * (u / 100)
      mazak_kw <- input$mazak_power_kw / (input$mazak_efficiency / 100)
      okuma_kw <- input$okuma_power_kw / (input$okuma_efficiency / 100)
      tibble(
        utilization = u,
        Mazak = mazak_kw * hours * input$electricity * disc_sum,
        Okuma = okuma_kw * hours * input$electricity * disc_sum,
        Gap   = Mazak - Okuma
      )
    }) %>% bind_rows()
    
    plot_ly(gap, x = ~utilization, y = ~Gap, type = "bar",
            marker = list(color = ifelse(gap$Gap > 0, "#e76f51", "#0d3b66")),
            hovertemplate = "Gap: $%{y:,.0f}<extra></extra>") %>%
      layout(
        title = "5-Year Energy Cost Gap (Mazak - Okuma)",
        xaxis = list(title = "Utilization (%)"),
        yaxis = list(title = "Energy Gap ($)", tickformat = "$,.0f")
      )
  })
  
  # ---------- Maintenance gap ----------
  output$maint_gap <- renderPlotly({
    df <- calculate_tco()
    gap <- df$maintenance[1] - df$maintenance[2]
    
    plot_ly(
      x = c("Mazak", "Okuma"),
      y = c(df$maintenance[1], df$maintenance[2]),
      type = "bar",
      marker = list(color = c("#0d3b66", "#e76f51")),
      hovertemplate = "%{x}: $%{y:,.0f}<extra></extra>"
    ) %>%
      layout(
        title = paste0("5-Year Maintenance (gap: $",
                       format(round(gap), big.mark = ","), ")"),
        xaxis = list(title = ""),
        yaxis = list(title = "USD", tickformat = "$,.0f")
      )
  })
  
  # ---------- Drill-down: specs table ----------
  output$drill_header <- renderUI({
    tags$div(
      style = "padding:14px; border-radius:10px; background:#f7fafc;",
      tags$h4("Machine Specifications"),
      tags$p(style = "color:#666; margin:0;",
             "Full technical comparison of both machines.")
    )
  })
  
  output$specs_table <- renderDT({
    spec_rows <- tibble(
      Specification = c(
        "Manufacturer", "Design", "X Travel (mm)", "Y Travel (mm)",
        "Z Travel (mm)", "Table Size", "Spindle Speed (RPM)",
        "Spindle Power (kW)", "Tool Capacity", "Weight (kg)",
        "Rapid Rate (m/min)", "Chip-to-Chip (sec)",
        "Price Low ($)", "Price High ($)"
      ),
      Mazak = c(
        machines$manufacturer[1], machines$design_note[1],
        as.character(machines$x_travel[1]),
        as.character(machines$y_travel[1]),
        as.character(machines$z_travel[1]),
        machines$table_size[1],
        format(machines$spindle_rpm[1], big.mark = ","),
        as.character(machines$spindle_kw[1]),
        as.character(machines$tool_capacity[1]),
        format(machines$weight_kg[1], big.mark = ","),
        as.character(machines$rapid_rate[1]),
        as.character(machines$chip_to_chip[1]),
        dollar(machines$price_low[1]),
        dollar(machines$price_high[1])
      ),
      Okuma = c(
        machines$manufacturer[2], machines$design_note[2],
        as.character(machines$x_travel[2]),
        as.character(machines$y_travel[2]),
        as.character(machines$z_travel[2]),
        machines$table_size[2],
        format(machines$spindle_rpm[2], big.mark = ","),
        as.character(machines$spindle_kw[2]),
        as.character(machines$tool_capacity[2]),
        format(machines$weight_kg[2], big.mark = ","),
        as.character(machines$rapid_rate[2]),
        as.character(machines$chip_to_chip[2]),
        dollar(machines$price_low[2]),
        dollar(machines$price_high[2])
      )
    )
    
    datatable(
      spec_rows,
      rownames = FALSE,
      options  = list(dom = "t", pageLength = 20, ordering = FALSE),
      class    = "stripe hover"
    )
  })
  
  # ---------- Download report ----------
  output$dl_report <- downloadHandler(
    filename = function() paste0("tco_report_", Sys.Date(), ".csv"),
    content  = function(file) {
      df <- calculate_tco() %>%
        select(machine, purchase, energy, maintenance, tooling, labor,
               downtime, residual, total_tco, cost_per_hr,
               effective_kw, energy_annual) %>%
        mutate(
          hours_per_year    = input$hours_year,
          utilization_pct   = input$utilization,
          electricity_price = input$electricity,
          discount_rate     = input$discount_rate,
          residual_pct      = input$residual_pct
        )
      write.csv(df, file, row.names = FALSE)
    }
  )
}

# --- 5. RUN APP ----------------------------------------------
shinyApp(ui, server)