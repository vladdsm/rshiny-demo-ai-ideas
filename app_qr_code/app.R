# =============================================================
# QR Code Generator - R Shiny App (v3)
# Author: V. Zhbanko
# Purpose: Generate a styled QR code from user text, with
#          adjustable size and an optional embedded logo.
#
# v3 fixes:
#   - Forces error correction level "H" (30%) so logo overlay
#     does not break scannability.
#   - Caps logo size at 25% of QR width with a UI warning.
#   - Increases white halo padding around the logo.
#   - Renders QR on a fixed 1024px canvas for crisp modules.
# =============================================================

# --- 1. LOAD LIBRARIES ---------------------------------------
library(shiny)        # Core Shiny framework
library(bslib)        # Modern Bootstrap themes
library(qrcode)       # QR code generation
library(png)          # Read/write PNG
library(magick)       # Image compositing (logo overlay)
library(base64enc)    # Inline image embedding in UI

# --- 2. HELPER: Generate QR with optional logo ---------------
# Renders the QR to a PNG at the requested size. If a logo is
# provided, it is scaled and composited in the center. A white
# halo is drawn behind the logo to preserve scannability.
generate_qr_png <- function(text,
                            size_px    = 400,
                            logo_path  = NULL,
                            logo_scale = 0.18,
                            output_file = tempfile(fileext = ".png")) {
  
  # -- 2a. Build the base QR with HIGH error correction --
  # ecc = "H" tolerates up to 30% damage — required for logos.
  qr <- qrcode::qr_code(text, ecl = "H")
  tmp_qr <- tempfile(fileext = ".png")
  
  # Render at a large fixed size for crisp module edges
  render_size <- 1024
  png(tmp_qr, width = render_size, height = render_size,
      bg = "white", res = 96)
  par(mar = c(0, 0, 0, 0))
  plot(qr)
  dev.off()
  
  qr_img <- magick::image_read(tmp_qr)
  
  # -- 2b. Overlay the logo if provided --
  if (!is.null(logo_path)) {
    logo <- magick::image_read(logo_path)
    
    # Hard cap: never let the logo exceed 25% of QR width
    effective_scale <- min(logo_scale, 0.25)
    
    logo_w <- as.integer(magick::image_info(qr_img)$width * effective_scale)
    
    # Preserve aspect ratio of the logo
    logo <- magick::image_resize(logo, geometry = paste0(logo_w, "x", logo_w))
    
    # Re-read actual dimensions after resize
    info <- magick::image_info(logo)
    pad  <- as.integer(max(info$width, info$height) * 0.25)
    
    white_box <- magick::image_blank(
      width  = info$width  + 2 * pad,
      height = info$height + 2 * pad,
      color  = "white"
    )
    
    # Composite: QR -> white halo -> logo
    qr_img <- magick::image_composite(qr_img, white_box,
                                      operator = "over",
                                      gravity  = "center")
    qr_img <- magick::image_composite(qr_img, logo,
                                      operator = "over",
                                      gravity  = "center")
  }
  
  # -- 2c. Resize to the user-selected size and save --
  qr_img <- magick::image_resize(
    qr_img,
    geometry = paste0(size_px, "x", size_px)
  )
  magick::image_write(qr_img, path = output_file, format = "png")
  
  output_file
}

# --- 3. USER INTERFACE ---------------------------------------
ui <- page_sidebar(
  theme = bs_theme(
    version    = 5,
    bootswatch = "flatly",
    primary    = "#0d3b66",         # Corporate navy
    base_font  = font_google("Inter")
  ),
  title = tags$div(
    style = "display:flex; align-items:center; gap:10px;",
    tags$span("📱"), tags$span("QR Code Generator")
  ),
  
  # --- Sidebar: input controls ---
  sidebar = sidebar(
    width = 320,
    
    h5("1. Enter your content"),
    textInput("single_text",
              label       = NULL,
              placeholder = "https://example.com"),
    actionButton("generate_single",
                 "Generate QR Code",
                 class = "btn-primary w-100"),
    
    hr(),
    
    h5("2. Customize"),
    sliderInput("qr_size",
                label = "QR image size (px):",
                min   = 100, max = 1200,
                value = 400, step = 50),
    
    fileInput("logo_file",
              label  = "Embed a logo (PNG / JPG, optional):",
              accept = c(".png", ".jpg", ".jpeg")),
    helpText("Logo is centered with a white halo so the QR ",
             "remains scannable. Recommended: ≥ 200×200 px, ",
             "square, transparent background."),
    
    sliderInput("logo_scale",
                label = "Logo size (% of QR):",
                min   = 10, max = 25,
                value = 18, step = 1,
                post  = "%"),
    helpText(style = "color:#b45309;",
             "⚠️ Values above 25% will break scannability even ",
             "with high error correction."),
    
    uiOutput("logo_warning"),
    
    hr(),
    
    conditionalPanel(
      condition = "output.qr_ready == true",
      downloadButton("dl_qr", "Download QR Code",
                     class = "btn-outline-primary w-100")
    )
  ),
  
  # --- Main panel ---
  navset_card_tab(
    nav_panel(
      "Preview",
      br(),
      uiOutput("qr_preview")
    ),
    nav_panel(
      "How to use",
      br(),
      tags$ol(
        tags$li("Type a URL or any text in the sidebar."),
        tags$li("Adjust the slider to change the output size (100–1200 px)."),
        tags$li("Optionally upload a square PNG/JPG logo to embed in the center."),
        tags$li("Control how large the logo appears (10–25% of the QR)."),
        tags$li("Preview the result and download it as a PNG.")
      ),
      tags$p(style = "color:#888;",
             "Tip: Test the QR with your phone before printing. If it fails, ",
             "reduce the logo size.")
    )
  )
)

# --- 4. SERVER LOGIC -----------------------------------------
server <- function(input, output, session) {
  
  # Path to the currently generated QR PNG
  qr_path <- eventReactive(input$generate_single, {
    req(input$single_text)
    validate(need(nchar(trimws(input$single_text)) > 0,
                  "Please enter some text or a URL."))
    
    logo_path <- NULL
    if (!is.null(input$logo_file)) {
      logo_path <- input$logo_file$datapath
    }
    
    generate_qr_png(
      text       = input$single_text,
      size_px    = input$qr_size,
      logo_path  = logo_path,
      logo_scale = input$logo_scale / 100
    )
  })
  
  output$qr_preview <- renderUI({
    if (is.null(qr_path())) {
      return(tags$p(style = "color:#888;",
                    "No QR code generated yet. Enter text in the sidebar."))
    }
    tagList(
      tags$img(
        src   = base64enc::dataURI(file = qr_path(), mime = "image/png"),
        style = paste0("max-width:100%; width:", input$qr_size,
                       "px; border:1px solid #eee; padding:10px; ",
                       "border-radius:8px;")
      ),
      tags$p(style = "margin-top:10px; color:#555;",
             paste0("Encoded: ", input$single_text)),
      tags$p(style = "color:#888; font-size:13px;",
             paste0("Size: ", input$qr_size, " × ", input$qr_size, " px"))
    )
  })
  
  # Live warning when the user nears the safe logo size limit
  output$logo_warning <- renderUI({
    req(input$logo_scale)
    if (input$logo_scale >= 23) {
      tags$p(style = "color:#b91c1c; font-size:13px; font-weight:600;",
             "⚠️ Logo this large may prevent scanning. Test with your phone.")
    } else if (input$logo_scale >= 20) {
      tags$p(style = "color:#b45309; font-size:13px;",
             "Heads-up: large logos reduce reliability on older scanners.")
    } else {
      NULL
    }
  })
  
  # Flag for the conditional download button
  output$qr_ready <- reactive({ !is.null(qr_path()) })
  outputOptions(output, "qr_ready", suspendWhenHidden = FALSE)
  
  output$dl_qr <- downloadHandler(
    filename = function() {
      paste0("qr_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      file.copy(qr_path(), file)
    }
  )
}

# --- 5. RUN APP ----------------------------------------------
shinyApp(ui, server)