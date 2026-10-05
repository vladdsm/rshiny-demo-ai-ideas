# =============================================================
# QR Code Generator - R Shiny App (v4.1)
# Author: V. Zhbanko
# Purpose: Generate a styled QR code from user text, with
#          adjustable size and an optional embedded logo.
#
# v4.1 changes (vs v3 / my earlier v4 attempt):
#   - REMOVED the `magick` dependency entirely (was breaking in
#     Docker due to libMagick++-6.Q16.so version mismatches).
#   - Uses qrcode::add_logo() which composites the logo natively
#     in R with no external system libraries.
#   - FIXED: my earlier v4 wrongly passed a `size=` argument to
#     add_logo(). That argument does not exist. Logo area is now
#     controlled solely by the error-correction gap between
#     qr_code(ecl = "H") and add_logo(ecl = "L").
#   - REMOVED the logo_scale slider, since add_logo() does not
#     expose a size parameter. The logo automatically uses the
#     full safe budget (~23% of QR area) and preserves aspect
#     ratio, shrinking wide/tall logos as needed.
#   - Logo file is copied to a tempfile with a guaranteed .png /
#     .jpg extension before being passed to add_logo(), because
#     Shiny's fileInput()$datapath has no extension and the
#     `qrcode` package dispatches on file extension.
#
# Required packages (all pure-R, no system libraries):
#   shiny, bslib, qrcode, png, base64enc
# =============================================================

# --- 1. LOAD LIBRARIES ---------------------------------------
library(shiny)        # Core Shiny framework
library(bslib)        # Modern Bootstrap themes
library(qrcode)       # QR code generation + add_logo()
library(png)          # PNG read/write (used by qrcode internally)
library(base64enc)    # Inline image embedding in UI

# --- 2. HELPER: Generate QR with optional logo ---------------
# Renders the QR to a PNG at the requested size. If a logo is
# provided, it is composited in the center by qrcode::add_logo().
# No ImageMagick required.
generate_qr_png <- function(text,
                            size_px     = 400,
                            logo_path   = NULL,
                            output_file = tempfile(fileext = ".png")) {
  
  # -- 2a. Build the base QR with HIGH error correction --
  # ecl = "H" tolerates up to 30% damage. Combined with the
  # ecl = "L" passed to add_logo(), this gives the logo the
  # maximum safe budget while keeping the QR scannable.
  qr <- qrcode::qr_code(text, ecl = "H")
  
  # -- 2b. Overlay the logo if provided --
  if (!is.null(logo_path) && nzchar(logo_path)) {
    # Shiny's fileInput()$datapath has no file extension, but
    # qrcode::add_logo() dispatches on the extension. Copy the
    # uploaded file to a tempfile that preserves the extension.
    ext <- tolower(tools::file_ext(logo_path))
    if (!ext %in% c("png", "jpg", "jpeg")) {
      # Fall back to png if we can't tell; add_logo() will error
      # clearly if the content doesn't match.
      ext <- "png"
    }
    logo_tmp <- tempfile(fileext = paste0(".", ext))
    file.copy(logo_path, logo_tmp, overwrite = TRUE)
    
    # NOTE: add_logo() has NO size argument. The logo area is
    # governed entirely by the ecl gap: H in qr_code, L here.
    # Wide/tall logos are automatically shrunk to fit, preserving
    # aspect ratio, so an oversized source image will not break
    # scannability.
    qr <- qrcode::add_logo(
      qr,
      logo = logo_tmp,
      ecl  = "L"
    )
  }
  
  # -- 2c. Render to PNG at the user-selected size --
  png(output_file, width = size_px, height = size_px,
      bg = "white", res = 96)
  par(mar = c(0, 0, 0, 0))
  plot(qr)
  dev.off()
  
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
    tags$span("\U0001F4F1"), tags$span("QR Code Generator")
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
    helpText("The logo is centered automatically and sized to ",
             "the maximum safe area (\u224823% of the QR) so the ",
             "code remains scannable. Recommended: \u2265 200\u00d7200 px, ",
             "square, transparent background."),
    
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
        tags$li("Adjust the slider to change the output size (100\u20131200 px)."),
        tags$li("Optionally upload a square PNG/JPG logo to embed in the center."),
        tags$li("Preview the result and download it as a PNG.")
      ),
      tags$p(style = "color:#888;",
             "Tip: Test the QR with your phone before printing. If it fails, ",
             "try a simpler or higher-contrast logo, or remove it."),
      tags$p(style = "color:#888; font-size:13px;",
             "v4.1 note: this build uses qrcode::add_logo() and has ",
             "no ImageMagick dependency. Logo size is set automatically ",
             "to the maximum safe area.")
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
      text      = input$single_text,
      size_px   = input$qr_size,
      logo_path = logo_path
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
             paste0("Size: ", input$qr_size, " \u00d7 ", input$qr_size, " px"))
    )
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