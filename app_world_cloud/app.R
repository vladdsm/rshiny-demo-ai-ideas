# =============================================================
# Text Insight Explorer - R Shiny App
# Author: V. Zhbanko
# Purpose: Let non-technical users analyze PDF or TXT files by
#          extracting the most common terms, visualizing them in
#          a word cloud, and computing an overall sentiment score.
#
# Stack: shiny + bslib + tidytext + wordcloud2 + pdftools +
#        sentimentr + DT + dplyr + ggplot2
# =============================================================

# --- 1. LOAD LIBRARIES ---------------------------------------
library(shiny)         # Core Shiny framework
library(bslib)         # Modern Bootstrap 5 theming
library(dplyr)         # Data manipulation
library(tidyr)         # Tidying
library(tidytext)      # Text mining (unnest_tokens, stop_words)
library(wordcloud2)    # Interactive word cloud
library(pdftools)      # PDF text extraction
library(sentimentr)    # Sentence-level sentiment scoring
library(DT)            # Interactive tables
library(ggplot2)       # Bar chart for top terms

# --- 2. MOCK DATA GENERATOR ----------------------------------
# Builds a small in-memory text so the app runs without any
# uploaded file — perfect for demos and first-time users.
mock_text <- paste(
  "The quarterly report highlights strong growth in customer satisfaction.",
  "Our team delivered excellent results across all regions.",
  "Revenue increased significantly, and the outlook remains positive.",
  "However, some operational challenges persist in the supply chain.",
  "Management is confident about the upcoming product launch.",
  "Customer feedback was overwhelmingly positive this quarter.",
  "We observed a decline in support tickets and a rise in retention.",
  "The new marketing campaign performed better than expected.",
  "There were minor delays in delivery, but the impact was limited.",
  "Overall, the results reflect a healthy and improving business.",
  sep = " "
)

# --- 3. HELPER FUNCTIONS -------------------------------------

# Extract raw text from an uploaded file (PDF or TXT)
extract_text <- function(path, name) {
  ext <- tolower(tools::file_ext(name))
  if (ext == "pdf") {
    # pdftools::pdf_text returns a character vector, one element per page
    paste(pdftools::pdf_text(path), collapse = "\n")
  } else if (ext %in% c("txt", "csv", "md")) {
    paste(readLines(path, warn = FALSE, encoding = "UTF-8"),
          collapse = "\n")
  } else {
    stop("Unsupported file type. Please upload a PDF or TXT file.")
  }
}

# Compute the top-N most frequent terms (excluding stop words)
top_terms <- function(text, n = 30) {
  tibble(text = text) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words, by = "word") %>%
    filter(!grepl("^[0-9]+$", word)) %>%      # drop pure numbers
    filter(nchar(word) > 2) %>%               # drop tiny words
    count(word, sort = TRUE) %>%
    slice_head(n = n)
}

# --- 4. USER INTERFACE ---------------------------------------
ui <- page_sidebar(
  theme = bs_theme(
    version     = 5,
    bootswatch  = "flatly",
    primary     = "#0d3b66",       # Corporate navy
    base_font   = font_google("Inter")
  ),
  title = tags$div(
    style = "display:flex; align-items:center; gap:10px;",
    tags$span("📊"), tags$span("Text Insight Explorer")
  ),
  
  # ---------- Sidebar: inputs ----------
  sidebar = sidebar(
    width = 320,
    
    h5("1. Upload your document"),
    fileInput("file",
              label  = NULL,
              accept = c(".pdf", ".txt", ".csv", ".md")),
    helpText("Supported: PDF or TXT. Max size: 10 MB."),
    
    actionButton("use_demo", "Use demo text instead",
                 class = "btn-outline-secondary w-100"),
    
    hr(),
    
    h5("2. Analysis settings"),
    sliderInput("top_n",
                "Number of top terms:",
                min = 10, max = 100, value = 30, step = 5),
    
    selectInput("sentiment_unit",
                "Sentiment granularity:",
                choices = c("Sentence" = "sentence",
                            "Paragraph" = "paragraph"),
                selected = "sentence"),
    
    hr(),
    
    conditionalPanel(
      condition = "output.has_data == true",
      downloadButton("dl_terms", "Download term list (CSV)",
                     class = "btn-outline-primary w-100")
    )
  ),
  
  # ---------- Main panel: tabs ----------
  navset_card_tab(
    id = "main_tabs",
    
    # ---- Tab 1: Overview ----
    nav_panel(
      "Overview",
      br(),
      uiOutput("overview_boxes"),
      br(),
      card(
        card_header("What this app does"),
        card_body(
          tags$p("Text Insight Explorer helps you understand any PDF or ",
                 "TXT document at a glance. It answers three questions:"),
          tags$ul(
            tags$li(tags$b("Which terms appear most often?"),
                    " — a ranked list and word cloud."),
            tags$li(tags$b("How is the overall tone?"),
                    " — a sentiment score between -1 (very negative) ",
                    "and +1 (very positive)."),
            tags$li(tags$b("Which specific terms drive the message?"),
                    " — click any term in the table to see how often ",
                    "it appears and in what context.")
          ),
          tags$p(tags$b("How to use:"),
                 "Upload a file (or click “Use demo text”), adjust the ",
                 "sliders on the left, and explore the tabs above.")
        )
      )
    ),
    
    # ---- Tab 2: Word cloud ----
    nav_panel(
      "Word Cloud",
      br(),
      uiOutput("cloud_ui")
    ),
    
    # ---- Tab 3: Top terms ----
    nav_panel(
      "Top Terms",
      br(),
      plotOutput("terms_bar", height = "420px"),
      br(),
      DTOutput("terms_table")
    ),
    
    # ---- Tab 4: Sentiment ----
    nav_panel(
      "Sentiment",
      br(),
      uiOutput("sentiment_ui"),
      br(),
      plotOutput("sentiment_plot", height = "320px")
    ),
    
    # ---- Tab 5: Drill-down ----
    nav_panel(
      "Drill-down",
      br(),
      uiOutput("drill_ui"),
      br(),
      DTOutput("drill_table")
    )
  )
)

# --- 5. SERVER LOGIC -----------------------------------------
server <- function(input, output, session) {
  
  # ---------- Reactive: raw text ----------
  raw_text <- reactiveVal(NULL)
  
  # Load uploaded file
  observeEvent(input$file, {
    req(input$file)
    txt <- tryCatch(
      extract_text(input$file$datapath, input$file$name),
      error = function(e) {
        showNotification(paste("Error:", e$message),
                         type = "error", duration = 6)
        NULL
      }
    )
    raw_text(txt)
  })
  
  # Load demo text
  observeEvent(input$use_demo, {
    raw_text(mock_text)
    showNotification("Demo text loaded.", type = "message", duration = 3)
  })
  
  # Flag so conditional UI can react
  output$has_data <- reactive({ !is.null(raw_text()) && nchar(raw_text()) > 0 })
  outputOptions(output, "has_data", suspendWhenHidden = FALSE)
  
  # ---------- Reactive: top terms ----------
  terms_data <- reactive({
    req(raw_text())
    top_terms(raw_text(), n = input$top_n)
  })
  
  # ---------- Reactive: sentiment ----------
  sentiment_data <- reactive({
    req(raw_text())
    txt <- raw_text()
    
    # sentimentr expects a character vector of sentences/paragraphs
    units <- if (input$sentiment_unit == "paragraph") {
      # split on blank lines or periods followed by space
      unlist(strsplit(txt, "(?<=[.!?])\\s+", perl = TRUE))
    } else {
      unlist(strsplit(txt, "(?<=[.!?])\\s+", perl = TRUE))
    }
    units <- units[nchar(trimws(units)) > 0]
    
    # sentiment_by returns avg sentiment per element
    sent <- sentimentr::sentiment_by(units)
    sent
  })
  
  # ---------- Overview boxes ----------
  output$overview_boxes <- renderUI({
    req(raw_text())
    words  <- sum(terms_data()$n)
    unique_terms <- nrow(terms_data())
    sent_score   <- round(mean(sentiment_data()$ave_sentiment, na.rm = TRUE), 3)
    
    layout_column_wrap(
      width = 1/3,
      value_box(
        title = "Total top-term mentions",
        value = format(words, big.mark = ","),
        showcase = bsicons::bs_icon("bar-chart")
      ),
      value_box(
        title = "Unique top terms",
        value = unique_terms,
        showcase = bsicons::bs_icon("list-ol")
      ),
      value_box(
        title = "Overall sentiment",
        value = sent_score,
        showcase = bsicons::bs_icon(
          if (sent_score > 0.1) "emoji-smile"
          else if (sent_score < -0.1) "emoji-frown"
          else "emoji-neutral"
        ),
        theme = if (sent_score > 0.1) "success"
        else if (sent_score < -0.1) "danger"
        else "secondary"
      )
    )
  })
  
  # ---------- Word cloud ----------
  output$cloud_ui <- renderUI({
    req(terms_data())
    wordcloud2Output("cloud", height = "500px")
  })
  
  output$cloud <- wordcloud2::renderWordcloud2({
    req(terms_data())
    # wordcloud2 expects columns named 'word' and 'freq'
    df <- terms_data() %>% rename(freq = n)
    wordcloud2::wordcloud2(df,
                           size   = 1.2,
                           color  = "random-dark",
                           backgroundColor = "white")
  })
  
  # ---------- Top terms bar chart ----------
  output$terms_bar <- renderPlot({
    req(terms_data())
    terms_data() %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(n, word, fill = n)) +
      geom_col(show.legend = FALSE) +
      scale_fill_gradient(low = "#a8c5e0", high = "#0d3b66") +
      labs(x = "Frequency", y = NULL,
           title = paste("Top", nrow(terms_data()), "terms")) +
      theme_minimal(base_family = "Inter") +
      theme(plot.title = element_text(face = "bold"))
  })
  
  # ---------- Top terms table ----------
  output$terms_table <- renderDT({
    req(terms_data())
    datatable(
      terms_data() %>% rename(Term = word, Frequency = n),
      selection = "single",
      rownames  = FALSE,
      options   = list(pageLength = 10, dom = "tip")
    )
  })
  
  # ---------- Sentiment UI + plot ----------
  output$sentiment_ui <- renderUI({
    req(sentiment_data())
    score <- round(mean(sentiment_data()$ave_sentiment, na.rm = TRUE), 3)
    label <- if (score > 0.1) "Positive"
    else if (score < -0.1) "Negative"
    else "Neutral"
    
    tags$div(
      style = "padding:14px; border-radius:10px; background:#f7fafc;",
      tags$h4(paste0("Overall sentiment: ", score, " (", label, ")")),
      tags$p(style = "color:#666;",
             "Scores range from -1 (very negative) to +1 (very positive). ",
             "Based on ", nrow(sentiment_data()), " text units.")
    )
  })
  
  output$sentiment_plot <- renderPlot({
    req(sentiment_data())
    df <- sentiment_data()
    df$element_id <- seq_len(nrow(df))
    
    ggplot(df, aes(x = element_id, y = ave_sentiment)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "#999") +
      geom_col(aes(fill = ave_sentiment > 0), show.legend = FALSE) +
      scale_fill_manual(values = c(`TRUE` = "#2a9d8f", `FALSE` = "#e76f51")) +
      labs(x = "Text unit", y = "Sentiment score",
           title = "Sentiment by text unit") +
      theme_minimal(base_family = "Inter") +
      theme(plot.title = element_text(face = "bold"))
  })
  
  # ---------- Drill-down ----------
  # When a user clicks a row in the Top Terms table, show every
  # sentence in which that term appears.
  output$drill_ui <- renderUI({
    sel <- input$terms_table_rows_selected
    if (length(sel) == 0) {
      return(tags$p(style = "color:#888;",
                    "Select a term in the 'Top Terms' tab to see ",
                    "every sentence where it appears."))
    }
    term <- terms_data()$word[sel]
    tags$h5(paste0("Context for: “", term, "”"))
  })
  
  output$drill_table <- renderDT({
    sel <- input$terms_table_rows_selected
    req(length(sel) > 0)
    term <- terms_data()$word[sel]
    
    # Split the raw text into sentences
    sentences <- unlist(strsplit(raw_text(), "(?<=[.!?])\\s+", perl = TRUE))
    hits <- sentences[grepl(term, sentences, ignore.case = TRUE)]
    
    if (length(hits) == 0) {
      return(datatable(data.frame(Message = "No context found."),
                       rownames = FALSE, options = list(dom = "t")))
    }
    
    datatable(data.frame(Sentence = hits),
              rownames = FALSE,
              options  = list(pageLength = 10, dom = "tip"))
  })
  
  # ---------- Download term list ----------
  output$dl_terms <- downloadHandler(
    filename = function() paste0("top_terms_", Sys.Date(), ".csv"),
    content  = function(file) {
      write.csv(terms_data() %>% rename(Term = word, Frequency = n),
                file, row.names = FALSE)
    }
  )
}

# --- 6. RUN APP ----------------------------------------------
shinyApp(ui, server)