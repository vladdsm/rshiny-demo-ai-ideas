# ============================================================
# Homomorphic Encryption Demo: Privacy-Preserving Health Data
# Paillier Additive Homomorphism in R Shiny
# ============================================================

library(shiny)
library(homomorpheR)
library(gmp)
library(ggplot2)

# ------------------------------------------------------------
# 1. SAMPLE DATA (integer values required for Paillier)
# ------------------------------------------------------------
patients <- data.frame(
  id = 1:5,
  age = c(34L, 67L, 52L, 45L, 71L),
  systolic_bp = c(120L, 145L, 132L, 128L, 150L),
  smoker = c("No", "Yes", "No", "No", "Yes"),
  diabetes = c("No", "Yes", "No", "No", "No"),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# 2. KEY GENERATION (once per session, outside reactive context)
# ------------------------------------------------------------
cat("Generating Paillier keypair (1024-bit)... this may take a moment.\n")
keyPair <- PaillierKeyPair$new(modulusBits = 1024)
pubkey <- keyPair$pubkey
privkey <- keyPair$getPrivateKey()
cat("Keypair ready.\n")

# Helper: encrypt a vector of integers
encrypt_vec <- function(x) {
  lapply(as.integer(x), function(v) pubkey$encrypt(as.bigz(v)))
}

# Helper: homomorphically sum a list of ciphertexts
hom_sum <- function(ct_list) {
  Reduce(function(a, b) pubkey$add(a, b), ct_list)
}

# Helper: truncate ciphertext for display
trunc_ct <- function(ct, n = 20) {
  s <- as.character(ct)
  if (nchar(s) > n) paste0(substr(s, 1, n), "...") else s
}

# ------------------------------------------------------------
# 3. UI
# ------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Homomorphic Encryption Demo: Compute Without Seeing"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      radioButtons("role", "Switch Role:",
                   choices = c("Data Owner (Patient)" = "owner",
                               "Analyst (Server)" = "analyst"),
                   selected = "owner"),
      
      hr(),
      
      conditionalPanel(
        condition = "input.role == 'owner'",
        h4("Owner Controls"),
        p("You hold the private key. Only you can decrypt results."),
        actionButton("btn_generate", "1. Generate Keys", class = "btn-primary"),
        br(), br(),
        actionButton("btn_encrypt", "2. Encrypt My Data", class = "btn-warning"),
        br(), br(),
        actionButton("btn_send", "3. Send Ciphertext to Analyst", class = "btn-info"),
        br(), br(),
        actionButton("btn_decrypt_result", "5. Decrypt Aggregate Result", class = "btn-success")
      ),
      
      conditionalPanel(
        condition = "input.role == 'analyst'",
        h4("Analyst Controls"),
        p("You have only the public key and ciphertexts. You never see raw data."),
        actionButton("btn_compute", "4. Compute Encrypted Sum", class = "btn-danger"),
        br(), br(),
        actionButton("btn_return", "Return Encrypted Result to Owner", class = "btn-info")
      ),
      
      hr(),
      helpText("Paillier is additively homomorphic: E(x) + E(y) = E(x + y).",
               "All values are integers. Categorical variables are shown as metadata only.")
    ),
    
    mainPanel(
      width = 9,
      
      tabsetPanel(
        id = "tabs",
        
        # ---------- TAB 1: Raw Data (Owner Only) ----------
        tabPanel("Raw Data (Owner's Eyes Only)",
                 br(),
                 h3("Sensitive Patient Data"),
                 p("The Data Owner has access to this full dataset. The Analyst must never see it."),
                 tableOutput("raw_table"),
                 hr(),
                 h4("Selected Patient for Encryption"),
                 p("For demonstration, we encrypt the entire dataset's numerical columns."),
                 verbatimTextOutput("owner_note")
        ),
        
        # ---------- TAB 2: Encryption ----------
        tabPanel("Encryption & Ciphertexts",
                 br(),
                 h3("What the Analyst Receives"),
                 p("After encryption, only these ciphertexts leave the Owner's system."),
                 tableOutput("cipher_table"),
                 hr(),
                 h4("Homomorphic Addition in Action"),
                 p("When the Analyst adds ciphertexts, they are computing the encrypted sum:"),
                 verbatimTextOutput("hom_add_note")
        ),
        
        # ---------- TAB 3: Analyst View ----------
        tabPanel("Analyst's View (Ciphertext Only)",
                 br(),
                 h3("The Analyst Sees Only Ciphertexts"),
                 p("No raw values are visible. The Analyst can still perform computation."),
                 tableOutput("analyst_cipher_table"),
                 hr(),
                 h3("Encrypted Sum Result"),
                 verbatimTextOutput("encrypted_sum_output"),
                 br(),
                 p(em("This encrypted sum is sent back to the Owner for decryption."))
        ),
        
        # ---------- TAB 4: Decrypted Result & Proof ----------
        tabPanel("Proof: Results Match",
                 br(),
                 h3("The Owner Decrypts the Aggregate"),
                 p("The Owner uses their private key to decrypt the sum computed by the Analyst."),
                 tableOutput("decrypted_result_table"),
                 hr(),
                 h3("Verification: Homomorphic vs. Raw Computation"),
                 p("The decrypted homomorphic results exactly match the raw-data computation,",
                   "proving correctness without the Analyst ever seeing individual values."),
                 tableOutput("verification_table"),
                 plotOutput("comparison_plot", height = "300px")
        ),
        
        # ---------- TAB 5: Explanation ----------
        tabPanel("How It Works",
                 br(),
                 h3("The Principle"),
                 p("Paillier encryption is additively homomorphic:"),
                 withMathJax(helpText("$$E(x) + E(y) = E(x + y)$$")),
                 p("This means the Analyst can add encrypted values together, and the result,",
                   "when decrypted, equals the sum of the original values."),
                 hr(),
                 h4("Role Separation"),
                 tags$ul(
                   tags$li(strong("Data Owner:"), "Holds the private key. Encrypts data. Decrypts final results."),
                   tags$li(strong("Analyst:"), "Holds only the public key. Computes on ciphertexts. Never sees raw values.")
                 ),
                 hr(),
                 h4("Limitations (Honest Disclosure)"),
                 tags$ul(
                   tags$li("Paillier supports addition only (not multiplication of two ciphertexts)."),
                   tags$li("Integer values only. Floating-point requires scaling with precision trade-offs."),
                   tags$li("Categorical variables cannot be directly processed with additive homomorphism."),
                   tags$li("This is a proof-of-concept, not production-grade performance.")
                 )
        )
      )
    )
  )
)

# ------------------------------------------------------------
# 4. SERVER
# ------------------------------------------------------------
server <- function(input, output, session) {
  
  # Reactive state
  keys_ready      <- reactiveVal(FALSE)
  encrypted_data  <- reactiveVal(NULL)   # list of ciphertext lists
  sent_to_analyst <- reactiveVal(FALSE)
  encrypted_sum   <- reactiveVal(NULL)   # list of two ciphertexts
  result_ready    <- reactiveVal(FALSE)
  
  # ---------- Owner: Generate Keys ----------
  observeEvent(input$btn_generate, {
    keys_ready(TRUE)
    showNotification("Keys generated. Public key ready to share.", type = "message")
  })
  
  # ---------- Owner: Encrypt Data ----------
  observeEvent(input$btn_encrypt, {
    req(keys_ready())
    
    enc_age <- encrypt_vec(patients$age)
    enc_bp  <- encrypt_vec(patients$systolic_bp)
    
    encrypted_data(list(age = enc_age, bp = enc_bp))
    showNotification("Data encrypted. Ciphertexts ready.", type = "message")
  })
  
  # ---------- Owner: Send to Analyst ----------
  observeEvent(input$btn_send, {
    req(encrypted_data())
    sent_to_analyst(TRUE)
    showNotification("Ciphertexts sent to Analyst. Switch role to Analyst view.",
                     type = "warning", duration = 5)
  })
  
  # ---------- Analyst: Compute Encrypted Sum ----------
  observeEvent(input$btn_compute, {
    req(sent_to_analyst())
    ed <- encrypted_data()
    enc_sum_age <- hom_sum(ed$age)
    enc_sum_bp  <- hom_sum(ed$bp)
    encrypted_sum(list(age = enc_sum_age, bp = enc_sum_bp))
    showNotification("Encrypted sums computed using homomorphic addition.", type = "message")
  })
  
  # ---------- Analyst: Return Result ----------
  observeEvent(input$btn_return, {
    req(encrypted_sum())
    result_ready(TRUE)
    showNotification("Encrypted result returned to Owner. Switch role to decrypt.",
                     type = "warning", duration = 5)
  })
  
  # ---------- Owner: Decrypt Result ----------
  observeEvent(input$btn_decrypt_result, {
    req(result_ready())
    es <- encrypted_sum()
    dec_age <- as.integer(privkey$decrypt(es$age))
    dec_bp  <- as.integer(privkey$decrypt(es$bp))
    
    n <- nrow(patients)
    raw_sum_age <- sum(patients$age)
    raw_sum_bp  <- sum(patients$systolic_bp)
    
    output$decrypted_result_table <- renderTable({
      data.frame(
        Variable = c("Age (sum)", "Age (mean)", "Systolic BP (sum)", "Systolic BP (mean)"),
        Decrypted_Value = c(
          dec_age,
          round(dec_age / n, 2),
          dec_bp,
          round(dec_bp / n, 2)
        )
      )
    })
    
    output$verification_table <- renderTable({
      data.frame(
        Variable = c("Age sum", "BP sum"),
        Raw_Computation = c(raw_sum_age, raw_sum_bp),
        Homomorphic_Decrypted = c(dec_age, dec_bp),
        Match = c(raw_sum_age == dec_age, raw_sum_bp == dec_bp)
      )
    })
    
    output$comparison_plot <- renderPlot({
      df <- data.frame(
        Variable = rep(c("Age", "Systolic BP"), each = 2),
        Method = rep(c("Raw Data", "Homomorphic"), 2),
        Sum = c(raw_sum_age, dec_age, raw_sum_bp, dec_bp)
      )
      ggplot(df, aes(x = Variable, y = Sum, fill = Method)) +
        geom_bar(stat = "identity", position = "dodge", width = 0.6) +
        labs(title = "Sums: Raw Computation vs. Homomorphic Decryption",
             y = "Sum", x = NULL) +
        theme_minimal(base_size = 14) +
        scale_fill_manual(values = c("Raw Data" = "#2c3e50", "Homomorphic" = "#e74c3c"))
    })
    
    showNotification("Result decrypted. Check the Proof tab.", type = "message")
  })
  
  # ---------- Outputs: Raw Data Table ----------
  output$raw_table <- renderTable({
    patients
  })
  
  output$owner_note <- renderText({
    if (!keys_ready()) {
      "Step 1: Click 'Generate Keys' to create your Paillier keypair."
    } else if (is.null(encrypted_data())) {
      "Keys ready. Step 2: Click 'Encrypt My Data'."
    } else {
      "Data encrypted. Step 3: Click 'Send Ciphertext to Analyst'."
    }
  })
  
  # ---------- Outputs: Ciphertext Table ----------
  output$cipher_table <- renderTable({
    req(encrypted_data())
    ed <- encrypted_data()
    data.frame(
      Patient = patients$id,
      Age_Ciphertext = sapply(ed$age, trunc_ct),
      BP_Ciphertext = sapply(ed$bp, trunc_ct),
      Smoker = patients$smoker,
      Diabetes = patients$diabetes
    )
  })
  
  output$hom_add_note <- renderText({
    "E(age_1) + E(age_2) + ... + E(age_5) = E(age_1 + age_2 + ... + age_5)\nThe Analyst performs these additions without decrypting anything."
  })
  
  # ---------- Outputs: Analyst View ----------
  output$analyst_cipher_table <- renderTable({
    req(sent_to_analyst())
    ed <- encrypted_data()
    data.frame(
      Patient = patients$id,
      Age_Ciphertext = sapply(ed$age, trunc_ct),
      BP_Ciphertext = sapply(ed$bp, trunc_ct)
    )
  })
  
  output$encrypted_sum_output <- renderText({
    req(encrypted_sum())
    es <- encrypted_sum()
    paste0(
      "Encrypted Sum of Ages:  ", trunc_ct(es$age, 40), "\n",
      "Encrypted Sum of BP:    ", trunc_ct(es$bp, 40), "\n\n",
      "These are still ciphertexts. The Analyst cannot read the sums."
    )
  })
}

# ------------------------------------------------------------
# 5. RUN APP
# ------------------------------------------------------------
shinyApp(ui = ui, server = server)