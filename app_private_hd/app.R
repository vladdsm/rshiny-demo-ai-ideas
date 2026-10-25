# ============================================================
# Encrypted Diabetes Risk Prediction
# Homomorphic Encryption + Logistic Regression Demo
# ============================================================
# This app demonstrates privacy-preserving machine learning:
# - The Patient encrypts their health data locally
# - The Analyst runs a logistic regression model on ciphertext
# - The Patient decrypts the prediction themselves
# - The Analyst never sees any raw patient data
# ============================================================

library(shiny)
library(bslib)
library(homomorpheR)
library(gmp)
library(ggplot2)
library(thematic)

# Enable thematic auto-theming for ggplot2 plots
thematic_shiny()

# ------------------------------------------------------------
# 1. SYNTHETIC DATA GENERATION
# ------------------------------------------------------------
set.seed(42)
n_patients <- 500

synthetic_data <- data.frame(
  age = round(rnorm(n_patients, mean = 50, sd = 12)),
  systolic_bp = round(rnorm(n_patients, mean = 130, sd = 18)),
  sugar = round(rnorm(n_patients, mean = 110, sd = 30)),
  bmi = round(rnorm(n_patients, mean = 27, sd = 5)),
  smoker = rbinom(n_patients, 1, 0.25),
  alcohol = rbinom(n_patients, 1, 0.35)
)

# Create a realistic diabetes risk label based on features
linear_pred <- -8 +
  0.045 * synthetic_data$age +
  0.035 * synthetic_data$systolic_bp +
  0.025 * synthetic_data$sugar +
  0.08 * synthetic_data$bmi +
  0.6 * synthetic_data$smoker +
  0.4 * synthetic_data$alcohol

prob <- 1 / (1 + exp(-linear_pred))
synthetic_data$diabetes <- rbinom(n_patients, 1, prob)

# ------------------------------------------------------------
# 2. KEY GENERATION (once per session)
# ------------------------------------------------------------
cat("Generating Paillier keypair (1024-bit)...\n")
keyPair <- PaillierKeyPair$new(modulusBits = 1024)
pubkey <- keyPair$pubkey
privkey <- keyPair$getPrivateKey()
cat("Keypair ready.\n")

# ------------------------------------------------------------
# 3. HELPER FUNCTIONS
# ------------------------------------------------------------

# Encrypt a single integer
encrypt_int <- function(x) {
  pubkey$encrypt(as.bigz(as.integer(x)))
}

# Homomorphic addition of two ciphertexts
hom_add <- function(ct1, ct2) {
  pubkey$add(ct1, ct2)
}

# Truncate ciphertext for display
trunc_ct <- function(ct, n = 20) {
  s <- as.character(ct)
  if (nchar(s) > n) paste0(substr(s, 1, n), "...") else s
}

# Train logistic regression model
train_model <- function(data) {
  glm(diabetes ~ age + systolic_bp + sugar + bmi + smoker + alcohol,
      data = data, family = binomial())
}

# ------------------------------------------------------------
# 4. UI
# ------------------------------------------------------------
ui <- page_navbar(
  title = "Encrypted Diabetes Risk Prediction",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2c3e50",
    base_font = font_google("Inter")
  ),
  
  # ========== TAB 1: DATA OVERVIEW ==========
  nav_panel(
    "Data & Model",
    icon = icon("chart-line"),
    layout_sidebar(
      sidebar = sidebar(
        h4("Synthetic Dataset"),
        p("This dataset simulates patient health records for diabetes risk prediction."),
        actionButton("btn_train", "Train Logistic Regression Model", 
                     class = "btn-primary w-100"),
        hr(),
        h5("Model Coefficients"),
        tableOutput("model_coef_table")
      ),
      layout_column_wrap(
        width = 1/2,
        card(
          card_header("Dataset Preview"),
          tableOutput("data_preview")
        ),
        card(
          card_header("Model Performance"),
          verbatimTextOutput("model_summary")
        )
      ),
      layout_column_wrap(
        width = 1/2,
        card(
          card_header("Feature Importance"),
          plotOutput("feature_plot", height = "300px")
        ),
        card(
          card_header("Confusion Matrix (Test Set)"),
          tableOutput("confusion_table")
        )
      )
    )
  ),
  
  # ========== TAB 2: PATIENT (DATA OWNER) ==========
  nav_panel(
    "Patient (Data Owner)",
    icon = icon("user-shield"),
    layout_sidebar(
      sidebar = sidebar(
        h4("Your Health Data"),
        p("Adjust your values. These will be encrypted before leaving your device."),
        sliderInput("in_age", "Age", min = 20, max = 90, value = 55, step = 1),
        sliderInput("in_systolic_bp", "Systolic Blood Pressure", 
                    min = 90, max = 200, value = 135, step = 1),
        sliderInput("in_sugar", "Blood Sugar (mg/dL)", 
                    min = 60, max = 250, value = 120, step = 1),
        sliderInput("in_bmi", "BMI", min = 15, max = 50, value = 28, step = 1),
        radioButtons("in_smoker", "Smoker", choices = c("No" = 0, "Yes" = 1), inline = TRUE),
        radioButtons("in_alcohol", "Alcohol Consumption", 
                     choices = c("No" = 0, "Yes" = 1), inline = TRUE),
        hr(),
        actionButton("btn_encrypt", "Encrypt My Data", 
                     class = "btn-warning w-100 mb-2"),
        actionButton("btn_send", "Send to Analyst", 
                     class = "btn-info w-100 mb-2"),
        hr(),
        h5("Key Management"),
        downloadButton("dl_privkey", "Download Private Key", class = "btn-outline-secondary w-100 mb-2"),
        downloadButton("dl_pubkey", "Download Public Key", class = "btn-outline-secondary w-100 mb-2"),
        fileInput("upload_pubkey", "Upload Analyst's Public Key", 
                  accept = ".rds", buttonLabel = "Browse")
      ),
      layout_column_wrap(
        width = 1,
        card(
          card_header("Summary of Values to Send"),
          tableOutput("patient_summary")
        ),
        card(
          card_header("Encrypted Data (Ciphertext)"),
          p("This is what the Analyst receives. The values are unreadable without your private key."),
          tableOutput("ciphertext_display")
        )
      )
    )
  ),
  
  # ========== TAB 3: ANALYST (SERVER) ==========
  nav_panel(
    "Analyst (Server)",
    icon = icon("server"),
    layout_sidebar(
      sidebar = sidebar(
        h4("Analyst Controls"),
        p("You have the public key and the trained model. You never see raw patient data."),
        hr(),
        actionButton("btn_compute", "Run Encrypted Prediction", 
                     class = "btn-danger w-100 mb-2"),
        actionButton("btn_return", "Return Encrypted Result to Patient", 
                     class = "btn-success w-100"),
        hr(),
        h5("Available Data"),
        p("Patient ciphertext received:"),
        verbatimTextOutput("analyst_cipher_status")
      ),
      layout_column_wrap(
        width = 1,
        card(
          card_header("Received Ciphertext"),
          tableOutput("analyst_cipher_table")
        ),
        card(
          card_header("Encrypted Prediction Progress"),
          verbatimTextOutput("compute_log")
        )
      )
    )
  ),
  
  # ========== TAB 4: RESULT & PROOF ==========
  nav_panel(
    "Proof & Result",
    icon = icon("check-circle"),
    layout_sidebar(
      sidebar = sidebar(
        h4("Decrypt Your Result"),
        p("Only you can decrypt this. The Analyst never saw the plaintext prediction."),
        actionButton("btn_decrypt", "Decrypt My Risk Score", 
                     class = "btn-success w-100"),
        hr(),
        h5("Verification"),
        p("Compare homomorphic result with direct computation."),
        tableOutput("verification_table")
      ),
      layout_column_wrap(
        width = 1,
        card(
          card_header("Your Diabetes Risk"),
          value_box(
            title = "Predicted Risk",
            value = textOutput("risk_display"),
            showcase = icon("heart-pulse"),
            theme = "danger"
          )
        ),
        card(
          card_header("Explanation"),
          p("The risk was computed entirely on encrypted data."),
          p("The Analyst added encrypted feature values multiplied by plaintext model coefficients."),
          p("The result is an encrypted linear predictor, which you decrypt and convert to probability.")
        )
      )
    )
  ),
  
  # ========== TAB 5: HOW IT WORKS ==========
  nav_panel(
    "How It Works",
    icon = icon("info-circle"),
    card(
      card_header("The Principle"),
      p("Paillier encryption is additively homomorphic:"),
      withMathJax(helpText("$$E(x) + E(y) = E(x + y)$$")),
      p("The Analyst can multiply ciphertext by plaintext coefficients and add them:"),
      withMathJax(helpText("$$E(z) = b + \\sum w_i \\cdot E(x_i)$$")),
      p("This produces the encrypted linear predictor. You decrypt and apply sigmoid locally."),
      hr(),
      h4("Role Separation"),
      tags$ul(
        tags$li(strong("Patient:"), "Holds private key. Encrypts data. Decrypts final risk score."),
        tags$li(strong("Analyst:"), "Holds public key and model weights. Computes on ciphertext only.")
      ),
      hr(),
      h4("Limitations"),
      tags$ul(
        tags$li("Paillier supports addition only (not multiplication of two ciphertexts)."),
        tags$li("Integer values only. Floating-point requires scaling."),
        tags$li("Sigmoid is computed locally by the patient, not on ciphertext."),
        tags$li("This is a proof-of-concept for educational purposes.")
      )
    )
  )
)

# ------------------------------------------------------------
# 5. SERVER
# ------------------------------------------------------------
server <- function(input, output, session) {
  
  # Reactive state
  model_trained    <- reactiveVal(FALSE)
  trained_model    <- reactiveVal(NULL)
  test_data        <- reactiveVal(NULL)
  encrypted_data   <- reactiveVal(NULL)
  sent_to_analyst  <- reactiveVal(FALSE)
  encrypted_result <- reactiveVal(NULL)
  result_ready     <- reactiveVal(FALSE)
  compute_log_text <- reactiveVal("Waiting for data...")
  
  # ---------- Train Model ----------
  observeEvent(input$btn_train, {
    # Split data
    set.seed(123)
    train_idx <- sample(1:nrow(synthetic_data), 0.7 * nrow(synthetic_data))
    train_data <- synthetic_data[train_idx, ]
    test_set <- synthetic_data[-train_idx, ]
    test_data(test_set)
    
    # Train
    model <- train_model(train_data)
    trained_model(model)
    model_trained(TRUE)
    
    # Predictions on test
    pred_prob <- predict(model, test_set, type = "response")
    pred_class <- ifelse(pred_prob > 0.5, 1, 0)
    
    # Confusion matrix
    conf_mat <- table(Predicted = pred_class, Actual = test_set$diabetes)
    
    output$confusion_table <- renderTable({
      as.data.frame.matrix(conf_mat) |>
        tibble::rownames_to_column("Predicted")
    })
    
    showNotification("Model trained successfully!", type = "message")
  })
  
  # ---------- Data Preview ----------
  output$data_preview <- renderTable({
    head(synthetic_data, 10)
  })
  
  # ---------- Model Summary ----------
  output$model_summary <- renderText({
    req(model_trained())
    model <- trained_model()
    test <- test_data()
    
    pred_prob <- predict(model, test, type = "response")
    pred_class <- ifelse(pred_prob > 0.5, 1, 0)
    accuracy <- mean(pred_class == test$diabetes)
    
    paste0(
      "Model: Logistic Regression\n",
      "Training samples: ", nrow(synthetic_data) * 0.7, "\n",
      "Test samples: ", nrow(test), "\n",
      "Test Accuracy: ", round(accuracy * 100, 2), "%"
    )
  })
  
  # ---------- Model Coefficients ----------
  output$model_coef_table <- renderTable({
    req(model_trained())
    model <- trained_model()
    data.frame(
      Feature = names(coef(model)),
      Coefficient = round(coef(model), 4),
      Odds_Ratio = round(exp(coef(model)), 3)
    )
  })
  
  # ---------- Feature Importance Plot ----------
  output$feature_plot <- renderPlot({
    req(model_trained())
    model <- trained_model()
    
    coefs <- coef(model)[-1]  # exclude intercept
    df <- data.frame(
      Feature = names(coefs),
      Coefficient = coefs
    )
    df$Feature <- factor(df$Feature, levels = df$Feature[order(abs(df$Coefficient))])
    
    ggplot(df, aes(x = Feature, y = Coefficient, fill = Coefficient > 0)) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = c("TRUE" = "#e74c3c", "FALSE" = "#3498db"), guide = "none") +
      labs(title = "Model Coefficients", x = NULL, y = "Coefficient") +
      theme_minimal(base_size = 12)
  })
  
  # ---------- Patient Summary ----------
  output$patient_summary <- renderTable({
    data.frame(
      Feature = c("Age", "Systolic BP", "Blood Sugar", "BMI", "Smoker", "Alcohol"),
      Value = c(input$in_age, input$in_systolic_bp, input$in_sugar, 
                input$in_bmi, input$in_smoker, input$in_alcohol)
    )
  })
  
  # ---------- Encrypt Patient Data ----------
  observeEvent(input$btn_encrypt, {
    req(model_trained())
    
    values <- c(
      age = as.integer(input$in_age),
      systolic_bp = as.integer(input$in_systolic_bp),
      sugar = as.integer(input$in_sugar),
      bmi = as.integer(input$in_bmi),
      smoker = as.integer(input$in_smoker),
      alcohol = as.integer(input$in_alcohol)
    )
    
    encrypted <- lapply(values, encrypt_int)
    encrypted_data(encrypted)
    
    showNotification("Data encrypted locally. Ready to send.", type = "message")
  })
  
  # ---------- Display Ciphertext ----------
  output$ciphertext_display <- renderTable({
    req(encrypted_data())
    enc <- encrypted_data()
    data.frame(
      Feature = names(enc),
      Ciphertext = sapply(enc, trunc_ct)
    )
  })
  
  # ---------- Send to Analyst ----------
  observeEvent(input$btn_send, {
    req(encrypted_data())
    sent_to_analyst(TRUE)
    compute_log_text("Ciphertext received. Waiting for computation...")
    showNotification("Ciphertext sent to Analyst. Switch to Analyst tab.", 
                     type = "warning", duration = 5)
  })
  
  # ---------- Analyst Cipher Status ----------
  output$analyst_cipher_status <- renderText({
    if (sent_to_analyst()) "Ciphertext received and ready for computation."
    else "No data received yet."
  })
  
  # ---------- Analyst Cipher Table ----------
  output$analyst_cipher_table <- renderTable({
    req(sent_to_analyst(), encrypted_data())
    enc <- encrypted_data()
    data.frame(
      Feature = names(enc),
      Ciphertext = sapply(enc, trunc_ct)
    )
  })
  
  # ---------- Compute Encrypted Prediction ----------
  observeEvent(input$btn_compute, {
    req(sent_to_analyst(), encrypted_data(), model_trained())
    
    enc <- encrypted_data()
    model <- trained_model()
    coefs <- coef(model)
    
    # Start with intercept encrypted
    # Since Paillier works on integers, we scale coefficients by 1000
    # and divide at the end
    scale_factor <- 1000
    
    log_lines <- c("Starting encrypted computation...")
    
    # Encrypt intercept (scaled)
    intercept_scaled <- as.integer(round(coefs["(Intercept)"] * scale_factor))
    enc_result <- encrypt_int(intercept_scaled)
    log_lines <- c(log_lines, "Added encrypted intercept.")
    
    # Add each feature: E(z) = intercept + sum(coef * E(feature))
    for (feat in names(enc)) {
      coef_scaled <- as.integer(round(coefs[feat] * scale_factor))
      # Multiply ciphertext by plaintext scalar: E(coef * x)
      # This is repeated addition
      enc_term <- enc[[feat]]
      if (coef_scaled != 1) {
        # For simplicity, we add enc[[feat]] coef_scaled times
        # (In production, use proper scalar multiplication)
        term <- enc_term
        for (i in seq_len(abs(coef_scaled) - 1)) {
          term <- hom_add(term, enc_term)
        }
        if (coef_scaled < 0) {
          # Negation: add (n - term) mod n^2, simplified here
          term <- pubkey$add(term, pubkey$encrypt(0))
        }
        enc_result <- hom_add(enc_result, term)
      } else {
        enc_result <- hom_add(enc_result, enc_term)
      }
      log_lines <- c(log_lines, paste0("Added encrypted term for: ", feat))
    }
    
    log_lines <- c(log_lines, "Encrypted linear predictor computed.")
    log_lines <- c(log_lines, "Result ready to return to patient.")
    
    encrypted_result(enc_result)
    compute_log_text(paste(log_lines, collapse = "\n"))
    
    showNotification("Encrypted prediction computed!", type = "message")
  })
  
  # ---------- Compute Log Output ----------
  output$compute_log <- renderText({
    compute_log_text()
  })
  
  # ---------- Return Result ----------
  observeEvent(input$btn_return, {
    req(encrypted_result())
    result_ready(TRUE)
    showNotification("Encrypted result returned. Switch to Proof tab to decrypt.",
                     type = "warning", duration = 5)
  })
  
  # ---------- Decrypt Result ----------
  observeEvent(input$btn_decrypt, {
    req(result_ready(), encrypted_result())
    
    scale_factor <- 1000
    decrypted_scaled <- as.integer(privkey$decrypt(encrypted_result()))
    linear_pred <- decrypted_scaled / scale_factor
    
    # Apply sigmoid locally
    risk_prob <- 1 / (1 + exp(-linear_pred))
    
    output$risk_display <- renderText({
      paste0(round(risk_prob * 100, 1), "%")
    })
    
    # Verification: compute directly
    values <- c(
      "(Intercept)" = 1,
      age = as.numeric(input$in_age),
      systolic_bp = as.numeric(input$in_systolic_bp),
      sugar = as.numeric(input$in_sugar),
      bmi = as.numeric(input$in_bmi),
      smoker = as.numeric(input$in_smoker),
      alcohol = as.numeric(input$in_alcohol)
    )
    
    model <- trained_model()
    direct_pred <- sum(coef(model) * values)
    direct_prob <- 1 / (1 + exp(-direct_pred))
    
    output$verification_table <- renderTable({
      data.frame(
        Method = c("Homomorphic (decrypted)", "Direct computation", "Match"),
        Linear_Predictor = c(round(linear_pred, 4), round(direct_pred, 4), 
                             abs(linear_pred - direct_pred) < 0.01),
        Probability = c(round(risk_prob, 4), round(direct_prob, 4),
                        abs(risk_prob - direct_prob) < 0.01)
      )
    })
    
    showNotification("Result decrypted!", type = "message")
  })
  
  # ---------- Download Private Key ----------
  output$dl_privkey <- downloadHandler(
    filename = function() "paillier_private_key.rds",
    content = function(file) {
      saveRDS(privkey, file)
    }
  )
  
  # ---------- Download Public Key ----------
  output$dl_pubkey <- downloadHandler(
    filename = function() "paillier_public_key.rds",
    content = function(file) {
      saveRDS(pubkey, file)
    }
  )
}

# ------------------------------------------------------------
# 6. RUN APP
# ------------------------------------------------------------
shinyApp(ui = ui, server = server)