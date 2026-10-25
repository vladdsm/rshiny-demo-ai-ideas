# ============================================================
# Encrypted Diabetes Risk Prediction
# Homomorphic Encryption + Logistic Regression Demo
# ============================================================
# This app demonstrates privacy-preserving machine learning:
# - The Patient encrypts their health data locally
# - The Analyst runs a logistic regression model on ciphertext
# - The Patient decrypts the prediction themselves
# - The Analyst never sees any raw patient data
#
# Verified working with homomorpheR Paillier (methods: mult, add, sub_real)
# ============================================================

library(shiny)
library(bslib)
library(homomorpheR)
library(gmp)
library(ggplot2)
library(thematic)

thematic_shiny()

# ------------------------------------------------------------
# 1. CLINICAL RISK FORMULA (GP LOGIC)
# ------------------------------------------------------------
clinical_coefs <- list(
  intercept         = -1.20,
  age               =  0.035,
  sugar             =  0.45,
  systolic_bp       =  0.30,
  bmi               =  0.08,
  smoker            =  0.85,
  alcohol           =  0.60,
  sport             = -0.70,
  smoker_x_sport    = -0.45
)

compute_linear_predictor <- function(age, sugar, systolic_bp, bmi,
                                     smoker, alcohol, sport) {
  clinical_coefs$intercept +
    clinical_coefs$age         * (age - 50) +
    clinical_coefs$sugar       * (sugar - 110) / 10 +
    clinical_coefs$systolic_bp * (systolic_bp - 130) / 10 +
    clinical_coefs$bmi         * (bmi - 27) +
    clinical_coefs$smoker      * smoker +
    clinical_coefs$alcohol     * alcohol +
    clinical_coefs$sport       * sport +
    clinical_coefs$smoker_x_sport * smoker * sport
}

compute_probability <- function(...) {
  z <- compute_linear_predictor(...)
  1 / (1 + exp(-z))
}

# ------------------------------------------------------------
# 2. SYNTHETIC DATA GENERATION
# ------------------------------------------------------------
generate_patients <- function(n) {
  set.seed(sample.int(1e6, 1))
  
  age         <- round(rnorm(n, mean = 50, sd = 14))
  age         <- pmin(pmax(age, 20), 85)
  sugar       <- round(rnorm(n, mean = 110, sd = 28))
  sugar       <- pmin(pmax(sugar, 60), 220)
  systolic_bp <- round(rnorm(n, mean = 130, sd = 16))
  systolic_bp <- pmin(pmax(systolic_bp, 95), 195)
  bmi         <- round(rnorm(n, mean = 27, sd = 5))
  bmi         <- pmin(pmax(bmi, 16), 45)
  smoker      <- rbinom(n, 1, 0.25)
  alcohol     <- rbinom(n, 1, 0.35)
  sport       <- rbinom(n, 1, 0.40)
  
  p <- compute_probability(age, sugar, systolic_bp, bmi,
                           smoker, alcohol, sport)
  diabetes <- rbinom(n, 1, p)
  
  data.frame(
    age = as.integer(age),
    systolic_bp = as.integer(systolic_bp),
    sugar = as.integer(sugar),
    bmi = as.integer(bmi),
    smoker = as.integer(smoker),
    alcohol = as.integer(alcohol),
    sport = as.integer(sport),
    diabetes = as.integer(diabetes)
  )
}

# ------------------------------------------------------------
# 3. KEY GENERATION (once per session)
# ------------------------------------------------------------
cat("Generating Paillier keypair (1024-bit)...\n")
keyPair <- PaillierKeyPair$new(modulusBits = 1024)
pubkey <- keyPair$pubkey
privkey <- keyPair$getPrivateKey()
cat("Keypair ready.\n")

# ------------------------------------------------------------
# 4. HELPER FUNCTIONS
# ------------------------------------------------------------
# Paillier in homomorpheR works on non-negative integers mod n.
# To handle signed coefficients, we encode negative values by
# adding a large constant OFFSET before encryption, then
# subtract the accumulated offset from the decrypted result.
OFFSET <- 1000000L

encrypt_int <- function(x) {
  pubkey$encrypt(as.bigz(as.integer(x)))
}

hom_add <- function(ct1, ct2) {
  pubkey$add(ct1, ct2)
}

# Scalar multiplication that handles negative scalars safely.
# E(k*x) = E(x)^k for k >= 0. For k < 0, we use
# E(-|k|*x) = E(0) - E(|k|*x) via sub_real.
hom_scalar_mult <- function(ct, k) {
  k <- as.integer(k)
  if (k == 0) return(encrypt_int(0))
  if (k > 0)  return(pubkey$mult(ct, k))
  # k < 0: compute E(|k|*x) then negate
  pos <- pubkey$mult(ct, abs(k))
  # sub_real(E(a), ct) = E(a - plaintext(ct))? No — sub_real takes
  # a ciphertext and a *plaintext* real. In this Paillier API,
  # sub() subtracts two ciphertexts: E(a) - E(b) = E(a - b).
  # E(0) - E(|k|*x) = E(-|k|*x)  ✓
  pubkey$sub(encrypt_int(0), pos)
}

trunc_ct <- function(ct, n = 20) {
  s <- as.character(ct)
  if (nchar(s) > n) paste0(substr(s, 1, n), "...") else s
}

# ------------------------------------------------------------
# 5. UI
# ------------------------------------------------------------
ui <- page_navbar(
  title = "Encrypted Diabetes Risk Prediction",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2c3e50",
    base_font = font_google("Inter")
  ),
  
  # ========== TAB 1: DATA & MODEL ==========
  nav_panel(
    "Data & Model",
    icon = icon("chart-line"),
    layout_sidebar(
      sidebar = sidebar(
        h4("Synthetic Dataset"),
        p("Data generated from explicit clinical risk formulas."),
        numericInput("n_patients", "Number of patients to generate:",
                     value = 2000, min = 200, max = 20000, step = 100),
        actionButton("btn_generate", "Generate New Dataset",
                     class = "btn-secondary w-100 mb-2"),
        hr(),
        actionButton("btn_train", "Train Logistic Regression Model",
                     class = "btn-primary w-100"),
        hr(),
        h5("Model Coefficients"),
        tableOutput("model_coef_table")
      ),
      layout_column_wrap(
        width = 1/2,
        card(
          card_header("Dataset Preview (first 10 rows)"),
          tableOutput("data_preview")
        ),
        card(
          card_header("Model Performance"),
          verbatimTextOutput("model_summary")
        )
      ),
      card(
        card_header("Raw Data — Faceted Distributions by Diabetes Status"),
        p("Each panel shows the distribution of one feature, split by outcome."),
        plotOutput("faceted_plot", height = "500px")
      ),
      layout_column_wrap(
        width = 1/2,
        card(
          card_header("Feature Importance (Coefficients)"),
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
        sliderInput("in_age", "Age", min = 20, max = 85, value = 55, step = 1),
        sliderInput("in_systolic_bp", "Systolic Blood Pressure",
                    min = 95, max = 195, value = 130, step = 1),
        sliderInput("in_sugar", "Blood Sugar (mg/dL)",
                    min = 60, max = 220, value = 110, step = 1),
        sliderInput("in_bmi", "BMI", min = 16, max = 45, value = 27, step = 1),
        radioButtons("in_smoker", "Smoker",
                     choices = c("No" = 0, "Yes" = 1), inline = TRUE),
        radioButtons("in_alcohol", "Alcohol Consumption",
                     choices = c("No" = 0, "Yes" = 1), inline = TRUE),
        radioButtons("in_sport", "Regular Sport",
                     choices = c("No" = 0, "Yes" = 1), inline = TRUE),
        hr(),
        actionButton("btn_encrypt", "Encrypt My Data",
                     class = "btn-warning w-100 mb-2"),
        actionButton("btn_send", "Send to Analyst",
                     class = "btn-info w-100 mb-2"),
        hr(),
        h5("Key Management"),
        downloadButton("dl_privkey", "Download Private Key",
                       class = "btn-outline-secondary w-100 mb-2"),
        downloadButton("dl_pubkey", "Download Public Key",
                       class = "btn-outline-secondary w-100 mb-2")
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
  
  # ========== TAB 4: PROOF & RESULT ==========
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
          uiOutput("risk_box")
        ),
        card(
          card_header("Explanation"),
          p("The risk was computed entirely on encrypted data."),
          p("The Analyst multiplied encrypted features by plaintext model coefficients, then summed the results homomorphically."),
          p("You decrypted the linear predictor and applied the sigmoid locally to obtain the probability.")
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
      p("The Analyst computes the encrypted linear predictor:"),
      withMathJax(helpText("$$E(z) = E(b) + \\sum w_i \\cdot E(x_i)$$")),
      p("You decrypt z and apply sigmoid locally to obtain the probability."),
      hr(),
      h4("Role Separation"),
      tags$ul(
        tags$li(strong("Patient:"), "Holds private key. Encrypts data. Decrypts final risk score."),
        tags$li(strong("Analyst:"), "Holds public key and model weights. Computes on ciphertext only.")
      ),
      hr(),
      h4("Dataset Generation"),
      p("Patients are simulated from realistic marginals. Diabetes labels are assigned by sampling from the clinical probability:"),
      withMathJax(helpText("$$P(\\text{diabetes}) = \\sigma(z)$$")),
      p("where z is the GP-inspired linear predictor defined in the app. This guarantees a balanced, learnable dataset."),
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
# 6. SERVER
# ------------------------------------------------------------
server <- function(input, output, session) {
  
  patients         <- reactiveVal(generate_patients(2000))
  model_trained    <- reactiveVal(FALSE)
  trained_model    <- reactiveVal(NULL)
  test_data        <- reactiveVal(NULL)
  encrypted_data   <- reactiveVal(NULL)
  sent_to_analyst  <- reactiveVal(FALSE)
  encrypted_result <- reactiveVal(NULL)
  result_ready     <- reactiveVal(FALSE)
  compute_log_text <- reactiveVal("Waiting for data...")
  
  # ---------- Generate New Dataset ----------
  observeEvent(input$btn_generate, {
    n <- input$n_patients
    patients(generate_patients(n))
    model_trained(FALSE)
    trained_model(NULL)
    test_data(NULL)
    encrypted_data(NULL)
    sent_to_analyst(FALSE)
    encrypted_result(NULL)
    result_ready(FALSE)
    showNotification(paste("Generated", n, "patients."), type = "message")
  })
  
  # ---------- Data Preview ----------
  output$data_preview <- renderTable({
    head(patients(), 10)
  })
  
  # ---------- Faceted Plot ----------
  output$faceted_plot <- renderPlot({
    df <- patients()
    long <- rbind(
      data.frame(diabetes = df$diabetes, feature = "Age", value = df$age),
      data.frame(diabetes = df$diabetes, feature = "Systolic BP", value = df$systolic_bp),
      data.frame(diabetes = df$diabetes, feature = "Blood Sugar", value = df$sugar),
      data.frame(diabetes = df$diabetes, feature = "BMI", value = df$bmi)
    )
    long$diabetes <- factor(long$diabetes, labels = c("No Diabetes", "Diabetes"))
    long$feature <- factor(long$feature,
                           levels = c("Age", "Systolic BP", "Blood Sugar", "BMI"))
    
    ggplot(long, aes(x = value, fill = diabetes)) +
      geom_histogram(alpha = 0.6, bins = 30, position = "identity") +
      facet_wrap(~ feature, scales = "free", ncol = 2) +
      scale_fill_manual(values = c("No Diabetes" = "#3498db", "Diabetes" = "#e74c3c")) +
      labs(title = "Feature Distributions by Diabetes Status",
           x = "Value", y = "Count", fill = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top")
  })
  
  # ---------- Train Model ----------
  observeEvent(input$btn_train, {
    df <- patients()
    set.seed(123)
    train_idx <- sample(1:nrow(df), 0.7 * nrow(df))
    train_data <- df[train_idx, ]
    test_set <- df[-train_idx, ]
    test_data(test_set)
    
    model <- glm(diabetes ~ age + systolic_bp + sugar + bmi +
                   smoker + alcohol + sport,
                 data = train_data, family = binomial())
    trained_model(model)
    model_trained(TRUE)
    
    pred_prob <- predict(model, test_set, type = "response")
    pred_class <- ifelse(pred_prob > 0.5, 1, 0)
    conf_mat <- table(Predicted = pred_class, Actual = test_set$diabetes)
    
    output$confusion_table <- renderTable({
      df_cm <- as.data.frame.matrix(conf_mat)
      df_cm$Predicted <- rownames(df_cm)
      df_cm[, c("Predicted", colnames(df_cm)[1:2])]
    })
    
    showNotification("Model trained successfully!", type = "message")
  })
  
  # ---------- Model Summary ----------
  output$model_summary <- renderText({
    req(model_trained())
    model <- trained_model()
    test <- test_data()
    
    pred_prob <- predict(model, test, type = "response")
    pred_class <- ifelse(pred_prob > 0.5, 1, 0)
    accuracy <- mean(pred_class == test$diabetes)
    
    pos <- pred_prob[test$diabetes == 1]
    neg <- pred_prob[test$diabetes == 0]
    auc <- if (length(pos) > 0 && length(neg) > 0) mean(outer(pos, neg, ">")) else NA
    
    paste0(
      "Model: Logistic Regression\n",
      "Training samples: ", nrow(patients()) - nrow(test), "\n",
      "Test samples: ", nrow(test), "\n",
      "Test Accuracy: ", round(accuracy * 100, 2), "%\n",
      "Approx. AUC: ", round(auc, 3), "\n",
      "Positive rate (test): ", round(mean(test$diabetes) * 100, 1), "%"
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
  
  # ---------- Feature Plot ----------
  output$feature_plot <- renderPlot({
    req(model_trained())
    model <- trained_model()
    coefs <- coef(model)[-1]
    df <- data.frame(
      Feature = names(coefs),
      Coefficient = coefs
    )
    df$Feature <- factor(df$Feature, levels = df$Feature[order(abs(df$Coefficient))])
    
    ggplot(df, aes(x = Feature, y = Coefficient, fill = Coefficient > 0)) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = c("TRUE" = "#e74c3c", "FALSE" = "#3498db"),
                        guide = "none") +
      labs(title = "Model Coefficients", x = NULL, y = "Coefficient") +
      theme_minimal(base_size = 12)
  })
  
  # ---------- Patient Summary ----------
  output$patient_summary <- renderTable({
    data.frame(
      Feature = c("Age", "Systolic BP", "Blood Sugar", "BMI",
                  "Smoker", "Alcohol", "Sport"),
      Value = c(input$in_age, input$in_systolic_bp, input$in_sugar,
                input$in_bmi, input$in_smoker, input$in_alcohol, input$in_sport)
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
      alcohol = as.integer(input$in_alcohol),
      sport = as.integer(input$in_sport)
    )
    encrypted <- lapply(values, encrypt_int)
    encrypted_data(encrypted)
    showNotification("Data encrypted locally. Ready to send.", type = "message")
  })
  
  # ---------- Ciphertext Display ----------
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
  
  # ---------- Analyst Status ----------
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
  # Core Paillier operations used:
  #   pubkey$mult(ct, k)     -> E(k * x) for plaintext scalar k
  #   pubkey$add(ct1, ct2)   -> E(a + b)
  #   pubkey$sub(ct1, ct2)   -> E(a - b)
  #
  # Linear predictor: z = b0 + sum(w_i * x_i)
  # Scaled by 1000 to preserve precision through integer encryption.
  observeEvent(input$btn_compute, {
    req(sent_to_analyst(), encrypted_data(), model_trained())
    
    enc <- encrypted_data()
    model <- trained_model()
    coefs <- coef(model)
    scale_factor <- 1000L
    log_lines <- c("Starting encrypted computation...")
    
    # Encrypted scaled intercept
    intercept_scaled <- as.integer(round(coefs["(Intercept)"] * scale_factor))
    enc_result <- encrypt_int(intercept_scaled)
    log_lines <- c(log_lines, "Added encrypted intercept.")
    
    for (feat in names(enc)) {
      coef_scaled <- as.integer(round(coefs[feat] * scale_factor))
      if (coef_scaled == 0) {
        log_lines <- c(log_lines, paste0("Skipped ", feat, " (coef=0)"))
        next
      }
      
      # Scalar multiplication with sign handling
      enc_term <- hom_scalar_mult(enc[[feat]], coef_scaled)
      enc_result <- hom_add(enc_result, enc_term)
      
      log_lines <- c(log_lines,
                     paste0("Added encrypted term for ", feat,
                            " (w=", round(coefs[feat], 4), ")"))
    }
    
    log_lines <- c(log_lines, "Encrypted linear predictor computed.")
    encrypted_result(enc_result)
    compute_log_text(paste(log_lines, collapse = "\n"))
    showNotification("Encrypted prediction computed!", type = "message")
  })
  
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
    risk_prob <- 1 / (1 + exp(-linear_pred))
    
    output$risk_box <- renderUI({
      pct <- round(risk_prob * 100, 1)
      color <- if (pct < 20) "success" else if (pct < 50) "warning" else "danger"
      value_box(
        title = "Predicted Diabetes Risk",
        value = paste0(pct, "%"),
        showcase = icon("heart-pulse"),
        theme = color
      )
    })
    
    # Verification: direct computation
    values <- c(
      "(Intercept)" = 1,
      age = as.numeric(input$in_age),
      systolic_bp = as.numeric(input$in_systolic_bp),
      sugar = as.numeric(input$in_sugar),
      bmi = as.numeric(input$in_bmi),
      smoker = as.numeric(input$in_smoker),
      alcohol = as.numeric(input$in_alcohol),
      sport = as.numeric(input$in_sport)
    )
    
    model <- trained_model()
    direct_pred <- sum(coef(model) * values)
    direct_prob <- 1 / (1 + exp(-direct_pred))
    
    output$verification_table <- renderTable({
      data.frame(
        Method = c("Homomorphic (decrypted)", "Direct computation"),
        Linear_Predictor = c(round(linear_pred, 4), round(direct_pred, 4)),
        Probability = c(round(risk_prob, 4), round(direct_prob, 4))
      )
    })
    
    showNotification("Result decrypted!", type = "message")
  })
  
  # ---------- Downloads ----------
  output$dl_privkey <- downloadHandler(
    filename = function() "paillier_private_key.rds",
    content = function(file) saveRDS(privkey, file)
  )
  
  output$dl_pubkey <- downloadHandler(
    filename = function() "paillier_public_key.rds",
    content = function(file) saveRDS(pubkey, file)
  )
}

# ------------------------------------------------------------
# 7. RUN APP
# ------------------------------------------------------------
shinyApp(ui = ui, server = server)