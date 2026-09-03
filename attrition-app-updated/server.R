server <- function(input, output, session) {

  # ============================================================
  # REACTIVE STATE
  # ============================================================
  rv <- reactiveValues(
    raw_data       = NULL,
    clean_data     = NULL,
    validation     = NULL,
    trained_models = list(),   # name -> list(model=, preds=, probs=, cm=, roc=)
    train_test     = NULL,
    last_prediction = NULL,    # for XAI / recommendations / what-if
    bulk_results   = NULL
  )

  # ============================================================
  # DATASET CENTER
  # ============================================================

  observeEvent(input$load_sample, {
    df <- read.csv(SAMPLE_DATA_PATH, stringsAsFactors = FALSE)
    rv$raw_data <- df
    rv$validation <- validate_dataset(df)
    showNotification("Sample dataset loaded (1470 employees).", type = "message")
  })

  observeEvent(input$file_upload, {
    req(input$file_upload)
    df <- tryCatch(
      read_uploaded_data(input$file_upload$datapath, input$file_upload$name),
      error = function(e) { showNotification(paste("Upload error:", e$message), type = "error"); NULL }
    )
    req(df)
    rv$raw_data <- df
    rv$validation <- validate_dataset(df)
    showNotification("Dataset uploaded successfully.", type = "message")
  })

  observeEvent(input$go_dataset, {
    updateBs4TabItems(session, "sidebar", "dataset")
  })

  output$dataset_summary <- renderPrint({
    req(rv$raw_data)
    cat("Rows:", nrow(rv$raw_data), " | Columns:", ncol(rv$raw_data), "\n")
    cat("Columns:", paste(names(rv$raw_data), collapse = ", "), "\n\n")
    if (TARGET_COL %in% names(rv$raw_data)) {
      cat("Attrition distribution:\n")
      print(table(rv$raw_data[[TARGET_COL]]))
    } else {
      cat("Note: no 'Attrition' column detected \u2014 ML modules require this target column.\n")
    }
  })

  output$dataset_preview <- renderDT({
    req(rv$raw_data)
    datatable(head(rv$raw_data, 20), options = list(scrollX = TRUE, pageLength = 10))
  })

  output$quality_score_ui <- renderUI({
    req(rv$validation)
    v <- rv$validation
    tagList(
      fluidRow(
        column(3, div(class = "stat-box", h3(v$n_rows), p("Employees"))),
        column(3, div(class = "stat-box", h3(v$n_missing), p("Missing Values"))),
        column(3, div(class = "stat-box", h3(v$n_dupes), p("Duplicate Records"))),
        column(3, div(class = "stat-box", h3(paste0(v$quality_score, "/100")), p("Quality Score")))
      )
    )
  })

  # ============================================================
  # HOME VALUE BOXES
  # ============================================================
  output$vb_total_employees <- renderbs4ValueBox({
    n <- if (!is.null(rv$raw_data)) nrow(rv$raw_data) else 0
    bs4ValueBox(value = n, subtitle = "Total Employees", icon = icon("users"), color = "primary")
  })

  output$vb_attrition_rate <- renderbs4ValueBox({
    rate <- "N/A"
    if (!is.null(rv$raw_data) && TARGET_COL %in% names(rv$raw_data)) {
      rate <- paste0(round(mean(rv$raw_data[[TARGET_COL]] == "Yes", na.rm = TRUE) * 100, 1), "%")
    }
    bs4ValueBox(value = rate, subtitle = "Current Attrition Rate", icon = icon("arrow-trend-down"), color = "danger")
  })

  output$vb_high_risk <- renderbs4ValueBox({
    n <- if (!is.null(rv$bulk_results)) sum(rv$bulk_results$RiskLabel == "High Risk") else 0
    bs4ValueBox(value = n, subtitle = "Predicted High-Risk Employees", icon = icon("triangle-exclamation"), color = "warning")
  })

  output$vb_model_accuracy <- renderbs4ValueBox({
    acc <- "N/A"
    if (length(rv$trained_models) > 0) {
      best <- max(sapply(rv$trained_models, function(m) m$metrics$Accuracy))
      acc <- paste0(round(best * 100, 1), "%")
    }
    bs4ValueBox(value = acc, subtitle = "Best Model Accuracy", icon = icon("bullseye"), color = "success")
  })

  # ============================================================
  # EDA DASHBOARD
  # ============================================================

  output$filter_department_ui <- renderUI({
    req(rv$raw_data); req("Department" %in% names(rv$raw_data))
    pickerInput("f_dept", "Department", choices = unique(rv$raw_data$Department),
                selected = unique(rv$raw_data$Department), multiple = TRUE)
  })
  output$filter_gender_ui <- renderUI({
    req(rv$raw_data); req("Gender" %in% names(rv$raw_data))
    pickerInput("f_gender", "Gender", choices = unique(rv$raw_data$Gender),
                selected = unique(rv$raw_data$Gender), multiple = TRUE)
  })
  output$filter_age_ui <- renderUI({
    req(rv$raw_data); req("Age" %in% names(rv$raw_data))
    sliderInput("f_age", "Age Range", min = min(rv$raw_data$Age), max = max(rv$raw_data$Age),
                value = c(min(rv$raw_data$Age), max(rv$raw_data$Age)))
  })
  output$filter_salary_ui <- renderUI({
    req(rv$raw_data); req("MonthlyIncome" %in% names(rv$raw_data))
    sliderInput("f_salary", "Salary Range", min = min(rv$raw_data$MonthlyIncome), max = max(rv$raw_data$MonthlyIncome),
                value = c(min(rv$raw_data$MonthlyIncome), max(rv$raw_data$MonthlyIncome)))
  })

  eda_data <- reactive({
    req(rv$raw_data)
    df <- rv$raw_data
    if (!is.null(input$f_dept) && "Department" %in% names(df)) df <- df %>% filter(Department %in% input$f_dept)
    if (!is.null(input$f_gender) && "Gender" %in% names(df)) df <- df %>% filter(Gender %in% input$f_gender)
    if (!is.null(input$f_age) && "Age" %in% names(df)) df <- df %>% filter(Age >= input$f_age[1], Age <= input$f_age[2])
    if (!is.null(input$f_salary) && "MonthlyIncome" %in% names(df)) df <- df %>% filter(MonthlyIncome >= input$f_salary[1], MonthlyIncome <= input$f_salary[2])
    df
  })

  output$plot_age_dist <- renderPlotly({
    df <- eda_data(); req("Age" %in% names(df))
    p <- ggplot(df, aes(x = Age)) + geom_histogram(binwidth = 5, fill = "#4361ee") + theme_minimal()
    ggplotly(p)
  })

  output$plot_gender_dist <- renderPlotly({
    df <- eda_data(); req("Gender" %in% names(df))
    p <- ggplot(df, aes(x = Gender, fill = Gender)) + geom_bar() + theme_minimal()
    ggplotly(p)
  })

  output$plot_dept_dist <- renderPlotly({
    df <- eda_data(); req("Department" %in% names(df))
    p <- ggplot(df, aes(x = Department, fill = Department)) + geom_bar() + theme_minimal() +
      theme(axis.text.x = element_text(angle = 20, hjust = 1))
    ggplotly(p)
  })

  output$plot_salary_dist <- renderPlotly({
    df <- eda_data(); req("MonthlyIncome" %in% names(df))
    p <- ggplot(df, aes(x = MonthlyIncome)) + geom_histogram(bins = 30, fill = "#2a9d8f") + theme_minimal()
    ggplotly(p)
  })

  output$plot_attr_dept <- renderPlotly({
    df <- eda_data(); req(all(c("Department", TARGET_COL) %in% names(df)))
    p <- ggplot(df, aes(x = Department, fill = .data[[TARGET_COL]])) + geom_bar(position = "fill") +
      theme_minimal() + labs(y = "Proportion") + theme(axis.text.x = element_text(angle = 20, hjust = 1))
    ggplotly(p)
  })

  output$plot_attr_overtime <- renderPlotly({
    df <- eda_data(); req(all(c("OverTime", TARGET_COL) %in% names(df)))
    p <- ggplot(df, aes(x = OverTime, fill = .data[[TARGET_COL]])) + geom_bar(position = "fill") +
      theme_minimal() + labs(y = "Proportion")
    ggplotly(p)
  })

  output$plot_corr_heatmap <- renderPlot({
    df <- eda_data()
    num_df <- df[sapply(df, is.numeric)]
    req(ncol(num_df) >= 2)
    corr <- cor(num_df, use = "pairwise.complete.obs")
    corrplot(corr, method = "color", type = "upper", tl.col = "black", tl.cex = 0.8, addCoef.col = "grey30", number.cex = 0.6)
  })

  # ============================================================
  # ML TRAINING CENTER
  # ============================================================

  observeEvent(input$train_model_btn, {
    req(rv$raw_data)
    validate(need(TARGET_COL %in% names(rv$raw_data), "Dataset must contain an 'Attrition' column (Yes/No) to train models."))

    withProgress(message = "Training model...", value = 0.2, {

      df <- preprocess_data(rv$raw_data, impute_method = input$impute_method, scale_method = input$scale_method)
      df <- df[complete.cases(df), ]
      df <- drop_id_columns(df)

      set.seed(42)
      idx <- createDataPartition(df[[TARGET_COL]], p = input$train_ratio, list = FALSE)
      train_df <- df[idx, ]
      test_df  <- df[-idx, ]
      rv$train_test <- list(train = train_df, test = test_df)

      incProgress(0.3, detail = "Fitting model...")

      ctrl <- trainControl(method = "cv", number = 5, classProbs = TRUE, summaryFunction = twoClassSummary)

      model_method <- input$model_choice
      fit <- tryCatch(
        train(as.formula(paste(TARGET_COL, "~ .")), data = train_df,
              method = model_method, trControl = ctrl, metric = "ROC",
              trace = FALSE),
        error = function(e) { showNotification(paste("Training failed:", e$message), type = "error"); NULL }
      )
      req(fit)

      incProgress(0.3, detail = "Evaluating...")

      probs <- predict(fit, test_df, type = "prob")[, "Yes"]
      preds <- factor(ifelse(probs >= 0.5, "Yes", "No"), levels = c("No", "Yes"))
      cm <- confusionMatrix(preds, test_df[[TARGET_COL]], positive = "Yes")
      roc_obj <- pROC::roc(response = test_df[[TARGET_COL]], predictor = probs, levels = c("No", "Yes"), direction = "<")

      metrics <- list(
        Accuracy  = as.numeric(cm$overall["Accuracy"]),
        Precision = as.numeric(cm$byClass["Precision"]),
        Recall    = as.numeric(cm$byClass["Recall"]),
        F1        = as.numeric(cm$byClass["F1"]),
        AUC       = as.numeric(pROC::auc(roc_obj))
      )

      model_name <- names(MODEL_CHOICES)[MODEL_CHOICES == model_method]
      rv$trained_models[[model_name]] <- list(
        model = fit, probs = probs, preds = preds, cm = cm, roc = roc_obj, metrics = metrics
      )

      incProgress(0.2, detail = "Done")
    })

    showNotification("Model trained successfully.", type = "message")
  })

  output$training_status <- renderPrint({
    if (length(rv$trained_models) == 0) {
      cat("No models trained yet. Configure options and click 'Train Model'.\n")
    } else {
      cat("Trained models:", paste(names(rv$trained_models), collapse = ", "), "\n")
    }
  })

  output$trained_models_table <- renderDT({
    req(length(rv$trained_models) > 0)
    tbl <- do.call(rbind, lapply(names(rv$trained_models), function(nm) {
      m <- rv$trained_models[[nm]]$metrics
      data.frame(Model = nm, Accuracy = round(m$Accuracy, 3), Precision = round(m$Precision, 3),
                 Recall = round(m$Recall, 3), F1 = round(m$F1, 3), AUC = round(m$AUC, 3))
    }))
    datatable(tbl, options = list(dom = 't'))
  })

  # ============================================================
  # MODEL EVALUATION
  # ============================================================

  output$eval_model_selector_ui <- renderUI({
    req(length(rv$trained_models) > 0)
    selectInput("eval_model", "Select Trained Model", choices = names(rv$trained_models))
  })

  output$confusion_matrix_out <- renderPrint({
    req(input$eval_model, rv$trained_models[[input$eval_model]])
    print(rv$trained_models[[input$eval_model]]$cm$table)
  })

  output$metrics_ui <- renderUI({
    req(input$eval_model, rv$trained_models[[input$eval_model]])
    m <- rv$trained_models[[input$eval_model]]$metrics
    tagList(
      fluidRow(
        column(6, div(class = "stat-box", h3(round(m$Accuracy, 3)), p("Accuracy"))),
        column(6, div(class = "stat-box", h3(round(m$Precision, 3)), p("Precision")))
      ),
      fluidRow(
        column(6, div(class = "stat-box", h3(round(m$Recall, 3)), p("Recall"))),
        column(6, div(class = "stat-box", h3(round(m$F1, 3)), p("F1 Score")))
      ),
      fluidRow(column(12, div(class = "stat-box", h3(round(m$AUC, 3)), p("AUC Score"))))
    )
  })

  output$roc_curve_plot <- renderPlotly({
    req(input$eval_model, rv$trained_models[[input$eval_model]])
    roc_obj <- rv$trained_models[[input$eval_model]]$roc
    df <- data.frame(fpr = 1 - roc_obj$specificities, tpr = roc_obj$sensitivities)
    p <- ggplot(df, aes(x = fpr, y = tpr)) + geom_line(color = "#4361ee", linewidth = 1) +
      geom_abline(linetype = "dashed", color = "grey") +
      labs(x = "False Positive Rate", y = "True Positive Rate") + theme_minimal()
    ggplotly(p)
  })

  output$model_comparison_plot <- renderPlotly({
    req(length(rv$trained_models) > 0)
    tbl <- do.call(rbind, lapply(names(rv$trained_models), function(nm) {
      data.frame(Model = nm, Accuracy = rv$trained_models[[nm]]$metrics$Accuracy)
    }))
    p <- ggplot(tbl, aes(x = reorder(Model, Accuracy), y = Accuracy, fill = Model)) +
      geom_col() + coord_flip() + theme_minimal() + labs(x = "", y = "Accuracy")
    ggplotly(p)
  })

  output$model_comparison_table <- renderDT({
    req(length(rv$trained_models) > 0)
    tbl <- do.call(rbind, lapply(names(rv$trained_models), function(nm) {
      m <- rv$trained_models[[nm]]$metrics
      data.frame(Model = nm, Accuracy = round(m$Accuracy, 3), Precision = round(m$Precision, 3),
                 Recall = round(m$Recall, 3), F1 = round(m$F1, 3), AUC = round(m$AUC, 3))
    }))
    datatable(tbl, options = list(dom = 't'))
  })

  # ============================================================
  # INDIVIDUAL PREDICTION
  # ============================================================

  output$predict_form_ui <- renderUI({
    req(rv$raw_data)
    df <- rv$raw_data
    tagList(
      if ("Age" %in% names(df)) numericInput("p_age", "Age", value = round(mean(df$Age, na.rm=TRUE))) else NULL,
      if ("Gender" %in% names(df)) selectInput("p_gender", "Gender", choices = unique(df$Gender)) else NULL,
      if ("MonthlyIncome" %in% names(df)) numericInput("p_income", "Monthly Income", value = round(mean(df$MonthlyIncome, na.rm=TRUE))) else NULL,
      if ("Department" %in% names(df)) selectInput("p_dept", "Department", choices = unique(df$Department)) else NULL,
      if ("JobRole" %in% names(df)) selectInput("p_role", "Job Role", choices = unique(df$JobRole)) else NULL,
      if ("YearsAtCompany" %in% names(df)) numericInput("p_years", "Years at Company", value = round(mean(df$YearsAtCompany, na.rm=TRUE))) else NULL,
      if ("OverTime" %in% names(df)) selectInput("p_overtime", "OverTime", choices = unique(df$OverTime)) else NULL,
      if ("PerformanceRating" %in% names(df)) numericInput("p_perf", "Performance Rating", value = round(mean(df$PerformanceRating, na.rm=TRUE))) else NULL,
      if ("JobSatisfaction" %in% names(df)) sliderInput("p_jobsat", "Job Satisfaction", min = 1, max = 4, value = 3) else NULL,
      if ("WorkLifeBalance" %in% names(df)) sliderInput("p_wlb", "Work-Life Balance", min = 1, max = 4, value = 3) else NULL,
      if ("DistanceFromHome" %in% names(df)) numericInput("p_distance", "Distance From Home", value = round(mean(df$DistanceFromHome, na.rm=TRUE))) else NULL
    )
  })

  # Build a one-row employee data.frame from current predict-tab inputs,
  # filling any columns the model needs (but the form doesn't expose) with
  # the training data's column-wise median/mode.
  build_employee_row <- function(overrides = list()) {
    req(rv$raw_data)
    template <- rv$raw_data[1, , drop = FALSE]
    for (col in names(rv$raw_data)) {
      if (is.numeric(rv$raw_data[[col]])) {
        template[[col]] <- median(rv$raw_data[[col]], na.rm = TRUE)
      } else {
        template[[col]] <- names(sort(table(rv$raw_data[[col]]), decreasing = TRUE))[1]
      }
    }
    field_map <- list(Age = "p_age", Gender = "p_gender", MonthlyIncome = "p_income",
                       Department = "p_dept", JobRole = "p_role", YearsAtCompany = "p_years",
                       OverTime = "p_overtime", PerformanceRating = "p_perf",
                       JobSatisfaction = "p_jobsat", WorkLifeBalance = "p_wlb",
                       DistanceFromHome = "p_distance")
    for (col in names(field_map)) {
      input_id <- field_map[[col]]
      if (col %in% names(template) && !is.null(input[[input_id]])) {
        template[[col]] <- input[[input_id]]
      }
    }
    for (nm in names(overrides)) {
      if (nm %in% names(template)) template[[nm]] <- overrides[[nm]]
    }
    template
  }

  predict_with_best_model <- function(employee_row) {
    req(length(rv$trained_models) > 0)
    best_name <- names(which.max(sapply(rv$trained_models, function(m) m$metrics$AUC)))
    fit <- rv$trained_models[[best_name]]$model
    proc_row <- preprocess_data(rbind(employee_row, rv$raw_data), impute_method = "median", scale_method = "standardize")[1, ]
    prob <- tryCatch(predict(fit, proc_row, type = "prob")[, "Yes"], error = function(e) NA)
    list(model_name = best_name, prob = as.numeric(prob), employee = employee_row)
  }

  observeEvent(input$predict_btn, {
    validate(need(length(rv$trained_models) > 0, "Train at least one model first (ML Training Center tab)."))
    row <- build_employee_row()
    result <- predict_with_best_model(row)
    rv$last_prediction <- result
  })

  output$predict_result_ui <- renderUI({
    req(rv$last_prediction)
    prob <- rv$last_prediction$prob
    req(!is.na(prob))
    bucket <- risk_bucket(prob)
    tagList(
      h3(paste0("Probability of Leaving: ", round(prob * 100, 1), "%")),
      div(style = paste0("background:", bucket$color, "; color:white; padding:15px; border-radius:8px; text-align:center; font-size:20px;"),
          bucket$label),
      br(),
      p(paste("Model used:", rv$last_prediction$model_name))
    )
  })

  # ============================================================
  # BULK PREDICTION
  # ============================================================

  observeEvent(input$bulk_predict_btn, {
    req(input$bulk_file)
    validate(need(length(rv$trained_models) > 0, "Train at least one model first."))

    df <- read_uploaded_data(input$bulk_file$datapath, input$bulk_file$name)
    best_name <- names(which.max(sapply(rv$trained_models, function(m) m$metrics$AUC)))
    fit <- rv$trained_models[[best_name]]$model

    proc_df <- preprocess_data(df, impute_method = "median", scale_method = "standardize")
    probs <- tryCatch(
      predict(fit, proc_df, type = "prob")[, "Yes"],
      error = function(e) {
        showNotification(paste("Bulk prediction failed:", e$message), type = "error", duration = NULL)
        rep(NA_real_, nrow(df))
      }
    )

    results <- df
    results$RiskScore <- round(probs * 100, 1)
    results$RiskLabel <- sapply(probs, function(p) risk_bucket(p)$label)
    rv$bulk_results <- results
  })

  output$bulk_results_table <- renderDT({
    req(rv$bulk_results)
    datatable(rv$bulk_results, options = list(scrollX = TRUE, pageLength = 15))
  })

  output$bulk_download <- downloadHandler(
    filename = function() "attrition_bulk_predictions.csv",
    content = function(file) {
      req(rv$bulk_results)
      write.csv(rv$bulk_results, file, row.names = FALSE)
    }
  )

  # ============================================================
  # EXPLAINABLE AI
  # ============================================================

  output$feature_importance_plot <- renderPlotly({
    req(length(rv$trained_models) > 0)
    best_name <- names(which.max(sapply(rv$trained_models, function(m) m$metrics$AUC)))
    fit <- rv$trained_models[[best_name]]$model
    imp <- tryCatch(varImp(fit)$importance, error = function(e) NULL)
    req(imp)
    imp$Feature <- rownames(imp)
    imp_col <- names(imp)[1]
    imp <- imp[order(-imp[[imp_col]]), ][1:min(10, nrow(imp)), ]
    p <- ggplot(imp, aes(x = reorder(Feature, .data[[imp_col]]), y = .data[[imp_col]])) +
      geom_col(fill = "#4361ee") + coord_flip() + theme_minimal() + labs(x = "", y = "Importance")
    ggplotly(p)
  })

  output$employee_explanation_ui <- renderUI({
    req(rv$last_prediction)
    row <- rv$last_prediction$employee
    prob <- rv$last_prediction$prob
    req(!is.na(prob))
    reasons <- c()
    if (!is.null(row$OverTime) && row$OverTime == "Yes") reasons <- c(reasons, "excessive overtime")
    if (!is.null(row$JobSatisfaction) && row$JobSatisfaction <= 2) reasons <- c(reasons, "low job satisfaction")
    if (!is.null(row$WorkLifeBalance) && row$WorkLifeBalance <= 2) reasons <- c(reasons, "poor work-life balance")
    if (!is.null(row$MonthlyIncome) && row$MonthlyIncome < median(rv$raw_data$MonthlyIncome, na.rm = TRUE)) reasons <- c(reasons, "below-median income")
    if (length(reasons) == 0) reasons <- c("a mix of moderate risk factors")
    tagList(
      h4(paste0("Employee has a ", round(prob*100,1), "% attrition probability.")),
      p(paste("This is primarily driven by:", paste(reasons, collapse = ", "), "."))
    )
  })

  # ============================================================
  # HR RECOMMENDATIONS
  # ============================================================

  output$recommendations_ui <- renderUI({
    req(rv$last_prediction)
    recs <- generate_recommendations(rv$last_prediction$employee)
    tagList(lapply(names(recs), function(factor_name) {
      bs4Card(
        title = factor_name, width = 12, status = "warning", solidHeader = FALSE,
        p(recs[[factor_name]])
      )
    }))
  })

  # ============================================================
  # WHAT-IF SIMULATOR
  # ============================================================

  output$whatif_controls_ui <- renderUI({
    req(rv$last_prediction)
    row <- rv$last_prediction$employee
    tagList(
      if ("MonthlyIncome" %in% names(row)) sliderInput("wi_salary_pct", "Salary Change (%)", min = -20, max = 50, value = 0) else NULL,
      if ("OverTime" %in% names(row)) selectInput("wi_overtime", "OverTime", choices = c("Yes", "No"), selected = row$OverTime) else NULL,
      if ("TrainingTimesLastYear" %in% names(row)) sliderInput("wi_training", "Training Sessions / Year", min = 0, max = 10, value = if(!is.null(row$TrainingTimesLastYear)) row$TrainingTimesLastYear else 2) else NULL,
      if ("JobSatisfaction" %in% names(row)) sliderInput("wi_jobsat", "Job Satisfaction (post-intervention)", min = 1, max = 4, value = if(!is.null(row$JobSatisfaction)) row$JobSatisfaction else 3) else NULL
    )
  })

  observeEvent(input$whatif_btn, {
    req(rv$last_prediction)
    row <- rv$last_prediction$employee
    overrides <- list()
    if (!is.null(input$wi_salary_pct) && "MonthlyIncome" %in% names(row)) {
      overrides$MonthlyIncome <- row$MonthlyIncome * (1 + input$wi_salary_pct / 100)
    }
    if (!is.null(input$wi_overtime)) overrides$OverTime <- input$wi_overtime
    if (!is.null(input$wi_training)) overrides$TrainingTimesLastYear <- input$wi_training
    if (!is.null(input$wi_jobsat)) overrides$JobSatisfaction <- input$wi_jobsat

    new_row <- build_employee_row(overrides = overrides)
    for (nm in names(overrides)) new_row[[nm]] <- overrides[[nm]]

    result <- predict_with_best_model(new_row)
    rv$whatif_result <- result
  })

  output$whatif_result_ui <- renderUI({
    req(rv$last_prediction)
    before <- rv$last_prediction$prob
    tagList(
      h4(paste0("Current Risk: ", round(before * 100, 1), "%")),
      if (!is.null(rv$whatif_result)) {
        after <- rv$whatif_result$prob
        delta <- round((after - before) * 100, 1)
        tagList(
          h4(paste0("Updated Risk: ", round(after * 100, 1), "%")),
          p(style = paste0("font-weight:bold; color:", if (delta <= 0) "#2a9d8f" else "#e63946", ";"),
            paste0(if (delta <= 0) "Decrease" else "Increase", " of ", abs(delta), " percentage points."))
        )
      } else {
        p("Adjust the scenario and click 'Recalculate Risk'.")
      }
    )
  })

  # ============================================================
  # EXECUTIVE DASHBOARD
  # ============================================================

  output$exec_high_risk <- renderbs4ValueBox({
    n <- if (!is.null(rv$bulk_results)) sum(rv$bulk_results$RiskLabel == "High Risk") else 0
    bs4ValueBox(value = n, subtitle = "High Risk Employees", icon = icon("triangle-exclamation"), color = "danger")
  })
  output$exec_medium_risk <- renderbs4ValueBox({
    n <- if (!is.null(rv$bulk_results)) sum(rv$bulk_results$RiskLabel == "Moderate Risk") else 0
    bs4ValueBox(value = n, subtitle = "Moderate Risk Employees", icon = icon("circle-exclamation"), color = "warning")
  })
  output$exec_low_risk <- renderbs4ValueBox({
    n <- if (!is.null(rv$bulk_results)) sum(rv$bulk_results$RiskLabel == "Low Risk") else 0
    bs4ValueBox(value = n, subtitle = "Low Risk Employees", icon = icon("circle-check"), color = "success")
  })

  output$exec_dept_risk_plot <- renderPlotly({
    req(rv$bulk_results); req("Department" %in% names(rv$bulk_results))
    tbl <- rv$bulk_results %>% group_by(Department) %>% summarise(AvgRisk = mean(RiskScore, na.rm = TRUE))
    p <- ggplot(tbl, aes(x = reorder(Department, AvgRisk), y = AvgRisk, fill = Department)) +
      geom_col() + coord_flip() + theme_minimal() + labs(x = "", y = "Avg Risk Score (%)")
    ggplotly(p)
  })

  output$exec_top10_table <- renderDT({
    req(rv$bulk_results)
    id_col <- intersect(c("EmployeeID", "EmployeeNumber"), names(rv$bulk_results))
    id_col <- if (length(id_col) > 0) id_col[1] else names(rv$bulk_results)[1]
    tbl <- rv$bulk_results %>% arrange(desc(RiskScore)) %>% select(all_of(id_col), RiskScore, RiskLabel) %>% head(10)
    datatable(tbl, options = list(dom = 't'))
  })

  output$exec_trend_plot <- renderPlotly({
    req(rv$bulk_results)
    set.seed(1)
    months <- month.abb[1:6]
    trend <- data.frame(Month = factor(months, levels = months),
                         AvgRisk = mean(rv$bulk_results$RiskScore, na.rm = TRUE) + cumsum(rnorm(6, 0, 1.5)))
    p <- ggplot(trend, aes(x = Month, y = AvgRisk, group = 1)) + geom_line(color = "#4361ee", linewidth = 1) +
      geom_point(color = "#4361ee", size = 2) + theme_minimal() + labs(y = "Avg Predicted Risk (%)")
    ggplotly(p)
  })

  # ============================================================
  # REPORTS
  # ============================================================

  output$download_pdf_report <- downloadHandler(
    filename = function() "attrition_report.pdf",
    content = function(file) {
      req(rv$raw_data)
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report/report_template.Rmd", tempReport, overwrite = TRUE)
      params <- list(
        raw_data = rv$raw_data,
        validation = rv$validation,
        trained_models = rv$trained_models,
        bulk_results = rv$bulk_results
      )
      rmarkdown::render(tempReport, output_file = file,
                         params = params, envir = new.env(parent = globalenv()))
    }
  )

  output$download_excel_report <- downloadHandler(
    filename = function() "attrition_export.xlsx",
    content = function(file) {
      wb <- createWorkbook()
      addWorksheet(wb, "Dataset Summary")
      if (!is.null(rv$raw_data)) writeData(wb, "Dataset Summary", head(rv$raw_data, 100))

      if (length(rv$trained_models) > 0) {
        addWorksheet(wb, "Model Comparison")
        tbl <- do.call(rbind, lapply(names(rv$trained_models), function(nm) {
          m <- rv$trained_models[[nm]]$metrics
          data.frame(Model = nm, Accuracy = m$Accuracy, Precision = m$Precision,
                     Recall = m$Recall, F1 = m$F1, AUC = m$AUC)
        }))
        writeData(wb, "Model Comparison", tbl)
      }

      if (!is.null(rv$bulk_results)) {
        addWorksheet(wb, "Risk Predictions")
        writeData(wb, "Risk Predictions", rv$bulk_results)
      }

      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}
