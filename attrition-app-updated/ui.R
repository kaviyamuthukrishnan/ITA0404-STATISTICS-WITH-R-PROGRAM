# ================================================================
# ui.R
# ================================================================

ui <- bs4DashPage(
  title = APP_TITLE,
  fullscreen = TRUE,
  dark = NULL,
  
  header = bs4DashNavbar(
    title = bs4DashBrand(
      title = span("Attrition AI", style = "font-weight:700;"),
      color = "primary",
      href = "#"
    ),
    status = "white",
    fixed = FALSE
  ),
  
  sidebar = bs4DashSidebar(
    status = "primary",
    elevation = 3,
    bs4SidebarMenu(
      id = "sidebar",
      bs4SidebarMenuItem("Home", tabName = "home", icon = icon("house")),
      bs4SidebarMenuItem("Dataset Center", tabName = "dataset", icon = icon("database")),
      bs4SidebarMenuItem("EDA Dashboard", tabName = "eda", icon = icon("chart-simple")),
      bs4SidebarMenuItem("ML Training Center", tabName = "training", icon = icon("brain")),
      bs4SidebarMenuItem("Model Evaluation", tabName = "evaluation", icon = icon("check-double")),
      bs4SidebarMenuItem("Predict Attrition", tabName = "predict", icon = icon("search")),
      bs4SidebarMenuItem("Bulk Prediction", tabName = "bulk", icon = icon("table-list")),
      bs4SidebarMenuItem("Explainable AI", tabName = "xai", icon = icon("lightbulb")),
      bs4SidebarMenuItem("HR Recommendations", tabName = "recommend", icon = icon("comments")),
      bs4SidebarMenuItem("What-If Simulator", tabName = "whatif", icon = icon("sliders")),
      bs4SidebarMenuItem("Executive Dashboard", tabName = "executive", icon = icon("chart-line")),
      bs4SidebarMenuItem("Report", tabName = "report", icon = icon("file-pdf")),
      bs4SidebarMenuItem("About / Team", tabName = "about", icon = icon("users"))
    )
  ),
  
  controlbar = NULL,
  
  footer = bs4DashFooter(
    left = "AI-Powered Employee Attrition Prediction System — R Programming Project",
    right = "Built with R Shiny"
  ),
  
  body = bs4DashBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    bs4TabItems(
      
      # ---------------- HOME ----------------
      bs4TabItem(
        tabName = "home",
        fluidRow(
          column(
            width = 12,
            div(class = "hero-banner",
                h1("Predict employee turnover before it happens."),
                p("Empower HR teams with AI-driven workforce analytics and retention strategies."),
                actionButton("go_dataset", "Get Started", class = "btn-hero", icon = icon("arrow-right"))
            )
          )
        ),
        fluidRow(
          bs4ValueBoxOutput("vb_total_employees", width = 3),
          bs4ValueBoxOutput("vb_attrition_rate", width = 3),
          bs4ValueBoxOutput("vb_high_risk", width = 3),
          bs4ValueBoxOutput("vb_model_accuracy", width = 3)
        ),
        fluidRow(
          bs4Card(
            title = "Why This Matters", width = 12, status = "primary", solidHeader = TRUE,
            fluidRow(
              column(3, div(class = "benefit-box", icon("sack-dollar"), h5("Reduce Hiring Costs"))),
              column(3, div(class = "benefit-box", icon("heart"), h5("Improve Retention"))),
              column(3, div(class = "benefit-box", icon("magnifying-glass-chart"), h5("Identify Risk Factors"))),
              column(3, div(class = "benefit-box", icon("brain"), h5("Support HR Decisions")))
            )
          )
        )
      ),
      
      # ---------------- DATASET CENTER ----------------
      bs4TabItem(
        tabName = "dataset",
        fluidRow(
          bs4Card(
            title = "1. Upload Dataset", width = 4, status = "primary", solidHeader = TRUE,
            fileInput("file_upload", "Upload CSV / Excel", accept = c(".csv", ".xlsx", ".xls")),
            actionButton("load_sample", "Or Load Sample Dataset", icon = icon("flask"), class = "btn-outline-primary w-100")
          ),
          bs4Card(
            title = "2. Dataset Quality Score", width = 8, status = "success", solidHeader = TRUE,
            withSpinner(uiOutput("quality_score_ui"))
          )
        ),
        fluidRow(
          bs4Card(
            title = "Dataset Summary", width = 12, status = "info", solidHeader = TRUE,
            withSpinner(verbatimTextOutput("dataset_summary"))
          )
        ),
        fluidRow(
          bs4Card(
            title = "Dataset Preview (first 20 rows)", width = 12, status = "secondary", solidHeader = TRUE,
            withSpinner(DTOutput("dataset_preview"))
          )
        )
      ),
      
      # ---------------- EDA ----------------
      bs4TabItem(
        tabName = "eda",
        fluidRow(
          bs4Card(
            title = "Filters", width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
            fluidRow(
              column(3, uiOutput("filter_department_ui")),
              column(3, uiOutput("filter_gender_ui")),
              column(3, uiOutput("filter_age_ui")),
              column(3, uiOutput("filter_salary_ui"))
            )
          )
        ),
        fluidRow(
          bs4Card(title = "Age Distribution", width = 6, withSpinner(plotlyOutput("plot_age_dist"))),
          bs4Card(title = "Gender Distribution", width = 6, withSpinner(plotlyOutput("plot_gender_dist")))
        ),
        fluidRow(
          bs4Card(title = "Department-wise Employees", width = 6, withSpinner(plotlyOutput("plot_dept_dist"))),
          bs4Card(title = "Salary Distribution", width = 6, withSpinner(plotlyOutput("plot_salary_dist")))
        ),
        fluidRow(
          bs4Card(title = "Attrition by Department", width = 6, withSpinner(plotlyOutput("plot_attr_dept"))),
          bs4Card(title = "Attrition by Overtime", width = 6, withSpinner(plotlyOutput("plot_attr_overtime")))
        ),
        fluidRow(
          bs4Card(title = "Correlation Heatmap", width = 12, withSpinner(plotOutput("plot_corr_heatmap", height = "500px")))
        )
      ),
      
      # ---------------- ML TRAINING ----------------
      bs4TabItem(
        tabName = "training",
        fluidRow(
          bs4Card(
            title = "Preprocessing Options", width = 4, status = "primary", solidHeader = TRUE,
            selectInput("impute_method", "Missing Value Handling",
                        choices = c("Median Imputation" = "median", "Mean Imputation" = "mean",
                                    "Mode Imputation" = "mode", "Delete Missing Records" = "delete")),
            selectInput("scale_method", "Feature Scaling",
                        choices = c("Standardization" = "standardize", "Normalization" = "normalize", "None" = "none")),
            sliderInput("train_ratio", "Training Set Ratio", min = 0.5, max = 0.9, value = 0.8, step = 0.05),
            selectInput("model_choice", "Select Model", choices = MODEL_CHOICES),
            actionButton("train_model_btn", "Train Model", icon = icon("play"), class = "btn-primary w-100")
          ),
          bs4Card(
            title = "Training Status", width = 8, status = "success", solidHeader = TRUE,
            withSpinner(verbatimTextOutput("training_status"))
          )
        ),
        fluidRow(
          bs4Card(
            title = "Trained Models Summary", width = 12, status = "secondary", solidHeader = TRUE,
            withSpinner(DTOutput("trained_models_table"))
          )
        )
      ),
      
      # ---------------- MODEL EVALUATION ----------------
      bs4TabItem(
        tabName = "evaluation",
        fluidRow(
          column(4, uiOutput("eval_model_selector_ui"))
        ),
        fluidRow(
          bs4Card(title = "Confusion Matrix", width = 6, withSpinner(verbatimTextOutput("confusion_matrix_out"))),
          bs4Card(title = "Performance Metrics", width = 6, withSpinner(uiOutput("metrics_ui")))
        ),
        fluidRow(
          bs4Card(title = "ROC Curve", width = 6, withSpinner(plotlyOutput("roc_curve_plot"))),
          bs4Card(title = "Model Comparison", width = 6, withSpinner(plotlyOutput("model_comparison_plot")))
        ),
        fluidRow(
          bs4Card(title = "Model Comparison Table", width = 12, withSpinner(DTOutput("model_comparison_table")))
        )
      ),
      
      # ---------------- INDIVIDUAL PREDICTION ----------------
      bs4TabItem(
        tabName = "predict",
        fluidRow(
          bs4Card(
            title = "Employee Details", width = 5, status = "primary", solidHeader = TRUE,
            uiOutput("predict_form_ui"),
            actionButton("predict_btn", "Predict Attrition Risk", class = "btn-primary w-100", icon = icon("magnifying-glass-chart"))
          ),
          bs4Card(
            title = "Prediction Result", width = 7, status = "warning", solidHeader = TRUE,
            withSpinner(uiOutput("predict_result_ui"))
          )
        )
      ),
      
      # ---------------- BULK PREDICTION ----------------
      bs4TabItem(
        tabName = "bulk",
        fluidRow(
          bs4Card(
            title = "Upload Employee Dataset for Bulk Scoring", width = 12, status = "primary", solidHeader = TRUE,
            fileInput("bulk_file", "Upload CSV / Excel", accept = c(".csv", ".xlsx", ".xls")),
            actionButton("bulk_predict_btn", "Run Bulk Prediction", class = "btn-primary", icon = icon("play")),
            downloadButton("bulk_download", "Export Results")
          )
        ),
        fluidRow(
          bs4Card(title = "Bulk Prediction Results", width = 12, withSpinner(DTOutput("bulk_results_table")))
        )
      ),
      
      # ---------------- EXPLAINABLE AI ----------------
      bs4TabItem(
        tabName = "xai",
        fluidRow(
          bs4Card(title = "Global Feature Importance", width = 12, status = "primary", solidHeader = TRUE,
                  withSpinner(plotlyOutput("feature_importance_plot", height = "450px")))
        ),
        fluidRow(
          bs4Card(
            title = "Employee-Level Explanation", width = 12, status = "info", solidHeader = TRUE,
            p("Run a prediction on the Predict Attrition tab first, then view its explanation here."),
            withSpinner(uiOutput("employee_explanation_ui"))
          )
        )
      ),
      
      # ---------------- RECOMMENDATIONS ----------------
      bs4TabItem(
        tabName = "recommend",
        fluidRow(
          bs4Card(
            title = "HR Recommendation Engine", width = 12, status = "primary", solidHeader = TRUE,
            p("Recommendations are generated automatically for the most recent prediction."),
            withSpinner(uiOutput("recommendations_ui"))
          )
        )
      ),
      
      # ---------------- WHAT-IF ----------------
      bs4TabItem(
        tabName = "whatif",
        fluidRow(
          bs4Card(
            title = "Adjust Scenario", width = 5, status = "primary", solidHeader = TRUE,
            uiOutput("whatif_controls_ui"),
            actionButton("whatif_btn", "Recalculate Risk", class = "btn-primary w-100", icon = icon("rotate"))
          ),
          bs4Card(
            title = "Before vs After", width = 7, status = "success", solidHeader = TRUE,
            withSpinner(uiOutput("whatif_result_ui"))
          )
        )
      ),
      
      # ---------------- EXECUTIVE DASHBOARD ----------------
      bs4TabItem(
        tabName = "executive",
        fluidRow(
          bs4ValueBoxOutput("exec_high_risk", width = 4),
          bs4ValueBoxOutput("exec_medium_risk", width = 4),
          bs4ValueBoxOutput("exec_low_risk", width = 4)
        ),
        fluidRow(
          bs4Card(title = "Department-wise Risk", width = 6, withSpinner(plotlyOutput("exec_dept_risk_plot"))),
          bs4Card(title = "Top 10 High-Risk Employees", width = 6, withSpinner(DTOutput("exec_top10_table")))
        ),
        fluidRow(
          bs4Card(title = "Predicted Risk Trend", width = 12, withSpinner(plotlyOutput("exec_trend_plot")))
        )
      ),
      
      # ---------------- REPORT ----------------
      bs4TabItem(
        tabName = "report",
        fluidRow(
          bs4Card(
            title = "Generate Report", width = 12, status = "primary", solidHeader = TRUE,
            p("Download a full PDF report covering dataset summary, analytics, predictions and recommendations."),
            downloadButton("download_pdf_report", "Download PDF Report", class = "btn-primary"),
            br(), br(),
            downloadButton("download_excel_report", "Download Excel Export", class = "btn-outline-primary")
          )
        )
      ),
      
      # ---------------- ABOUT / TEAM ----------------
      bs4TabItem(
        tabName = "about",
        fluidRow(
          bs4Card(
            title = "Project Documentation", width = 12, status = "primary", solidHeader = TRUE,
            h4("Problem Statement"),
            p("Employee attrition is costly and disruptive. This system predicts attrition risk using machine learning to enable proactive retention strategies."),
            h4("Objectives"),
            tags$ul(
              tags$li("Predict individual and organization-wide attrition risk"),
              tags$li("Compare multiple ML algorithms for best performance"),
              tags$li("Explain predictions with interpretable AI"),
              tags$li("Recommend actionable retention strategies")
            ),
            h4("Methodology"),
            p("Data upload → validation → preprocessing → EDA → model training → evaluation → prediction → explanation → recommendation."),
            h4("Algorithms Used"),
            p("Logistic Regression, Decision Tree, Random Forest, Support Vector Machine, XGBoost.")
          )
        ),
        fluidRow(
          bs4Card(
            title = "Team Contribution", width = 12, status = "info", solidHeader = TRUE,
            fluidRow(
              column(4, div(class = "team-card",
                            h5("Member 1 — Data Science Lead"),
                            tags$ul(tags$li("Dataset collection"), tags$li("Data preprocessing"),
                                    tags$li("EDA development"), tags$li("Statistical analysis")))),
              column(4, div(class = "team-card",
                            h5("Member 2 — Machine Learning Lead"),
                            tags$ul(tags$li("Model development"), tags$li("Model tuning"),
                                    tags$li("Performance evaluation"), tags$li("Explainable AI")))),
              column(4, div(class = "team-card",
                            h5("Member 3 — Full-Stack & Dashboard Lead"),
                            tags$ul(tags$li("R Shiny development"), tags$li("UI/UX design"),
                                    tags$li("Report generation"), tags$li("Deployment"))))
            )
          )
        )
      )
    )
  )
)