# ================================================================
# global.R
# AI-Powered Employee Attrition Prediction System Using R
# Loads packages, defines helper functions and shared constants
# ================================================================

required_packages <- c(
  "shiny", "bs4Dash", "shinyWidgets", "shinycssloaders",
  "dplyr", "tidyr", "DT", "readxl", "readr",
  "plotly", "ggplot2", "corrplot",
  "caret", "randomForest", "e1071", "rpart", "xgboost", "pROC",
  "DALEX", "rmarkdown", "openxlsx", "scales"
)

installed <- rownames(installed.packages())
missing_pkgs <- setdiff(required_packages, installed)
if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}

invisible(lapply(required_packages, library, character.only = TRUE))

# ---------------------------------------------------------------
# Constants
# ---------------------------------------------------------------

APP_TITLE   <- "AI-Powered Employee Attrition Prediction System"
TARGET_COL  <- "Attrition"          # expected binary target column (Yes/No)
SAMPLE_DATA_PATH <- file.path("data", "sample_hr_data.csv")

MODEL_CHOICES <- c(
  "Logistic Regression" = "glm",
  "Decision Tree"       = "rpart",
  "Random Forest"       = "rf",
  "Support Vector Machine" = "svmRadial",
  "XGBoost"             = "xgbTree"
)

# ---------------------------------------------------------------
# Helper: safely read an uploaded dataset (csv or excel)
# ---------------------------------------------------------------
read_uploaded_data <- function(filepath, filename) {
  ext <- tolower(tools::file_ext(filename))
  df <- switch(ext,
    csv  = readr::read_csv(filepath, show_col_types = FALSE),
    tsv  = readr::read_tsv(filepath, show_col_types = FALSE),
    xlsx = readxl::read_excel(filepath),
    xls  = readxl::read_excel(filepath),
    stop("Unsupported file type: ", ext)
  )
  as.data.frame(df)
}

# ---------------------------------------------------------------
# Helper: basic dataset validation summary
# ---------------------------------------------------------------
validate_dataset <- function(df) {
  n_missing   <- sum(is.na(df))
  n_dupes     <- sum(duplicated(df))
  n_rows      <- nrow(df)
  n_cols      <- ncol(df)
  numeric_cols <- names(df)[sapply(df, is.numeric)]

  outlier_count <- 0
  if (length(numeric_cols) > 0) {
    outlier_count <- sum(sapply(df[numeric_cols], function(x) {
      q <- quantile(x, probs = c(.25, .75), na.rm = TRUE)
      iqr <- q[2] - q[1]
      sum(x < (q[1] - 1.5 * iqr) | x > (q[2] + 1.5 * iqr), na.rm = TRUE)
    }))
  }

  # simple 0-100 quality score
  penalty <- (n_missing / max(n_rows * n_cols, 1)) * 50 +
             (n_dupes / max(n_rows, 1)) * 30 +
             (outlier_count / max(n_rows * max(length(numeric_cols),1), 1)) * 20
  quality_score <- round(max(0, 100 - penalty * 100), 1)
  quality_score <- min(quality_score, 100)

  list(
    n_rows = n_rows, n_cols = n_cols,
    n_missing = n_missing, n_dupes = n_dupes,
    outlier_count = outlier_count,
    quality_score = quality_score,
    has_target = TARGET_COL %in% names(df)
  )
}

# ---------------------------------------------------------------
# Helper: preprocess dataset for modeling
# ---------------------------------------------------------------
preprocess_data <- function(df, target = TARGET_COL,
                             impute_method = "median",
                             scale_method = "standardize") {

  df <- as.data.frame(df)

  # 1. Missing value handling
  for (col in names(df)) {
    if (any(is.na(df[[col]]))) {
      if (is.numeric(df[[col]])) {
        val <- switch(impute_method,
          mean   = mean(df[[col]], na.rm = TRUE),
          median = median(df[[col]], na.rm = TRUE),
          mode   = as.numeric(names(sort(table(df[[col]]), decreasing = TRUE))[1]),
          NA
        )
        if (impute_method == "delete") next
        df[[col]][is.na(df[[col]])] <- val
      } else {
        mode_val <- names(sort(table(df[[col]]), decreasing = TRUE))[1]
        df[[col]][is.na(df[[col]])] <- mode_val
      }
    }
  }
  if (impute_method == "delete") df <- na.omit(df)

  # 2. Encode target
  if (target %in% names(df)) {
    df[[target]] <- factor(df[[target]], levels = c("No", "Yes"))
  }

  # 3. Encode categorical predictors as factors (model handles dummy encoding)
  cat_cols <- names(df)[sapply(df, function(x) is.character(x) || is.logical(x))]
  cat_cols <- setdiff(cat_cols, target)
  for (col in cat_cols) df[[col]] <- as.factor(df[[col]])

  # 4. Scale numeric predictors
  num_cols <- names(df)[sapply(df, is.numeric)]
  if (scale_method != "none" && length(num_cols) > 0) {
    pp <- preProcess(df[num_cols],
                      method = if (scale_method == "standardize") c("center","scale") else "range")
    df[num_cols] <- predict(pp, df[num_cols])
  }

  df
}

# ---------------------------------------------------------------
# Helper: drop ID-like columns before modeling (e.g. EmployeeID) so
# they aren't treated as huge, meaningless categorical predictors
# ---------------------------------------------------------------
drop_id_columns <- function(df, target = TARGET_COL) {
  is_id <- sapply(names(df), function(col) {
    if (col == target) return(FALSE)
    if (grepl("id$", col, ignore.case = TRUE) || grepl("^id", col, ignore.case = TRUE)) return(TRUE)
    if (!is.numeric(df[[col]]) && length(unique(df[[col]])) >= 0.9 * nrow(df)) return(TRUE)
    FALSE
  })
  df[, !is_id, drop = FALSE]
}

# ---------------------------------------------------------------
# Helper: risk level bucket + colour
# ---------------------------------------------------------------
risk_bucket <- function(prob) {
  if (is.na(prob)) return(list(label = "Unknown", color = "#adb5bd"))
  if (prob >= 0.6) return(list(label = "High Risk", color = "#e63946"))
  if (prob >= 0.3) return(list(label = "Moderate Risk", color = "#f4a300"))
  list(label = "Low Risk", color = "#2a9d8f")
}

# ---------------------------------------------------------------
# Helper: HR recommendation engine (rule-based)
# ---------------------------------------------------------------
generate_recommendations <- function(employee_row) {
  recs <- list()
  if (!is.null(employee_row$OverTime) && employee_row$OverTime == "Yes") {
    recs[["High Overtime"]] <- "Introduce flexible work schedules or redistribute workload to reduce overtime."
  }
  if (!is.null(employee_row$MonthlyIncome) && employee_row$MonthlyIncome < 3500) {
    recs[["Below-Market Salary"]] <- "Consider a compensation review benchmarked against market rates."
  }
  if (!is.null(employee_row$JobSatisfaction) && employee_row$JobSatisfaction <= 2) {
    recs[["Low Job Satisfaction"]] <- "Conduct 1:1 check-ins and employee engagement programs."
  }
  if (!is.null(employee_row$WorkLifeBalance) && employee_row$WorkLifeBalance <= 2) {
    recs[["Poor Work-Life Balance"]] <- "Evaluate workload distribution and offer flexible/remote options."
  }
  if (!is.null(employee_row$YearsAtCompany) && employee_row$YearsAtCompany <= 2) {
    recs[["Early Tenure Risk"]] <- "Strengthen onboarding and assign a mentor for first two years."
  }
  if (!is.null(employee_row$DistanceFromHome) && employee_row$DistanceFromHome > 15) {
    recs[["Long Commute"]] <- "Offer remote/hybrid work options to offset commute burden."
  }
  if (length(recs) == 0) {
    recs[["Stable Profile"]] <- "No major risk factors detected — maintain current engagement practices."
  }
  recs
}
