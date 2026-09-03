# AI-Powered Employee Attrition Prediction System Using R

R Shiny web application for the R Programming course project. Built with `bs4Dash` for a
polished, professional dashboard UI.

## Project Structure

```
attrition-app/
├── app.R                    # entry point
├── global.R                 # packages, constants, helper functions
├── ui.R                     # dashboard UI (bs4Dash)
├── server.R                 # all reactive/server logic
├── www/
│   └── custom.css           # styling
├── report/
│   └── report_template.Rmd  # PDF report template
└── data/
    └── sample_hr_data.csv   # bundled sample dataset (1470 synthetic employees)
```

## Modules Implemented

1. **Home** — hero banner, key stat cards, benefits section
2. **Dataset Center** — upload CSV/Excel or load sample data, validation (missing values,
   duplicates, outliers), quality score, preview table
3. **EDA Dashboard** — demographics, workforce charts, attrition breakdowns, correlation
   heatmap, interactive filters
4. **ML Training Center** — preprocessing options (imputation, scaling, train/test split) +
   Logistic Regression, Decision Tree, Random Forest, SVM, XGBoost (via `caret`)
5. **Model Evaluation** — confusion matrix, accuracy/precision/recall/F1/AUC, ROC curve,
   model comparison table + chart
6. **Predict Attrition** — individual employee risk prediction with risk-level indicator
7. **Bulk Prediction** — score an uploaded employee dataset in bulk, export results
8. **Explainable AI** — global feature importance + plain-language per-employee explanation
9. **HR Recommendations** — rule-based retention recommendations for the last prediction
10. **What-If Simulator** — adjust salary/overtime/training/satisfaction and see risk change
11. **Executive Dashboard** — risk category counts, department-wise risk, top-10 high-risk
    leaderboard, trend chart
12. **Report** — downloadable PDF (via `rmarkdown`) and Excel (via `openxlsx`) exports
13. **About / Team** — problem statement, objectives, methodology, team contribution page

## Running Locally

1. Install R (>= 4.2) and RStudio (recommended).
2. Open the `attrition-app` folder as your working directory / RStudio project.
3. Run:

```r
shiny::runApp()
```

On first run, `global.R` will auto-install any missing packages. This can take a few minutes.

## Using the App

1. Go to **Dataset Center** → click **"Load Sample Dataset"** (or upload your own CSV/Excel
   with an `Attrition` column of Yes/No values).
2. Explore **EDA Dashboard** for visual insights.
3. Go to **ML Training Center**, pick a model, click **Train Model**. Repeat for each
   algorithm you want to compare.
4. Check **Model Evaluation** to compare metrics and pick the best model (the app
   automatically uses the highest-AUC trained model for predictions).
5. Use **Predict Attrition** for a single employee, or **Bulk Prediction** to score a
   whole file.
6. View **Explainable AI** and **HR Recommendations** for the last prediction made.
7. Try **What-If Simulator** to test retention interventions.
8. Check **Executive Dashboard** after running a bulk prediction.
9. Download the **Report** (PDF/Excel) for submission or presentation.

## Deploying (shinyapps.io — free tier)

```r
install.packages("rsconnect")
rsconnect::setAccountInfo(name = "<your-account>", token = "<token>", secret = "<secret>")
rsconnect::deployApp("path/to/attrition-app")
```

Get your token/secret from https://www.shinyapps.io/admin/#/tokens after creating a free
account.

## Notes for the Team

- **Data Science Lead**: `global.R` (validation, preprocessing) + EDA section of `server.R`
  are the best starting points to extend/tune.
- **ML Lead**: `ML Training Center` and `Model Evaluation` sections of `server.R` — easy to
  add more models by adding entries to `MODEL_CHOICES` in `global.R` (any `caret`-supported
  method works).
- **Full-Stack/Dashboard Lead**: `ui.R` and `www/custom.css` control layout and styling;
  `report/report_template.Rmd` controls the PDF report.
- Dataset must have an `Attrition` column with values `Yes`/`No` for ML modules to work.
  The sample dataset (`data/sample_hr_data.csv`) follows this format and mirrors the
  classic IBM HR Attrition dataset's fields, so it's a good reference schema.
