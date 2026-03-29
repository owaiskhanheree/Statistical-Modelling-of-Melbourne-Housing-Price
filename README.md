# Statistical Modelling of Melbourne Housing Price

Statistical modelling of Melbourne housing prices using multiple linear regression in R/RStudio, with interactive Tableau visualizations.

## Project Overview

This project analyzes and models housing prices in Melbourne using multiple linear regression, interaction effects, feature selection, and model diagnostics. The goal is to identify key price drivers and build a reliable predictive model for housing prices.

## Key Results

| Metric | Value |
|--------|-------|
| **Best Model** | Stepwise Regression |
| **R-squared** | 0.87 |
| **AIC** | 5058.61 |
| **Full Model R-squared** | 0.88 |
| **Top Price Predictors** | Bathrooms, Rooms, Distance to CBD |

## Key Questions Explored

- Which regression model best predicts house prices?
- How do rooms and distance relate to price?
- Do property types differ in pricing patterns?
- How do bathrooms influence price?
- Do room effects interact with region (Southern Metropolitan)?

## Methods & Modeling

### Feature Engineering
- Centered variables for Rooms and Distance (subtracting the mean)
- Binary indicators for property type (Unit, Townhouse) and region (Southern Metropolitan)
- Pairwise interaction terms among predictors

### Models Compared
1. **Full model** — wide set of predictors plus interactions (R² = 0.88)
2. **Additive-only model** — no interactions
3. **Interaction model** — includes many interaction terms

### Model Selection
- Best subset selection via `leaps::regsubsets`
- Backward/forward selection and stepwise regression via `step()`
- **Stepwise model selected** — AIC = 5058.61, R² = 0.87 (outperformed backward AIC = 5166.8 and forward AIC = 5093.2)

### Diagnostics & Transformations
- Normality checks (QQ plot + Shapiro-Wilk test)
- Breusch-Pagan test for heteroscedasticity
- Cook's distance for influential observations
- Box-Cox transformation analysis (AIC improved from 5628.18 to 79.32)

## Tools & Technologies

- **R / RStudio** — Statistical modeling and analysis
- **Tableau** — Data visualization and dashboard design
- **Statistical Methods** — Multiple regression, stepwise selection, Box-Cox transformation
- **R Packages** — ggplot2, leaps, lmtest, MASS, gridExtra

## Repository Contents

| File | Description |
|------|-------------|
| `project_report.pdf` | Full project report with methodology, results, and conclusions |
| `README.md` | Project documentation |

## Collaborators

This project was completed as part of the Linear Regression & Time Series course at Clark University.

- **Muhammad Owais Khan** — [GitHub](https://github.com/owaiskhanheree) · [LinkedIn](https://www.linkedin.com/in/owaiskhann/)
- **Satyaki Mitra** — [GitHub](https://github.com/retronoob99) · [LinkedIn](https://www.linkedin.com/in/satyaki1/)

## License

MIT License
