# Analysis Templates

Contains customized visualization templates for expedited graphics creation. Sections in the templates.r script will be added based on the type of data the graphics are for. All visualizations will be tested with the mtcars and iris data set. Additional data sets may be added depending on visualization need (time series data etc.). Visualization types will be added to this readme as they are implemented.

Current languages:
- R (primary)
- SAS

Future languages:
- Python
- STATA

## R

### Analysis.r - templates for common analysis techniques

This file incorporates templates and functions from Colors.R, Functions.R, and Cleaning.R

- Regression
  - Simple linear regression
    - Diagnostics of model assumptions
    - Transforming variables (y & x)
  - Logistic regression
    - Diagnostics of model assumptions
    - AUC - ROC curves
  - Multivariate regression
    - Diagnostics of model assumptions
    - Interaction assessments
    - Predicted outcome using dummy dataframes

### Cleaning.r - commonly used data cleaning commands

- factor reordering
- df value matching
- content filtering

### Colors.r - clean colors and useful symbols

- ggplot preferred bold/muted colors
- ggplot preferred shapes
- ascii codes for stats symbols (squared, mu)

### Functions.r

- Plot_Relabeller: Relabels plots to include r2 and p-values (used in facet scenarios)
- Plot_Predictor: Prints scatter plots given a significant association in a linear model
- Model_Diagnostics:Print model diagnostics and transformation changes
- Handle_Outliers:Creates new vars that exclude outliers
- LR_Test: LR test on a numeric var
- Cat_Test: Kruskal tests on a categorical var
- Shapiro_Get: Shapiro p-values for a series of variables
- Lassofier: LASSO analysis to identify likely significant predictors
- Regressifier: linear regression on significant predictors identified in LASSO

### Graphics.r

- Histograms (color, no background, minimal)
- Box plots (color, no background, minimal)
  - Faceted box plots
  - Horizontal box plots
  - Box plots with significance indicators
- Scatter plots (color, no background, minimal)
  - Faceted scatter plots
  - Scatter plots with linear regression

### Markdown.rmd

- YAML options
- Setup options
- Chunk options
- Latex options

## SAS

### Analysis.sas

Introductory analysis & file manipulation. More to come

### Graphics.sas

Coming soon
