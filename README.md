# Machine Learning for Macroeconomic Regime Identification in India

An R-based project to identify shifts in macroeconomic regimes using machine learning methods. Currently based on 'Taylor-rule' variables - real output, inflation, and short-term interest rates. 

## Core Methodology
* **Feature Engineering:** Seasonal Adjustment, Y-o-Y changes, scaling for (quarterly) data to capture macro-financial 'regimes'.
* **Modeling Choice:** K-Means Clustering and Gaussian Mixture Models (GMM) implemented via native R packages.
* **Indicative Output:** Visualizes cycle transitions and structural breaks to assist in predictive policy analysis.

## Project Status
* **Core Framework:** WIP - `macro_regimes.R` has code for K-Means and GMM but robustness checks remain.
* **Next Steps:** Corrections for 'shocks' (COVID, base-year, and structural policy changes) required, more variables, cleaning up of code, and documentation. 
