# Description
Bachelor's diploma work focusing on predicting volatility of the Polish WIG20 index and assessing the accuracy of the value at risk (VaR) and expected shortfall (ES) forecasts using models from the GARCH family.

# Content
- Wig20Daily - raw dataset
- analysis.R - analysis, modeling and testing procedure stored as R script
- Research.pdf - main file containing full research description

# Methodology
Final estimates were achieved by applying the Nelson's exponential GARCH model EGARCH(1,1) with the t-Student distribution.

# Findings
The small number of exceedances and independence over time at the 5% and 1% confidence levels for the Estimated Shortfall prove that EGARCH model is able to effectively predict future losses and allow for financial risk management of WIG20 index.