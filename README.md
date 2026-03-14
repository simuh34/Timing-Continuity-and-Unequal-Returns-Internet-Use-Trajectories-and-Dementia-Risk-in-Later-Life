# Timing, Continuity, and Unequal Returns: Internet Use Trajectories and Dementia Risk in Later Life

# Overview
This project utilizes 18 years of panel data from the U.S. Health and Retirement Study to examine whether longitudinal internet-use trajectories—classified as Consistent Users, Adopters, Intermittent Users, Dropouts, and Never Users—predict subsequent dementia among 9,317 adults aged 50 and older. Guided by Diffusion of Innovations theory and digital divide scholarship, the analysis demonstrates that relative to Never Users, the hazard of developing dementia is nearly 60% lower among Consistent Users and Adopters and 48% lower among Intermittent Users, while associations vary by gender, race/ethnicity, urbanicity, and wealth through patterns of resource amplification and resource compensation. The findings underscore that digital engagement is a critical modifiable factor for healthy aging, suggesting that public health interventions must move beyond one-time access to support sustained engagement, prevent withdrawal, and facilitate re-engagement. Analysis code, methodology based on Cox proportional hazards models with inverse probability of treatment weighting, and supplementary materials are included.

# Requirement
R version 4.2 or higher.

# Scripts descriptions
- The file "HRS 01 Data Extraction.R" extract raw data from HRS survey databases for subsequent analysis.
- The file "HRS 02 Data Cleaning.R" conduct data cleaning on the extracted datasets to generate clean, analysis-ready datasets.
- The file "HRS 03 Dementia.R", outline the methodology employed to process and analyze data related to dementia in this project.
- The file "HRS 04 Sensitivity Analysis.R" conduct sensitivity analysis.
- The file "HRS 05 Forest Plot.R" is designed to generate forest plots for model results.
