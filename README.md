# CELF-5 Shiny Assessment Tool

## Overview
A Shiny web application for administering and scoring the CELF-5 (Clinical Evaluation of Language Fundamentals — Fifth Edition) assessment.

## Authors
Elisabeth H. Wiig, Eleanor Semel, Wayne A. Secord
© 2013 NCS Pearson, Inc. All rights reserved.

## Structure
```
celf5_shiny/
├── app.R              # Main Shiny application (UI + Server)
├── global.R           # Norm tables, scoring functions, subtest definitions
├── report_celf5.Rmd   # Word report template (rmarkdown)
└── README.md
```

## Setup
```r
# Install required packages
install.packages(c("shiny","shinyjs","dplyr","tidyr","DT","flextable","officedown","rmarkdown","ggplot2"))

# Run the app
shiny::runApp(".")
```

## Age Groups
- Ages 5–8: Core SC, LC, WS, WC, FD, FS, RS + Supplementary RC, SW
- Ages 9–12: Core WC, FD, FS, RS, SR + Supplementary RC, SW
- Ages 13–21: Core FS, RS, USP, WC, SR + Supplementary RC, SW

## Scoring Flow
1. Examiner enters student demographics → age calculated automatically
2. Examiner selects age-appropriate tab
3. Examiner answers each item (MC, point-to, free response, rating)
4. Click "Calculate & Generate Report" → scaled scores + index scores computed
5. Download Word report

## ⚠️ Important Disclaimer
This tool is for **authorized clinical use only**. Norm tables are derived from the CELF-5 Examiner's Manual (© 2013 NCS Pearson, Inc.). This application does not replace the official CELF-5 materials or professional clinical judgment.
