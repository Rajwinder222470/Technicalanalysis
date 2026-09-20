# TechnicalAnalysis

**Student:** Rajwinder Kaur  
**Course:** BDA400 - Data Science Tools and Techniques  
**Assignment:** Assignment 2 - Technical Analysis using R (Preliminary Stage)

## Project purpose
This project demonstrates the preliminary technical-analysis workflow in R and RStudio. It reads a portfolio of stock symbols, imports historical market data, calculates descriptive statistics and a 20-day simple moving average, and produces basic displays/visualizations.

## Files
- `portfolio.txt` - stock symbols, one per line.
- `load_and_statistics.R` - portfolio loading, stock-data import, mode function, moving average and required statistics.
- `display_and_visualizations.R` - display functions and charts.
- `TechnicalAnalysis_Report.docx` - written documentation and rubric checklist.

## How to run
1. Open the project in RStudio.
2. Make sure R and RStudio are installed.
3. Install packages once:
   `install.packages(c("quantmod", "TTR"))`
4. Run `load_and_statistics.R`.
5. Run `display_and_visualizations.R`.
6. Confirm that `statistics_output.csv` is created.
7. Commit and push the project to the GitHub repository named `TechnicalAnalysis`.

## Reproducibility note
The scripts retrieve market data from Yahoo Finance through `quantmod` at runtime. Therefore, the exact numerical output can change depending on the date/time the script is executed.

## Learning resources
The student used course readings and online tutorials to understand concepts before implementing the workflow. Examples are listed in the report.
