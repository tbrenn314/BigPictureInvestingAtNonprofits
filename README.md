# Replication Code: "Big Picture" Investing at Nonprofits

Replication code for all computational results and tables in:

**"'Big Picture' Investing at Nonprofits: Accounting for Future Expenses and Donations"**  
Thomas J. Brennan and David M. Schizer  
*George Mason Law Review*, 2026

## Overview

This repository contains R code to replicate all numerical examples, tables, and text variables reported in the paper. The analysis examines optimal investment strategies for nonprofits when accounting for uncertain future donations and expenses.

## Requirements

- **R version**: 4.5.1 or higher (code developed with R 4.5.1)
- **Required R packages**: 
  - `knitr`

### Installing Required Packages

```r
install.packages("knitr")
```

### Running the Replication

Open R or RStudio with this repository folder as the working directory, then run:

```r
source("00_main.R")
```

Run `00_main.R` from the repository's top-level folder so it can find the other R files.
