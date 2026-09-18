################################################################################
# 00_main.R
# 
# Main script to replicate all results from:
#   "'Big Picture' Investing at Nonprofits: 
#    Accounting for Future Expenses and Donations"
#   by Thomas J. Brennan and David M. Schizer
#   George Mason Law Review, 2026
#
# USAGE:
#   To replicate all results: source("00_main.R")
#
# OUTPUT:
#   - Prints all formatted tables to console
#   - Creates all result objects in global environment
#   - Prints session info for reproducibility
#
# FILES SOURCED (in order):
#   01_functions.R              - Loads packages and defines functions
#   02_examples_main.R          - Calculates Examples 1-6 (Section IV)
#   03_examples_intertemporal.R - Calculates intertemporal examples (Section V)
#   04_output_formatting.R      - Formats and prints all results
################################################################################

# Clear workspace (optional - comment out if you want to preserve existing objects)
# rm(list = ls())

# Record start time
start_time <- Sys.time()
cat("=======================================================\n")
cat("Replication Code: 'Big Picture' Investing at Nonprofits\n")
cat("=======================================================\n")
cat("Start time:", format(start_time), "\n\n")

# Check if required files exist
required_files <- c("01_functions.R", 
                    "02_examples_main.R", 
                    "03_examples_intertemporal.R",
                    "04_output_formatting.R")

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("ERROR: The following required files are missing:\n  ",
       paste(missing_files, collapse = "\n  "),
       "\n\nPlease ensure all files are in the working directory.")
}

# Create output directory (optional - uncomment to save results to files)
# if (!dir.exists("output")) {
#   dir.create("output")
#   cat("Created 'output' directory for results\n\n")
# }

################################################################################
## Source all files in order
################################################################################

cat("Step 1: Loading functions and packages...\n")
source("01_functions.R")
cat("  ? Functions loaded successfully\n\n")

cat("Step 2: Calculating main examples (Section IV)...\n")
source("02_examples_main.R")
cat("  ? Examples 1-6 calculated\n\n")

cat("Step 3: Calculating intertemporal examples (Section V)...\n")
source("03_examples_intertemporal.R")
cat("  ? Intertemporal examples calculated\n\n")

cat("Step 4: Formatting and printing results...\n")
source("04_output_formatting.R")
cat("  ? All results formatted and printed\n\n")

################################################################################
## Print completion message and session info
################################################################################

end_time <- Sys.time()
elapsed_time <- difftime(end_time, start_time, units = "secs")

cat("\n=======================================================\n")
cat("REPLICATION COMPLETE\n")
cat("=======================================================\n")
cat("Elapsed time:", round(elapsed_time, 2), "seconds\n")
cat("End time:", format(end_time), "\n\n")

cat("All results are now available in the global environment.\n")
cat("Formatted tables have been printed above.\n\n")

# Print session info for reproducibility
cat("\n=======================================================\n")
cat("SESSION INFO FOR REPRODUCIBILITY\n")
cat("=======================================================\n")
print(sessionInfo())

cat("\n=======================================================\n")
cat("To cite this code, please reference:\n")
cat("Thomas J. Brennan & David M. Schizer,\n")
cat("'Big Picture' Investing at Nonprofits:\n") 
cat("Accounting for Future Expenses and Donations,\n")
cat("George Mason Law Review (2026)\n")
cat("=======================================================\n")


