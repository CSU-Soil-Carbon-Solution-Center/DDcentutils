# Read arguments
args <- commandArgs(trailingOnly = TRUE)

# Assign them to variables
input_file  <- args[1]
output_file <- args[2]
n_iter      <- as.numeric(args[3])

cat("Input file:", input_file, "\n")
cat("Output file:", output_file, "\n")
cat("Iterations:", n_iter, "\n")
