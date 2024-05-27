# Load necessary libraries
library(tseries)

# Function to read and process the raw binary data
read_and_process_data <- function(file_path) {
  # Get the file size
  file_size <- file.info(file_path)$size
  
  # Open the file
  con <- file(file_path, "rb")
  
  # Read the raw binary data
  binary_data <- readBin(con, what = integer(), size = 1, n = file_size, signed = FALSE)
  close(con)
  
  return(binary_data)
}

# Main function to handle command line arguments and perform the tests
main <- function() {
  # Get command line arguments
  args <- commandArgs(trailingOnly = TRUE)
  
  # Check if the correct number of arguments is provided
  if (length(args) != 1) {
    stop("Usage: Rscript script_name.R <file_path>")
  }
  
  # Parse arguments
  file_path <- args[1]
  
  # Read and process the binary data
  data <- read_and_process_data(file_path)
  
  # Number of draws
  num_draws <- length(data)
  
  # Ljung-Box Test
  ts_data <- ts(data)
  lag_value <- floor(log(length(ts_data)))
  box_test_result <- Box.test(ts_data, lag = lag_value, type = "Ljung-Box")
  
  # Print the Ljung-Box test result
  print("Ljung-Box Test Result:")
  print(box_test_result)
  
  # Extract and print the p-value for Ljung-Box test
  p_value_lb <- box_test_result$p.value
  print(paste("Ljung-Box Test P-value:", p_value_lb))
  
  # Chi-square Test
  binary_data <- as.integer(data)
  
  # Count occurrences of 0s and 1s
  counts <- table(factor(binary_data, levels = 0:1))
  
  # Ensure counts include both 0s and 1s
  counts <- as.integer(counts)
  
  # Perform the Chi-square test
  expected_counts <- rep(num_draws / 2, 2) # Expecting equal counts of 0s and 1s
  chi_square_test <- chisq.test(counts, p = expected_counts / sum(expected_counts))
  
  # Print the Chi-square test result
  print("Chi-square Test Result:")
  print(chi_square_test)
  
  # Extract and print the p-value for Chi-square test
  p_value_chi <- chi_square_test$p.value
  print(paste("Chi-square Test P-value:", p_value_chi))
  
  # Runs Test
  binary_sequence <- ifelse(data > 0, 1, 0)
  runs_result <- runs.test(as.factor(binary_sequence))
  
  # Print the Runs test result
  print("Runs Test Result:")
  print(runs_result)
  
  # Extract and print the p-value for Runs test
  p_value_runs <- runs_result$p.value
  print(paste("Runs Test P-value:", p_value_runs))
}

# Run the main function
main()
