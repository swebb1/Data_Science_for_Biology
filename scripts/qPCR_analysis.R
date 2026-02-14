library(readxl)

# Read in the data
qpcr_data <- read_excel("data/qPCR_data.xlsx")

# Function to perform qPCR analysis
qPCR_analysis <- function(qpcr_table,plot_file) {
  
  # Calculate delta_ct values
  qpcr_table$delta_ct <- qpcr_table$ct_gene - qpcr_table$ct_ref
  
  # Perform t-test
  t_test_result <- t.test(delta_ct ~ Condition, data = qpcr_table, var.equal=TRUE)
  
  # Calculate fold change
  mean_control <- mean(qpcr_table$delta_ct[qpcr_table$Condition == "Control"])
  mean_treatment <- mean(qpcr_table$delta_ct[qpcr_table$Condition == "Treatment"])
  delta_delta_ct <- mean_treatment - mean_control
  fold_change <- 2^(-delta_delta_ct)
  
  # Save boxplot as a PDF
  png(plot_file)
  boxplot(delta_ct ~ Condition, 
          data = qpcr_table, 
          main = "Delta Ct values by condition", 
          ylab = "Delta Ct",
          sub = paste("Fold change:", round(fold_change, 2), 
                      "p-value:", round(t_test_result$p.value, 4))
          )
  dev.off()
}

qPCR_analysis(qpcr_data, "qPCR_boxplot.png")