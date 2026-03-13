
# Version 2 - Programmable script

# Usage Rscript <filename> <input_file> <columns>
# The input file should be a CSV file containing the spine data, and the columns argument should be a comma-separated list of the column names to average for the mean spine density calculation.
# Example: Rscript spine_data_script.R https://bifx-core3.bio.ed.ac.uk/training/DSB/data/spine_data.csv Neuron_1,Neuron_2,Neuron_3

# Read in input file name from command line arguments
args <- commandArgs(trailingOnly = TRUE)
input_file <- args[1]
columns <- args[2]

# convert the columns argument from a string to a vector of column names using the strsplit function
# The output of strsplit is a list, so we need to use [[1]] to extract the first element
# Lists use double square brackets [[ ]] to extract elements, while data frames use single square brackets [ ] to subset rows and columns
columns <- strsplit(columns, ",")[[1]]

# Read in Spine data as a data frame
spine_data <- read.csv(input_file, header = TRUE)

# Inspect the data
head(spine_data)

# Create a function that adds a mean column to a data frame. 
# The columns to average and the name of the new column should be passed as arguments to the function.
mean_column <- function(data, columns, name) {
  # add a mean column to the data frame by averaging the values from the specified columns 
  data[name] <- rowMeans(data[,columns])
  return(data)
}

# Run the function to add the mean spine density column to the data frame
spine_data_mean <- mean_column(spine_data, columns, "Mean_Spine_Density")

# Perform a t-test to compare the mean spine density between the Control and NeuroBoost groups
t.test(Mean_Spine_Density ~ Treatment, data = spine_data_mean, var.equal = T)

# Create a boxplot to visualize the distribution of spine densities for each treatment group
# Save the plot to a PNG file
png("spine_density_boxplot.png")
boxplot(spine_data_mean$Mean_Spine_Density ~ spine_data_mean$Treatment, 
        main = "Mean Spine Density by Treatment", 
        xlab = "Treatment", 
        ylab = "Mean Spine Density (spines/10um)")
dev.off()
