
# Any lines starting with a # are comments, and are not executed as code. 
# You can use comments to explain what your code does
# or to temporarily disable a line of code, which can be useful for debugging

# Homework exercise - Introduction to R and R Studio
# Write an R script to perform a data analysis task
# https://swebb1.github.io/Data_Science_for_Biology/02-HW.html

# Version 1 - Hard-coded script

# Read in Spine data as a data frame
spine_data <- read.csv("https://bifx-core3.bio.ed.ac.uk/training/DSB/data/spine_data.csv", header = TRUE)

# Inspect the data
head(spine_data)

# Calculate the mean spine density for each batch by averaging the values from the 3 neurons
spine_data$Mean_Spine_Density <- rowMeans(spine_data[, c("Neuron_1", "Neuron_2", "Neuron_3")])

# Perform a t-test to compare the mean spine density between the Control and NeuroBoost groups
t.test(Mean_Spine_Density ~ Treatment, data = spine_data, var.equal = T)

# Create a boxplot to visualize the distribution of spine densities for each treatment group
# Save the plot to a PNG file
png("spine_density_boxplot.png")
boxplot(spine_data$Mean_Spine_Density ~ spine_data$Treatment, 
        main = "Mean Spine Density by Treatment", 
        xlab = "Treatment", 
        ylab = "Mean Spine Density (spines/10um)")
dev.off()
