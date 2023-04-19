
#Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)

#Load dataframe
Results <- as.data.frame(read.csv("PreDimensionalityData"))


#############
#histogram of all features
############

cols <- names(Results)[4:40]

# Set up the plotting window to show multiple histograms side by side
par(mfrow = c(9,4)) # change the numbers to adjust the layout

# Generate a histogram for each selected column
# Create a list to store the histograms
hist_list <- list()

# Loop through the columns and create a histogram for each
for (col in cols) {
  hist_list[[col]] <- hist(Results[[col]], main = col, xlab = "Values", plot = FALSE)
}

# Set up the LaTeX code for the figure
cat("\\begin{figure}[ht]\n")
cat("\\centering\n")

# Loop through the histograms and create subfigures
for (i in 1:length(hist_list)) {
  cat(paste0("\\begin{subfigure}[b]{0.3\\textwidth}\n"))
  cat(paste0("\\centering\n"))
  cat(paste0("\\includegraphics[width=\\textwidth]{", names(hist_list)[i], ".pdf}\n"))
  cat(paste0("\\caption{", names(hist_list)[i], "}\n"))
  cat(paste0("\\end{subfigure}\n"))
  if (i %% 3 == 0) {
    cat(paste0("\\\\ \n"))
  }
}

# Close the LaTeX code for the figure
cat("\\caption{Histograms}\n")
cat("\\end{figure}\n")

# Save each histogram as a separate PDF file
for (i in 1:length(hist_list)) {
  pdf(paste0(names(hist_list)[i], ".pdf"), height = 3, width = 3)
  plot(hist_list[[i]], main = "", xlab = "Values", ylab = "Frequency", col = "white", border = "black")
  dev.off()
}


##############



