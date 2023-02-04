# Make figure displaying model differences

# load packages
library(dplyr)
library(ggplot2)

# load data
data = read.csv("/home/max/Documents/Projects/Brain_Age/Variability_in_brain_age/Submission/RR2/revised_figures/Fig2_data.csv")

# process data
names(data) = c("DiffModel", "StatsModel", "X2")
data$DiffModel = factor(data$DiffModel) 
data$StatsModel = factor(data$StatsModel) 

# make figure
plot = ggplot(data, aes(StatsModel, DiffModel, col = X2, fill = X2, label = X2)) +
  geom_tile() +
  geom_text(col = "black") +
  theme_minimal() +
  scale_fill_gradient2(low = "white", mid = "yellow", high = "red") +
  scale_color_gradient2(low = "white", mid = "yellow", high = "red") +
  ylab("Diffusion Model") + xlab("Statistical Model")

ggsave("/home/max/Documents/Projects/Brain_Age/Variability_in_brain_age/Submission/RR2/New_Fig2.pdf", plot, height = 7, width = 7)
