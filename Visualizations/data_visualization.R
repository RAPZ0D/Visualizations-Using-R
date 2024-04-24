# Reading the CSV file into R make sure the dataset is in the same location as the R code
economist_data <- read.csv("Economist.csv")

# Printing the first few rows of the dataset
head(economist_data)
library(ggplot2)
# A simple Scatterplot of  CPI vs HDI
ggplot(economist_data, aes(x = CPI, y = HDI)) +
  geom_point() +
  labs(title = "Scatterplot of CPI vs HDI",
       x = "Corruption Perceptions Index (CPI)",
       y = "Human Development Index (HDI)")

# Scatterplot of every region 
ggplot(economist_data, aes(x = CPI, y = HDI, fill = Region)) +
  geom_point(shape = 21, size = 4, alpha = 0.6, color = "black") +  # Enhance point appearance
  labs(title = "Scatterplot of CPI vs HDI by Region",
       x = "Corruption Perceptions Index (CPI)",
       y = "Human Development Index (HDI)") +
  theme_minimal() +
  theme(legend.position = "right") +  # Move legend to the right
  scale_fill_discrete(name = "Region")  # Customizing legend title

# Creating a scatterplot with hollow rings and a smooth line
ggplot(economist_data, aes(x = CPI, y = HDI)) +
  geom_point(shape = 1, size = 3, color = "darkblue", fill = "transparent") +  # Use hollow rings
  geom_smooth(method = "loess", color = "red", se = FALSE) +  # Add a smooth line using LOESS
  labs(title = "Scatterplot of CPI vs HDI with Smooth Line",
       x = "Corruption Perceptions Index (CPI)",
       y = "Human Development Index (HDI)") +
  theme_minimal()

# Creating a plot using Region as labels instead of scatterplot dots
ggplot(economist_data, aes(x = CPI, y = HDI, color = Region)) +
  geom_smooth(method = "loess", se = FALSE) +  # Add a smooth line using LOESS
  geom_text(aes(label = Region), size = 3, nudge_x = 0.1, nudge_y = 0.1, check_overlap = TRUE) +  # Add region labels
  labs(title = "Scatterplot of CPI vs HDI with Region Labels",
       x = "Corruption Perceptions Index (CPI)",
       y = "Human Development Index (HDI)") +
  scale_color_manual(values = c("Asia Pacific" = "red", "East EU Cemt Asia" = "blue", "MENA" = "green", "SSA" = "orange")) +  # Assign different colors to each region
  theme_minimal(base_size = 14)

# Country and Region Scatterplot 
ggplot(economist_data, aes(x = CPI, y = HDI, color = Region, label = Country)) +
  geom_point() +
  geom_text(size = 3, nudge_x = 0.1, nudge_y = 0.1, check_overlap = TRUE) +  # Add country labels
  labs(title = "Scatterplot of CPI vs HDI with Country Labels",
       x = "Corruption Perceptions Index (CPI)",
       y = "Human Development Index (HDI)",
       color = "Region") +
  scale_color_manual(values = c("Asia Pacific" = "red", "East EU Cemt Asia" = "blue", "MENA" = "green", "SSA" = "orange")) +  # Assign different colors to each region
  guides(color = guide_legend(title = "Region")) +  # Set legend title
  theme_bw()

# HEXMAP
ggplot(data = economist_data, aes(x = HDI.Rank, y = CPI)) +
  geom_hex(aes(fill = stat(count)), color = "red") +  # Add hexagonal heatmap with custom fill color
  scale_fill_gradient(low = "orange", high = "darkblue") +  # Gradient fill color
  labs(title = "Customized Hexagonal Heatmap of HDI Rank vs CPI",
       x = "HDI Rank",
       y = "CPI") +
  theme_bw()

# Violin chart
ggplot(data = economist_data, aes(x = Region, y = HDI)) +
  geom_violin(fill = "skyblue", color = "red") +  # Add violin plot with custom fill and border colors
  labs(title = "Violin Plot of HDI by Region",
       x = "Region",
       y = "HDI") +
  theme_minimal()

# BOXPLOT
ggplot(data = economist_data, aes(x = Region, y = HDI)) +
  geom_boxplot(fill = "skyblue", color = "blue", outlier.color = "red", outlier.shape = 16, outlier.size = 2) +  # Add boxplot with custom aesthetics
  geom_point(shape = 1, position = position_jitter(width = 0.2), color = "darkblue", size = 2) +  # Add points for outliers
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, color = "darkgreen", fill = "green") +  # Add mean points
  labs(title = "Advanced Boxplot of HDI by Region",
       x = "Region",
       y = "HDI") +
  theme_minimal()

# Ridgeline Plot
library(ggridges)
ggplot(economist_data, aes(x = HDI, y = Region, fill = factor(stat(quantile)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = 4, quantile_lines = TRUE) +
  scale_fill_viridis_d(name = "Quartiles") +
  labs(title = "Ridgeline Plot of HDI by Region") +
  theme_minimal()