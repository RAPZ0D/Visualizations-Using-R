# Plotting IRIS DATASET 
library(plotly)
library(ggplot2)
p <- ggplot(data=iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(aes(color=Species, shape=Species)) +
  labs(title = "Iris Sepal Width vs Length", 
       x = "Sepal Length", 
       y = "Sepal Width") +
  theme(plot.title = element_text(size = 18, face = "bold", color = "gold"), # Gold title
        axis.title = element_text(size = 12, face = "bold", color = "red"), # Red axis titles
        axis.text = element_text(size = 10, color = "black"), # Black axis labels
        legend.title = element_text(size = 10, face = "italic", color = "blue"), # Blue legend title
        legend.text = element_text(size = 6, color = "darkgreen")) # Dark green legend text

ggplotly(p)

# DUMBELL chart for mtcars dataset
# Calculate the mean of mpg for each number of cylinders
mtcars_summary <- aggregate(mpg ~ cyl, data = mtcars, FUN = mean)

# Calculate the difference between the means of adjacent levels of cylinders
mtcars_summary$diff <- c(NA, diff(mtcars_summary$mpg))

# Define positions for dumbbell chart
mtcars_summary$x_pos <- 20

# Create the dumbbell chart
p <- ggplot(mtcars_summary, aes(x = mpg, y = as.factor(cyl))) +
  geom_point(aes(color = as.factor(cyl)), size = 3) +
  geom_segment(aes(xend = x_pos, yend = as.factor(cyl), color = as.factor(cyl)), size = 1) +
  geom_text(aes(label = paste("D:", round(diff, 2)), x = x_pos + 1, y = as.factor(cyl)), 
            color = "black", size = 4, hjust = 0) +
  scale_color_manual(values = c("#6545da", "#9feae9", "#38ef2a")) +
  labs(title = "Dumbbell Chart of MPG by Cylinders",
       x = "MPG",
       y = "Cylinders") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", color = "gold"), # Gold title
    axis.title = element_text(size = 12, face = "bold", color = "red"), # Red axis titles
    axis.text = element_text(size = 10, color = "black"), # Black axis labels
    legend.title = element_text(size = 10, face = "italic", color = "lightblue"), # Blue legend title
    legend.text = element_text(size = 6, color = "darkgreen") # Dark green legend text
  )

p

#INTERACTIVE BARPLOT
set.seed(123)
df <- diamonds[sample(1:nrow(diamonds), size = 1000),]

p <- ggplot(df, aes(x = color)) + 
  geom_bar(aes(y = ..count../sum(..count..), fill = cut), stat = "count") + 
  scale_fill_brewer(palette = "Set3") +  # Using a different color palette
  ylab("Percent") + 
  ggtitle("Diamond cut vs percent") +
  theme_minimal() +  # Using a minimal theme
  theme(
    axis.title = element_text(face = "bold"),  # Making axis titles bold
    axis.text = element_text(size = 10),       # Adjusting the size of axis text
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),  # Centering the plot title
    legend.title = element_text(face = "italic", size = 10),           # Adjusting legend title
    legend.text = element_text(size = 8)        # Adjusting legend text size
  )

ggplotly(p)


# BUBBLEPLOT on gapminder dataset
library(gapminder)
library(ggplot2)
library(plotly)

# Define a color palette
my_palette <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")

# Filter data for the year 2007
data <- gapminder %>%
  filter(year == 2007)

# Most basic bubble plot with custom color palette and text color
p <- data %>%
  arrange(desc(pop)) %>%
  ggplot(aes(x = gdpPercap, y = lifeExp, size = pop, color = continent)) +
  geom_point(alpha = 0.5) +
  scale_size(range = c(1, 24), name = "Population (M)") +
  scale_color_manual(values = my_palette) +  # Using custom color palette
  labs(title = "Bubble Plot for the Year 2007",
       x = "GDP per Capita",
       y = "Life Expectancy") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "red"),  # Title color set to red
    axis.title.x = element_text(color = "blue"),  # X-axis text color set to yellow
    axis.title.y = element_text(color = "orange")   # Y-axis text color set to yellow
  )

# Convert ggplot to plotly
plotly_plot <- ggplotly(p)

# Print plotly plot
plotly_plot

# HEATMAP 
# Load the mtcars dataset
data(mtcars)

# Create a heatmap using the mtcars dataset
p <- ggplot(mtcars, aes(x = factor(cyl), y = factor(gear), fill = mpg)) +
  geom_tile() +
  scale_fill_gradient(low = "purple", high = "orange", trans = "log10") +  # Adjusting color gradient
  labs(title = "Heatmap of MPG by Cylinders and Gears",
       x = "Cylinders",
       y = "Gears",
       fill = "MPG") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Centering plot title

# Convert ggplot to plotly
fig <- ggplotly(p)

# Print the heatmap
fig

# STACKED DENSITY PLOT on TXhousing dataset
# Load the txhousing dataset
data(txhousing)

# Filter data for the specified cities and year 2000
filtered_data <- txhousing[txhousing$city %in% c("San Antonio", "Austin", "Dallas") & 
                             txhousing$year == 2015, ]

# Create a stacked density plot
p <- ggplot(filtered_data, aes(x = sales, fill = city)) + 
  geom_density(alpha = 0.5, position = "stack") +
  ggtitle("Stacked Density Plot of Sales Year 2015") +
  labs(x = "Sales", y = "Density") +
  theme_minimal()

# Convert ggplot to plotly
fig <- ggplotly(p)

# Print the stacked density plot
fig

# OVERLAY Scatterplot using IRIS dataset
# Create a scatterplot with overlay 2D density plot
p <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + 
  geom_point(alpha = 0.5) + 
  geom_density_2d() + 
  theme(panel.background = element_rect(fill = '#9fadc1')) + 
  ggtitle("2D Density Plot with Scatterplot Overlay")

# Convert ggplot to plotly
fig <- ggplotly(p)

# Print the plotly plot
fig

# PARALLEL COORDINATES plot
library(plotly)

library(GGally)

library(viridis)

library(hrbrthemes)
# Load the iris dataset
data <- iris

# Create the parallel coordinate plot with GGally
p <- ggparcoord(data,
                columns = 1:4, 
                groupColumn = 5, 
                order = "anyClass",
                showPoints = TRUE, 
                title = "Parallel Coordinate Plot for the Iris Data",
                alphaLines = 0.3
)

# Enhance the plot
p <- p +
  scale_color_viridis(discrete = TRUE) +  # Adjust color scale
  theme_ipsum() +                         # Use hrbrthemes' theme_ipsum
  theme(plot.title = element_text(size = 18))  # Increase title size

# Convert ggplot to plotly
fig <- ggplotly(p)

# Print the enhanced plot
fig


# TIMESERIES DATA GOOGLE
library(plotly)

library(tidyverse)

library(tidyquant)

library(ggplot2)
# Load FANG dataset
data("FANG") 

# Get Google stock prices
GOOGL <- tq_get("GOOGL", get = "stock.prices", from = "2000-01-01", to = "2016-12-31")

# Create the plot
p <- GOOGL %>%
  ggplot(aes(x = date, y = adjusted)) +
  geom_line(color = "#1c4392") +  # Change line color to blue
  scale_y_log10() +
  geom_smooth(method = "lm", color = "#dbed2d") +  # Change trendline color to orange
  labs(title = "Google (GOOGL) Line Chart", 
       subtitle = "Log Scale, Applying Linear Trendline", 
       y = "Adjusted Closing Price", 
       x = "") + 
  theme_tq()

# Convert ggplot to plotly
fig <- ggplotly(p)

# Print the plot
fig

# BASIC ANIMATION
library(plotly)

library(ggplot2)

library(gapminder)
# Create a base ggplot object
p <- ggplot(gapminder, aes(gdpPercap, lifeExp, color = continent, text = paste("Country: ", country, "<br>Year: ", year))) +
  geom_point(aes(size = pop, frame = year, ids = country)) +
  scale_x_log10() +
  theme_minimal()

# Convert ggplot to plotly and add animation options
fig <- ggplotly(p, tooltip = "text") %>%
  animation_opts(frame = 1000, easing = "cubic-in-out") %>%
  animation_slider(currentvalue = list(prefix = "Year: ")) %>%
  layout(
    title = "Life Expectancy vs GDP per Capita (Gapminder Data)",
    xaxis = list(title = "GDP per Capita (log scale)"),
    yaxis = list(title = "Life Expectancy"),
    legend = list(title = "Continent"),
    font = list(family = "Arial", size = 12),
    plot_bgcolor = "#506e7b",
    paper_bgcolor = "white"
  )

# Print the plot
fig
