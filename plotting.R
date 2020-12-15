library(ggplot2)
library(plotly)
library(tidyverse)
library(htmlwidgets)

# Import data
data <- read.csv("data.csv")
# Clean data
# Reformat Category column
data <- data %>% rename(Category = SlpSplit.)
data$Category <- gsub(";$", "", data$Category)
data$Category <- gsub('^[[:digit:]]+', '', data$Category)
data$Category <- gsub(" - ", "", data$Category)
# generate counts for each category and HR
counts <- data %>% count(HR, Category)
# sum counts for each HR
HR_sums <- data.frame(counts %>%
                        group_by(HR) %>%
                        summarize(sum = sum(n)))
# join a sum to each row in df counts
counts_joined <- left_join(counts, HR_sums, by = "HR")
# create new column with percentages
counts_joined$percentage <-
  counts_joined$n / counts_joined$sum * 100

# order for Category
levels <-
  c(
    "Increasing Trend > 2.5",
    "Increasing Trend Up to 2.5",
    "No Trend",
    "Decreasing Trend Down to 2.5",
    "Decreasing Trend > 2.5"
  )

# order HRs by number
counts_joined <- counts_joined %>% mutate(HR = factor(
  HR,
  levels =   c(
    "North Coast",
    "San Francisco Bay",
    "Central Coast",
    "South Coast",
    "Sacramento River",
    "San Joaquin River",
    "Tulare Lake",
    "North Lahontan",
    "South Lahontan",
    "Colorado River"
  )
))

# reverse order to have North Coast on top
counts_joined$HR <- fct_rev(counts_joined$HR)

# factor Category based on levels
counts_joined$Category <-
  factor(counts_joined$Category, levels = levels)

# order labels based on Category and HR
text_labels <- arrange(counts_joined, Category, HR)

# Create the bar chart
barChart <-
  counts_joined %>% ggplot(aes(
    x = HR,
    y = percentage,
    fill = Category,
    # text is for tooltips
    text = paste(HR, "\n",
                 Category, ":", n)
  )) + geom_bar(stat = "identity") +
  geom_text(
    data = text_labels,
    aes(label = n),
    size = 3,
    check_overlap = TRUE,
    color = "#6E6E5C",
    position = position_stack(vjust = 0.5) # label in center of bar
  ) +
  coord_flip()

barChart <-
  barChart + scale_fill_manual(values = c("#267300", "#02E5A9", "#FFFF00", "#FFAA00", "#E60000")) +
  ggtitle("Hydrologic Region Summary") +
  labs(x = "Hydrologic Region", y = "% of wells by estimates slope (labels show count)", fill = "Estimated Trends")

p <- ggplotly(barChart, tooltip = "text")

# Save html file locally
htmlwidgets::saveWidget(as_widget(p), "index.html") # name index.html for hosting on GitHub Pages
