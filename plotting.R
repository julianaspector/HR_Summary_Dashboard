library(ggplot2)
library(plotly)
library(tidyverse)
library(htmlwidgets)

# Import and clean data
data <- read.csv("data.csv")
data <- data %>% rename(Category = SlpSplit.)
data$Category <- gsub(";$", "", data$Category)
data$Category <- gsub('^[[:digit:]]+', '', data$Category)
data$Category <- gsub(" - ", "", data$Category)
counts <- data %>% count(HR, Category)
HR_sums <- data.frame(counts %>%
                        group_by(HR) %>%
                        summarize(sum = sum(n)))
counts_joined <- left_join(counts, HR_sums, by = "HR")
counts_joined$percentage <-
  counts_joined$n / counts_joined$sum * 100


levels <-
  c(
    "Increasing Trend > 2.5",
    "Increasing Trend Up to 2.5",
    "No Trend",
    "Decreasing Trend Down to 2.5",
    "Decreasing Trend > 2.5"
  )

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

counts_joined$HR <- fct_rev(counts_joined$HR)

# Create the bar chart
barChart <-
  counts_joined %>% ggplot(aes(x = HR, y = percentage, text = paste("Estimated Trends:", Category, "\n",
                                                                    "Hydrologic Region:", HR, "\n",
                                                                    "% of wells by estimates slope:", round(percentage,1))
                                                                    )) + geom_col(aes(fill =
                                                                         factor(Category, levels = levels)), width = 0.7)
barChart <- barChart + coord_flip() + scale_fill_manual(values = c("#267300", "#02E5A9", "#FFFF00", "#FFAA00", "#E60000")) + 
  ggtitle("Hydrologic Region Summary") +
  labs(x="Hydrologic Region", y="% of wells by estimates slope", fill = "Estimated Trends")

p <- ggplotly(barChart, tooltip = "text")

##### Save it locally
htmlwidgets::saveWidget(as_widget(p), "index.html")
