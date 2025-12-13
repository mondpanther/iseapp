library(ggplot2)
library(ggiraph)

df <- data.frame(
  category = c("Apple", "Google", "Microsoft"),
  value = c(25, 40, 35),
  url = c("https://apple.com", "https://google.com", "https://microsoft.com")
)

p <- ggplot(df, aes(x = category, y = value)) +
  geom_col_interactive(
    aes(onclick = sprintf("window.open(\"%s\")", url),
        tooltip = category),
    fill = "steelblue"
  ) +
  theme_minimal()

#p
ggplotly(p)
girafe(ggobj = p)





espacenet_family_search <- function(docdb_family_id) {
  query <- paste0("site:worldwide.espacenet.com ", docdb_family_id)
  paste0("https://www.google.com/search?q=", URLencode(query, reserved = TRUE))
}

# Example usage
espacenet_family_search("12345678")



library(ggplot2)
library(plotly)
library(dplyr)

# Create sample data
set.seed(123)
companies <- data.frame(
  company = rep(c("Apple", "Google", "Microsoft", "Amazon"), each = 12),
  month = rep(1:12, 4),
  revenue = c(runif(12, 80, 120), runif(12, 70, 110), 
              runif(12, 60, 100), runif(12, 90, 130)),
  url = rep(c("https://apple.com", "https://google.com", 
              "https://microsoft.com", "https://amazon.com"), each = 12)
)

# Create complex ggplot
p <- ggplot(companies, aes(x = month, y = revenue, 
                           color = company, group = company,
                           customdata = url)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2, aes(text = paste0(company, "\n",
                                         "Month: ", month, "\n",
                                         "Revenue: $", round(revenue, 1), "B"))) +
  scale_x_continuous(breaks = 1:12) +
  labs(title = "Quarterly Revenue Trends",
       x = "Month", y = "Revenue (Billions $)",
       color = "Company") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Convert to interactive plotly
ggplotly(p, tooltip = "text")