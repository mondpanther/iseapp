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

p
girafe(ggobj = p)





espacenet_family_search <- function(docdb_family_id) {
  query <- paste0("site:worldwide.espacenet.com ", docdb_family_id)
  paste0("https://www.google.com/search?q=", URLencode(query, reserved = TRUE))
}

# Example usage
espacenet_family_search("12345678")
