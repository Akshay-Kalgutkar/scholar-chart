library(scholar)
library(plotly)
library(htmlwidgets)

# Fetch year-wise citations
scholar_id <- "nFbaL2gAAAAJ&hl=en"
ct <- get_citation_history(scholar_id)

# Create a clean interactive bar chart
fig <- plot_ly(ct, x = ~year, y = ~cites, type = 'bar',
               width = 700, height = 400,
               marker = list(color = '#4285F4')) %>%
  layout(title = "Citations per Year",
         xaxis = list(title = "Year", dtick = 1),
         yaxis = list(title = "Citations"))

# Export as a self-contained HTML file
saveWidget(fig, "index.html", selfcontained = TRUE)
