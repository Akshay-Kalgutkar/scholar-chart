library(httr)
library(jsonlite)
library(plotly)
library(htmlwidgets)

# 1. Configuration
scholar_id <- "nFbaL2gAAAAJ" # <-- REPLACE WITH YOUR OWN GOOGLE SCHOLAR ID
api_key <- Sys.getenv("SERPAPI_KEY")

if (api_key == "") {
  stop("SERPAPI_KEY environment variable is missing.")
}

# 2. Fetch Google Scholar Author JSON directly from SerpApi
api_url <- paste0(
  "https://serpapi.com",
  "&author_id=", scholar_id,
  "&api_key=", api_key
)

response <- GET(api_url, timeout(60))

if (status_code(response) != 200) {
  stop(paste("SerpApi request failed. Status code:", status_code(response)))
}

# 3. Parse the year-wise citation graph directly from JSON
data <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
ct <- data$cited_by$graph

if (is.null(ct) || nrow(ct) == 0) {
  stop("No citation graph data found. Verify your scholar_id is correct.")
}

# 4. Generate the interactive bar chart
fig <- plot_ly(ct, x = ~year, y = ~citations, type = 'bar',
               width = 700, height = 400,
               marker = list(color = '#4285F4')) %>%
  layout(title = "Citations per Year",
         xaxis = list(title = "Year", dtick = 1),
         yaxis = list(title = "Citations"))

# 5. Export as a self-contained HTML file
saveWidget(fig, "index.html", selfcontained = TRUE)
message("Successfully generated index.html!")


