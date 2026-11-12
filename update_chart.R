library(httr)
library(jsonlite)
library(plotly)
library(htmlwidgets)

# 1. Configuration
scholar_id <- trimws("nFbaL2gAAAAJ&hl=en") # <-- REPLACE WITH YOUR OWN GOOGLE SCHOLAR ID
api_key <- trimws(Sys.getenv("SERPAPI_KEY"))

if (api_key == "") {
  stop("SERPAPI_KEY environment variable is missing.")
}

# 2. Fetch Google Scholar Author JSON from SerpApi
response <- GET(
  url = "https://serpapi.com",
  query = list(
    engine = "google_scholar_author",
    author_id = scholar_id,
    api_key = api_key,
    output = "json"
  ),
  add_headers("Accept" = "application/json"),
  timeout(60)
)

raw_body <- content(response, as = "text", encoding = "UTF-8")

if (status_code(response) != 200) {
  message("API Error Details: ", substr(raw_body, 1, 500))
  stop(paste("SerpApi request failed. Status code:", status_code(response)))
}

# Check if HTML was returned instead of JSON
if (grepl("^\\s*<", raw_body)) {
  message("Final URL hit: ", response$url)
  message("HTML Preview: ", substr(raw_body, 1, 400))
  stop("Received HTML instead of JSON. Check the Final URL and HTML Preview above.")
}

# 3. Parse the year-wise citation graph directly from JSON
data <- fromJSON(raw_body)

if (!is.null(data$error)) {
  stop(paste("SerpApi returned an error:", data$error))
}

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

