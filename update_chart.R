library(httr)
library(jsonlite)
library(plotly)
library(htmlwidgets)

# 1. Configuration
scholar_id <- gsub("\\s+", "", "nFbaL2gAAAAJ&hl=en") # <-- REPLACE WITH YOUR OWN GOOGLE SCHOLAR ID
api_key <- gsub("\\s+", "", Sys.getenv("SERPAPI_KEY"))

if (nchar(api_key) < 30) {
  stop("SERPAPI_KEY is missing or too short. Check GitHub Repository Secrets.")
}

# 2. Fetch Google Scholar Author JSON using a standard curl User-Agent
response <- GET(
  url = "https://serpapi.com/search.json",
  query = list(
    engine = "google_scholar_author",
    author_id = scholar_id,
    api_key = api_key
  ),
  user_agent("curl/8.5.0"),
  add_headers("Accept" = "application/json"),
  timeout(60)
)

raw_body <- content(response, as = "text", encoding = "UTF-8")

# Verify we received JSON rather than an HTML redirect
if (status_code(response) != 200 || grepl("^\\s*<", raw_body)) {
  message("Status code: ", status_code(response))
  message("Content-Type: ", headers(response)[["content-type"]])
  message("Response Preview: ", substr(raw_body, 1, 300))
  stop("SerpApi still returned non-JSON output. Check the Content-Type and Preview above.")
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

