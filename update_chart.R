library(httr)
library(jsonlite)
library(plotly)
library(htmlwidgets)

# 1. Configuration
scholar_id <- gsub("\\s+", "", "nFbaL2gAAAAJ&hl=en")
api_key <- gsub("\\s+", "", Sys.getenv("SERPAPI_KEY"))

if (nchar(api_key) < 30) {
  stop("SERPAPI_KEY is missing or too short. Check GitHub Repository Secrets.")
}

# 2. Fetch Google Scholar Author JSON using a standard curl User-Agent
response <- GET(
  url = "https://serpapi.com",
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

if (status_code(response) != 200 || grepl("^\\s*<", raw_body)) {
  message("Status code: ", status_code(response))
  message("Content-Type: ", headers(response)[["content-type"]])
  message("Response Preview: ", substr(raw_body, 1, 300))
  stop("SerpApi did not return valid JSON. Check the Content-Type and Preview above.")
}

# 3. Parse year-wise citations and summary metrics from JSON
data <- fromJSON(raw_body)

if (!is.null(data$error)) {
  stop(paste("SerpApi returned an error:", data$error))
}

ct <- data$cited_by$graph
if (is.null(ct) || nrow(ct) == 0) {
  stop("No citation graph data found. Verify your scholar_id is correct.")
}

# Extract Total Citations, h-index, and i10-index
tbl <- data$cited_by$table
total_cites <- tbl$citations$all
h_index     <- tbl$h_index$all
i10_index   <- tbl$i10_index$all

summary_text <- paste0(
  "<b>Total Citations:</b> ", total_cites,
  "   |   <b>h-index:</b> ", h_index,
  "   |   <b>i10-index:</b> ", i10_index
)

# 4. Generate a responsive, transparent interactive bar chart
fig <- plot_ly(ct, x = ~year, y = ~citations, type = 'bar',
               marker = list(color = '#4285F4'),
               text = ~citations, textposition = 'outside',
               cliponaxis = FALSE) %>%
  layout(
    autosize      = TRUE,
    paper_bgcolor = 'rgba(0,0,0,0)',
    plot_bgcolor  = 'rgba(0,0,0,0)',
    title = list(
      text = paste0("Google Scholar Citations per Year<br><span style='font-size:14px; color:#555555;'>", summary_text, "</span>"),
      y = 0.96
    ),
    margin = list(t = 85, b = 45, l = 45, r = 20),
    xaxis = list(title = "Year", dtick = 1),
    yaxis = list(title = "Citations")
  ) %>%
  config(responsive = TRUE, displayModeBar = FALSE)

# Force the widget container to fill 100% of the iframe width and height
fig$sizingPolicy <- sizingPolicy(
  defaultWidth = "100%",
  defaultHeight = "100%",
  browser.fill = TRUE,
  padding = 0
)

# 5. Export as a self-contained HTML file
saveWidget(fig, "index.html", selfcontained = TRUE, background = "transparent")
message("Successfully generated responsive transparent index.html!")


