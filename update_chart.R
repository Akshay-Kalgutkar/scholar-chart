library(httr)
library(jsonlite)
library(plotly)
library(htmlwidgets)

# 1. Configuration (strip whitespace and '&hl=en' to keep only the 12-character ID)
scholar_id <- gsub("\\s+|&.*", "", "nFbaL2gAAAAJ&hl=en")
api_key <- gsub("\\s+", "", Sys.getenv("SERPAPI_KEY"))

if (nchar(api_key) < 30) {
  stop("SERPAPI_KEY is missing or too short. Check GitHub Repository Secrets.")
}

# 2. Fetch Google Scholar Author JSON with automatic retry
max_retries <- 3
raw_body <- ""
status <- 0

for (attempt in 1:max_retries) {
  response <- GET(
    url = "https://serpapi.com",
    query = list(
      engine = "google_scholar_author",
      author_id = scholar_id,
      api_key = api_key,
      num = "100"
    ),
    user_agent("Mozilla/5.0 (compatible; R-httr-client/1.0)"),
    add_headers("Accept" = "application/json"),
    timeout(60)
  )
  
  status <- status_code(response)
  raw_body <- content(response, as = "text", encoding = "UTF-8")
  
  if (status == 200 && !grepl("^\\s*<", raw_body)) {
    break
  }
  
  if (attempt < max_retries) {
    Sys.sleep(5 * attempt)
  }
}

if (status != 200 || grepl("^\\s*<", raw_body)) {
  stop(paste0(
    "SerpApi failed (HTTP ", status, "). ",
    "Content-Type: ", headers(response)[["content-type"]], " | ",
    "Preview: ", gsub("\\s+", " ", substr(raw_body, 1, 250))
  ))
}

# 3. Parse year-wise citations, publications, and summary metrics from JSON
data <- fromJSON(raw_body)

if (!is.null(data$error)) {
  stop(paste("SerpApi returned an error:", data$error))
}

ct <- data$cited_by$graph
if (is.null(ct) || nrow(ct) == 0) {
  stop("No citation graph data found. Verify your scholar_id is correct.")
}

# Count publications per year from data$articles
articles <- data$articles
if (!is.null(articles) && "year" %in% names(articles)) {
  pub_years <- as.numeric(articles$year)
  pub_years <- pub_years[!is.na(pub_years)]
  
  if (length(pub_years) > 0) {
    pub_df <- as.data.frame(table(year = pub_years), stringsAsFactors = FALSE)
    pub_df$year <- as.numeric(pub_df$year)
    names(pub_df) <- "publications"
  } else {
    pub_df <- data.frame(year = numeric(0), publications = numeric(0))
  }
} else {
  pub_df <- data.frame(year = numeric(0), publications = numeric(0))
}

# Merge citation years and publication years into one complete timeline
merged_df <- merge(ct, pub_df, by = "year", all = TRUE)
merged_df$citations[is.na(merged_df$citations)] <- 0
merged_df$publications[is.na(merged_df$publications)] <- 0
merged_df <- merged_df[order(merged_df$year), ]

# Extract Total Citations, h-index, and i10-index from data$cited_by$table
tbl <- data$cited_by$table
total_cites <- tbl$citations$all[1]
h_index     <- tbl$h_index$all[2]
i10_index   <- tbl$i10_index$all[3]

# Build clean summary header text
summary_text <- paste0(
  "<b>Total Citations:</b> ", total_cites,
  "   |   <b>h-index:</b> ", h_index,
  "   |   <b>i10-index:</b> ", i10_index
)

# 4. Generate dual-axis line chart: Citations (Line 1) + Publications (Line 2)
fig <- plot_ly(merged_df, x = ~year, width = 750, height = 450) %>%
  # Citations per year as a solid blue line (Left Y-axis)
  add_trace(
    y = ~citations,
    name = "Citations",
    type = "scatter",
    mode = "lines+markers+text",
    line = list(color = "#4285F4", width = 3),
    marker = list(color = "#4285F4", size = 8),
    text = ~citations,
    textposition = "top center",
    textfont = list(color = "#1a73e8", size = 11),
    hovertemplate = "Citations: %{y}<extra></extra>"
  ) %>%
  # Publications per year as a dashed green line (Right Y-axis)
  add_trace(
    y = ~publications,
    name = "Publications",
    yaxis = "y2",
    type = "scatter",
    mode = "lines+markers",
    line = list(color = "#0F9D58", width = 2.5, dash = "dash"),
    marker = list(color = "#0F9D58", size = 7, symbol = "diamond"),
    hovertemplate = "Publications: %{y}<extra></extra>"
  ) %>%
  layout(
    title = list(
      text = paste0(
        "Google Scholar Citations & Publications per Year<br>",
        "<span style='font-size:14px; color:#555555;'>", summary_text, "</span>"
      ),
      y = 0.95
    ),
    margin = list(t = 90, r = 60, b = 60, l = 60),
    xaxis = list(title = "Year", dtick = 1, showgrid = FALSE),
    yaxis = list(
      title = "Citations",
      titlefont = list(color = "#4285F4"),
      tickfont = list(color = "#4285F4"),
      rangemode = "tozero"
    ),
    yaxis2 = list(
      title = "Publications",
      overlaying = "y",
      side = "right",
      titlefont = list(color = "#0F9D58"),
      tickfont = list(color = "#0F9D58"),
      dtick = 1,
      rangemode = "tozero",
      showgrid = FALSE
    ),
    legend = list(orientation = "h", x = 0.35, y = -0.18),
    hovermode = "x unified"
  )

# 5. Export as a self-contained HTML file
saveWidget(fig, "index.html", selfcontained = TRUE)
message("Successfully generated index.html with dual line chart!")
