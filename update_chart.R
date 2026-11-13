
library(httr)
library(jsonlite)
library(plotly)
library(htmlwidgets)

# 1. Configuration (keep only the 12-character Scholar ID without '&hl=en')
scholar_id <- gsub("\\s+", "", "nFbaL2gAAAAJ&hl=en")
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

total_cites <- tbl$citations$all[1]
h_index     <- tbl$h_index$all[2]
i10_index   <- tbl$i10_index$all[3]

# 4. Extract year-wise publication counts
articles <- data$articles

if (is.null(articles) || is.null(articles$year)) {
  stop("No publication year data found in the SerpApi response.")
}

publication_years <- as.integer(as.character(articles$year))

pub_counts <- as.data.frame(
  table(year = publication_years)
)

pub_counts <- pub_counts[pub_counts$Freq > 0, ]
names(pub_counts) <- c("year", "publications")

pub_counts$year <- as.integer(as.character(pub_counts$year))

# Combine citation and publication data by year
plot_data <- merge(ct, pub_counts, by = "year", all = TRUE)

plot_data$citations[is.na(plot_data$citations)] <- 0
plot_data$publications[is.na(plot_data$publications)] <- 0

plot_data <- plot_data[order(plot_data$year), ]

# Build clean summary header text
summary_text <- paste0(
  "<b>Total Citations:</b> ", total_cites,
  "   |   <b>h-index:</b> ", h_index,
  "   |   <b>i10-index:</b> ", i10_index
)

# 5. Generate interactive side-by-side grouped bar chart
# with independently scaled Y-axes

# Calculate horizontal offsets for side-by-side bars
bar_width <- 0.35

fig <- plot_ly() %>%

  # Citations bar (left Y-axis)
  add_bars(
    data = plot_data,
    x = ~I(year - bar_width / 2),
    y = ~citations,
    name = "Citations per Year",
    marker = list(color = '#4285F4'),
    width = 0.35,
    yaxis = "y",
    hovertemplate = paste0(
      "Year: %{x:.0f}<br>",
      "Citations: %{y}<extra></extra>"
    )
  ) %>%

  # Publications bar (right Y-axis)
  add_bars(
    data = plot_data,
    x = ~I(year + bar_width / 2),
    y = ~publications,
    name = "Publications per Year",
    marker = list(color = '#DB4437'),
    width = 0.35,
    yaxis = "y2",
    hovertemplate = paste0(
      "Year: %{x:.0f}<br>",
      "Publications: %{y}<extra></extra>"
    )
  ) %>%

  layout(
    title = list(
      text = paste0(
        "Google Scholar Citations and Publications per Year<br>",
        "<span style='font-size:14px; color:#555555;'>",
        summary_text,
        "</span>"
      ),
      y = 0.95
    ),

    margin = list(t = 85),

    # X-axis (no title)
    xaxis = list(
      title = "",
      tickmode = "array",
      tickvals = plot_data$year,
      ticktext = as.character(plot_data$year),
      range = c(
        min(plot_data$year) - 0.6,
        max(plot_data$year) + 0.6
      )
    ),

    # Left Y-axis: Citations (no title)
    yaxis = list(
      title = "",
      side = "left",
      rangemode = "tozero",
      showgrid = TRUE,
      tickfont = list(color = '#4285F4')
    ),

    # Right Y-axis: Publications (no title)
    yaxis2 = list(
      title = "",
      overlaying = "y",
      side = "right",
      rangemode = "tozero",
      showgrid = FALSE,
      tickfont = list(color = '#DB4437')
    ),

    bargap = 0.25,

    legend = list(
      orientation = "h",
      x = 0.25,
      y = -0.2
    )
  )

# 6. Export as a self-contained HTML file
saveWidget(fig, "index.html", selfcontained = TRUE)

message("Successfully generated index.html with citation and publication metrics!")
