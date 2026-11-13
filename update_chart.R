
# ============================================================
# GOOGLE SCHOLAR CITATION DASHBOARD
# Fully Responsive Plotly Chart for Google Sites
# ============================================================

# 1. Load required libraries
library(httr)
library(jsonlite)
library(plotly)
library(htmlwidgets)

# 2. Configuration
# Keep only the 12-character Scholar ID

scholar_id <- gsub(
  "\\s+",
  "",
  "nFbaL2gAAAAJ"
)

api_key <- gsub(
  "\\s+",
  "",
  Sys.getenv("SERPAPI_KEY")
)

if (nchar(api_key) < 30) {
  stop(
    "SERPAPI_KEY is missing or too short. ",
    "Check GitHub Repository Secrets."
  )
}

# 3. Fetch Google Scholar Author JSON

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

raw_body <- content(
  response,
  as = "text",
  encoding = "UTF-8"
)

# Check API response

if (
  status_code(response) != 200 ||
  grepl("^\\s*<", raw_body)
) {
  message("Status code: ", status_code(response))
  message(
    "Content-Type: ",
    headers(response)[["content-type"]]
  )
  message(
    "Response Preview: ",
    substr(raw_body, 1, 300)
  )

  stop(
    "SerpApi did not return valid JSON. ",
    "Check the Content-Type and Preview above."
  )
}

# 4. Parse JSON data

data <- fromJSON(raw_body)

if (!is.null(data$error)) {
  stop(
    paste(
      "SerpApi returned an error:",
      data$error
    )
  )
}

# Extract citation graph

ct <- data$cited_by$graph

if (is.null(ct) || nrow(ct) == 0) {
  stop(
    "No citation graph data found. ",
    "Verify your Scholar ID."
  )
}

# Extract citation summary metrics

tbl <- data$cited_by$table

total_cites <- tbl$citations$all[1]
h_index     <- tbl$h_index$all[2]
i10_index   <- tbl$i10_index$all[3]

# 5. Build summary metrics banner

summary_text <- paste0(
  "<b>Total Citations:</b> ", total_cites,
  " &nbsp; | &nbsp; ",
  "<b>h-index:</b> ", h_index,
  " &nbsp; | &nbsp; ",
  "<b>i10-index:</b> ", i10_index
)

# 6. Generate responsive interactive Plotly chart

fig <- plot_ly(
  data = ct,
  x = ~year,
  y = ~citations,
  type = "bar",
  marker = list(
    color = "#4285F4"
  ),
  text = ~citations,
  textposition = "outside",
  hovertemplate = paste(
    "<b>Year:</b> %{x}<br>",
    "<b>Citations:</b> %{y}",
    "<extra></extra>"
  )
) %>%

  layout(
    autosize = TRUE,

    title = list(
      text = paste0(
        "Google Scholar Citations per Year<br>",
        "<span style='font-size:14px; color:#555555;'>",
        summary_text,
        "</span>"
      ),
      y = 0.96,
      x = 0.5,
      xanchor = "center"
    ),

    margin = list(
      t = 100,
      b = 65,
      l = 65,
      r = 30
    ),

    xaxis = list(
      title = "Year",
      dtick = 1,
      automargin = TRUE
    ),

    yaxis = list(
      title = "Citations",
      automargin = TRUE,
      rangemode = "tozero"
    ),

    paper_bgcolor = "white",
    plot_bgcolor = "white",

    font = list(
      family = "Arial, sans-serif",
      size = 12,
      color = "#333333"
    ),

    showlegend = FALSE
  ) %>%

  config(
    responsive = TRUE,
    displayModeBar = TRUE,
    displaylogo = FALSE,
    modeBarButtonsToRemove = c(
      "lasso2d",
      "select2d"
    )
  )

# 7. Add responsive HTML/CSS styling

responsive_html <- tags$div(
  style = paste0(
    "width:100%;",
    "height:100%;",
    "min-height:350px;",
    "margin:0;",
    "padding:0;",
    "overflow:hidden;"
  ),
  fig
)

# 8. Export self-contained responsive HTML

html_file <- "index.html"

saveWidget(
  widget = responsive_html,
  file = html_file,
  selfcontained = TRUE,
  background = "white"
)

message("----------------------------------------")
message("Successfully generated index.html!")
message("Google Scholar Citation Dashboard")
message("----------------------------------------")
message("Total Citations: ", total_cites)
message("h-index: ", h_index)
message("i10-index: ", i10_index)
message("----------------------------------------")
message("Chart layout: Responsive")
message("Chart width: 100% of available space")
message("Chart height: Adapts to container")
message("----------------------------------------")
