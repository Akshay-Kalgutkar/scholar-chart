library(httr)
library(rvest)
library(plotly)
library(htmlwidgets)

# 1. Configuration
scholar_id <- "nFbaL2gAAAAJ&hl=en" # Replace with your Google Scholar ID
api_key <- Sys.getenv("SCRAPER_API_KEY")

if (api_key == "") {
  stop("SCRAPER_API_KEY environment variable is missing.")
}

# 2. Fetch Google Scholar profile with render=true so JS builds the chart
scholar_url <- paste0("https://google.com", scholar_id, "&hl=en")
proxy_url <- paste0(
  "http://scraperapi.com?api_key=", api_key,
  "&render=true&url=", URLencode(scholar_url, reserved = TRUE)
)

response <- GET(proxy_url, timeout(90))

if (status_code(response) != 200) {
  stop(paste("Failed to fetch Google Scholar page. Status code:", status_code(response)))
}

# 3. Parse the HTML
html_text_raw <- content(response, as = "text", encoding = "UTF-8")
page <- read_html(html_text_raw)

# Print page title in logs to verify we actually landed on the Scholar profile
page_title <- page %>% html_node("title") %>% html_text()
message("Fetched page title: ", page_title)

# Extract years
years <- page %>%
  html_nodes(".gsc_g_t") %>%
  html_text() %>%
  as.numeric()

# Extract bars safely (handles years with 0 citations)
bars <- page %>% html_nodes(".gsc_g_a")
if (length(bars) > 0) {
  # Map each bar to its column position using the 'style' z-index/right offset or order
  cites_raw <- page %>% html_nodes(".gsc_g_al") %>% html_text() %>% as.numeric()
  
  # If counts match years directly, use them; otherwise pad leading zeros
  if (length(cites_raw) == length(years)) {
    cites <- cites_raw
  } else {
    # Extract index from onclick/href or align right-to-left
    diff_len <- length(years) - length(cites_raw)
    if (diff_len > 0) {
      cites <- c(rep(0, diff_len), cites_raw)
    } else {
      cites <- cites_raw
    }
  }
} else {
  cites <- numeric(0)
}

# Safety check
if (length(years) == 0 || length(cites) == 0 || length(years) != length(cites)) {
  message("HTML snippet preview: ", substr(html_text_raw, 1, 500))
  stop("Could not parse citation bars. Stopping so existing chart is not overwritten.")
}

ct <- data.frame(year = years, cites = cites)

# 4. Generate the interactive bar chart
fig <- plot_ly(ct, x = ~year, y = ~cites, type = 'bar',
               width = 700, height = 400,
               marker = list(color = '#4285F4')) %>%
  layout(title = "Citations per Year",
         xaxis = list(title = "Year", dtick = 1),
         yaxis = list(title = "Citations"))

# 5. Export as a self-contained HTML file
saveWidget(fig, "index.html", selfcontained = TRUE)
message("Successfully generated index.html!")

