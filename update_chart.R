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

# 2. Fetch Google Scholar profile page through ScraperAPI to avoid HTTP 403
scholar_url <- paste0("https://google.com", scholar_id, "&hl=en")
proxy_url <- paste0("http://scraperapi.com?api_key=", api_key, "&url=", URLencode(scholar_url, reserved = TRUE))

response <- GET(proxy_url, timeout(60))

if (status_code(response) != 200) {
  stop(paste("Failed to fetch Google Scholar page. Status code:", status_code(response)))
}

# 3. Parse the year-wise citation bar chart data from the HTML
page <- read_html(content(response, as = "text", encoding = "UTF-8"))

years <- page %>%
  html_nodes(".gsc_g_t") %>%
  html_text() %>%
  as.numeric()

cites <- page %>%
  html_nodes(".gsc_g_al") %>%
  html_text() %>%
  as.numeric()

# Safety check: stop if Google Scholar returned an empty page or CAPTCHA
if (length(years) == 0 || length(cites) == 0 || length(years) != length(cites)) {
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
