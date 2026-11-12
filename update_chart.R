library(httr)
library(rvest)
library(plotly)
library(htmlwidgets)

# 1. Configuration
scholar_id <- "nFbaL2gAAAAJ" # Make sure this is YOUR Scholar ID!
api_key <- Sys.getenv("SCRAPER_API_KEY")

if (api_key == "") {
  stop("SCRAPER_API_KEY environment variable is missing.")
}

# 2. Fetch Google Scholar profile using a US IP and JS rendering
scholar_url <- paste0("https://google.com", scholar_id, "&hl=en")
proxy_url <- paste0(
  "http://scraperapi.com?api_key=", api_key,
  "&country_code=us&render=true&url=", URLencode(scholar_url, reserved = TRUE)
)

response <- GET(proxy_url, timeout(90))
html_text_raw <- content(response, as = "text", encoding = "UTF-8")

if (status_code(response) != 200) {
  message("Response body: ", substr(html_text_raw, 1, 500))
  stop(paste("Failed to fetch Google Scholar page. Status code:", status_code(response)))
}

# 3. Parse the HTML and inspect what we got
page <- read_html(html_text_raw)
page_title <- page %>% html_node("title") %>% html_text()
message("=== FETCHED PAGE TITLE: ", page_title, " ===")

# Extract years and citation counts
years <- page %>% html_nodes(".gsc_g_t") %>% html_text() %>% as.numeric()
cites_raw <- page %>% html_nodes(".gsc_g_al") %>% html_text() %>% as.numeric()

message("Years found: ", paste(years, collapse = ", "))
message("Citations found: ", paste(cites_raw, collapse = ", "))

# Align years and citations if leading years had 0 citations
if (length(years) > 0 && length(cites_raw) > 0) {
  if (length(cites_raw) == length(years)) {
    cites <- cites_raw
  } else if (length(years) > length(cites_raw)) {
    cites <- c(rep(0, length(years) - length(cites_raw)), cites_raw)
  } else {
    cites <- numeric(0)
  }
} else {
  cites <- numeric(0)
}

# Safety check with detailed diagnostic output
if (length(years) == 0 || length(cites) == 0) {
  message("=== HTML BODY TEXT PREVIEW ===")
  body_text <- page %>% html_node("body") %>% html_text()
  message(substr(gsub("\\s+", " ", body_text), 1, 600))
  stop("Could not parse citation bars. Check the FETCHED PAGE TITLE and HTML BODY TEXT PREVIEW above.")
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


