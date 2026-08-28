## code to prepare `DATASET` dataset goes here

# libraries
install.packages("httr")
install.packages("jsonlite")

# Call all reports in JSON

# Function to write JSON to citations format for bib file
# default is an example API call
json_to_bib <- function(API = "https://jsonplaceholder.typicode.com/posts") {
  metadata <- httr::GET(API)
  # could add "query = " to GET and call since previous months and set to append or just remake every month?
  # extract json metadata
  sis_df <- jsonlite::fromJSON(rawToChar(metadata$content))
  # create citation format
  # create keys list to verify each are unique
  all_keys <- c()
  citations <- c()
  for (i in 1:nrow(sis_df)) {
    # extract metadata
    title <- sis_df$title[i]
    # TODO: properly format authors for citation
    authors <- sis_df$authors[i]
    institution <- sis_df$institution[i]
    city <- sis_df$city[i]
    state <- sis_df$state[i]
    DOI <- sis_df$DOI[i]
    URL <- sis_df$URL[i]
    year <- sis_df$year[i]
    month <- sis_df$month[i]
    
    key = glue::glue("{authors}_{year}") # need to separate out authors and only use the first
    # check if non-unique
    if (key %in% all_keys) {
      key <- glue::glue("{key}b") # TOOD: add some unique IDfier
    } 
    all_keys <- append(all_keys, key)
    # template
    citation <- paste0(
      "@techreport{", key, ",\n",
      "  type        = {stockassessment},\n",
      "  title       = {", title, "},\n",
      "  author      = {", authors, "},\n",
      "  institution = {", institution, "},\n",
      "  address     = {", city,", ", state, "},\n",
      "  DOI         = {", DOI, "},\n",
      "  URL         = {", URL, "},\n",
      "  year        = {", year, "},\n",
      "  month       = {", month, "}\n",
      "}\n\n"
    )
    citations <- c(citations, citation)
  } # close row loop
  # format and save
  writeLines(cat(citations), "inst/bib/nmfs_sars.bib")
} # close function

json_to_bib()