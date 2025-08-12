library(rjson)
library(rstac)

# Null-coalescing operator (define if missing)
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

# ---- helpers -------------------------------------------------------------
# Why: robustly extract base STAC URL and collection id from a collection URL
parse_collection_url <- function(u) {
  u <- trimws(u)
  if (!grepl("^https?://", u)) biab_error_stop("'collection_url' must start with http:// or https://")
  u2 <- sub("/+$", "", u)
  m <- regexec("/collections/([^/?#]+)", u2, perl = TRUE)
  mm <- regmatches(u2, m)
  if (length(mm) == 0 || length(mm[[1]]) < 2) {
    biab_error_stop("'collection_url' must include /collections/{collection_id}")
  }
  coll_id <- utils::URLdecode(mm[[1]][2])
  base <- sub("(/collections/.*)$", "", u2)
  list(base = base, id = coll_id)
}

# Why: build STAC datetime range or return NULL when dates are empty
build_datetime_range <- function(start, end) {
  norm <- function(s) {
    if (is.null(s)) return("")
    s <- trimws(as.character(s))
    if (identical(s, "NULL") || identical(s, "NA")) s <- ""
    s
  }
  start <- norm(start); end <- norm(end)
  if (start == "" || end == "") return(NULL)
  is_iso <- function(s) is.character(s) && grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", s) && !is.na(as.Date(s))
  if (!is_iso(start) || !is_iso(end)) biab_error_stop("Dates must be in 'YYYY-MM-DD' format when provided.")
  if (as.Date(start) > as.Date(end)) biab_error_stop("'Initial date' must be on or before 'Final date'.")
  # STAC expects RFC3339; use full-day bounds in UTC
  paste0(start, "T00:00:00Z/", end, "T23:59:59Z")
}

# Core: fetch all items and return vector of "collection|item_id"
fetch_item_tokens <- function(base_url, collection_id, datetime_range) {
  req <- stac(base_url) |>
    stac_search(collections = collection_id, datetime = datetime_range, limit = 1000) |>
    get_request() |>
    items_fetch()

  feats <- req$features %||% list()
  if (length(feats) == 0) return(character(0))

  # Defensive: if server ignored `datetime`, filter client-side using properties$datetime
  if (!is.null(datetime_range)) {
    rng <- strsplit(datetime_range, "/", fixed = TRUE)[[1]]
    parse_z <- function(x) as.POSIXct(sub("Z$", "", x), format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
    start_dt <- parse_z(rng[1]); end_dt <- parse_z(rng[2])

    feat_dt <- vapply(feats, function(f) {
      dt <- tryCatch(f$properties$datetime, error = function(e) NULL)
      if (is.null(dt)) return(NA_character_)
      as.character(dt)
    }, character(1))

    keep <- vapply(feat_dt, function(dt) {
      if (is.na(dt) || !nzchar(dt)) return(FALSE)
      ts <- parse_z(dt)
      !is.na(ts) && ts >= start_dt && ts <= end_dt
    }, logical(1))
    feats <- feats[keep]
  }

  if (length(feats) == 0) return(character(0))
  ids <- vapply(feats, function(f) as.character(f$id), character(1))
  paste(collection_id, ids, sep = "|")
}

# ---- BON in a Box I/O ---------------------------------------------------
input <- biab_inputs()
collection_url <- input$collection_url
initial_date   <- input$initial_date
final_date     <- input$final_date

if (is.null(collection_url) || nchar(trimws(collection_url)) == 0) {
  biab_error_stop("Input 'collection_url' is required and must be a non-empty string.")
}


parsed <- parse_collection_url(collection_url)
dt_range <- build_datetime_range(initial_date, final_date)

if (is.null(dt_range)) {
  biab_info(sprintf("Listing all items from collection '%s'", parsed$id))
} else {
  biab_info(sprintf("Listing items from collection '%s' between %s", parsed$id, dt_range))
} 

# fetch
tokens <- tryCatch(
  fetch_item_tokens(parsed$base, parsed$id, dt_range),
  error = function(e) {
    biab_error_stop(sprintf(
      "Failed to query STAC: the URL '%s' is not a valid STAC collection or is unreachable.",
      collection_url
    ))
  }
)

# output file name: items_<collection>.csv
slug <- tolower(gsub("[^a-zA-Z0-9]+", "_", parsed$id))
slug <- gsub("_+", "_", slug)
slug <- sub("^_+|_+$", "", slug)
if (nchar(slug) == 0) slug <- "collection"

out_csv <- file.path(outputFolder, sprintf("items_%s.csv", slug))

# write: single line, comma-separated, no header
if (length(tokens) == 0) {
  file.create(out_csv)  # create empty file if no items
} else {
  con <- file(out_csv, open = "wt", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(paste(tokens, collapse = ","), con = con, useBytes = TRUE)
}

biab_output("items_csv", out_csv)
biab_info(sprintf("Wrote %d item ids to %s", length(tokens), out_csv))