library(rjson)
library(rstac)

# Null-coalescing operator (define if missing to prevent runtime errors)
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

# Load BON in a Box input 
input <- biab_inputs()
stac_url <- input$stac_url  # STAC API URL
if (is.null(stac_url) || !is.character(stac_url) || nchar(trimws(stac_url)) == 0) {
  biab_error_stop("Input 'stac_url' is required and must be a non-empty string.")
}

biab_info(sprintf("Listing STAC collections from: %s", stac_url))


# stac_coll function to list collections and some characteristics
stac_coll <- function(url) {
  stac_obj <- stac(url)
  coll_req <- collections(stac_obj)
  response <- get_request(coll_req)

  meta <- lapply(response$collections, function(x) {
    coll_url <- paste0(url, "/collections/", utils::URLencode(x$id, reserved = TRUE))
    list(
      id = x$id,
      title = x$title %||% NA,
      #description = x$description %||% NA,
      collection_url = coll_url
    )
  })

  do.call(rbind, lapply(meta, as.data.frame))
}

# Run
collections_df <- tryCatch(
  stac_coll(stac_url),
  error = function(e) biab_error_stop(sprintf("Failed to query STAC: %s", conditionMessage(e)))
)

# Write outputs
out_csv <- file.path(outputFolder, "collections.csv")
utils::write.csv(collections_df, out_csv, row.names = FALSE)

# Register outputs
biab_output("collections_csv", out_csv)

biab_info(sprintf("Wrote %d collections to %s", nrow(collections_df), out_csv))
