library(rjson)
library(rstac)
library(terra)

# Null-coalescing operator (define if missing)
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

#Bon in a Box inputs
input <- biab_inputs()
collection_url <- input$collection_url
initial_date   <- input$initial_date
final_date     <- input$final_date

if (is.null(collection_url) || nchar(trimws(collection_url)) == 0) {
  biab_error_stop("Input 'collection_url' is required and must be a non-empty string.")
}

# Extract base STAC URL and collection id from a collection URL
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

# Build STAC datetime range or return NULL when dates are empty
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

# Fetch all items (optionally filtered) and return list of features
.get_stac <- function(base_url) {
  # Try autodetect; if it fails, force STAC 1.0.0 (common for servers without landing page hints)
  out <- tryCatch(stac(base_url), error = function(e) NULL)
  if (!is.null(out)) return(out)
  tryCatch(stac(base_url, force_version = "1.0.0"), error = function(e) stop(e))
}

fetch_items <- function(base_url, collection_id, datetime_range) {
  s <- .get_stac(base_url)
  req <- collections(s, collection_id) |>
    items(limit = 1000, datetime = datetime_range) |>
    get_request() |>
    items_fetch()
  req$features %||% list()
}

# read json bands.spatial_resolution; if not present read GeoTIFF to derive pixel size
asset_spatial_resolution <- function(asset, verbose_item_id = NULL) {
  rb <- asset[["raster:bands"]]
  if (!is.null(rb) && length(rb) >= 1) {
    sr <- rb[[1]][["spatial_resolution"]]
    if (!is.null(sr)) return(as.character(sr))
  }
  href <- asset$href %||% ""
  if (!nzchar(href)) return(NA_character_)
  res_str <- tryCatch({
    r <- terra::rast(href)
    rr <- terra::res(r)
    if (length(rr) >= 2) {
      if (isTRUE(all.equal(rr[1], rr[2]))) as.character(rr[1]) else paste(rr[1], rr[2], sep = ",")
    } else as.character(rr[1])
  }, error = function(e) NA_character_)
  res_str
}

# Select the FIRST item and its FIRST usable asset
first_item_and_asset <- function(feats) {
  if (!length(feats)) return(list(feature = NULL, asset_key = NULL, asset = NULL))
  # choose the first feature that has at least one asset
  for (f in feats) {
    assets <- tryCatch(f$assets, error = function(e) NULL)
    if (!is.null(assets) && length(assets) > 0) {
      keys <- names(assets)
      # preference: asset with raster:bands -> tiff by media_type/href -> first
      has_rb <- which(vapply(assets, function(a) !is.null(a[["raster:bands"]]), logical(1)))
      if (length(has_rb)) {
        k <- keys[has_rb[1]]; return(list(feature = f, asset_key = k, asset = assets[[k]]))
      }
      is_tif <- function(a) {
        mt <- a[["type"]] %||% a[["media_type"]] %||% ""
        href <- a$href %||% ""
        grepl("tif(f)?$", tolower(href)) || grepl("tif", tolower(mt))
      }
      tif_idx <- which(vapply(assets, is_tif, logical(1)))
      if (length(tif_idx)) {
        k <- keys[tif_idx[1]]; return(list(feature = f, asset_key = k, asset = assets[[k]]))
      }
      k <- keys[1]; return(list(feature = f, asset_key = k, asset = assets[[k]]))
    }
  }
  list(feature = NULL, asset_key = NULL, asset = NULL)
}


# Parse collection URL and build RFC3339 datetime range (or NULL).
parsed <- parse_collection_url(collection_url)
dt_range <- build_datetime_range(initial_date, final_date)

if (is.null(dt_range)) {
  biab_info(sprintf("Processing ALL items from collection '%s'", parsed$id))
} else {
  biab_info(sprintf("Processing items from collection '%s' between %s", parsed$id, dt_range))
}

# Fetch items
feats <- tryCatch(
  fetch_items(parsed$base, parsed$id, dt_range),
  error = function(e) {
    biab_error_stop(sprintf(
      "Failed to query STAC: the URL '%s' is not a valid STAC collection or is unreachable.",
      collection_url
    ))
  }
)

# ---------- Output 1: items_csv ------------------
# Build tokens "collection|item"
if (length(feats) == 0) {
  tokens <- character(0)
} else {
  ids <- vapply(feats, function(f) as.character(f$id), character(1))
  tokens <- paste(parsed$id, ids, sep = "|")
}

# Filename
slug <- tolower(gsub("[^a-zA-Z0-9]+", "_", parsed$id))
slug <- gsub("_+", "_", slug)
slug <- sub("^_+|_+$", "", slug)
if (nchar(slug) == 0) slug <- "collection"

items_csv <- file.path(outputFolder, sprintf("items_%s.csv", slug))

# Write items line
if (length(tokens) == 0) {
  file.create(items_csv)
} else {
  con <- file(items_csv, open = "wt", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(paste(tokens, collapse = ","), con = con, useBytes = TRUE)
}
biab_output("items_csv", items_csv)
biab_info(sprintf("Wrote %d item ids to %s", length(tokens), items_csv))

# ---------- Output 2: items_props_csv ----------------
# Filename for item properties
props_csv <- file.path(outputFolder, sprintf("items_props_%s.csv", slug))

write_props_lines <- function(path, bbox, epsg, sres, asset_key = NULL) {
  key_bbox <- "features.properties.proj:bbox"
  key_epsg <- "features.properties.proj:epsg"
  key_sres <- if (!is.null(asset_key)) sprintf("features.assets.%s.raster:bands.spatial_resolution", asset_key)
              else "features.assets.raster:bands.spatial_resolution"
  lines <- c(
    sprintf("%s: %s", key_bbox, ifelse(is.na(bbox) | !nzchar(bbox), "NA", bbox)),
    sprintf("%s: %s", key_epsg, ifelse(is.na(epsg) | !nzchar(as.character(epsg)), "NA", as.character(epsg))),
    sprintf("%s: %s", key_sres, ifelse(is.na(sres) | !nzchar(as.character(sres)), "NA", as.character(sres)))
  )
  con <- file(path, open = "wt", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(lines, con, useBytes = TRUE)
}

if (length(feats) == 0) {
  file.create(props_csv)  # no items -> empty file
  biab_output("items_props_csv", props_csv)
} else {
  sel <- first_item_and_asset(feats)
  if (is.null(sel$feature) || is.null(sel$asset)) {
    file.create(props_csv)  # no usable asset -> empty file
    biab_output("items_props_csv", props_csv)
  } else {
    f1 <- sel$feature
    asset_key <- sel$asset_key
    asset <- sel$asset

    bbox_vals <- f1$properties[["proj:bbox"]]
    if (is.null(bbox_vals)) bbox_vals <- f1$bbox
    bbox <- if (is.null(bbox_vals)) NA_character_ else paste(as.numeric(unlist(bbox_vals)), collapse = ",")
    epsg <- f1$properties[["proj:epsg"]] %||% NA
    sres <- asset_spatial_resolution(asset, verbose_item_id = f1$id)

    write_props_lines(props_csv, bbox, epsg, sres, asset_key)
    biab_output("items_props_csv", props_csv)
  }
}
