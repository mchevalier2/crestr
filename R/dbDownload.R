#' Download gbif4crest data from Figshare
#'
#' Downloads a packaged gbif4crest release hosted on Figshare and saves it as a
#' local `.zip` file.
#'
#' @param filename Character. Destination file path for the downloaded archive.
#'   If it has no `.zip` extension, `.zip` is appended. If left as the default
#'   `"gbif4crest_03.zip"`, it is replaced by the appropriate release filename.
#' @param version Integer. Dataset major version. Currently supported: `2` or `3`.
#' @param lite Logical. Only relevant for `version = 2` and `res = "15min"`.
#'   If `TRUE`, downloads the curated/lite variant when available.
#'   Ignored for `version = 3`.
#' @param res Character. Spatial resolution. One of `"15min"` or `"5min"`.
#'   Note: `version = 3` is currently available only as `"5min"`.
#' @param timeout Numeric. Maximum time in seconds for the transfer (passed to libcurl).
#' @param overwrite Logical. If `TRUE`, overwrite an existing `filename`.
#'   If `FALSE`, an existing file triggers an error.
#' @param quiet Logical. If `TRUE`, suppresses the download progress meter.
#' @param max_tries Integer. Maximum number of attempts for transient failures.
#'   (Most issues should not require retries when using the API endpoint.)
#'
#' @return Invisibly returns the absolute path to the downloaded file.
#'
#' @examples
#' \dontrun{
#' # Default: v3 5min
#' dbDownload()
#'
#' # Version 2, 15min lite
#' dbDownload(version = 2, res = "15min", lite = TRUE)
#'
#' # Save to a custom location
#' dbDownload(filename = "data/gbif4crest.zip", quiet = FALSE)
#' }
#'
#' @export
dbDownload <- function(filename  = "gbif4crest_03.zip",
                       version   = 3,
                       lite      = TRUE,
                       res       = "5min",
                       timeout   = 10000,
                       overwrite = TRUE,
                       quiet     = FALSE,
                       max_tries = 3) {

  # Dependency (kept minimal: curl only)
  if (!requireNamespace("curl", quietly = TRUE)) {
    stop("Package 'curl' is required for dbDownload(). Install it with install.packages('curl').",
         call. = FALSE)
  }

  # ---- validate args ----
  version <- as.integer(version)
  if (!version %in% c(2L, 3L)) stop("`version` must be 2 or 3.", call. = FALSE)

  res <- tolower(res)
  if (!res %in% c("15min", "5min")) stop("`res` must be '15min' or '5min'.", call. = FALSE)

  if (!isTRUE(overwrite) && !isFALSE(overwrite)) stop("`overwrite` must be TRUE or FALSE.", call. = FALSE)
  if (!isTRUE(quiet) && !isFALSE(quiet)) stop("`quiet` must be TRUE or FALSE.", call. = FALSE)

  if (!is.numeric(timeout) || length(timeout) != 1 || is.na(timeout) || timeout <= 0) {
    stop("`timeout` must be a single positive number (seconds).", call. = FALSE)
  }

  if (!is.numeric(max_tries) || length(max_tries) != 1 || is.na(max_tries) || max_tries < 1) {
    stop("`max_tries` must be a single integer >= 1.", call. = FALSE)
  }
  max_tries <- as.integer(max_tries)

  if (version == 3L && !isTRUE(lite)) {
    warning("`lite` is ignored for version 3.", call. = FALSE)
  }

  # Ensure .zip extension
  if (tools::file_ext(filename) != "zip") filename <- paste0(filename, ".zip")

  # ---- map (version, res, lite) -> figshare file_id + default filename ----
  key <- if (version == 2L) {
    paste0("v2_", res, "_", if (isTRUE(lite) && res == "15min") "lite" else "full")
  } else {
    paste0("v3_", res)
  }

  # Store Figshare *file_id*s (not ndownloader URLs)
  map <- list(
    v2_15min_lite = list(file_id = 25071872L, default = "gbif4crest_02_15min_lite.zip"),
    v2_15min_full = list(file_id = 36126908L, default = "gbif4crest_02_15min.zip"),
    v2_5min_full  = list(file_id = 42606571L, default = "gbif4crest_02_5min.zip"),
    v3_5min       = list(file_id = 51983723L, default = "gbif4crest_03_5min.zip")
  )

  if (!key %in% names(map)) {
    if (version == 3L && res != "5min") {
      stop("Version 3 is only available at res = '5min'.", call. = FALSE)
    }
    stop("Requested combination not available. Check `version`, `res`, and `lite`.", call. = FALSE)
  }

  info <- map[[key]]

  # Replace the generic default with the dataset-specific default
  if (identical(filename, "gbif4crest_03.zip")) filename <- info$default

  if (file.exists(filename)) {
    if (isTRUE(overwrite)) {
      unlink(filename)
    } else {
      stop("Destination file already exists and `overwrite = FALSE`: ", filename, call. = FALSE)
    }
  }

  # Figshare public API download endpoint (more reliable than /ndownloader/)
  url <- paste0("https://api.figshare.com/v2/file/download/", info$file_id)

  # Atomic download: write to .part then rename
  tmp <- paste0(filename, ".part")
  on.exit({
    if (file.exists(tmp)) unlink(tmp)
  }, add = TRUE)

  # Configure curl handle
  h <- curl::new_handle()
  curl::handle_setopt(
    h,
    followlocation = 1L,
    timeout        = as.integer(timeout),
    connecttimeout = 30L
  )
  curl::handle_setheaders(
    h,
    "User-Agent" = "Mozilla/5.0",
    "Referer"    = "https://figshare.com/",
    "Accept"     = "*/*"
  )

  # ---- download with limited retries ----
  last_error <- NULL
  for (i in seq_len(max_tries)) {
    if (file.exists(tmp)) unlink(tmp)

    ok <- tryCatch({
      # This prints libcurl's progress meter when quiet = FALSE
      curl::curl_download(url, destfile = tmp, handle = h, quiet = quiet)
      TRUE
    }, error = function(e) {
      last_error <<- e
      FALSE
    })

    size <- if (file.exists(tmp)) file.info(tmp)$size else NA_real_

    if (isTRUE(ok) && !is.na(size) && size > 0) {
      # Move into place
      if (!file.rename(tmp, filename)) {
        file.copy(tmp, filename, overwrite = TRUE)
        unlink(tmp)
      }
      out <- tools::file_path_as_absolute(filename)
      if (!quiet) cat("File downloaded and saved at:", out, "\n")
      return(invisible(out))
    }

    # brief backoff for transient issues
    Sys.sleep(min(2^(i - 1), 10))
  }

  # If we get here, all tries failed
  if (!is.null(last_error)) {
    stop("Download failed after ", max_tries, " attempt(s): ", conditionMessage(last_error),
         call. = FALSE)
  }
  stop("Download failed after ", max_tries, " attempt(s).", call. = FALSE)
}
