# dropbox_auth.R
# Include this file in your Shiny app project

library(httr)
library(fst)

# Token cache (persists for the R session)
.dropbox_cache <- new.env(parent = emptyenv())

# Function to get a fresh access token using refresh token
get_dropbox_token <- function(force_refresh = FALSE) {
  
  
  # Check if we have a cached token that's still valid
  if (!force_refresh && 
      exists("access_token", envir = .dropbox_cache) &&
      exists("expires_at", envir = .dropbox_cache)) {
    
    expires_at <- get("expires_at", envir = .dropbox_cache)
    
    # Refresh if less than 5 minutes remaining (300 seconds buffer)
    if (Sys.time() < expires_at - 300) {
      return(get("access_token", envir = .dropbox_cache))
    }
  }
  
  # Need to refresh - get credentials from environment
  
  refresh_token <- Sys.getenv("DROPBOX_REFRESH_TOKEN")
  app_key <- Sys.getenv("DROPBOX_APP_KEY")
  app_secret <- Sys.getenv("DROPBOX_APP_SECRET")
  
  if (refresh_token == "" || app_key == "" || app_secret == "") {
    stop("Dropbox credentials not found in environment variables")
  }
  
  response <- POST(
    "https://api.dropboxapi.com/oauth2/token",
    body = list(
      refresh_token = refresh_token,
      grant_type = "refresh_token",
      client_id = app_key,
      client_secret = app_secret
    ),
    encode = "form"
  )
  
  if (http_error(response)) {
    stop("Failed to refresh Dropbox token: ", content(response, "text"))
  }
  
  token_data <- content(response)
  
  # Cache the new token
  # Dropbox tokens typically expire in 14400 seconds (4 hours)
  expires_in <- token_data$expires_in %||% 14400
  
  assign("access_token", token_data$access_token, envir = .dropbox_cache)
  assign("expires_at", Sys.time() + expires_in, envir = .dropbox_cache)
  
  message("Dropbox token refreshed, expires at: ", 
          get("expires_at", envir = .dropbox_cache))
  
  token_data$access_token
}

# Clear the token cache (useful for debugging)
dropbox_clear_cache <- function() {
  rm(list = ls(envir = .dropbox_cache), envir = .dropbox_cache)
  message("Dropbox token cache cleared")
}

# Check cache status
dropbox_cache_status <- function() {
  if (!exists("expires_at", envir = .dropbox_cache)) {
    message("No token cached")
    return(invisible(NULL))
  }
  
  expires_at <- get("expires_at", envir = .dropbox_cache)
  remaining <- as.numeric(difftime(expires_at, Sys.time(), units = "mins"))
  
  if (remaining > 0) {
    message(sprintf("Token valid for %.1f more minutes", remaining))
  } else {
    message("Token expired")
  }
  
  invisible(list(
    expires_at = expires_at,
    remaining_minutes = remaining
  ))
}

# Download a file from Dropbox
dropbox_download <- function(dropbox_path, local_path = NULL) {
  token <- get_dropbox_token()
  
  if (is.null(local_path)) {
    local_path <- tempfile(fileext = paste0(".", tools::file_ext(dropbox_path)))
  }
  
  response <- POST(
    "https://content.dropboxapi.com/2/files/download",
    add_headers(
      Authorization = paste("Bearer", token),
      `Dropbox-API-Arg` = jsonlite::toJSON(
        list(path = dropbox_path),
        auto_unbox = TRUE
      )
    ),
    write_disk(local_path, overwrite = TRUE)
  )
  
  if (http_error(response)) {
    # If auth error, try once more with a fresh token
    if (status_code(response) == 401) {
      message("Token rejected, forcing refresh...")
      token <- get_dropbox_token(force_refresh = TRUE)
      
      response <- POST(
        "https://content.dropboxapi.com/2/files/download",
        add_headers(
          Authorization = paste("Bearer", token),
          `Dropbox-API-Arg` = jsonlite::toJSON(
            list(path = dropbox_path),
            auto_unbox = TRUE
          )
        ),
        write_disk(local_path, overwrite = TRUE)
      )
    }
    
    if (http_error(response)) {
      stop("Failed to download file: ", content(response, "text"))
    }
  }
  
  local_path
}

# Read a CSV directly from Dropbox
dropbox_read_csv <- function(dropbox_path, ...) {
  local_file <- dropbox_download(dropbox_path)
  on.exit(unlink(local_file))
  read.csv(local_file, ...)
}

# Read an RDS file from Dropbox
dropbox_read_rds <- function(dropbox_path) {
  local_file <- dropbox_download(dropbox_path)
  on.exit(unlink(local_file))
  readRDS(local_file)
}


# Read an FST file from Dropbox (requires fst)
dropbox_read_fst <- function(dropbox_path, ...) {
  if (!requireNamespace("fst", quietly = TRUE)) {
    stop("Package 'fst' required for FST files")
  }
  local_file <- dropbox_download(dropbox_path)
  on.exit(unlink(local_file))
  fst::read_fst(local_file, ...)
}



# Read an Excel file from Dropbox (requires readxl)
dropbox_read_excel <- function(dropbox_path, ...) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package 'readxl' required for Excel files")
  }
  local_file <- dropbox_download(dropbox_path)
  on.exit(unlink(local_file))
  readxl::read_excel(local_file, ...)
}

# List files in a Dropbox folder
dropbox_list_files <- function(dropbox_folder_path) {
  token <- get_dropbox_token()

  response <- POST(
    "https://api.dropboxapi.com/2/files/list_folder",
    add_headers(
      Authorization = paste("Bearer", token),
      `Content-Type` = "application/json"
    ),
    body = jsonlite::toJSON(
      list(path = dropbox_folder_path),
      auto_unbox = TRUE
    )
  )

  if (http_error(response)) {
    # Retry with fresh token if auth error
    if (status_code(response) == 401) {
      token <- get_dropbox_token(force_refresh = TRUE)

      response <- POST(
        "https://api.dropboxapi.com/2/files/list_folder",
        add_headers(
          Authorization = paste("Bearer", token),
          `Content-Type` = "application/json"
        ),
        body = jsonlite::toJSON(
          list(path = dropbox_folder_path),
          auto_unbox = TRUE
        )
      )
    }

    if (http_error(response)) {
      stop("Failed to list folder: ", content(response, "text"))
    }
  }

  entries <- content(response)$entries

  if (length(entries) == 0) {
    return(data.frame(
      name = character(),
      path = character(),
      type = character()
    ))
  }

  data.frame(
    name = sapply(entries, `[[`, "name"),
    path = sapply(entries, `[[`, "path_display"),
    type = sapply(entries, `[[`, ".tag")
  )
}

# Upload a file to Dropbox
dropbox_upload <- function(local_path, dropbox_path, mode = "overwrite") {
  token <- get_dropbox_token()

  # Read the file as raw bytes
  file_content <- readBin(local_path, "raw", file.info(local_path)$size)

  response <- POST(
    "https://content.dropboxapi.com/2/files/upload",
    add_headers(
      Authorization = paste("Bearer", token),
      `Content-Type` = "application/octet-stream",
      `Dropbox-API-Arg` = jsonlite::toJSON(
        list(
          path = dropbox_path,
          mode = mode,
          autorename = FALSE,
          mute = FALSE
        ),
        auto_unbox = TRUE
      )
    ),
    body = file_content
  )

  if (http_error(response)) {
    # If auth error, try once more with a fresh token
    if (status_code(response) == 401) {
      message("Token rejected, forcing refresh...")
      token <- get_dropbox_token(force_refresh = TRUE)

      response <- POST(
        "https://content.dropboxapi.com/2/files/upload",
        add_headers(
          Authorization = paste("Bearer", token),
          `Content-Type` = "application/octet-stream",
          `Dropbox-API-Arg` = jsonlite::toJSON(
            list(
              path = dropbox_path,
              mode = mode,
              autorename = FALSE,
              mute = FALSE
            ),
            auto_unbox = TRUE
          )
        ),
        body = file_content
      )
    }

    if (http_error(response)) {
      stop("Failed to upload file: ", content(response, "text"))
    }
  }

  result <- content(response)
  message("File uploaded to: ", result$path_display)
  invisible(result)
}

# Write an FST file to Dropbox (requires fst)
dropbox_write_fst <- function(x, dropbox_path, ...) {
  if (!requireNamespace("fst", quietly = TRUE)) {
    stop("Package 'fst' required for FST files")
  }

  # Write to temporary file first
  local_file <- tempfile(fileext = ".fst")
  on.exit(unlink(local_file))

  fst::write_fst(x, local_file, ...)

  # Upload to Dropbox
  dropbox_upload(local_file, dropbox_path)
}