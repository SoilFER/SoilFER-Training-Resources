# =============================================================================
# download_module5_data.R
# =============================================================================
# PURPOSE:
#   Downloads the large GeoTIFF raster files required by Module 5 from Zenodo
#   and verifies their integrity using SHA256 checksums.
#
# USAGE:
#   Set your working directory to this folder, then:
#     source("download_module5_data.R")
#
#   Or from any location:
#     source("path/to/01_data/module5/download_module5_data.R")
#
# REQUIREMENTS:
#   - R >= 4.0
#   - Package: digest (for SHA256 verification)
#
# CONFIGURATION:
#   To update download URLs after publishing to Zenodo, edit ONLY the
#   ZENODO_RECORD variable below and the checksums_module5.csv file.
# =============================================================================

# ── Configuration ──
# Replace PLACEHOLDER with the actual Zenodo record number once published.
ZENODO_RECORD <- "18548350"
ZENODO_BASE   <- paste0("https://zenodo.org/records/", ZENODO_RECORD, "/files/")

# ── Resolve paths ──
# Determine the directory where this script lives
SCRIPT_DIR <- if (interactive()) {
  # RStudio: use the active document's directory
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    dirname(rstudioapi::getActiveDocumentContext()$path)
  } else {
    getwd()
  }
} else {
  # Rscript: parse --file= argument
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- args[grep("--file=", args)]
  if (length(file_arg) > 0) {
    dirname(normalizePath(sub("--file=", "", file_arg)))
  } else {
    getwd()
  }
}

DEST_DIR     <- SCRIPT_DIR
MANIFEST     <- file.path(DEST_DIR, "checksums_module5.csv")

# ── Check for digest package ──
if (!requireNamespace("digest", quietly = TRUE)) {
  message("Installing required package: digest")
  install.packages("digest", repos = "https://cloud.r-project.org")
}
library(digest)

# ── Read manifest ──
if (!file.exists(MANIFEST)) {
  stop("Checksum manifest not found: ", MANIFEST,
       "\nEnsure checksums_module5.csv is in the same folder as this script.")
}

manifest <- read.csv(MANIFEST, stringsAsFactors = FALSE)
cat("=== Module 5 Data Download ===\n")
cat("Destination:", DEST_DIR, "\n")
cat("Files to download:", nrow(manifest), "\n\n")

# ── Download and verify each file ──
results <- data.frame(
  file     = character(),
  status   = character(),
  message  = character(),
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(manifest))) {
  fname    <- manifest$filename[i]
  expected <- manifest$sha256[i]
  url      <- manifest$source_url[i]
  size_mb  <- round(manifest$size_bytes[i] / 1e6, 1)
  dest     <- file.path(DEST_DIR, fname)

  cat(sprintf("[%d/%d] %s (%.0f MB)\n", i, nrow(manifest), fname, size_mb))

  # Skip if file already exists and checksum matches
  if (file.exists(dest)) {
    cat("  File exists. Verifying checksum... ")
    actual <- digest(dest, algo = "sha256", file = TRUE)
    if (identical(actual, expected)) {
      cat("OK (already verified)\n")
      results <- rbind(results, data.frame(
        file = fname, status = "SKIP", message = "Already present, checksum OK"
      ))
      next
    } else {
      cat("MISMATCH. Re-downloading.\n")
    }
  }

  # Download
  cat("  Downloading from Zenodo... ")
  tryCatch({
    download.file(url, dest, mode = "wb", quiet = TRUE)
    cat("done.\n")
  }, error = function(e) {
    cat("FAILED.\n")
    msg <- paste("Download failed:", conditionMessage(e))
    message("  ERROR: ", msg)
    results <<- rbind(results, data.frame(
      file = fname, status = "FAIL", message = msg
    ))
    return()
  })

  # Verify checksum
  cat("  Verifying SHA256... ")
  actual <- digest(dest, algo = "sha256", file = TRUE)
  if (identical(actual, expected)) {
    cat("OK\n")
    results <- rbind(results, data.frame(
      file = fname, status = "OK", message = "Downloaded and verified"
    ))
  } else {
    cat("MISMATCH!\n")
    msg <- paste("Expected:", expected, "Got:", actual)
    message("  ERROR: Checksum mismatch for ", fname)
    message("  ", msg)
    results <- rbind(results, data.frame(
      file = fname, status = "FAIL", message = paste("Checksum mismatch.", msg)
    ))
  }

  cat("\n")
}

# ── Summary ──
cat("\n=== Summary ===\n")
print(results, row.names = FALSE)

n_fail <- sum(results$status == "FAIL")
if (n_fail > 0) {
  warning(n_fail, " file(s) failed. Check messages above.")
  cat("\nIf downloads fail, you can manually download from:\n")
  cat("  https://zenodo.org/records/", ZENODO_RECORD, "\n", sep = "")
  cat("Place files in:", DEST_DIR, "\n")
} else {
  cat("\nAll files are ready. You can now run the Module 5 workflow.\n")
}
