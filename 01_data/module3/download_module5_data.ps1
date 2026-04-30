# =============================================================================
# download_module5_data.ps1
# =============================================================================
# PURPOSE:
#   Downloads the large GeoTIFF raster files required by Module 5 from Zenodo
#   and verifies their integrity using SHA256 checksums.
#
# USAGE:
#   Open PowerShell, navigate to this folder, then run:
#     .\download_module5_data.ps1
#
# CONFIGURATION:
#   To update download URLs after publishing to Zenodo, edit ONLY the
#   $ZenodoRecord variable below and the checksums_module5.csv file.
# =============================================================================

# ── Configuration ──
# Replace PLACEHOLDER with the actual Zenodo record number once published.
$ZenodoRecord = "18548350"

# ── Resolve paths ──
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$ManifestPath = Join-Path $ScriptDir "checksums_module5.csv"

Write-Host "=== Module 5 Data Download ===" -ForegroundColor Cyan
Write-Host "Destination: $ScriptDir"

# ── Read manifest ──
if (-not (Test-Path $ManifestPath)) {
    Write-Error "Checksum manifest not found: $ManifestPath"
    Write-Error "Ensure checksums_module5.csv is in the same folder as this script."
    exit 1
}

$manifest = Import-Csv $ManifestPath
Write-Host "Files to download: $($manifest.Count)`n"

$failures = 0

foreach ($entry in $manifest) {
    $fname    = $entry.filename
    $expected = $entry.sha256
    $url      = $entry.source_url
    $sizeMB   = [math]::Round([int64]$entry.size_bytes / 1MB, 1)
    $dest     = Join-Path $ScriptDir $fname
    $index    = [array]::IndexOf($manifest, $entry) + 1

    Write-Host "[$index/$($manifest.Count)] $fname ($sizeMB MB)"

    # Skip if file already exists and checksum matches
    if (Test-Path $dest) {
        Write-Host "  File exists. Verifying checksum... " -NoNewline
        $hash = (Get-FileHash -Path $dest -Algorithm SHA256).Hash.ToLower()
        if ($hash -eq $expected) {
            Write-Host "OK (already verified)" -ForegroundColor Green
            continue
        } else {
            Write-Host "MISMATCH. Re-downloading." -ForegroundColor Yellow
        }
    }

    # Download
    Write-Host "  Downloading from Zenodo... " -NoNewline
    try {
        # Use BITS for large files (shows progress), fall back to WebClient
        $ProgressPreference = 'SilentlyContinue'
        Invoke-WebRequest -Uri $url -OutFile $dest -UseBasicParsing
        Write-Host "done."
    } catch {
        Write-Host "FAILED." -ForegroundColor Red
        Write-Host "  ERROR: $($_.Exception.Message)" -ForegroundColor Red
        $failures++
        continue
    }

    # Verify checksum
    Write-Host "  Verifying SHA256... " -NoNewline
    $hash = (Get-FileHash -Path $dest -Algorithm SHA256).Hash.ToLower()
    if ($hash -eq $expected) {
        Write-Host "OK" -ForegroundColor Green
    } else {
        Write-Host "MISMATCH!" -ForegroundColor Red
        Write-Host "  Expected: $expected" -ForegroundColor Red
        Write-Host "  Got:      $hash" -ForegroundColor Red
        $failures++
    }

    Write-Host ""
}

# ── Summary ──
Write-Host "`n=== Summary ===" -ForegroundColor Cyan
if ($failures -gt 0) {
    Write-Host "$failures file(s) failed. Check messages above." -ForegroundColor Red
    Write-Host "You can manually download from:"
    Write-Host "  https://zenodo.org/records/$ZenodoRecord"
    Write-Host "Place files in: $ScriptDir"
} else {
    Write-Host "All files are ready. You can now run the Module 5 workflow." -ForegroundColor Green
}
