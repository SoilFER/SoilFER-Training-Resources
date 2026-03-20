# =============================================================================
# app.R - GLOSIS Harmonization Shiny App
# =============================================================================
# =============================================================================
# BOX STATUS COLORS SUMMARY
# =============================================================================
# The 'status' parameter in box() determines the header color:
#
# status = "primary"   → BLUE (#3c8dbc)      - General information, neutral
# status = "success"   → GREEN (#00a65a)     - Positive status, success
# status = "info"      → LIGHT BLUE (#00c0ef)- Informational content
# status = "warning"   → YELLOW/ORANGE (#f39c12) - Warnings, caution
# status = "danger"    → RED (#dd4b39)       - Critical issues, errors
#
# The skin = "red" parameter sets the overall theme color for:
# - Dashboard header background
# - Active menu items in sidebar
# - Progress bars
# - Primary buttons
# =============================================================================

# Load libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(openxlsx)
library(DT)
library(readxl)
library(lubridate)


# ---------- Helpers ----------

# Define your keyword list
keywords <- c("bd", "bulk_density", "soc", "som", "c_tot", "ph", "ph_h2o",
              "ph_water", "clay", "silt", "sand", "ec", "cec", "esp", "sar", 
              "n_tot", "p_tot", "p_mehlich", "p_olsen", "p_avail", 
              "k_tot", "k_ext", "k_avail")


# Create a pattern that matches columns starting with any keyword (case-insensitive)
pattern <- paste0("^(", paste(keywords, collapse = "|"), ")", collapse = "")


`%||%` <- function(a, b) if (!is.null(a)) a else b

# helper: pick first matching column name, otherwise fallback
pick_col <- function(cols, preferred, fallback = cols[1]) {
  hit <- preferred[preferred %in% cols]
  if (length(hit) > 0) hit[1] else fallback
}

safe_read_csv <- function(path) {
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

clear_region <- function(wb, sheet, rows, cols) {
  if (sheet %in% names(wb)) {
    deleteData(wb, sheet = sheet, rows = rows, cols = cols, gridExpand = TRUE)
  }
}

safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", x)

# Robust reader for your semicolon-separated procedures file + encoding quirks + trailing ;;;;
read_procedures_csv <- function(path) {
  validate(need(file.exists(path),
                paste("Missing file:", path, "(place it in the app folder).")))
  
  txt <- readLines(path, warn = FALSE, encoding = "UTF-8")
  
  # If invalid bytes show up as replacement char, try latin1 and convert
  if (any(grepl("\uFFFD", txt))) {
    txt <- readLines(path, warn = FALSE, encoding = "latin1")
    txt <- iconv(txt, from = "latin1", to = "UTF-8", sub = "")
  }
  
  # Remove trailing semicolons from each line to avoid empty columns
  txt <- gsub(";+$", "", txt)
  
  # Semicolon format: read.csv2 uses ';' by default
  ref <- read.csv2(
    text = txt,
    header = TRUE,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    quote = "\"",
    comment.char = "",
    na.strings = c("", "NA"),
    fill = TRUE,
    blank.lines.skip = TRUE
  )
  
  # Drop unnamed / empty columns created by extra separators
  bad_names <- which(is.na(names(ref)) | names(ref) == "" | grepl("^\\.+$", names(ref)))
  if (length(bad_names) > 0) ref <- ref[, -bad_names, drop = FALSE]
  
  ref
}

numify <- function(x) suppressWarnings(as.numeric(gsub(",", ".", as.character(x))))

# Parse semicolon-separated values
parse_semicolon <- function(x) {
  if (is.na(x) || !nzchar(trimws(x))) return(character(0))
  trimws(unlist(strsplit(as.character(x), ";")))
}

# Parse date with year-only conversion
parse_date_with_year <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA")] <- NA_character_
  
  out <- rep(as.Date(NA), length(x))
  
  is_year <- !is.na(x) & grepl("^\\d{4}$", x)
  is_ymd  <- !is.na(x) & grepl("^\\d{4}-\\d{2}-\\d{2}$", x)
  
  out[is_year] <- as.Date(paste0(x[is_year], "-01-01"), format = "%Y-%m-%d")
  out[is_ymd]  <- as.Date(x[is_ymd], format = "%Y-%m-%d")
  
  out
}

# Validate profile_code format (profile_N where N is a number)
validate_profile_code_format <- function(x) {
  grepl("\\d+$", x, ignore.case = TRUE)
}

# Validate plot_code format (plot_N where N is a number)
validate_plot_code_format <- function(x) {
  grepl("\\d+$", x, ignore.case = TRUE)
}

# Get common units for a property
get_common_units_for_property <- function(soil_prop, ref_data) {
  # First try to match by soil_property column
  ref_subset <- ref_data[ref_data$soil_property == soil_prop, ]
  
  if (nrow(ref_subset) == 0) {
    # Fallback: try matching property_phys_chem_id
    ref_subset <- ref_data[ref_data$property_phys_chem_id == soil_prop, ]
  }
  
  if (nrow(ref_subset) == 0) return(NULL)
  
  # Check if common_input_units column exists
  if (!"common_input_units" %in% names(ref_subset)) {
    # Fallback to reference unit if column doesn't exist
    return(ref_subset$unit_of_measure_id[1])
  }
  
  # Parse semicolon-separated common units
  common_units <- parse_semicolon(ref_subset$common_input_units[1])
  
  if (length(common_units) == 0) {
    # Fallback to reference unit if no common units defined
    return(ref_subset$unit_of_measure_id[1])
  }
  
  return(common_units)
}

# Get reference unit for a property/procedure combination
get_reference_unit <- function(prop_id, proc_id, ref_data) {
  ref_subset <- ref_data[
    ref_data$property_phys_chem_id == prop_id & 
      ref_data$procedure_phys_chem_id == proc_id, 
  ]
  
  if (nrow(ref_subset) > 0) {
    return(ref_subset$unit_of_measure_id[1])
  }
  return(NA_character_)
}

# Get conversion factor from input unit to reference unit
get_conversion_factor <- function(prop_id, proc_id, input_unit, ref_data) {
  ref_subset <- ref_data[
    ref_data$property_phys_chem_id == prop_id & 
      ref_data$procedure_phys_chem_id == proc_id, 
  ]
  
  if (nrow(ref_subset) == 0) return(1)
  
  reference_unit <- ref_subset$unit_of_measure_id[1]
  
  # If input unit matches reference, no conversion needed
  if (input_unit == reference_unit) return(1)
  
  # Check if conversion columns exist
  if (!"common_input_units" %in% names(ref_subset) || 
      !"conversion_factors" %in% names(ref_subset)) {
    return(1)  # No conversion data available
  }
  
  # Parse common units and conversion factors
  common_units <- parse_semicolon(ref_subset$common_input_units[1])
  factors_str <- parse_semicolon(ref_subset$conversion_factors[1])
  
  if (length(common_units) == 0 || length(factors_str) == 0) return(1)
  
  factors <- suppressWarnings(as.numeric(factors_str))
  
  # Find the index of the input unit
  idx <- which(common_units == input_unit)
  
  if (length(idx) > 0 && !is.na(factors[idx])) {
    return(factors[idx])
  }
  
  # Default: no conversion
  return(1)
}

# Apply conversion to numeric vector
convert_values <- function(values, conversion_factor) {
  numeric_vals <- numify(values)
  converted <- numeric_vals * conversion_factor
  return(converted)
}

# ---------- UI ----------
ui <- dashboardPage(
  skin = "red",
  
  dashboardHeader(
    title = "GloSIS Harmonization",
    tags$li(
      class = "dropdown",
      tags$img(
        src = "fao_logo1.png",
        height = "40px",
        style = "position: absolute; right: 20px; top: 5px;"
      )
    ),
    titleWidth = 460
  ),
  
  dashboardSidebar(
    tags$head(
      tags$style(HTML("
        .main-sidebar, .left-side { width: 300px !important; background-color: #FFC527 !important; }
        .content-wrapper, .right-side, .main-footer { 
           margin-left: 300px !important;
           background-color: #F7F7F7 !important;
        }
        .main-header .logo {
           width: 300px !important;
        }
        .main-header .navbar {
           margin-left: 300px !important;
        }
        .sidebar-menu > li > a { font-weight: 700; }
        .box { border-top: 3px solid #b30000 !important; }
        
        .soil-card{
          padding:10px;
          background:#fff;
          border:1px solid #ddd;
          border-radius:10px;
          margin-bottom:12px;
          box-shadow: 0 1px 2px rgba(0,0,0,0.06);
          border-left: 6px solid #3477c9;
        }
        .soil-card-title{
          font-weight:800;
          margin-bottom:8px;
          display:flex;
          align-items:center;
          gap:8px;
        }
        .soil-chip{
          display:inline-block;
          font-weight:700;
          padding:2px 8px;
          border-radius:999px;
          background:#f2f2f2;
          border:1px solid #ddd;
        }
        .soil-method-note{
          margin-top:8px;
          padding:8px 10px;
          background:#f7f7f7;
          border-left:4px solid #b30000;
          border-radius:6px;
        }
        .soil-method-note-label{
          font-weight:800;
          margin-right:6px;
        }
        .soil-method-note-text{
          opacity:.9;
        }
        .conversion-notice {
          margin-top: 8px;
          padding: 8px 10px;
          background: #fff3cd;
          border-left: 4px solid #ffc107;
          border-radius: 6px;
          font-weight: 600;
          font-size: 12px;
          color: #856404;
        }
        .soil-reference-unit {
          margin-top: 8px;
          padding: 6px 10px;
          background: #e3f2fd;
          border-left: 4px solid #1976d2;
          border-radius: 4px;
          font-size: 13px;
        }
      "))
    ),
    
    sidebarMenu(
      menuItem("Converter", tabName = "converter", icon = icon("file-arrow-up"))
    ),
    
    tags$hr(style = "border-top: 1px solid rgba(0,0,0,0.15);"),
    
    div(
      style = "padding: 0 12px;",
      
      # SECTION 1: Data Upload ----
      tags$strong(style = "color:#4f4f4f;","1) Upload your data (CSV or XLSX)"),
      fileInput("data_file", NULL, accept = c(".csv", ".xlsx", ".xls")),
      uiOutput("xlsx_sheet_ui"),
      
      # SECTION 2: Glosis template ----
      # tags$strong(style = "color:#4f4f4f;","2) Select Glosis Template (optional)"),
      # uiOutput("template_label"),
      # tags$br(),
      
      # SECTION 3: Project Settings ----
      #tags$strong(style = "color:#4f4f4f;","3) Configure Project Settings"),
      tags$strong(style = "color:#4f4f4f;","2) Configure Project Settings"),
      tags$br(),
      tags$br(),
      
      # Project name group
      tags$div(
        style = "background: rgba(255,255,255,.3); padding: 8px; border-radius: 6px; margin-bottom: 12px;",
        tags$div(
          style = "font-weight: 700; margin-bottom: 4px; font-size: 13px; color: #ffffff;",
          "Project name"
        ),
        tags$div(
          style = "margin-bottom: 0px;",
          checkboxInput("use_project_col", "Use 'Project name' from data", value = FALSE)
        ),
        tags$small(
          "Check this box if your 'Project name' is in your input data.",
          style = "display:block; opacity:.8; margin-top: -10px; margin-bottom: 8px;"
        ),
        conditionalPanel(
          condition = "input.use_project_col == false",
          textInput("project_name", NULL, value = "MyProject", placeholder = "Enter project name")
        ),
        conditionalPanel(
          condition = "input.use_project_col == true",
          uiOutput("project_col_selector")
        )
      ),
      
      # Site code group
      tags$div(
        style = "background: rgba(255,255,255,.3); padding: 8px; border-radius: 6px; margin-bottom: 12px;",
        tags$div(
          style = "font-weight: 700; margin-bottom: 4px; font-size: 13px; color: #ffffff;",
          "Site code"
        ),
        tags$div(
          style = "margin-bottom: 6px;",
          checkboxInput("use_site_col", "Use 'Site code' from data", value = FALSE),
          tags$small("Check this box if your 'Site code' is in your input data.",
                     style = "display:block; opacity:.8;"),
        ),
        conditionalPanel(
          condition = "input.use_site_col == false",
          textInput("site_code", NULL, value = "MySite", placeholder = "Enter site code")
        ),
        conditionalPanel(
          condition = "input.use_site_col == true",
          uiOutput("site_col_selector")
        )
      ),
      
      # Date group
      tags$div(
        style = "background: rgba(255,255,255,.3); padding: 8px; border-radius: 6px; margin-bottom: 12px;",
        tags$div(
          style = "font-weight: 700; margin-bottom: 4px; font-size: 13px; color: #ffffff;",
          "Date"
        ),
        tags$div(
          style = "margin-bottom: 6px;",
          checkboxInput("use_date_col", "Use 'Date' from data", value = FALSE),
          tags$small("Check this box if your 'Date' is in your input data.",
                     style = "display:block; opacity:.8;"),
        ),
        conditionalPanel(
          condition = "input.use_date_col == false",
          textInput("date_manual", NULL, value = as.character(Sys.Date()), placeholder = "YYYY-MM-DD"),
          tags$small("Format: YYYY-MM-DD (e.g., 2025-12-21). Year-only (e.g., 2025) will be converted to 2025-01-01.",
                     style = "display:block; opacity:.8; margin-top: -10px;")
        ),
        conditionalPanel(
          condition = "input.use_date_col == true",
          uiOutput("date_col_selector")
        )
      ),
      
      # Profile code group
      tags$div(
        style = "background: rgba(255,255,255,.3); padding: 8px; border-radius: 6px; margin-bottom: 12px;",
        tags$div(
          style = "font-weight: 700; margin-bottom: 4px; font-size: 13px; color: #ffffff;",
          "Profile code"
        ),
        tags$div(
          style = "margin-bottom: 6px;",
          checkboxInput("use_profile_col", "Use 'Profile code' from data", value = FALSE),
          tags$small("Check this box if your 'Profile code' is in your input data.",
                     style = "display:block; opacity:.8;"),
        ),
        conditionalPanel(
          condition = "input.use_profile_col == false",
          tags$small("Profile codes will be auto-generated as 'profile_1', 'profile_2', etc.",
                     style = "display:block; opacity:.8; font-style: italic;")
        ),
        conditionalPanel(
          condition = "input.use_profile_col == true",
          uiOutput("profile_col_selector")
        )
      ),
      
      # Plot code + Plot type
      tags$div(
        style = "background: rgba(255,255,255,.3); padding: 8px; border-radius: 6px; margin-bottom: 12px;",
        
        # --- Plot code section ---
        tags$div(
          style = "font-weight: 700; margin-bottom: 4px; font-size: 13px; color: #ffffff;",
          "Plot code"
        ),
        tags$div(
          style = "margin-bottom: 6px;",
          checkboxInput("use_plot_col", "Use 'Plot code' from data", value = FALSE),
          tags$small(
            "Check this box if your 'Plot code' is in your input data.",
            style = "display:block; opacity:.8;"
          )
        ),
        conditionalPanel(
          condition = "input.use_plot_col == false",
          tags$small(
            "Plot codes will be auto-generated as 'plot_1', 'plot_2', etc.",
            style = "display:block; opacity:.8; font-style: italic;"
          )
        ),
        conditionalPanel(
          condition = "input.use_plot_col == true",
          uiOutput("plot_col_selector")
        ),
        
        # divider inside the same group
        tags$hr(style = "margin: 10px 0; border-top: 1px solid rgba(255,255,255,0.25);"),
        
        # --- Plot type section ---
        tags$div(
          style = "font-weight: 700; margin-bottom: 4px; font-size: 13px; color: #ffffff;",
          "Plot type"
        ),
        tags$div(
          style = "margin-bottom: 6px;",
          selectInput(
            "plot_type",
            NULL,
            choices = c("Borehole", "TrialPit", "Surface"),
            selected = "TrialPit"
          )
        ),
        tags$small(
          "Choose the plot type used in the template.",
          style = "display:block; opacity:.8;"
        )
      ),
      
      
      # Horizon group: Horizon ID column (optional) + Horizon type
      tags$div(
        style = "background: rgba(255,255,255,.3); padding: 8px; border-radius: 6px; margin-bottom: 12px;",
        
        tags$div(
          style = "font-weight: 700; margin-bottom: 4px; font-size: 13px; color: #ffffff;",
          "Horizon ID"
        ),
        
        # Ask if Horizon ID is in the data
        tags$div(
          style = "margin-bottom: 6px;",
          checkboxInput("use_hor_col", "Use 'Horizon ID' from data", value = FALSE),
          tags$small(
            "Check this box if your Horizon ID/code is stored in a column in your input data.",
            style = "display:block; opacity:.8;"
          )
        ),
        
        # If not using a column, explain behavior (you can adjust text)
        conditionalPanel(
          condition = "input.use_hor_col == false",
          tags$small(
            "Horizon codes will be taken from the selected Horizon type logic or left empty (depending on your export rules).",
            style = "display:block; opacity:.8; font-style: italic;"
          )
        ),
        
        # If using a column, show selector
        conditionalPanel(
          condition = "input.use_hor_col == true",
          tags$div(
            style = "font-weight: 700; margin: 10px 0 4px 0; font-size: 13px; color: #ffffff;",
            "Horizon ID column"
          ),
          tags$div(
            style = "margin-bottom: 6px;",
            uiOutput("hor_col_selector")
          )
        ),
        
        tags$hr(style = "margin: 10px 0; border-top: 1px solid rgba(255,255,255,0.25);"),
        
        # Horizon type (always asked, in same group)
        tags$div(
          style = "font-weight: 700; margin-bottom: 4px; font-size: 13px; color: #ffffff;",
          "Horizon type"
        ),
        tags$div(
          style = "margin-bottom: 6px;",
          selectInput(
            "horizon_type",
            NULL,
            choices = c("Horizon", "Layer"),
            selected = "Horizon"
          )
        ),
        tags$small(
          "Choose whether your depth intervals represent Horizons or Layers.",
          style = "display:block; opacity:.8;"
        )
      ),
      
      
      tags$hr(),
      
      # SECTION 4: Map Properties ----
      #tags$strong(style = "color:#4f4f4f;", "4) Map fields"),
      tags$strong(style = "color:#4f4f4f;", "3) Map Plot Parameters"),
      tags$br(),
      tags$br(),
      
      tags$div(
        style = "background: rgba(255,255,255,.3); padding: 8px; border-radius: 6px; margin-bottom: 12px;",
        uiOutput("col_selectors"),
      ),
      
      tags$hr(),
      
      # SECTION 5: Export as filled template -----
      #tags$strong(style = "color:#4f4f4f;","5) Export Harmonized Data"),
      tags$strong(style = "color:#4f4f4f;","4) Download Harmonized Data"),
      tags$br(),
      tags$br(),
      
      tags$div(
        style = "background: rgba(255,255,255,.3); padding: 8px; border-radius: 6px; margin-bottom: 12px;",
        downloadButton("download_xlsx", "Download XLSX",
                       class = "btn btn-primary",
                       style = "width: 100%; font-weight: 800;"),
      ),
      tags$br(), tags$br(),
      uiOutput("download_hint")
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Force DT scroll header alignment */
        .dataTables_scrollHeadInner { width: 100% !important; }
        .dataTables_scrollHeadInner table { width: 100% !important; margin: 0 !important; }
        .dataTables_scrollBody table { width: 100% !important; margin: 0 !important; }
        div.dataTables_scrollHead table.dataTable { margin-bottom: 0 !important; }
        div.dataTables_scrollBody table.dataTable { margin-top: 0 !important; }
      ")),
      tags$script(HTML("
        // Adjust all visible DT tables when a bootstrap tab becomes visible
        $(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"]', function () {
          setTimeout(function() {
            $($.fn.dataTable.tables({ visible: true, api: true }))
              .columns.adjust()
              .responsive.recalc();
          }, 120);
        });

        // Allow server to request an adjust for a specific table
        Shiny.addCustomMessageHandler('dt_adjust_one', function(msg){
          var id = msg.id;
          setTimeout(function() {
            if ($.fn.dataTable.isDataTable('#' + id)) {
              $('#' + id).DataTable()
                .columns.adjust()
                .responsive.recalc();
            }
          }, 150);
        });

        // Adjust on window resize
        $(window).on('resize', function() {
          setTimeout(function() {
            $($.fn.dataTable.tables({ visible: true, api: true }))
              .columns.adjust();
          }, 150);
        });
      "))
    ),
    
    tabItems(
      tabItem(
        tabName = "converter",
        
        tabBox(
          width = 12,
          id = "mainTabs",
          
          tabPanel(
            title = tagList(icon("file-arrow-up"), "Converter"),
            value = "converter_tab",
            
            fluidRow(
              box(
                width = 12, status = "danger", solidHeader = TRUE,
                title = tagList(icon("list-check"), "What this app does:"),
                tagList("This app helps organize soil data for harmonization in the GloSIS database. Users upload a soil dataset and download a harmonized .xlsx file ready to be ingested into the GloSIS database. The app also identifies potential errors in the dataset."),
                tags$hr(style = "border-top: 1px solid rgba(0,0,0,0.15);"),
                tagList(tags$strong( icon("list-check"), "How to use:")),
                tags$ol(
                  tags$li("Upload your soil data as .CSV or .XLSX (choose sheet if XLSX)."),
                  #tags$li("Select the template (optional – otherwise default template is used)."),
                  tags$li("Configure project settings (project name, site code, date, profile/plot codes)."),
                  tags$li("Map lon/lat + required columns (sample ID / top-bottom depth)."),
                  tags$li("Select soil properties columns."),
                  tags$li("Map each soil property to property/method/unit using GloSIS standards."),
                  tags$li("Add metadata if needed (Metadata tab)."),
                  tags$li("Download the populated GloSIS template XLSX.")
                )
              )
            ),
            
            fluidRow(
              box(
                width = 7, status = "primary", solidHeader = TRUE,
                title = tagList(icon("table"), "Data Preview"),
                DTOutput("preview_dt")
              ),
              box(
                width = 5, status = "warning", solidHeader = TRUE,
                title = tagList(icon("circle-info"), "Status & Checks"),
                verbatimTextOutput("status"),
                tags$hr(),
                uiOutput("validation_panel"),
                tags$hr(),
                uiOutput("qc_panel")
              )
            ),
            
            fluidRow(
              box(
                width = 12, status = "info", solidHeader = TRUE,
                title = tagList(icon("flask"), "Identify the GLOSIS reference property name/analytical method/unit for each soil property in your input data"),
                uiOutput("procedures_mapping_ui")
              )
            )
          ),
          
          tabPanel(
            title = tagList(icon("shield-halved"), "Quality checks"),
            value = "qc_tab",
            
            fluidRow(
              box(
                width = 12, status = "danger", solidHeader = TRUE,
                title = tagList(icon("exclamation-triangle"), "Non-numeric coordinates (lon/lat)"),
                DTOutput("qc_non_numeric_coords_dt")
              )
            ),
            
            fluidRow(
              box(
                width = 12, status = "danger", solidHeader = TRUE,
                title = tagList(icon("exclamation-triangle"), "Non-numeric depth (top/bottom)"),
                DTOutput("qc_non_numeric_depth_dt")
              )
            ),
            
            fluidRow(
              box(
                width = 12, status = "danger", solidHeader = TRUE,
                title = tagList(icon("exclamation-triangle"), "Non-numeric soil property values"),
                DTOutput("qc_non_numeric_props_dt")
              )
            ),
            
            fluidRow(
              box(
                width = 12, status = "danger", solidHeader = TRUE,
                title = tagList(icon("copy"), "Duplicates (by Sample ID)"),
                DTOutput("qc_duplicates_dt")
              )
            ),
            fluidRow(
              box(
                width = 12, status = "warning", solidHeader = TRUE,
                title = tagList(icon("location-dot"), "Missing/Invalid coordinates (lon/lat)"),
                DTOutput("qc_missing_coords_dt")
              )
            ),
            fluidRow(
              box(
                width = 12, status = "warning", solidHeader = TRUE,
                title = tagList(icon("ruler-vertical"), "Missing depth (top/bottom)"),
                DTOutput("qc_missing_depth_dt")
              )
            ),
            fluidRow(
              box(
                width = 12, status = "danger", solidHeader = TRUE,
                title = tagList(icon("triangle-exclamation"), "Invalid depth (bottom ≤ top)"),
                DTOutput("qc_bad_depth_dt")
              )
            ),
            fluidRow(
              box(
                width = 12, status = "danger", solidHeader = TRUE,
                title = tagList(icon("calendar"), "Invalid dates"),
                DTOutput("qc_invalid_dates_dt")
              )
            ),
            fluidRow(
              box(
                width = 12, status = "danger", solidHeader = TRUE,
                title = tagList(icon("code-branch"), "Invalid profile_code format"),
                DTOutput("qc_invalid_profile_code_dt")
              )
            ),
            fluidRow(
              box(
                width = 12, status = "danger", solidHeader = TRUE,
                title = tagList(icon("map-pin"), "Inconsistent profile_code per coordinate group"),
                DTOutput("qc_profile_inconsistent_dt")
              )
            ),
            fluidRow(
              box(
                width = 12, status = "danger", solidHeader = TRUE,
                title = tagList(icon("code-branch"), "Invalid plot_code format"),
                DTOutput("qc_invalid_plot_code_dt")
              )
            ),
            fluidRow(
              box(
                width = 12, status = "danger", solidHeader = TRUE,
                title = tagList(icon("map-pin"), "Inconsistent plot_code per coordinate group"),
                DTOutput("qc_plot_inconsistent_dt")
              )
            ),
            fluidRow(
              box(
                width = 12, status = "danger", solidHeader = TRUE,
                title = tagList(icon("flask"), "Out-of-range soil properties (min/max from procedures)"),
                DTOutput("qc_out_of_range_dt")
              )
            )
          ),
          
          tabPanel(
            title = tagList(icon("address-card"), "Metadata"),
            value = "metadata_tab",
            fluidRow(
              box(
                width = 12, status = "info", solidHeader = TRUE,
                title = tagList(icon("pen-to-square"), "Edit Metadata (cell editing enabled)"),
                tags$p("Edit the metadata table below. Empty fields will be filled with 'unknown' in the export."),
                DTOutput("metadata_edit_dt")
              )
            )
          ),
          tabPanel(
            title = tagList(icon("magnifying-glass"), "Procedures reference"),
            value = "procedures_ref_tab",
            fluidRow(
              box(
                width = 12, status = "primary", solidHeader = TRUE,
                title = tagList(icon("table"), "glosis_procedures.csv (inspect)"),
                DTOutput("procedures_dt")
              )
            )
          )
        )
      )
    )
  )
)

# ---------- Server ----------
server <- function(input, output, session) {
  
  # ---- procedures lookup ----
  procedures_lookup_raw <- reactive({
    read_procedures_csv("templates/glosis_procedures_v2.csv")
  })
  
  procedures_lookup <- reactive({
    ref <- procedures_lookup_raw()
    
    needed_cols <- c("property_phys_chem_id", "procedure_phys_chem_id", "unit_of_measure_id", "definition")
    missing <- setdiff(needed_cols, names(ref))
    validate(need(length(missing) == 0,
                  paste("glosis_procedures.csv missing columns:", paste(missing, collapse = ", "))))
    
    if (!"soil_property" %in% names(ref)) {
      ref$soil_property <- ref$property_phys_chem_id
    }
    
    # Ensure new columns exist (for backwards compatibility)
    if (!"common_input_units" %in% names(ref)) {
      ref$common_input_units <- ref$unit_of_measure_id
    }
    if (!"conversion_factors" %in% names(ref)) {
      ref$conversion_factors <- "1"
    }
    
    ref %>%
      mutate(
        soil_property = trimws(as.character(soil_property)),
        property_phys_chem_id = trimws(as.character(property_phys_chem_id)),
        procedure_phys_chem_id = trimws(as.character(procedure_phys_chem_id)),
        unit_of_measure_id = trimws(as.character(unit_of_measure_id)),
        definition = as.character(definition),
        common_input_units = as.character(common_input_units),
        conversion_factors = as.character(conversion_factors)
      )
  })
  
  # ---- procedures reference DT (server=TRUE + auto adjust) ----
  output$procedures_dt <- renderDT({
    ref <- procedures_lookup_raw()
    
    datatable(
      ref,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        scrollCollapse = TRUE,
        autoWidth = TRUE,
        deferRender = TRUE,
        searchDelay = 250
      ),
      callback = JS(
        "table.on('draw.dt', function(){ table.columns.adjust(); });",
        "table.on('init.dt', function(){ table.columns.adjust(); });",
        "table.on('xhr.dt', function(){ table.columns.adjust(); });"
      )
    )
  }, server = TRUE)
  
  observeEvent(input$mainTabs, {
    if (identical(input$mainTabs, "procedures_ref_tab")) {
      session$sendCustomMessage("dt_adjust_one", list(id = "procedures_dt"))
    }
  }, ignoreInit = TRUE)
  
  # ---- template path ----
  template_path <- reactive({
    default <- "templates/glosis_template_v6.xlsx"
    if (!is.null(input$template_file) && nzchar(input$template_file$datapath)) {
      return(input$template_file$datapath)
    }
    default
  })
  
  output$template_label <- renderUI({
    tp <- template_path()
    using_uploaded <- !is.null(input$template_file) && nzchar(input$template_file$datapath)
    
    tags$div(
      style = "background: rgba(255,255,255,.3); padding: 8px; border-radius: 6px; margin-top:12px;",
      icon("file-excel"),
      if (using_uploaded) tags$b("Using uploaded template") else tags$b("Using default template (Recommended)"),
      tags$br(),
      fileInput("template_file", "Or use another template (XLSX)", accept = c(".xlsx")),
    )
  })
  
  # ---- XLSX sheet picker ----
  xlsx_sheets <- reactive({
    req(input$data_file)
    ext <- tolower(tools::file_ext(input$data_file$name))
    if (ext %in% c("xlsx", "xls")) readxl::excel_sheets(input$data_file$datapath) else NULL
  })
  
  output$xlsx_sheet_ui <- renderUI({
    req(input$data_file)
    sh <- xlsx_sheets()
    if (is.null(sh)) return(NULL)
    selectInput("xlsx_sheet", "XLSX sheet to import", choices = sh, selected = sh[1])
  })
  
  # ---- read data ----
  k_data <- reactive({
    req(input$data_file)
    ext <- tolower(tools::file_ext(input$data_file$name))
    
    if (ext == "csv") {
      safe_read_csv(input$data_file$datapath)
    } else if (ext %in% c("xlsx", "xls")) {
      req(input$xlsx_sheet)
      as.data.frame(readxl::read_excel(input$data_file$datapath, sheet = input$xlsx_sheet))
    } else {
      validate(need(FALSE, "Unsupported file type. Upload a .csv, .xlsx, or .xls file."))
    }
  })
  
  
  # Helper to avoid error message if data is not mapped 
  mapping_ready <- reactive({
    req(input$data_file)
    df <- k_data()
    cols <- names(df)
    
    needed <- c("col_lon","col_lat","col_top","col_bottom","col_texid")
    
    # Check that all columns are selected and valid
    all_selected <- all(vapply(needed, function(x){
      !is.null(input[[x]]) && nzchar(input[[x]]) && input[[x]] %in% cols
    }, logical(1)))
    
    if (!all_selected) return(FALSE)
    
    # Check that all selected columns are DIFFERENT (no duplicates)
    selected_cols <- vapply(needed, function(x) input[[x]], character(1))
    all_unique <- length(unique(selected_cols)) == length(selected_cols)
    
    return(all_unique)
  })
  
  
  
  # ---- PROJECT NAME, SITE CODE, HORIZON CODE, DATE, PROFILE CODE, PLOT CODE COLUMN SELECTORS ----
  output$project_col_selector <- renderUI({
    req(k_data())
    cols <- names(k_data())
    
    selectInput(
      "project_name_col", 
      NULL,
      choices = cols,
      selected = if ("project_name" %in% cols) "project_name" else 
        if ("project" %in% cols) "project" else cols[1]
    )
  })
  
  output$site_col_selector <- renderUI({
    req(k_data())
    cols <- names(k_data())
    
    selectInput(
      "site_code_col", 
      NULL,
      choices = cols,
      selected = if ("site_code" %in% cols) "site_code" else 
        if ("site" %in% cols) "site" else cols[1]
    )
  })
  
  output$hor_col_selector <- renderUI({
    req(k_data())
    cols <- names(k_data())
    
    selectInput(
      "col_hor",
      NULL,
      choices = cols,
      selected = if ("hor" %in% cols) "hor" else cols[1]
    )
  })
  
  output$date_col_selector <- renderUI({
    req(k_data())
    cols <- names(k_data())
    
    selectInput(
      "date_col", 
      NULL,
      choices = cols,
      selected = if ("date" %in% cols) "date" else 
        if ("Date" %in% cols) "Date" else cols[1]
    )
  })
  
  output$profile_col_selector <- renderUI({
    req(k_data())
    cols <- names(k_data())
    
    selectInput(
      "profile_code_col", 
      NULL,
      choices = cols,
      selected = if ("profile_code" %in% cols) "profile_code" else 
        if ("profile" %in% cols) "profile" else cols[1]
    )
  })
  
  output$plot_col_selector <- renderUI({
    req(k_data())
    cols <- names(k_data())
    
    selectInput(
      "plot_code_col", 
      NULL,
      choices = cols,
      selected = if ("plot_code" %in% cols) "plot_code" else 
        if ("plot" %in% cols) "plot" else cols[1]
    )
  })
  
  # ---- mapping UI ----
  output$col_selectors <- renderUI({
    req(k_data())
    cols <- names(k_data())
    # Find matching property columns (case-insensitive)
    auto_selected <- cols[grepl(pattern, cols, ignore.case = TRUE)]
    
    tagList(
      selectInput("col_texid", "Sample ID", choices = cols,
                  selected = if ("texid" %in% cols) "texid" else cols[1]),
      
      selectInput("col_lon", "Longitude (X) column",
        choices  = cols,
        selected = pick_col(cols, c("X","x","lon","Lon","LON","long","Long","LONG","longitude","Longitude","LONGITUDE","xcoord","Xcoord","easting","Easting"))),
      
      selectInput("col_lat", "Latitude (Y) column",
        choices  = cols,
        selected = pick_col(cols, c("Y","y","lat","Lat","LAT","latitude","Latitude","LATITUDE","ycoord","Ycoord","northing","Northing"))),
      
      selectInput(
        "col_top", "Upper depth (top) column",
        choices  = cols,
        selected = pick_col(cols, c(
          "top","Top","TOP","top_depth","Top_depth","TOP_depth",
          "upper_depth","Upper_depth","Upper_Depth","UPPER_DEPTH",
          "upperDepth","UpperDepth",
          "depth_top","Depth_top","DEPTH_TOP","depth_u",
          "start_depth","Start_depth","START_DEPTH",
          "from","From","FROM",
          "depth_from","Depth_from","DEPTH_FROM",
          "upper","Upper","UPPER",
          "z_top","Z_top","Z_TOP"
        ))
      ),
      
      selectInput(
        "col_bottom", "Lower depth (bottom) column",
        choices  = cols,
        selected = pick_col(cols, c(
          "bottom","Bottom","BOTTOM","bottom_depth","Bottom_depth","BOTTOM_depth",
          "lower_depth","Lower_depth","Lower_Depth","LOWER_DEPTH",
          "lowerDepth","LowerDepth",
          "depth_bottom","Depth_bottom","DEPTH_BOTTOM","depth_l",
          "end_depth","End_depth","END_DEPTH",
          "to","To","TO",
          "depth_to","Depth_to","DEPTH_TO",
          "lower","Lower","LOWER",
          "z_bottom","Z_bottom","Z_BOTTOM"
        ))
      ),
      
      selectizeInput(
        "soil_props",
        "Soil properties (multi-select)",
        choices = cols,
        selected = auto_selected,  # ← Use the fuzzy-matched columns
        multiple = TRUE,
        options = list(placeholder = "Select one or more columns...")
      )
    )
  })
  
  # ---- preview (whole dataset, 5 rows per page) ----
  output$preview_dt <- renderDT({
    req(k_data())
    datatable(
      k_data(),
      rownames = FALSE,
      options = list(
        scrollX = TRUE,
        pageLength = 5,
        lengthMenu = c(5, 10, 25, 50, 100),
        deferRender = TRUE
      )
    )
  })
  
  output$download_hint <- renderUI({
    if (is.null(input$data_file)) return(NULL)
    tags$small("If download fails, an error message will appear (check console too).",
               style = "opacity:.8;")
  })
  
  # ---- Procedures mapping UI ----
  output$procedures_mapping_ui <- renderUI({
    req(input$soil_props)
    ref <- procedures_lookup()
    
    props <- input$soil_props
    if (length(props) == 0) {
      return(tags$div("Select soil properties in the sidebar to configure procedures mapping."))
    }
    
    ui_cards <- lapply(props, function(p) {
      pid <- safe_id(p)
      
      # Get property choices
      ref_p <- ref[ref$soil_property == p, , drop = FALSE]
      prop_choices <- sort(unique(ref_p$property_phys_chem_id))
      if (length(prop_choices) == 0) prop_choices <- sort(unique(ref$property_phys_chem_id))
      def_prop <- prop_choices[1]
      
      # Get procedure choices for default property
      ref_pp <- ref_p[ref_p$property_phys_chem_id == def_prop, , drop = FALSE]
      if (nrow(ref_pp) == 0) ref_pp <- ref[ref$property_phys_chem_id == def_prop, , drop = FALSE]
      
      proc_choices <- sort(unique(ref_pp$procedure_phys_chem_id))
      def_proc <- proc_choices[1]
      
      # Get common input units for this property
      common_units <- get_common_units_for_property(p, ref)
      if (is.null(common_units) || length(common_units) == 0) {
        # Fallback to reference unit if no common units defined
        ref_unit <- if (nrow(ref_pp) > 0) ref_pp$unit_of_measure_id[1] else "g/kg"
        common_units <- ref_unit
      }
      def_input_unit <- common_units[1]
      
      column(
        width = 3,
        tags$div(
          class = "soil-card",
          tags$div(
            class = "soil-card-title",
            tags$span(paste("Soil property:")),
            tags$span(class = "soil-chip", p)
          ),
          
          selectInput(
            paste0("map_property__", pid),
            "GloSIS reference name",
            choices = prop_choices,
            selected = def_prop
          ),
          selectInput(
            paste0("map_procedure__", pid),
            "Analytical procedure",
            choices = proc_choices,
            selected = def_proc
          ),
          selectInput(
            paste0("map_input_unit__", pid),
            HTML('Units of measurement in <span style="color:red;">YOUR</span> data'),
            choices = common_units,
            selected = def_input_unit
          ),
          
          # Display the reference unit (read-only) - use uiOutput for reactivity
          tags$div(
            class = "soil-reference-unit",
            tags$strong("GloSIS reference unit: "),
            uiOutput(paste0("ref_unit__", pid), inline = TRUE)
          ),
          
          # Show conversion notice - use uiOutput for reactivity
          uiOutput(paste0("conversion_notice__", pid)),
          
          # Method description
          uiOutput(paste0("desc__", pid))
        )
      )
    })
    
    # pack into rows of 4
    n <- length(ui_cards)
    rows <- split(ui_cards, ceiling(seq_len(n) / 4))
    do.call(tagList, lapply(rows, function(r) fluidRow(r)))
  })
  
  # ---- Add reactive outputs for reference units and conversion notices ----
  observeEvent(input$soil_props, {
    req(input$soil_props)
    ref <- procedures_lookup()
    
    lapply(input$soil_props, function(p) {
      local({
        p_local <- p
        pid <- safe_id(p_local)
        
        prop_id <- paste0("map_property__", pid)
        proc_id <- paste0("map_procedure__", pid)
        input_unit_id <- paste0("map_input_unit__", pid)
        ref_unit_id <- paste0("ref_unit__", pid)
        notice_id <- paste0("conversion_notice__", pid)
        
        # Display reference unit
        output[[ref_unit_id]] <- renderText({
          req(input[[prop_id]], input[[proc_id]])
          ref_unit <- get_reference_unit(input[[prop_id]], input[[proc_id]], ref)
          if (is.na(ref_unit)) return("(not specified)")
          ref_unit
        })
        
        # Display conversion notice if units differ
        output[[notice_id]] <- renderUI({
          req(input[[prop_id]], input[[proc_id]], input[[input_unit_id]])
          
          ref_unit <- get_reference_unit(input[[prop_id]], input[[proc_id]], ref)
          input_unit <- input[[input_unit_id]]
          
          if (is.na(ref_unit) || input_unit == ref_unit) {
            return(NULL)
          }
          
          factor <- get_conversion_factor(
            input[[prop_id]], 
            input[[proc_id]], 
            input_unit, 
            ref
          )
          
          if (factor == 1) return(NULL)
          
          tags$div(
            class = "conversion-notice",
            icon("arrow-right"),
            sprintf(" Values will be multiplied by %.4g to convert from %s to %s", 
                    factor, input_unit, ref_unit)
          )
        })
      })
    })
  }, ignoreInit = FALSE)
  
  # ---- CORRECTED dependent dropdown updates ----
  observeEvent(input$soil_props, {
    req(input$soil_props)
    ref <- procedures_lookup()
    
    lapply(input$soil_props, function(p) {
      local({
        p_local <- p
        pid <- safe_id(p_local)
        
        prop_id <- paste0("map_property__", pid)
        proc_id <- paste0("map_procedure__", pid)
        input_unit_id <- paste0("map_input_unit__", pid)
        
        # When property changes, update procedure and input unit options
        observeEvent(input[[prop_id]], {
          req(p_local %in% input$soil_props)
          selected_prop <- input[[prop_id]]
          req(!is.null(selected_prop), nzchar(selected_prop))
          
          # Get procedures for this property
          ref_p <- ref[ref$soil_property == p_local & ref$property_phys_chem_id == selected_prop, , drop = FALSE]
          if (nrow(ref_p) == 0) ref_p <- ref[ref$property_phys_chem_id == selected_prop, , drop = FALSE]
          
          proc_choices <- sort(unique(ref_p$procedure_phys_chem_id))
          
          # Keep current procedure if it's still valid, otherwise use first
          current_proc <- input[[proc_id]]
          new_proc <- if (!is.null(current_proc) && current_proc %in% proc_choices) {
            current_proc
          } else {
            proc_choices[1]
          }
          
          updateSelectInput(session, proc_id, choices = proc_choices, selected = new_proc)
        }, ignoreInit = FALSE)
        
        # When procedure changes, update input unit options
        observeEvent(input[[proc_id]], {
          req(p_local %in% input$soil_props)
          selected_prop <- input[[prop_id]]
          selected_proc <- input[[proc_id]]
          req(!is.null(selected_prop), nzchar(selected_prop))
          req(!is.null(selected_proc), nzchar(selected_proc))
          
          # Get common units for this property/procedure combination
          ref_pp <- ref[
            ref$soil_property == p_local & 
              ref$property_phys_chem_id == selected_prop & 
              ref$procedure_phys_chem_id == selected_proc, 
            , drop = FALSE
          ]
          
          if (nrow(ref_pp) == 0) {
            ref_pp <- ref[
              ref$property_phys_chem_id == selected_prop & 
                ref$procedure_phys_chem_id == selected_proc, 
              , drop = FALSE
            ]
          }
          
          # Get common units
          common_units <- if (nrow(ref_pp) > 0 && "common_input_units" %in% names(ref_pp)) {
            parse_semicolon(ref_pp$common_input_units[1])
          } else {
            NULL
          }
          
          # Fallback to reference unit if no common units
          if (is.null(common_units) || length(common_units) == 0) {
            ref_unit <- if (nrow(ref_pp) > 0) ref_pp$unit_of_measure_id[1] else "g/kg"
            common_units <- ref_unit
          }
          
          # Keep current input unit if it's still valid, otherwise use first
          current_input_unit <- input[[input_unit_id]]
          new_input_unit <- if (!is.null(current_input_unit) && current_input_unit %in% common_units) {
            current_input_unit
          } else {
            common_units[1]
          }
          
          updateSelectInput(session, input_unit_id, 
                            choices = common_units, 
                            selected = new_input_unit)
        }, ignoreInit = FALSE)
      })
    })
  }, ignoreInit = FALSE)
  
  # ---- Method note rendering ----
  observeEvent(input$soil_props, {
    req(input$soil_props)
    ref <- procedures_lookup()
    
    lapply(input$soil_props, function(p) {
      local({
        p_local <- p
        pid <- safe_id(p_local)
        
        prop_id <- paste0("map_property__", pid)
        proc_id <- paste0("map_procedure__", pid)
        input_unit_id <- paste0("map_input_unit__", pid)
        out_id  <- paste0("desc__", pid)
        
        output[[out_id]] <- renderUI({
          req(input[[prop_id]])
          selected_prop <- input[[prop_id]]
          selected_proc <- input[[proc_id]] %||% ""
          selected_unit <- input[[input_unit_id]] %||% ""
          
          ref_match <- ref[
            ref$soil_property == p_local &
              ref$property_phys_chem_id == selected_prop &
              ref$procedure_phys_chem_id == selected_proc,
            , drop = FALSE
          ]
          if (nrow(ref_match) == 0) {
            ref_match <- ref[
              ref$property_phys_chem_id == selected_prop &
                ref$procedure_phys_chem_id == selected_proc,
              , drop = FALSE
            ]
          }
          if (nrow(ref_match) == 0) {
            ref_match <- ref[ref$property_phys_chem_id == selected_prop, , drop = FALSE]
          }
          
          desc <- if (nrow(ref_match) > 0) as.character(ref_match$definition[1]) else NA_character_
          if (is.na(desc)) return(NULL)
          desc <- trimws(desc)
          if (!nzchar(desc)) return(NULL)
          
          tags$div(
            class = "soil-method-note",
            tags$span(class = "soil-method-note-label", "Method note:"),
            tags$span(class = "soil-method-note-text", desc)
          )
        })
      })
    })
  }, ignoreInit = FALSE)
  
  
  # ---- Metadata editing (single-row, applied to ALL plots) ----
  # The user fills ONE row of contact/organization metadata. These values are replicated
  # for every plot_code when exporting the XLSX.
  metadata_reactive <- reactiveVal()
  
  # Initialize metadata once per uploaded dataset (single row)
  observeEvent(
    list(k_data(), input$col_lon, input$col_lat),
    {
      req(k_data())
      req(input$col_lon, input$col_lat)
      
      # one-row metadata (user fills once)
      metadata_df <- data.frame(
        name = "",
        honorific_title = "",
        role = "",
        email = "",
        telephone = "",
        url = "",
        organization = "",
        street_address = "",
        postal_code = "",
        locality = "",
        country = "",
        stringsAsFactors = FALSE
      )
      
      metadata_reactive(metadata_df)
    },
    ignoreInit = FALSE
  )
  
  output$metadata_edit_dt <- renderDT({
    req(metadata_reactive())
    
    datatable(
      metadata_reactive(),
      rownames = FALSE,
      editable = list(target = "cell"),  # all fields editable
      options = list(
        pageLength = 1,
        scrollX = TRUE,
        dom = "t"   # only the table (no search/paging needed)
      )
    )
  }, server = FALSE)
  
  # Update metadata when cells are edited
  observeEvent(input$metadata_edit_dt_cell_edit, {
    info <- input$metadata_edit_dt_cell_edit
    md <- metadata_reactive()
    
    cat("\n=== CELL EDIT EVENT ===\n")
    cat("Raw info:\n")
    print(info)
    
    # DT editable returns:
    # - row: 1-based row index (already correct for R)
    # - col: 0-based column index (need to add 1 for R)
    row <- as.integer(info$row)
    col <- as.integer(info$col) + 1
    
    cat(sprintf("Attempting to update: Row %d (DT row: %d), Col %d (DT col: %d)\n", 
                row, info$row, col, info$col))
    cat(sprintf("Data frame size: %d rows, %d cols\n", nrow(md), ncol(md)))
    cat(sprintf("Column name: %s\n", names(md)[col]))
    cat(sprintf("Value: '%s'\n", info$value))
    
    # Guard to avoid crashing the app
    if (is.na(row) || is.na(col) || row < 1 || row > nrow(md) || col < 1 || col > ncol(md)) {
      cat(sprintf("INVALID indices (row must be 1-%d, col must be 1-%d), skipping update\n", nrow(md), ncol(md)))
      cat("======================\n\n")
      return()
    }
    
    cat("Before update:\n")
    cat(sprintf("  md[%d, %d] = '%s'\n", row, col, md[row, col]))
    
    md[row, col] <- as.character(info$value)
    metadata_reactive(md)
    
    cat("After update:\n")
    cat(sprintf("  md[%d, %d] = '%s'\n", row, col, md[row, col]))
    cat("Full metadata row after update:\n")
    print(metadata_reactive()[1,])
    cat("======================\n\n")
  })
  
  # ---- QUALITY CHECKS ----
  qc_results <- reactive({
    req(k_data())
    
    # If mapping isn't ready, return empty results to avoid errors
    if (!isTRUE(mapping_ready())) {
      return(list(
        non_numeric_coords_df      = data.frame(),
        non_numeric_depth_df       = data.frame(),
        non_numeric_props_df       = data.frame(),
        duplicates_df              = data.frame(),
        missing_coords_df          = data.frame(),
        missing_depth_df           = data.frame(),
        bad_depth_df               = data.frame(),
        invalid_dates_df           = data.frame(),
        invalid_profile_code_df    = data.frame(),
        profile_inconsistent_df    = data.frame(),
        invalid_plot_code_df       = data.frame(),
        plot_inconsistent_df       = data.frame(),
        out_of_range_df            = data.frame()
      ))
    }
    
    req(input$col_lon, input$col_lat, input$col_top, input$col_bottom, input$col_texid)
    
    df <- k_data()
    
    # ---- helpers (local) ----
    pick_cols <- function(df, cols) {
      cols <- intersect(cols, names(df))
      df[, cols, drop = FALSE]
    }
    rename_rowcol <- function(df) {
      if (".row_in_input" %in% names(df)) {
        names(df)[names(df) == ".row_in_input"] <- "row_in_input"
      }
      df
    }
    is_on <- function(x) isTRUE(x)
    
    # ---- base df2 with standard names ----
    df2 <- df %>%
      mutate(.row_in_input = row_number()) %>%
      rename(
        X      = all_of(input$col_lon),
        Y      = all_of(input$col_lat),
        top    = all_of(input$col_top),
        bottom = all_of(input$col_bottom),
        texid  = all_of(input$col_texid)
      )
    
    # ---- optional columns (only if the inputs exist & are enabled) ----
    if (!is.null(input$use_date_col) && is_on(input$use_date_col) &&
        !is.null(input$date_col) && nzchar(input$date_col) &&
        input$date_col %in% names(df)) {
      df2 <- df2 %>% mutate(date_data = as.character(df[[input$date_col]]))
    }
    
    if (!is.null(input$use_profile_col) && is_on(input$use_profile_col) &&
        !is.null(input$profile_code_col) && nzchar(input$profile_code_col) &&
        input$profile_code_col %in% names(df)) {
      df2 <- df2 %>% mutate(profile_code_data = as.character(df[[input$profile_code_col]]))
    }
    
    if (!is.null(input$use_plot_col) && is_on(input$use_plot_col) &&
        !is.null(input$plot_code_col) && nzchar(input$plot_code_col) &&
        input$plot_code_col %in% names(df)) {
      df2 <- df2 %>% mutate(plot_code_data = as.character(df[[input$plot_code_col]]))
    }
    
    if (!is.null(input$use_hor_col) && is_on(input$use_hor_col) &&
        !is.null(input$hor_code_col) && nzchar(input$hor_code_col) &&
        input$hor_code_col %in% names(df)) {
      df2 <- df2 %>% mutate(input = as.character(df[[input$hor_code_col]]))
    }
    
    # numeric versions (safe)
    xnum <- numify(df2$X)
    ynum <- numify(df2$Y)
    topn <- numify(df2$top)
    botn <- numify(df2$bottom)
    
    base_cols <- c(".row_in_input", "texid", "X", "Y", "top", "bottom")
    
    # ============ TYPE CHECKS (NEW) ============
    
    # 0a) Non-numeric coordinates (longitude, latitude)
    non_numeric_coords_df <- data.frame()
    
    lon_non_num <- sapply(df2$X, function(x) {
      if (is.na(x) || !nzchar(trimws(as.character(x)))) return(FALSE)
      is.na(numify(x))
    })
    
    lat_non_num <- sapply(df2$Y, function(x) {
      if (is.na(x) || !nzchar(trimws(as.character(x)))) return(FALSE)
      is.na(numify(x))
    })
    
    non_numeric_coords_idx <- which(lon_non_num | lat_non_num)
    
    if (length(non_numeric_coords_idx) > 0) {
      temp_df <- df2[non_numeric_coords_idx, , drop = FALSE] %>%
        pick_cols(base_cols) %>%
        rename_rowcol()
      
      coord_issues <- character(length(non_numeric_coords_idx))
      for (i in seq_along(non_numeric_coords_idx)) {
        idx <- non_numeric_coords_idx[i]
        issues <- c()
        if (lon_non_num[idx]) issues <- c(issues, "lon_not_numeric")
        if (lat_non_num[idx]) issues <- c(issues, "lat_not_numeric")
        coord_issues[i] <- paste(issues, collapse = "; ")
      }
      
      temp_df$coord_type_issue <- coord_issues
      wanted <- c("row_in_input", "texid", "X", "Y", "coord_type_issue", "top", "bottom")
      non_numeric_coords_df <- pick_cols(temp_df, wanted)
    }
    
    # 0b) Non-numeric depth (top, bottom)
    non_numeric_depth_df <- data.frame()
    
    top_non_num <- sapply(df2$top, function(x) {
      if (is.na(x) || !nzchar(trimws(as.character(x)))) return(FALSE)
      is.na(numify(x))
    })
    
    bottom_non_num <- sapply(df2$bottom, function(x) {
      if (is.na(x) || !nzchar(trimws(as.character(x)))) return(FALSE)
      is.na(numify(x))
    })
    
    non_numeric_depth_idx <- which(top_non_num | bottom_non_num)
    
    if (length(non_numeric_depth_idx) > 0) {
      temp_df <- df2[non_numeric_depth_idx, , drop = FALSE] %>%
        pick_cols(base_cols) %>%
        rename_rowcol()
      
      depth_issues <- character(length(non_numeric_depth_idx))
      for (i in seq_along(non_numeric_depth_idx)) {
        idx <- non_numeric_depth_idx[i]
        issues <- c()
        if (top_non_num[idx]) issues <- c(issues, "top_not_numeric")
        if (bottom_non_num[idx]) issues <- c(issues, "bottom_not_numeric")
        depth_issues[i] <- paste(issues, collapse = "; ")
      }
      
      temp_df$depth_type_issue <- depth_issues
      wanted <- c("row_in_input", "texid", "X", "Y", "top", "bottom", "depth_type_issue")
      non_numeric_depth_df <- pick_cols(temp_df, wanted)
    }
    
    # 0c) Non-numeric soil properties
    non_numeric_props_df <- data.frame()
    
    if (!is.null(input$soil_props) && length(input$soil_props) > 0) {
      for (p in input$soil_props) {
        if (p %in% names(df2)) {
          prop_values <- df2[[p]]
          
          # Find rows where value is not NA/empty but not numeric
          non_numeric_prop_idx <- which(
            !is.na(prop_values) & 
            nzchar(trimws(as.character(prop_values))) & 
            is.na(numify(prop_values))
          )
          
          if (length(non_numeric_prop_idx) > 0) {
            for (idx in non_numeric_prop_idx) {
              non_numeric_props_df <- rbind(
                non_numeric_props_df,
                data.frame(
                  row_in_input = df2$.row_in_input[idx],
                  texid = as.character(df2$texid[idx]),
                  X = as.character(df2$X[idx]),
                  Y = as.character(df2$Y[idx]),
                  soil_property = p,
                  value = as.character(prop_values[idx]),
                  stringsAsFactors = FALSE
                )
              )
            }
          }
        }
      }
      
      if (nrow(non_numeric_props_df) > 0) {
        non_numeric_props_df <- non_numeric_props_df %>%
          as.data.frame(stringsAsFactors = FALSE)
      }
    }
    
    # ============ ORIGINAL CHECKS ============
    
    # 0) Duplicates by sample code (texid) - include first + duplicates
    tex <- trimws(as.character(df2$texid))
    dup_vals <- tex[!is.na(tex) & nzchar(tex) & duplicated(tex)]
    if (length(dup_vals) > 0) {
      dup_all_idx <- which(tex %in% unique(dup_vals))
    } else {
      dup_all_idx <- integer(0)
    }
    duplicates_df <- df2[dup_all_idx, , drop = FALSE] |>
      pick_cols(base_cols) |>
      rename_rowcol()
    
    # 1) Missing OR invalid coordinates (EPSG:4326)
    lon_missing <- is.na(xnum)
    lat_missing <- is.na(ynum)
    lon_bad <- !lon_missing & (xnum < -180 | xnum > 180)
    lat_bad <- !lat_missing & (ynum <  -90 | ynum >  90)
    
    miss_coord_idx <- which(lon_missing | lat_missing | lon_bad | lat_bad)
    
    coord_issue <- rep(NA_character_, nrow(df2))
    add_issue <- function(idx, label) {
      if (!any(idx, na.rm = TRUE)) return(invisible(NULL))
      coord_issue[idx] <<- ifelse(is.na(coord_issue[idx]), label, paste(coord_issue[idx], label, sep = ";"))
    }
    add_issue(lon_missing, "missing_lon")
    add_issue(lat_missing, "missing_lat")
    add_issue(lon_bad, "lon_out_of_range")
    add_issue(lat_bad, "lat_out_of_range")
    
    coord_issue <- gsub("^NA;|;NA$", "", coord_issue)
    coord_issue <- gsub("(^;|;$)", "", coord_issue)
    
    missing_coords_df <- df2[miss_coord_idx, , drop = FALSE] |>
      pick_cols(base_cols) |>
      rename_rowcol()
    if (nrow(missing_coords_df) > 0) {
      missing_coords_df$coord_issue <- coord_issue[miss_coord_idx]
      # reorder (only if columns exist)
      wanted <- c("row_in_input", "texid", "X", "Y", "coord_issue", "top", "bottom")
      missing_coords_df <- pick_cols(missing_coords_df, wanted)
    }
    
    # 2) Missing depth
    miss_depth_idx <- which(is.na(topn) | is.na(botn))
    missing_depth_df <- df2[miss_depth_idx, , drop = FALSE] |>
      pick_cols(base_cols) |>
      rename_rowcol()
    
    # 3) bottom <= top
    bad_depth_idx <- which(!is.na(topn) & !is.na(botn) & botn <= topn)
    bad_depth_df <- df2[bad_depth_idx, , drop = FALSE] |>
      pick_cols(base_cols) |>
      rename_rowcol()
    
    # 4) Invalid dates
    invalid_dates_df <- data.frame()
    if (!is.null(input$use_date_col) && is_on(input$use_date_col) && "date_data" %in% names(df2)) {
      dates_parsed <- sapply(df2$date_data, parse_date_with_year)
      dates_parsed <- as.Date(dates_parsed, origin = "1970-01-01")
      invalid_idx <- which(is.na(dates_parsed) & !is.na(df2$date_data) & nzchar(trimws(df2$date_data)))
      
      if (length(invalid_idx) > 0) {
        invalid_dates_df <- df2[invalid_idx, , drop = FALSE] |>
          pick_cols(c(".row_in_input","texid","X","Y","date_data")) |>
          rename_rowcol()
        if (nrow(invalid_dates_df) > 0 && "date_data" %in% names(invalid_dates_df)) {
          names(invalid_dates_df)[names(invalid_dates_df) == "date_data"] <- "date_value"
        }
      }
    } else if (!is.null(input$use_date_col) && !is_on(input$use_date_col)) {
      manual_date <- (input$date_manual %||% as.character(Sys.Date()))
      parsed <- parse_date_with_year(manual_date)
      if (is.na(parsed)) {
        invalid_dates_df <- data.frame(
          row_in_input = "manual_input",
          texid = "N/A",
          X = "N/A",
          Y = "N/A",
          date_value = manual_date,
          stringsAsFactors = FALSE
        )
      }
    }
    
    # 5) Invalid profile_code format
    invalid_profile_code_df <- data.frame()
    if (!is.null(input$use_profile_col) && is_on(input$use_profile_col) && "profile_code_data" %in% names(df2)) {
      invalid_format_idx <- which(!validate_profile_code_format(df2$profile_code_data))
      if (length(invalid_format_idx) > 0) {
        invalid_profile_code_df <- df2[invalid_format_idx, , drop = FALSE] |>
          pick_cols(c(".row_in_input","texid","X","Y","profile_code_data")) |>
          rename_rowcol()
        if (nrow(invalid_profile_code_df) > 0 && "profile_code_data" %in% names(invalid_profile_code_df)) {
          names(invalid_profile_code_df)[names(invalid_profile_code_df) == "profile_code_data"] <- "profile_code"
        }
      }
    }
    
    # 6) Inconsistent profile_code per coordinate group
    profile_inconsistent_df <- data.frame()
    if (!is.null(input$use_profile_col) && is_on(input$use_profile_col) && "profile_code_data" %in% names(df2)) {
      coord_groups <- df2 %>%
        group_by(X, Y) %>%
        summarise(
          n_profiles = n_distinct(profile_code_data),
          profiles = paste(unique(profile_code_data), collapse = "; "),
          .groups = "drop"
        ) %>%
        filter(n_profiles > 1)
      
      if (nrow(coord_groups) > 0) {
        profile_inconsistent_df <- df2 %>%
          inner_join(coord_groups %>% select(X, Y), by = c("X", "Y")) %>%
          select(.row_in_input, texid, X, Y, profile_code_data) %>%
          as.data.frame(stringsAsFactors = FALSE)
        names(profile_inconsistent_df) <- c("row_in_input","texid","X","Y","profile_code")
      }
    }
    
    # 7) Invalid plot_code format
    invalid_plot_code_df <- data.frame()
    if (!is.null(input$use_plot_col) && is_on(input$use_plot_col) && "plot_code_data" %in% names(df2)) {
      invalid_format_idx <- which(!validate_plot_code_format(df2$plot_code_data))
      if (length(invalid_format_idx) > 0) {
        invalid_plot_code_df <- df2[invalid_format_idx, , drop = FALSE] |>
          pick_cols(c(".row_in_input","texid","X","Y","plot_code_data")) |>
          rename_rowcol()
        if (nrow(invalid_plot_code_df) > 0 && "plot_code_data" %in% names(invalid_plot_code_df)) {
          names(invalid_plot_code_df)[names(invalid_plot_code_df) == "plot_code_data"] <- "plot_code"
        }
      }
    }
    
    # 8) Inconsistent plot_code per coordinate group
    plot_inconsistent_df <- data.frame()
    if (!is.null(input$use_plot_col) && is_on(input$use_plot_col) && "plot_code_data" %in% names(df2)) {
      coord_groups <- df2 %>%
        group_by(X, Y) %>%
        summarise(
          n_plots = n_distinct(plot_code_data),
          plots = paste(unique(plot_code_data), collapse = "; "),
          .groups = "drop"
        ) %>%
        filter(n_plots > 1)
      
      if (nrow(coord_groups) > 0) {
        plot_inconsistent_df <- df2 %>%
          inner_join(coord_groups %>% select(X, Y), by = c("X", "Y")) %>%
          select(.row_in_input, texid, X, Y, plot_code_data) %>%
          as.data.frame(stringsAsFactors = FALSE)
        names(plot_inconsistent_df) <- c("row_in_input","texid","X","Y","plot_code")
      }
    }
    
    # 9) Out-of-range using value_min/value_max for selected mapping (+ conversion factor)
    oor <- data.frame()
    ref_raw <- procedures_lookup_raw()
    has_bounds <- all(c("value_min", "value_max", "property_phys_chem_id", "procedure_phys_chem_id") %in% names(ref_raw))
    
    if (!is.null(input$soil_props) && length(input$soil_props) > 0 && has_bounds) {
      
      # For conversion factor lookups / reference unit
      ref_norm <- procedures_lookup()
      
      for (p in input$soil_props) {
        if (!p %in% names(df2)) next
        pid <- safe_id(p)
        
        prop_val <- input[[paste0("map_property__", pid)]]
        proc_val <- input[[paste0("map_procedure__", pid)]]
        input_unit_val <- input[[paste0("map_input_unit__", pid)]]
        
        if (is.null(prop_val) || !nzchar(prop_val)) next
        proc_val <- proc_val %||% ""
        
        m <- ref_raw[
          trimws(as.character(ref_raw$property_phys_chem_id)) == prop_val &
            trimws(as.character(ref_raw$procedure_phys_chem_id)) == proc_val,
          , drop = FALSE
        ]
        if (nrow(m) == 0) {
          m <- ref_raw[trimws(as.character(ref_raw$property_phys_chem_id)) == prop_val, , drop = FALSE]
        }
        if (nrow(m) == 0) next
        
        vmin <- numify(m$value_min[1])
        vmax <- numify(m$value_max[1])
        if (is.na(vmin) && is.na(vmax)) next
        
        # conversion
        conversion_factor <- get_conversion_factor(prop_val, proc_val, input_unit_val %||% "", ref_norm)
        
        vals <- numify(df2[[p]])
        vals_converted <- vals * conversion_factor
        
        bad <- which(!is.na(vals_converted) &
                       (( !is.na(vmin) & vals_converted < vmin) | (!is.na(vmax) & vals_converted > vmax)))
        
        if (length(bad) > 0) {
          ref_unit <- get_reference_unit(prop_val, proc_val, ref_norm)
          
          # Create data frame with properly replicated scalar values
          n_bad <- length(bad)
          oor <- rbind(
            oor,
            data.frame(
              row_in_input        = df2$.row_in_input[bad],
              texid               = as.character(df2$texid[bad]),
              X                   = as.character(df2$X[bad]),
              Y                   = as.character(df2$Y[bad]),
              top                 = as.character(df2$top[bad]),
              bottom              = as.character(df2$bottom[bad]),
              soil_property       = rep(p, n_bad),
              value_original      = vals[bad],
              value_converted     = vals_converted[bad],
              value_min           = rep(vmin, n_bad),
              value_max           = rep(vmax, n_bad),
              property_phys_chem_id  = rep(prop_val, n_bad),
              procedure_phys_chem_id = rep(proc_val, n_bad),
              unit_of_measure_id     = rep(ref_unit, n_bad),
              stringsAsFactors = FALSE
            )
          )
        }
      }
    }
    
    list(
      non_numeric_coords_df       = non_numeric_coords_df,
      non_numeric_depth_df        = non_numeric_depth_df,
      non_numeric_props_df        = non_numeric_props_df,
      duplicates_df               = duplicates_df,
      missing_coords_df           = missing_coords_df,
      missing_depth_df            = missing_depth_df,
      bad_depth_df                = bad_depth_df,
      invalid_dates_df            = invalid_dates_df,
      invalid_profile_code_df     = invalid_profile_code_df,
      profile_inconsistent_df     = profile_inconsistent_df,
      invalid_plot_code_df        = invalid_plot_code_df,
      plot_inconsistent_df        = plot_inconsistent_df,
      out_of_range_df             = oor
    )
  })
  
  
  output$download_qc_xlsx <- downloadHandler(
    filename = function() paste0("QC_report_", Sys.Date(), ".xlsx"),
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    content = function(file) {
      
      res <- qc_results()
      
      # Helper: Excel sheet names max 31 chars + no special chars
      safe_sheet <- function(x) {
        x <- gsub("[\\[\\]\\*\\?/\\\\:]", "_", x)
        substr(x, 1, 31)
      }
      
      wb <- openxlsx::createWorkbook()
      
      # ---- Summary sheet ----
      openxlsx::addWorksheet(wb, "Summary")
      summary_df <- data.frame(
        check = names(res),
        n_rows = vapply(res, function(x) if (is.data.frame(x)) nrow(x) else NA_integer_, integer(1)),
        stringsAsFactors = FALSE
      )
      openxlsx::writeData(wb, "Summary", summary_df, startRow = 1, startCol = 1)
      openxlsx::freezePane(wb, "Summary", firstRow = TRUE)
      openxlsx::setColWidths(wb, "Summary", cols = 1:2, widths = "auto")
      
      # ---- One sheet per QC table ----
      add_qc_sheet <- function(sheet_name, df) {
        sheet_name <- safe_sheet(sheet_name)
        openxlsx::addWorksheet(wb, sheet_name)
        
        if (!is.data.frame(df) || nrow(df) == 0) {
          openxlsx::writeData(wb, sheet_name, data.frame(message = "No issues found."), startRow = 1, startCol = 1)
          return(invisible())
        }
        
        # Write as a formatted table with filters
        openxlsx::writeDataTable(
          wb, sheet = sheet_name, x = df,
          startRow = 1, startCol = 1,
          tableStyle = "TableStyleLight9",
          withFilter = TRUE
        )
        openxlsx::freezePane(wb, sheet_name, firstRow = TRUE)
        openxlsx::setColWidths(wb, sheet_name, cols = 1:ncol(df), widths = "auto")
      }
      
      # If you want a specific order of sheets:
      sheet_order <- c(
        "non_numeric_coords_df",
        "non_numeric_depth_df",
        "non_numeric_props_df",
        "duplicates_df",
        "missing_coords_df",
        "missing_depth_df",
        "bad_depth_df",
        "invalid_dates_df",
        "invalid_profile_code_df",
        "profile_inconsistent_df",
        "invalid_plot_code_df",
        "plot_inconsistent_df",
        "out_of_range_df"
      )
      
      # Write in that order if present, then anything else that might exist
      for (nm in sheet_order) {
        if (nm %in% names(res)) add_qc_sheet(nm, res[[nm]])
      }
      for (nm in setdiff(names(res), sheet_order)) {
        add_qc_sheet(nm, res[[nm]])
      }
      
      openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
    }
  )
  
  # Check status box ----
  output$qc_panel <- renderUI({
    req(input$data_file)
    
    if (!isTRUE(mapping_ready())) {
      return(
        tags$div(
          style = paste(
            "margin-top:16px;",
            "padding:16px 14px;",
            "border-radius:10px;",
            "background:#fff3f3;",                 # light red background
            "border:1px solid rgba(176,0,32,.25);",
            "border-left:10px solid #b00020;",    # strong left bar
            "color:#b00020;",
            "font-weight:900;",
            "font-size:20px;",                    # bigger text
            "line-height:1.25;",
            "box-shadow:0 1px 2px rgba(0,0,0,0.06);"
          ),
          tags$div(
            style = "display:flex; gap:12px; align-items:flex-start;",
            tags$span(icon("triangle-exclamation"), style = "font-size:26px; margin-top:2px;"),
            tags$div(
              tags$div("Please map your variables in the sidebar menu."),
              tags$div(
                style = "margin-top:6px; font-weight:700; font-size:14px; color:rgba(176,0,32,.9);",
                "Required: sample ID, longitude, latitude, top depth and bottom depth."
              )
            )
          )
        )
      )
    }
    
    
    res <- qc_results()
    
    # Safe nrow in case any element is NULL
    n_safe <- function(x) if (is.data.frame(x)) nrow(x) else 0
    
    n_non_num_coord <- n_safe(res$non_numeric_coords_df)
    n_non_num_depth <- n_safe(res$non_numeric_depth_df)
    n_non_num_props <- n_safe(res$non_numeric_props_df)
    n_dup             <- n_safe(res$duplicates_df)
    n_coord           <- n_safe(res$missing_coords_df)
    n_depth           <- n_safe(res$missing_depth_df)
    n_bad             <- n_safe(res$bad_depth_df)
    n_dates           <- n_safe(res$invalid_dates_df)
    n_prof_fmt        <- n_safe(res$invalid_profile_code_df)
    n_prof_inc        <- n_safe(res$profile_inconsistent_df)
    n_plot_fmt        <- n_safe(res$invalid_plot_code_df)
    n_plot_inc        <- n_safe(res$plot_inconsistent_df)
    n_oor             <- n_safe(res$out_of_range_df)
    
    any_err <- (n_non_num_coord + n_non_num_depth + n_non_num_props +
                  n_dup + n_coord + n_depth + n_bad + n_dates +
                  n_prof_fmt + n_prof_inc + n_plot_fmt + n_plot_inc + n_oor) > 0
    
    tags$div(
      tags$div(
        style = "font-weight:800; margin-bottom:6px;",
        icon("shield-halved"), " Data quality checks"
      ),
      
      tags$ul(
        tags$li(paste0("Non-numeric coordinates (lon/lat): ", n_non_num_coord)),
        tags$li(paste0("Non-numeric depth (top/bottom): ", n_non_num_depth)),
        tags$li(paste0("Non-numeric soil properties: ", n_non_num_props)),
        tags$li(paste0("Duplicates (by sample ID): ", n_dup)),
        tags$li(paste0("Missing/invalid coordinates (EPSG:4326): ", n_coord)),
        tags$li(paste0("Missing depth (top OR bottom): ", n_depth)),
        tags$li(paste0("Invalid depth (bottom ≤ top): ", n_bad)),
        tags$li(paste0("Invalid dates: ", n_dates)),
        tags$li(paste0("Invalid profile_code format: ", n_prof_fmt)),
        tags$li(paste0("Inconsistent profile_code per coordinate: ", n_prof_inc)),
        tags$li(paste0("Invalid plot_code format: ", n_plot_fmt)),
        tags$li(paste0("Inconsistent plot_code per coordinate: ", n_plot_inc)),
        tags$li(paste0("Out-of-range soil property values: ", n_oor))
      ),
      
      if (any_err) {
        tagList(
          tags$div(
            style = paste(
              "margin-top:16px;",
              "padding:16px 14px;",
              "border-radius:10px;",
              "background:#fff3f3;",                 # light red background
              "border:1px solid rgba(176,0,32,.25);",
              "border-left:10px solid #b00020;",    # strong left bar
              "color:#b00020;",
              "font-weight:900;",
              "font-size:20px;",                    # bigger text
              "line-height:1.25;",
              "box-shadow:0 1px 2px rgba(0,0,0,0.06);"
            ),
            tags$div(
              style = "display:flex; gap:12px; align-items:flex-start;",
              
              # left warning icon
              tags$span(icon("triangle-exclamation"),
                        style = "font-size:26px; margin-top:2px; color:#b00020;"),
              
              # right text block
              tags$div(
                # first row: title + explanation together
                tags$div(
                  style = "font-weight:900; font-size:20px; line-height:1.25; color:#b00020;",
                  "Errors were found in the dataset. "
                ),
                
                tags$hr(style = "margin:10px 0; border-top: 1px solid rgba(0,0,0,0.15);"),
                
                # âœ… last row: imessage
                tags$div(
                  style = "margin-top:8px; padding:10px; background:rgba(176,0,32,0.05); border-left:4px solid rgba(176,0,32,.95); border-radius:4px;",
                  tags$div(
                    style = "font-weight:800; font-size:16px; color:rgba(176,0,32,.95); margin-bottom:8px;",
                    icon("circle-info"), " How to proceed:"
                  ),
                  tags$ol(
                    style = "margin:0; padding-left:20px; color:rgba(176,0,32,.9);",
                    tags$li(
                      style = "font-weight:600; font-size:14px; margin-bottom:4px;",
                      "Ensure your properties, methods, and units are correctly identified in the box below."
                    ),
                    tags$li(
                      style = "font-weight:600; font-size:14px;",
                      "Go to the 'Quality checks' tab for full details."
                    )
                  )
                )
              )
            )
          ),
          tags$br(),
          tags$hr(style = "margin: 8px 0;"),
          downloadButton(
            "download_qc_xlsx",
            "Download QC report (XLSX)",
            class = "btn btn-danger",
            style = "width: 100%; font-weight: 800; background-color:#ed022d; border-color:#ed022d;"
          ),
          tags$br()
        )
      } else {
        tags$div(
          style = "font-weight:800; margin-top:6px; color:#1a7f37;",
          icon("check"),
          tags$span("Data ready to harmonize."),
          tags$br(),
          tags$span(
            style = "color:#ff8c00;",
            icon("triangle-exclamation"),
            "Please ensure your soil properties are correctly mapped to the GloSIS properties, methods, and units."
          )
        )
      }
    )
  })
  
  
  # Render QC tables ---
  dt_qc_opts <- list(
    pageLength = 15,
    lengthMenu = c(15, 30, 50, 100),
    scrollX = TRUE,
    autoWidth = TRUE,
    deferRender = TRUE
  )
  
  output$qc_non_numeric_coords_dt <- renderDT({
    req(qc_results())
    df <- qc_results()$non_numeric_coords_df
    req(nrow(df) > 0)
    datatable(df, rownames = FALSE, filter = "top", options = dt_qc_opts)
  }, server = TRUE)
  
  output$qc_non_numeric_depth_dt <- renderDT({
    req(qc_results())
    df <- qc_results()$non_numeric_depth_df
    req(nrow(df) > 0)
    datatable(df, rownames = FALSE, filter = "top", options = dt_qc_opts)
  }, server = TRUE)
  
  output$qc_non_numeric_props_dt <- renderDT({
    req(qc_results())
    df <- qc_results()$non_numeric_props_df
    req(nrow(df) > 0)
    datatable(df, rownames = FALSE, filter = "top", options = dt_qc_opts)
  }, server = TRUE)
  
  output$qc_duplicates_dt <- renderDT({
    req(qc_results())
    df <- qc_results()$duplicates_df
    req(nrow(df) > 0)
    datatable(qc_results()$duplicates_df, rownames = FALSE, filter = "top", options = dt_qc_opts)
  }, server = TRUE)
  
  output$qc_missing_coords_dt <- renderDT({
    req(qc_results())
    df <- qc_results()$missing_coords_df
    req(nrow(df) > 0)
    datatable(qc_results()$missing_coords_df, rownames = FALSE, filter = "top", options = dt_qc_opts)
  }, server = TRUE)
  
  output$qc_missing_depth_dt <- renderDT({
    req(qc_results())
    df <- qc_results()$missing_depth_df
    req(nrow(df) > 0)
    datatable(qc_results()$missing_depth_df, rownames = FALSE, filter = "top", options = dt_qc_opts)
  }, server = TRUE)
  
  output$qc_bad_depth_dt <- renderDT({
    req(qc_results())
    df <- qc_results()$bad_depth_df
    req(nrow(df) > 0)
    datatable(qc_results()$bad_depth_df, rownames = FALSE, filter = "top", options = dt_qc_opts)
  }, server = TRUE)
  
  output$qc_invalid_dates_dt <- renderDT({
    req(qc_results())
    df <- qc_results()$invalid_dates_df
    req(nrow(df) > 0)
    datatable(qc_results()$invalid_dates_df, rownames = FALSE, filter = "top", options = dt_qc_opts)
  }, server = TRUE)
  
  output$qc_invalid_profile_code_dt <- renderDT({
    req(qc_results())
    df <- qc_results()$invalid_profile_code_df
    req(nrow(df) > 0)
    datatable(qc_results()$invalid_profile_code_df, rownames = FALSE, filter = "top", options = dt_qc_opts)
  }, server = TRUE)
  
  output$qc_profile_inconsistent_dt <- renderDT({
    req(qc_results())
    df <- qc_results()$profile_inconsistent_df
    req(nrow(df) > 0)
    datatable(qc_results()$profile_inconsistent_df, rownames = FALSE, filter = "top", options = dt_qc_opts)
  }, server = TRUE)
  
  output$qc_invalid_plot_code_dt <- renderDT({
    req(qc_results())
    df <- qc_results()$invalid_plot_code_df
    req(nrow(df) > 0)
    datatable(qc_results()$invalid_plot_code_df, rownames = FALSE, filter = "top", options = dt_qc_opts)
  }, server = TRUE)
  
  output$qc_plot_inconsistent_dt <- renderDT({
    req(qc_results())
    df <- qc_results()$plot_inconsistent_df
    req(nrow(df) > 0)
    datatable(qc_results()$plot_inconsistent_df, rownames = FALSE, filter = "top", options = dt_qc_opts)
  }, server = TRUE)
  
  output$qc_out_of_range_dt <- renderDT({
    req(qc_results())
    df <- qc_results()$out_of_range_df
    req(nrow(df) > 0)
    datatable(qc_results()$out_of_range_df, rownames = FALSE, filter = "top", options = dt_qc_opts)
  }, server = TRUE)
  
  observeEvent(input$mainTabs, {
    if (identical(input$mainTabs, "qc_tab")) {
      session$sendCustomMessage("dt_adjust_one", list(id = "qc_non_numeric_coords_dt"))
      session$sendCustomMessage("dt_adjust_one", list(id = "qc_non_numeric_depth_dt"))
      session$sendCustomMessage("dt_adjust_one", list(id = "qc_non_numeric_props_dt"))
      session$sendCustomMessage("dt_adjust_one", list(id = "qc_duplicates_dt"))
      session$sendCustomMessage("dt_adjust_one", list(id = "qc_missing_coords_dt"))
      session$sendCustomMessage("dt_adjust_one", list(id = "qc_missing_depth_dt"))
      session$sendCustomMessage("dt_adjust_one", list(id = "qc_bad_depth_dt"))
      session$sendCustomMessage("dt_adjust_one", list(id = "qc_invalid_dates_dt"))
      session$sendCustomMessage("dt_adjust_one", list(id = "qc_invalid_profile_code_dt"))
      session$sendCustomMessage("dt_adjust_one", list(id = "qc_profile_inconsistent_dt"))
      session$sendCustomMessage("dt_adjust_one", list(id = "qc_invalid_plot_code_dt"))
      session$sendCustomMessage("dt_adjust_one", list(id = "qc_plot_inconsistent_dt"))
      session$sendCustomMessage("dt_adjust_one", list(id = "qc_out_of_range_dt"))
    }
  }, ignoreInit = TRUE)
  
  # ---- status ----
  output$status <- renderPrint({
    if (is.null(input$data_file)) {
      cat("Upload a CSV or XLSX to start.\n")
      cat("Working directory:", getwd(), "\n")
      cat("Default template exists:", file.exists("templates/glosis_template_v6.xlsx"), "\n")
      cat("Procedures lookup exists:", file.exists("templates/glosis_procedures_v2.csv"), "\n")
      return()
    }
    
    ext <- tolower(tools::file_ext(input$data_file$name))
    df <- k_data()
    
    cat("File:", input$data_file$name, "\n")
    if (ext %in% c("xlsx", "xls") && !is.null(input$xlsx_sheet)) {
      cat("Imported sheet:", input$xlsx_sheet, "\n")
    }
    cat("Rows:", nrow(df), " | Cols:", ncol(df), "\n\n")
    
    cat("Using template:", template_path(), "\n")
    cat("Template exists:", file.exists(template_path()), "\n")
    cat("Procedures lookup exists:", file.exists("templates/glosis_procedures_v2.csv"), "\n\n")
    # âœ… Mapping hint in Status & Checks box
    # if (!isTRUE(mapping_ready())) {
    #   cat("\nâš ï¸ Please map your variables in the sidebar menu (Longitude, Latitude, Sample ID, Top, Bottom).\n")
    #   return()
    # }
    
    
    if (!is.null(input$soil_props)) {
      cat("Selected soil properties:", paste(input$soil_props, collapse = ", "), "\n")
    }
  })
  
  # ---- validation ----
  output$validation_panel <- renderUI({
    if (is.null(input$data_file)) {
      return(
        tags$div(
          style = paste(
            "margin-top:16px;",
            "padding:16px 14px;",
            "border-radius:10px;",
            "background:#f2f2f2;",                  # light neutral background
            "border:1px solid rgba(46,46,46,.18);", # soft border
            "border-left:10px solid #2e2e2e;",      # strong left bar (neutral)
            "color:#2e2e2e;",
            "font-weight:900;",
            "font-size:20px;",
            "line-height:1.25;",
            "box-shadow:0 1px 2px rgba(0,0,0,0.06);"
          ),
          tags$div(
            style = "display:flex; gap:12px; align-items:flex-start;",
            tags$span(
              icon("arrow-left"),
              style = "font-size:26px; margin-top:2px; color:#2e2e2e;"
            ),
            tags$div(
              tags$div(
                style = "font-weight:900; font-size:20px; line-height:1.2;",
                "No file uploaded."
              ),
              tags$div(
                style = "margin-top:6px; font-weight:700; font-size:14px; color:rgba(46,46,46,.85);",
                "Upload a file using the sidebar to begin."
              )
            )
          )
        )
      )
    }
    
    msgs <- c()
    
    if (is.null(input$soil_props) || length(input$soil_props) == 0) {
      msgs <- c(msgs, "Select at least one soil property column.")
    }
    
    tp <- template_path()
    if (!file.exists(tp)) msgs <- c(msgs, paste0("Template not found: ", tp))
    if (!file.exists("templates/glosis_procedures_v2.csv")) msgs <- c(msgs, "glosis_procedures_v2.csv not found in app folder.")
    
    needed <- c("col_lon","col_lat","col_texid","col_top","col_bottom")
    if (any(sapply(needed, function(x) is.null(input[[x]]) || input[[x]] == ""))) {
      msgs <- c(msgs, "Map all required fields (texid/lon/lat/top/bottom).")
    }
    
    if (length(msgs) == 0) {
      tags$div(icon("check"), "File processed.", style = "font-weight:800; color:#1a7f37;")
    } else {
      tags$div(
        icon("triangle-exclamation"),
        tags$ul(lapply(msgs, tags$li)),
        style = "font-weight:700; color:#b00020;"
      )
    }
  })
  
  # ---- build workbook ----
  build_workbook <- reactive({
    tryCatch({
      cat("\n=== BUILD WORKBOOK START ===\n")
      
      # Explicit validation instead of req() to avoid silent errors
      if (is.null(k_data())) {
        stop("No data loaded")
      }
      
      # Check each required column individually for better error messages
      missing_cols <- character(0)
      if (is.null(input$col_texid) || input$col_texid == "") missing_cols <- c(missing_cols, "Sample ID")
      if (is.null(input$col_lon) || input$col_lon == "") missing_cols <- c(missing_cols, "Longitude")
      if (is.null(input$col_lat) || input$col_lat == "") missing_cols <- c(missing_cols, "Latitude")
      if (is.null(input$col_top) || input$col_top == "") missing_cols <- c(missing_cols, "Upper depth")
      if (is.null(input$col_bottom) || input$col_bottom == "") missing_cols <- c(missing_cols, "Lower depth")
      
      if (length(missing_cols) > 0) {
        stop(paste("Required columns not selected:", paste(missing_cols, collapse = ", ")))
      }
      
      if (is.null(input$soil_props) || length(input$soil_props) == 0) {
        stop("No soil properties selected")
      }
      
      cat("Validations passed\n")
      
      df <- k_data()
      
      # Determine project_name source
      if (input$use_project_col && !is.null(input$project_name_col)) {
        if (is.null(input$project_name_col) || !nzchar(input$project_name_col)) {
          stop("Project name column not selected")
        }
        validate(need(input$project_name_col %in% names(df),
                      paste("Selected project name column not found in data:", input$project_name_col)))
        use_project_column <- TRUE
      } else {
        project_name <- input$project_name %||% "KSSL"
        use_project_column <- FALSE
      }
      
      # Determine site_code source
      if (input$use_site_col && !is.null(input$site_code_col)) {
        if (is.null(input$site_code_col) || !nzchar(input$site_code_col)) {
          stop("Site code column not selected")
        }
        validate(need(input$site_code_col %in% names(df),
                      paste("Selected site code column not found in data:", input$site_code_col)))
        use_site_column <- TRUE
      } else {
        site_code <- input$site_code %||% "MySite"
        use_site_column <- FALSE
      }
      
      # Determine date source
      if (input$use_date_col && !is.null(input$date_col)) {
        if (is.null(input$date_col) || !nzchar(input$date_col)) {
          stop("Date column not selected")
        }
        validate(need(input$date_col %in% names(df),
                      paste("Selected date column not found in data:", input$date_col)))
        use_date_column <- TRUE
      } else {
        use_date_column <- FALSE
        manual_date <- input$date_manual %||% as.character(Sys.Date())
      }
      
      # Determine profile_code source
      if (input$use_profile_col && !is.null(input$profile_code_col)) {
        if (is.null(input$profile_code_col) || !nzchar(input$profile_code_col)) {
          stop("Profile code column not selected")
        }
        validate(need(input$profile_code_col %in% names(df),
                      paste("Selected profile code column not found in data:", input$profile_code_col)))
        use_profile_column <- TRUE
      } else {
        use_profile_column <- FALSE
      }
      
      # Determine plot_code source
      if (input$use_plot_col && !is.null(input$plot_code_col)) {
        if (is.null(input$plot_code_col) || !nzchar(input$plot_code_col)) {
          stop("Plot code column not selected")
        }
        validate(need(input$plot_code_col %in% names(df),
                      paste("Selected plot code column not found in data:", input$plot_code_col)))
        use_plot_column <- TRUE
      } else {
        use_plot_column <- FALSE
      }
      
      plot_type    <- input$plot_type %||% "TrialPit"
      horizon_type <- input$horizon_type %||% "Horizon"
      
      # Rename columns to standard names (WITHOUT hor initially)
      df2 <- df %>%
        rename(
          X      = all_of(input$col_lon),
          Y      = all_of(input$col_lat),
          texid  = all_of(input$col_texid),
          top    = all_of(input$col_top),
          bottom = all_of(input$col_bottom)
        )
      
      # Only add hor column if it's being used
      if (!is.null(input$use_hor_col) && input$use_hor_col && 
          !is.null(input$col_hor) && nzchar(input$col_hor)) {
        if (input$col_hor %in% names(df)) {
          df2 <- df2 %>%
            mutate(hor_code_data = as.character(df[[input$col_hor]]))
        }
      }
      
      # Add columns from data if selected
      if (use_project_column) {
        df2 <- df2 %>%
          mutate(project_name_data = as.character(df[[input$project_name_col]]))
      }
      
      if (use_site_column) {
        df2 <- df2 %>%
          mutate(site_code_data = as.character(df[[input$site_code_col]]))
      }
      
      if (use_date_column) {
        df2 <- df2 %>%
          mutate(date_data = as.character(df[[input$date_col]]))
      }
      
      if (use_profile_column) {
        df2 <- df2 %>%
          mutate(profile_code_data = as.character(df[[input$profile_code_col]]))
      }
      
      if (use_plot_column) {
        df2 <- df2 %>%
          mutate(plot_code_data = as.character(df[[input$plot_code_col]]))
      }
      
      soil.properties <- input$soil_props
      validate(need(all(soil.properties %in% names(df2)),
                    "Some selected soil property columns are not present in the uploaded data."))
      
      # Prepare grouping variables
      group_vars <- c("X", "Y")
      if (use_project_column) group_vars <- c(group_vars, "project_name_data")
      if (use_site_column) group_vars <- c(group_vars, "site_code_data")
      if (use_profile_column) group_vars <- c(group_vars, "profile_code_data")
      if (use_plot_column) group_vars <- c(group_vars, "plot_code_data")
      
      plot_info <- df2 %>%
        mutate(row_pos = row_number()) %>%
        group_by(across(all_of(group_vars))) %>%
        summarise(n_layers = n(), first_row = min(row_pos), .groups = "drop") %>%
        arrange(first_row) %>%
        mutate(
          project_name = if (use_project_column) project_name_data else project_name,
          site_code = if (use_site_column) site_code_data else site_code,
          profile_index = row_number(),
          plot_code = if (use_plot_column) plot_code_data else paste0("plot_", profile_index),
          profile_code = if (use_profile_column) profile_code_data else paste0("profile_", profile_index),
          plot_type = plot_type,
          longitude = X,
          latitude = Y,
          altitude = NA,
          positional_accuracy = NA,
          extent = NA,
          map_sheet_code = NA
        )
      
      # Handle date
      if (use_date_column) {
        date_lookup <- df2 %>%
          group_by(across(all_of(group_vars))) %>%
          summarise(date_str = first(date_data), .groups = "drop") %>%
          mutate(date = parse_date_with_year(date_str))
        
        plot_info <- plot_info %>%
          left_join(date_lookup %>% select(-date_str), by = group_vars)
      } else {
        parsed_date <- parse_date_with_year(manual_date)
        plot_info <- plot_info %>%
          mutate(date = parsed_date)
      }
      
      plot_info <- plot_info %>%
        select(project_name, site_code, plot_code, profile_code, plot_type, 
               n_layers, date, longitude, latitude, altitude, positional_accuracy, 
               extent, map_sheet_code)
      
      profile_info <- tibble(profile_code = plot_info$profile_code)
      
      # FIXED: Create proper join keys that match the grouping logic
      # Add the grouping columns to df2 for proper joining
      df2_with_groups <- df2
      
      # Add join keys based on the same grouping variables used in plot_info
      if (use_project_column) {
        df2_with_groups <- df2_with_groups %>%
          mutate(join_project = project_name_data)
      } else {
        df2_with_groups <- df2_with_groups %>%
          mutate(join_project = project_name)
      }
      
      if (use_site_column) {
        df2_with_groups <- df2_with_groups %>%
          mutate(join_site = site_code_data)
      } else {
        df2_with_groups <- df2_with_groups %>%
          mutate(join_site = site_code)
      }
      
      if (use_profile_column) {
        df2_with_groups <- df2_with_groups %>%
          mutate(join_profile = profile_code_data)
      }
      
      if (use_plot_column) {
        df2_with_groups <- df2_with_groups %>%
          mutate(join_plot = plot_code_data)
      }
      
      # Prepare plot_info for joining
      plot_info_for_join <- plot_info %>%
        mutate(
          join_project = project_name,
          join_site = site_code
        )
      
      if (use_profile_column) {
        plot_info_for_join <- plot_info_for_join %>%
          mutate(join_profile = profile_code)
      }
      
      # if (use_plot_column) {
      #   df2_with_groups <- df2_with_groups %>%
      #     mutate(join_plot = plot_code_data)
      # }
      if (use_plot_column) {
        plot_info_for_join <- plot_info_for_join %>%  # ✅ CORRECT
          mutate(join_plot = plot_code)
      }
      
      # Build join_by vector dynamically
      join_by_vec <- c("X" = "longitude", "Y" = "latitude", 
                       "join_project" = "join_project", 
                       "join_site" = "join_site")
      
      if (use_profile_column) {
        join_by_vec <- c(join_by_vec, "join_profile" = "join_profile")
      }
      
      if (use_plot_column) {
        join_by_vec <- c(join_by_vec, "join_plot" = "join_plot")
      }
      
      # Get the columns to select from plot_info (the right side of the join)
      # These are the VALUES of join_by_vec (longitude, latitude, join_project, etc.)
      #plot_info_cols <- c("profile_code", unname(join_by_vec))
      plot_info_cols <- c("profile_code", unique(unname(join_by_vec)))
      
      # Now perform the joins with proper matching
      element_info <- df2_with_groups %>%
        left_join(plot_info_for_join %>% select(all_of(plot_info_cols)),
                  by = join_by_vec) %>%
        group_by(profile_code) %>%
        mutate(
          element_code = texid,
          type = horizon_type,
          order_element = row_number(),
          upper_depth = top,
          lower_depth = bottom,
          horizon_code = if (!is.null(input$use_hor_col) && input$use_hor_col && "hor_code_data" %in% names(.)) {
            hor_code_data
          } else {
            paste0("horizon_", order_element)  # Use order_element instead of profile_index
          }
        ) %>%
        ungroup() %>%
        select(profile_code, element_code, type, order_element, upper_depth, lower_depth, horizon_code)
      
      
      specimen_info <- df2_with_groups %>%
        left_join(plot_info_for_join %>% select(all_of(plot_info_cols)), 
                  by = join_by_vec) %>%
        group_by(profile_code) %>%
        mutate(element_code = texid, specimen_code = texid) %>%
        ungroup() %>%
        select(profile_code, element_code, specimen_code, all_of(soil.properties))
      
      # *** CRITICAL: Save original data BEFORE applying conversions ***
      # This preserves the actual uploaded values in their original units
      # for the "Original Data" sheet
      original_data_info <- specimen_info
      
      
      # Apply unit conversions to specimen_info
      ref <- procedures_lookup()
      for (p in soil.properties) {
        pid <- safe_id(p)
        
        prop_val <- input[[paste0("map_property__", pid)]]
        proc_val <- input[[paste0("map_procedure__", pid)]]
        input_unit <- input[[paste0("map_input_unit__", pid)]]
        
        validate(need(!is.null(prop_val) && nzchar(prop_val), 
                      paste("Select property_phys_chem_id for:", p)))
        validate(need(!is.null(proc_val) && nzchar(proc_val), 
                      paste("Select procedure_phys_chem_id for:", p)))
        validate(need(!is.null(input_unit) && nzchar(input_unit), 
                      paste("Select input unit for:", p)))
        
        conversion_factor <- get_conversion_factor(prop_val, proc_val, input_unit, ref)
        
        # Apply conversion
        if (conversion_factor != 1 && p %in% names(specimen_info)) {
          specimen_info[[p]] <- convert_values(specimen_info[[p]], conversion_factor)
        }
      }
      
      # ---- Metadata (apply single-row values to ALL plots) ----
      # Get metadata with error handling
      md <- tryCatch({
        metadata_reactive()
      }, error = function(e) {
        cat("ERROR retrieving metadata:", e$message, "\n")
        NULL
      })
      
      cat("\n=== METADATA DEBUG ===\n")
      cat("metadata_reactive() returned:\n")
      print(md)
      cat("Is NULL:", is.null(md), "\n")
      cat("Is data.frame:", is.data.frame(md), "\n")
      if (!is.null(md)) {
        cat("Rows:", nrow(md), "\n")
        cat("Columns:", paste(names(md), collapse = ", "), "\n")
        if (nrow(md) > 0) {
          cat("First row values:\n")
          print(md[1, ])
        }
      }
      cat("======================\n\n")
      
      if (is.null(md) || nrow(md) == 0 || !is.data.frame(md)) {
        cat("Using default empty metadata\n")
        md <- data.frame(
          name = "", honorific_title = "", role = "", email = "", telephone = "",
          url = "", organization = "", street_address = "", postal_code = "",
          locality = "", country = "",
          stringsAsFactors = FALSE
        )
      }
      
      # helper: blank -> "unknown", with safe access
      md_val <- function(x) {
        if (is.null(x) || length(x) == 0) return("unknown")
        x <- trimws(as.character(x))
        if (is.na(x) || !nzchar(x)) "unknown" else x
      }
      
      # Safe access to first row of metadata
      get_md_val <- function(md, col_name) {
        if (col_name %in% names(md) && nrow(md) > 0) {
          val <- md_val(md[[col_name]][1])
          cat(sprintf("  %s = '%s'\n", col_name, val))
          return(val)
        }
        cat(sprintf("  %s = 'unknown' (not found)\n", col_name))
        return("unknown")
      }
      
      cat("Getting metadata values:\n")
      metadata_info <- tibble(
        plot_code        = plot_info$plot_code,
        name             = get_md_val(md, "name"),
        honorific_title  = get_md_val(md, "honorific_title"),
        role             = get_md_val(md, "role"),
        email            = get_md_val(md, "email"),
        telephone        = get_md_val(md, "telephone"),
        url              = get_md_val(md, "url"),
        organization     = get_md_val(md, "organization"),
        street_address   = get_md_val(md, "street_address"),
        postal_code      = get_md_val(md, "postal_code"),
        locality         = get_md_val(md, "locality"),
        country          = get_md_val(md, "country")
      )
      cat("\n")
      
      
      # Procedures table (always use reference units)
      procedures_info <- tibble(
        soil_property          = soil.properties,
        property_phys_chem_id  = NA_character_,
        procedure_phys_chem_id = NA_character_,
        unit_of_measure_id     = NA_character_
      )
      
      for (i in seq_along(soil.properties)) {
        p <- soil.properties[i]
        pid <- safe_id(p)
        
        prop_val <- input[[paste0("map_property__", pid)]]
        proc_val <- input[[paste0("map_procedure__", pid)]]
        
        # Get the REFERENCE unit
        ref_unit <- get_reference_unit(prop_val, proc_val, ref)
        
        procedures_info$property_phys_chem_id[i]  <- prop_val
        procedures_info$procedure_phys_chem_id[i] <- proc_val
        procedures_info$unit_of_measure_id[i]     <- ref_unit
      }
      
      # Template
      tp <- template_path()
      validate(need(file.exists(tp), paste("Template not found:", tp)))
      wb <- loadWorkbook(tp)
      
      required_sheets <- c("Plot Data","Profile Data","Element Data","Specimen Data","Original Data","Procedures","Metadata")
      missing <- setdiff(required_sheets, names(wb))
      validate(need(length(missing) == 0,
                    paste("Template missing sheets:", paste(missing, collapse = ", "))))
      
      #clear_region(wb, "Plot Data", rows = 3:100, cols = 1:100)
      writeData(wb, "Plot Data", x = plot_info, startRow = 3, startCol = 1, colNames = FALSE, rowNames = FALSE)
      date_style <- openxlsx::createStyle(numFmt = "yyyy-mm-dd")
      
      date_col <- 7  # <-- adjust if your template/order differs
      n <- nrow(plot_info)
      
      openxlsx::addStyle(
        wb, "Plot Data",
        style = date_style,
        rows = 3:(3 + n - 1),
        cols = date_col,
        gridExpand = TRUE,
        stack = TRUE
      )
      
      #clear_region(wb, "Profile Data", rows = 3:100, cols = 1:100)
      writeData(wb, "Profile Data", x = profile_info, startRow = 3, startCol = 1, colNames = FALSE, rowNames = FALSE)
      
      #clear_region(wb, "Element Data", rows = 3:100, cols = 1:100)
      writeData(wb, "Element Data", x = element_info, startRow = 3, startCol = 1, colNames = FALSE, rowNames = FALSE)
      
      #clear_region(wb, "Specimen Data", rows = 1:100, cols = 1:100)
      writeData(wb, "Specimen Data", x = specimen_info, startRow = 1, startCol = 1, colNames = TRUE, rowNames = FALSE)
      
      #clear_region(wb, "Original Data", rows = 1:100, cols = 1:100)
      writeData(wb, "Original Data", x = original_data_info, startRow = 1, startCol = 1, colNames = TRUE, rowNames = FALSE)
      
      clear_region(wb, "Procedures", rows = 2:20, cols = 1:10)
      writeData(wb, "Procedures", x = procedures_info, startRow = 2, startCol = 1, colNames = FALSE, rowNames = FALSE)
      
      #clear_region(wb, "Metadata", rows = 3:100, cols = 1:100)
      writeData(wb, "Metadata", x = metadata_info, startRow = 3, startCol = 1, colNames = FALSE, rowNames = FALSE)
      
      cat("=== BUILD WORKBOOK COMPLETE ===\n\n")
      wb
      
    }, error = function(e) {
      cat("\n!!!!! BUILD WORKBOOK ERROR !!!!!\n")
      cat(sprintf("Error class: %s\n", class(e)))
      cat(sprintf("Error message: %s\n", e$message))
      cat("\nFull error object:\n")
      print(e)
      cat("\nCall stack:\n")
      print(sys.calls())
      cat("\n")
      
      # Re-throw with more informative message
      stop(sprintf("Build workbook failed: %s", e$message))
    })
  })
  
  # ---- download ----

  output$download_xlsx <- downloadHandler(
    filename = function() paste0("my_Glosis_data_", Sys.Date(), ".xlsx"),
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    content = function(file) {
      tryCatch({
        withProgress(message = "Generating XLSX…", value = 0, {
          incProgress(0.2, detail = "Building workbook")
          tmp <- tempfile(fileext = ".xlsx")
          wb <- isolate(build_workbook())
          incProgress(0.8, detail = "Writing file")
          openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
          incProgress(1, detail = "Done")
        })
      }, error = function(e) {
        error_msg <- paste("XLSX generation failed:", e$message)
        cat("Full error:\n")
        print(e)
        cat("\nTraceback:\n")
        print(traceback())
        showNotification(error_msg, type = "error", duration = 30)
        stop(e)
      })
    }
  )
}
shinyApp(ui, server)
