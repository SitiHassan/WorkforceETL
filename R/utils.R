#' Extract a provider name from a filename
#'
#' Searches a filename for a recognised NHS provider abbreviation, provider
#' name, or alternative provider code and returns the corresponding standard
#' provider name.
#'
#' The function first checks for direct provider names such as `"UHB"`,
#' `"BSMHFT"`, and `"Walsall"`. If no direct provider name is found, it checks
#' for alternative provider codes such as `"RRK"`, `"RXT"`, and `"RBK"` and
#' maps them to the corresponding standard provider name.
#'
#' Matching is case-insensitive. When more than one recognised provider is
#' present, the first direct provider name found is returned. Direct provider
#' names take precedence over alternative provider codes.
#'
#' @param filename A character string containing the filename to search.
#'
#' @return A single character string containing the standard provider name.
#'   If no recognised provider name or code is found, the function returns
#'   `"No provider abbreviation found in the filename"`.
#'
#' @details
#' The following alternative codes are mapped:
#'
#' \describe{
#'   \item{RRK}{UHB}
#'   \item{RXT}{BSMHFT}
#'   \item{RRJ}{ROH}
#'   \item{RQ3}{BWCH}
#'   \item{RYW}{BCHC}
#'   \item{TAJ}{BCHFT}
#'   \item{RNA}{DGFT}
#'   \item{DGH}{DGFT}
#'   \item{RL4}{RWT}
#'   \item{RXK}{SWB}
#'   \item{RBK}{Walsall}
#'   \item{RYA}{WMAS}
#' }
#'
#' @examples
#' extract_provider_name("UHB_Workforce_March_2026.xlsx")
#' extract_provider_name("workforce_RRK_202603.csv")
#' extract_provider_name("Monthly_Report_Walsall.xlsx")
#' extract_provider_name("unknown_provider_report.xlsx")
#'
#' @export
extract_provider_name <- function(filename) {
  
  # Define main provider names
  group_1 <- c(
    "UHB",
    "BSMHFT",
    "ROH",
    "BWCH",
    "BCHC",
    "WMAS",
    "DGFT",
    "SWB",
    "BCHFT",
    "Walsall",
    "RWT"
  )
  
  # Define alternative provider codes
  group_2 <- c(
    "RRK",  # UHB
    "RXT",  # BSMHFT
    "RRJ",  # ROH
    "RQ3",  # BWCH
    "RYW",  # BCHC
    "TAJ",  # BCHFT
    "RNA",  # DGFT
    "DGH",  # DGFT
    "RL4",  # RWT
    "RXK",  # SWB
    "RBK",  # Walsall
    "RYA"   # WMAS
  )
  
  # Mapping order must align with group_2
  group_2_mapping <- c(
    "UHB",
    "BSMHFT",
    "ROH",
    "BWCH",
    "BCHC",
    "BCHFT",
    "DGFT",
    "DGFT",
    "RWT",
    "SWB",
    "Walsall",
    "WMAS"
  )
  
  all_providers <- c(group_1, group_2)
  
  matches <- unlist(
    regmatches(
      filename,
      gregexpr(
        paste(all_providers, collapse = "|"),
        filename,
        ignore.case = TRUE
      )
    )
  )
  
  matches <- toupper(matches)
  
  group_1_upper <- toupper(group_1)
  group_2_upper <- toupper(group_2)
  
  # Check direct provider names first
  group_1_matches <- matches[matches %in% group_1_upper]
  
  if (length(group_1_matches) > 0) {
    return(
      group_1[
        match(group_1_matches[1], group_1_upper)
      ]
    )
  }
  
  # Check mapped provider codes
  group_2_matches <- matches[matches %in% group_2_upper]
  
  if (length(group_2_matches) > 0) {
    return(
      group_2_mapping[
        match(group_2_matches[1], group_2_upper)
      ]
    )
  }
  
  return("No provider abbreviation found in the filename")
}

#' Process an individual Provider Workforce Return (PWR) worksheet
#'
#' Extracts and reshapes data from a single PWR worksheet. The function
#' identifies the worksheet header using the first occurrence of `"Subscode"`, 
#' locates the columns labelled `"Month 12`, and converts the monthly values from 
#' wide to long format.
#' 
#' The twelve month columns are assigned month-end dates covering the financial year
#' from April of `start_year` to March of `end_year`. 
#' @param data A data frame or tibble containing the raw worksheet data.
#' @param provider A single character string containing the standard provider name. 
#' This can be produced by [extract_provider_name()].
#' @param sheet_name A single character string containing the name of the PWR worksheet being processed.
#' @param start_year A four-digit integer representing the first calendar year of the financial year. 
#' E.g., Use `2026` for financial year 2026/27.
#' @param end_year A four-digit integer representing the second calendar year of the financial year.
#' E.g., Use `2027` for financial year 2026/27. 
#'
#' @return A tibble in long format with the following columns: 
#' \describe{ 
#' \item{Provider}{The provider name or abbreviation supplied in `provider`.} 
#' \item{Sheet}{The worksheet name supplied in `sheet_name`.} 
#' \item{Maincode}{The workforce metric or subcode extracted from the sheet.} 
#' \item{Date}{The month-end date associated with the monthly value.} 
#' \item{Yearmonth}{The month represented as an integer in `YYYYMM` format.} 
#' \item{Value}{The workforce value converted to numeric.}
#'  }
#' @export
process_workforce_sheet <- function(data, provider, sheet_name, start_year, end_year) {
  
  # Identify all "Subcode" positions across the sheet
  subcode_position <- which(data == "Subcode", arr.ind = TRUE)
  
  if (nrow(subcode_position) == 0) {
    stop(paste0("Could not find 'Subcode' in sheet: ", sheet_name))
  }
  
  # Get the first position 
  subcode_position <- subcode_position[1, ]
  
  subcode_row <- subcode_position["row"]
  subcode_col <- subcode_position["col"]
  
  # Create new column name
  new_names <- trimws(as.character(unlist(data[subcode_row, ])))
  
  # Tidy column name
  new_names[new_names == "" | is.na(new_names)] <- 
    paste0("X", which(new_names == "" | is.na(new_names)))
  
  new_names <- make.unique(make.names(new_names))
  
  names(data) <- new_names
  
  # Identify "Month 1" and "Month 12" positions
  month_1_position <- which(data == "Month 1", arr.ind = TRUE)
  month_12_position <- which(data == "Month 12", arr.ind = TRUE)
  
  if (nrow(month_1_position) == 0 | nrow(month_12_position) == 0) {
    stop(
      paste0(
        "Could not find Month 1 or Month 12 in sheet: ",
        sheet_name
      )
    )
  }
  
  month_1_col <- month_1_position[1, "col"]
  month_12_col <- month_12_position[1, "col"]
  
  # Remove rows above header and keep relevant columns
  data <- data[
    (subcode_row + 1):nrow(data), # Below "Subcode" 
    c(month_1_col:month_12_col, subcode_col) # First 12 months & "Subcode"
  ]
  
  # Generate end dates
  month_names <- format(
    seq(
      as.Date(paste0(start_year, "-05-01")),
      as.Date(paste0(end_year, "-04-01")),
      by = "month"
    ) - 1,
    "%d/%m/%Y"
  )
  
  # Rename the first 12-month columns & Maincode
  names(data)[1:12] <- month_names
  names(data)[13] <- "Maincode"
  
  # Extract data
  data <- data |> 
    filter(
      !is.na(Maincode),
      !Maincode %in% c("Subcode", "Maincode")
    ) |>
    pivot_longer(
      cols = all_of(1:12),
      names_to = "Date",
      values_to = "Value"
    ) |> 
    mutate(
      Provider = provider,
      Sheet = sheet_name,
      Date = as.Date(Date, format = "%d/%m/%Y"),
      Yearmonth = as.integer(format(Date, "%Y%m")),
      Value = as.numeric(Value)
    ) |> 
    select(Provider, Sheet, Maincode, Date, Yearmonth, Value)
  
  return(data)
}

#' Process Provider Workforce Return (PWR) files
#'
#' Reads all valid Provider Workforce Return (PWR) Excel workbooks from the
#' specified input directory, extracts data from supported workforce sheets,
#' combines the results into a single dataset, saves the output as a CSV file,
#' and moves successfully processed files to an archive directory.
#'
#' @param input_file_path Character string specifying the directory containing
#'   the PWR Excel files to be processed.
#' @param output_file_path Character string specifying the file path where the
#'   combined extracted PWR data will be saved as a CSV file.
#' @param processed_file_path Character string specifying the directory where
#'   processed PWR files will be moved after successful processing.
#' @param latest_month Integer representing the latest reporting month to
#'   retain in the output dataset, in `YYYYMM` format. Records with
#'   `Yearmonth` values greater than this will be excluded.
#' @param start_year Four-digit integer representing the first calendar year of
#'   the financial year. For example, use `2026` for the 2026/27 financial
#'   year. This parameter is passed to [process_workforce_sheet()].
#' @param end_year Four-digit integer representing the second calendar year of
#'   the financial year. For example, use `2027` for the 2026/27 financial
#'   year. This parameter is passed to [process_workforce_sheet()].
#'
#' @return A tibble in long format containing the extracted workforce data with
#'   the following columns:
#' \describe{
#'   \item{Maincode}{Workforce metric code or subcode extracted from the source sheet.}
#'   \item{Yearmonth}{Reporting period in `YYYYMM` format.}
#'   \item{Provider}{Provider name or abbreviation extracted from the workbook filename.}
#'   \item{Value}{Workforce measure converted to a numeric value.}
#'   \item{LoadDate}{Date on which the data was processed and loaded.}
#' }
#'
#' @details
#' The function searches for Excel files (`.xlsx`, `.xlsm`, and `.xls`) in the
#' input directory and processes only sheets matching the supported PWR sheet
#' types. Temporary Excel files beginning with `~$` are ignored. Any errors
#' encountered while processing individual sheets are logged as warnings and do
#' not stop processing of the remaining files.
#'
#' @export
process_workforce_files <- function(
    input_file_path,
    output_file_path,
    processed_file_path,
    latest_month,
    start_year,
    end_year
) {
  
  # Check input directory exists
  if (!fs::dir_exists(input_file_path)) {
    warning("Input directory does not exist: ", input_file_path)
    return(NULL)
  }
  
  # Create output and processed folders if not already present
  fs::dir_create(
    dirname(output_file_path),
    recurse = TRUE
  )
  
  fs::dir_create(
    processed_file_path,
    recurse = TRUE
  )
  
  # Find Excel files
  excel_files <- list.files(
    path = input_file_path,
    pattern = "\\.(xlsx|xlsm|xls)$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  # Remove temporary Excel files
  excel_files <- excel_files[
    !grepl("^~\\$", basename(excel_files))
  ]
  
  # Skip if there are no files
  if (length(excel_files) == 0) {
    message("No Excel files found in: ", input_file_path)
    return(NULL)
  }
  
  # A list of PWR sheets to be processed
  target_sheet_patterns <- c(
    "WTE",
    "KPI",
    "International Recruitment",
    "AHP",
    "Maternity",
    "HCSW",
    "PNA",
    "PMA",
    "Time to hire",
    "Consultant Job Plans",
    "ETOC",
    "MTP"
  )
  
  # Initialise a list to store all data across PWR sheets and providers
  all_provider_data <- list()
  
  for (file_path in excel_files) {
    
    file_name <- basename(file_path)
    
    message("Processing file: ", file_name)
    
    # Get the provider name from the raw filename
    provider <- extract_provider_name(file_name)
    
    sheet_names <- readxl::excel_sheets(file_path)
    
    sheets_to_process <- sheet_names[
      grepl(
        paste(target_sheet_patterns, collapse = "|"),
        sheet_names,
        ignore.case = TRUE
      )
    ]
    
    if (length(sheets_to_process) == 0) {
      warning("No matching sheets found in: ", file_name)
      next
    }
    
    provider_sheet_data <- list()
    
    for (sheet in sheets_to_process) {
      
      message("Processing sheet: ", sheet, " in ", file_name)
      
      output <- tryCatch(
        {
          data <- readxl::read_excel(
            file_path,
            sheet = sheet
          )
          
          process_workforce_sheet(
            data = data,
            provider = provider,
            sheet_name = sheet,
            start_year = start_year,
            end_year = end_year
          )
        },
        error = function(e) {
          warning(
            "Error processing sheet ",
            sheet,
            " in file ",
            file_name,
            ": ",
            conditionMessage(e)
          )
          
          NULL
        }
      )
      
      if (!is.null(output)) {
        provider_sheet_data[[sheet]] <- output
      }
    }
    
    if (length(provider_sheet_data) > 0) {
      all_provider_data[[length(all_provider_data) + 1]] <-
        dplyr::bind_rows(provider_sheet_data)
    }
  }
  
  if (length(all_provider_data) == 0) {
    warning("No sheets were successfully processed.")
    return(NULL)
  }
  
  df_all <- dplyr::bind_rows(all_provider_data) |>
    dplyr::mutate(
      LoadDate = Sys.Date()
    ) |>
    dplyr::filter(
      Yearmonth <= latest_month
    )
  
  if (nrow(df_all) == 0) {
    warning("No usable data was extracted.")
    return(NULL)
  }
  
  # Save combined output
  readr::write_csv(
    df_all,
    output_file
  )
  
  # Move processed files
  destination_files <- file.path(
    processed_directory,
    basename(excel_files)
  )
  
  fs::file_move(
    excel_files,
    destination_files
  )
  
  return(df_all)
}