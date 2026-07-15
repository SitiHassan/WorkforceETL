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