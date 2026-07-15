source(
  testthat::test_path("..", "..", "R", "utils.R")
)

testthat::test_that(
  "extract_provider_name extracts direct provider names from a filename",
  {
    expect_equal(
      extract_provider_name("RRK_Workforce.xlsx"),
      "UHB"
    )
  }
)