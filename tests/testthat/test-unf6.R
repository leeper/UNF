context("UNFv6: Numerics")
test_that("Irrelevant rounding irrelevant", {})
test_that("Relevant rounding relevant", {})



context("UNFv6: Non-finites")
test_that("Nonfinites handled per specification", {})
test_that("Nonfinites optionally treated as missing", {})


context("UNFv6: Characters")
test_that("Tails of long characters irrelevant", {})
test_that("Tails of long characters optionally relevant", {})
test_that("Numerics stored as character not same as numeric", {})


context("UNFv6: Factors")
test_that("Factors treated as character", {})
test_that("Factors can be treated as integer", {})


context("UNFv6: Logicals")
test_that("TRUE treated as numeric 1", {})
test_that("FALSE treated as numeric 0", {})


context("UNFv6: Raw vectors")


context("UNFv6: Complex vectors")
test_that("Complex vectors treated as character", {})

context("UNFv6: Missing Values")


context("UNFv6: Dates")
test_that("Partial dates (year-only) supported", {})
test_that("Partial dates (year-month) supported", {})

context("UNFv6: Datetimes")
test_that("UNFs differ by timezone", {})
test_that("Correct UNF for UTC timezones", {})


context("UNFv6: Dataframes")
test_that("Variable order irrelevant", {})
test_that("Sort order relevant", {})
test_that("Subsetting relevant", {})

context("UNFv6: Lists")
test_that("Variable order irrelevant", {})
test_that("Sort order relevant", {})
test_that("Subsetting relevant", {})

context("UNFv6: Matrices")
test_that("Column order irrelevant", {})
test_that("Row order relevant", {})
test_that("Subsetting relevant", {})

