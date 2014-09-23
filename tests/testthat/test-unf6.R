context("UNFv6: Numerics")
test_that("Original documentation examples", {
    expect_equal(unf(1:20)$unf, "/FIOZM/29oC3TK/IE52m2A==")
    expect_equal(unf(-3:3)$unf, "7FsSuKWGIp6i7b0NFjckZQ==") # w/ dvn_zero: "pwzm1tdPaqypPWRWDeW6Jw=="
})

test_that("Irrelevant rounding irrelevant", {
    expect_equal(unf6(1)$unf, unf6(1+1e-7)$unf)
    expect_equal(unf6(1, digits = 6)$unf, unf6(1+1e-6, digits=6)$unf)
})
test_that("Relevant rounding relevant", {
    expect_false(unf6(1)$unf == unf6(1+1e-6)$unf)
})



context("UNFv6: Non-finites")
test_that("Nonfinites handled per specification", {})
test_that("Nonfinites optionally treated as missing", {})


context("UNFv6: Characters")
test_that("Original documentation examples", {
    expect_equal(unf(c('test','1','2','3'))$unf, "fH4NJMYkaAJ16OWMEE+zpQ==")
})




test_that("Tails of long characters irrelevant", {})
test_that("Tails of long characters optionally relevant", {})
test_that("Numerics stored as character not same as numeric", {})


context("UNFv6: Factors")
test_that("Factors treated as character", {
    expect_equal(unf(c('test','1','2','3'))$unf, unf(factor(c('test','1','2','3')))$unf)
})
test_that("Factors can be treated as integer", {})


context("UNFv6: Logicals")
test_that("TRUE treated as numeric 1", {
    expect_equal(unf(1)$unf, unf(TRUE)$unf)
})
test_that("FALSE treated as numeric 0", {
    expect_equal(unf(0)$unf, unf(FALSE)$unf)
})


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
test_that("Variable order irrelevant", {
    expect_equal(unf(data.frame(1:3,4:6,7:9))$unf,
                 unf(data.frame(7:9,1:3,4:6))$unf,
                 "ukDZSJXck7fn4SlPJMPFTQ==")
})
test_that("Sort order relevant", {})
test_that("Subsetting relevant", {})

context("UNFv6: Lists")
test_that("Variable order irrelevant", {
    expect_equal(unf(list(1:3,4:6,7:9))$unf,
                 unf(list(7:9,1:3,4:6))$unf,
                 "ukDZSJXck7fn4SlPJMPFTQ==")
})
test_that("Sort order relevant", {})
test_that("Subsetting relevant", {})
test_that("Dataframes and lists equivalent", {
    expect_equal(unf(data.frame(1:3,4:6,7:9))$unf,
                 unf(list(1:3,4:6,7:9))$unf,
                 "ukDZSJXck7fn4SlPJMPFTQ==")
})

context("UNFv6: Matrices")
test_that("Column order irrelevant", {})
test_that("Row order relevant", {})
test_that("Subsetting relevant", {})


context("UNFv6: Signature Printing")
test_that("Version is printed", {})
test_that("Digits are printed", {})
test_that("Characters are printed", {})
test_that("Truncation is printed", {})


