context("UNFv6: time-series")

test_that("ts vectors converted to character", {
    expect_true(as.unfvector(ts(1)) == "+1.e+")
    expect_true(identical(unf(c(1:5))$unf, unf(ts(1:5))$unf))
})
