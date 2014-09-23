context("UNFv6: Logicals")
test_that("Logical values treated as numeric 0/1s", {
    expect_equal(unf(1), unf(TRUE), label = "TRUE treated as numeric 1")
    expect_equal(unf(0), unf(FALSE), label = "FALSE treated as numeric 0")
})
