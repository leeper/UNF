context("UNFv6: AsIs")

test_that("AsIs vectors converted to character", {
    expect_true(as.unfvector(I(1)) == "1")
    expect_true(identical(unf(I(1))$unf, unf("1")$unf))
})
