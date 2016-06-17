context("UNFv6: Raw vectors")

test_that("Raw vectors converted to character", {
    expect_true(as.unfvector(as.raw(1), raw_as_character = TRUE) == "01")
    expect_true(as.unfvector(as.raw(1), raw_as_character = FALSE) == "AAE=")
    expect_false(identical(unf(as.raw(1:5), raw_as_character = TRUE)$unf, 
                           unf(as.raw(1:5), raw_as_character = FALSE)$unf))
})
