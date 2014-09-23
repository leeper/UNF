context("UNFv6: Characters")
test_that("Examples from original R package documentation", {
    expect_equal(unf(c('test','1','2','3'))$unf, "fH4NJMYkaAJ16OWMEE+zpQ==")
})

test_that("Tails of long characters irrelevant", {
})
test_that("Tails of long characters optionally relevant", {
})
test_that("Numerics stored as character not same as numeric", {
    expect_false(unf(c('1','2','3'))$unf == unf(1:3)$unf)
})
