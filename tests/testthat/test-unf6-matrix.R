context("UNFv6: Matrices")
test_that("Matrix treated as data.frame", {
    expect_equal(unf(matrix(1:6, nrow=3)),
                 unf(as.data.frame(matrix(1:6, nrow=3))))
})
test_that("Column order irrelevant", {
    expect_equal(unf(matrix(1:6, nrow=3))$unf,
                 unf(matrix(1:6, nrow=3)[,2:1])$unf)
})
test_that("Row order relevant", {
    expect_false(identical(unf(matrix(1:6, nrow=3))$unf,
                           unf(matrix(1:6, nrow=3)[3:1,])$unf))
})
test_that("Subsetting relevant", {
    expect_false(identical(unf(matrix(1:6, nrow=3))$unf,
                           unf(matrix(1:6, nrow=3)[1:2,])$unf))
})
