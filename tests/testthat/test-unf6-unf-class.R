context("UNFv6: UNF Class Object")
test_that("Object is 'unf' class", {
    expect_equal(class(unf6(1)), "unf")
})
test_that("Object slots", {
    expect_equal(names(unf6(1)), c("unf","hash","unflong","formatted"), label = "object names correct")
    expect_equal(class(unf6(1)$unf), "character", label = "unf is character")
    expect_equal(class(unf6(1)$hash), "raw", label = "hash is raw")
    expect_equal(class(unf6(1)$unflong), "character", label = "unflong is character")
    expect_equal(class(unf6(1)$formatted), "character", label = "formatted is character")
})
test_that("Attributes", {
    expect_equal(attr(unf6(1), "class"), "unf", label = "class")
    expect_equal(attr(unf6(1), "version"), "unf", label = "version")
    expect_equal(attr(unf6(1), "digits"), "unf", label = "digits")
    expect_equal(attr(unf6(1), "characters"), "unf", label = "characters")
    expect_equal(attr(unf6(1), "truncation"), "unf", label = "truncation")
})
