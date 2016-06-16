context("UNFv6: UNF equivalence operator")

test_that("Equivalence operator class and printing", {
    expect_equal(class(unf(1) %unf% unf(1)), "UNFtest")
    expect_equal(class(print(unf(1) %unf% unf(1))), "UNFtest")
})

test_that("Compare two UNFs", {
    expect_true((unf(1) %unf% unf(1))$unfmatch)
    expect_true(!(unf(1) %unf% unf(2))$unfmatch)
})

test_that("Compare two data.frames", {
    expect_true((data.frame(a = 1:3) %unf% data.frame(a = 1:3))$unfmatch)
    expect_true(!(data.frame(a = 3:1) %unf% data.frame(a = 1:3))$unfmatch)
})

test_that("Compare data.frames to unf", {
    expect_true((unf(data.frame(a = 1:3)) %unf% data.frame(a = 1:3))$unfmatch)
    expect_true(!(unf(data.frame(a = 1:3)) %unf% data.frame(a = 3:1))$unfmatch)
})

test_that("Compare data.frames to unf hash", {
    expect_true((unf(data.frame(1:3,4:6,7:9), version = 6) %unf% "UNF6:ukDZSJXck7fn4SlPJMPFTQ==")$unfmatch)
    expect_true(!(unf(data.frame(1:3,4:6,9:7), version = 6) %unf% "UNF6:ukDZSJXck7fn4SlPJMPFTQ==")$unfmatch)
})
