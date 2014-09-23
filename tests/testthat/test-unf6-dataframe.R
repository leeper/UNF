context("UNFv6: Dataframes")
test_that("Variable order irrelevant", {
    expect_equal(unf(data.frame(1:3,4:6,7:9))$unf,
                 unf(data.frame(7:9,1:3,4:6))$unf,
                 "ukDZSJXck7fn4SlPJMPFTQ==")
})
test_that("Variable names irrelevant", {
    expect_equal(unf(data.frame(x=1:3,y=4:6,z=7:9))$unf,
                 unf(data.frame(z=1:3,y=4:6,x=7:9))$unf)
})
test_that("Sort order relevant", {
    expect_false(identical(unf(iris), unf(iris[order(iris$Sepal.Length),])))
})
test_that("Subsetting relevant", {
    expect_false(identical(unf(iris), unf(head(iris))))
})

test_that("UNF for one-variable dataframe is univariate UNF", {
    expect_equal(unf(data.frame(x=-3:3)), unf(-3:3))
})
