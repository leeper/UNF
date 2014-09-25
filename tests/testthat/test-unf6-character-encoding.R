context("UNFv6: Character Encoding")

test_that("Characters with known encoding coerced to UTF-8", {
    x <- c("æ","å","ø")
    d <- data.frame(x=x, stringsAsFactors=FALSE)
    d_latin1 <- data.frame(x = sapply(d$x, "Encoding<-", "latin1"), stringsAsFactors=FALSE)
    d_utf8 <- data.frame(x = sapply(d$x, "Encoding<-", "UTF-8"), stringsAsFactors=FALSE)
    expect_equal(unf(d), unf(d_utf8))
    expect_equal(unf(d), unf(d_latin1))
    rm("d")
    rm("d_latin1")
    rm("d_utf8")
})

test_that("ASCII characters unaffected by encoding during file I/O", {
    d <- data.frame(x = c("a","b","c"))
    write.csv(d, file="temp.csv", row.names=FALSE, fileEncoding="UTF-8")
    d2 <- read.csv("temp.csv", fileEncoding="latin1")
    expect_equal(unf(d), unf(d2))
    unlink("temp.csv")
    rm("d")
    rm("d2")
})

test_that("non-ASCII characters unaffected by correct UTF-8 encoding during file I/O", {
    d <- data.frame(x = c("æ","å","ø"))
    write.csv(d, file="temp.csv", row.names=FALSE, fileEncoding="UTF-8")
    d2 <- read.csv("temp.csv", fileEncoding="UTF-8", stringsAsFactors=FALSE)
    expect_equal(unf(d), unf(d2))
    unlink("temp.csv")
    rm("d2")

    write.csv(d, file="temp.csv", row.names=FALSE, fileEncoding="latin1")
    d2 <- read.csv("temp.csv", fileEncoding="latin1", stringsAsFactors=FALSE)
    expect_equal(unf(d), unf(d2))
    unlink("temp.csv")
    rm("d")
    rm("d2")
})

test_that("non-ASCII characters affected by incorrect encoding during file I/O", {
    d <- data.frame(x = c("æ","å","ø"))
    write.csv(d, file="temp.csv", row.names=FALSE, fileEncoding="UTF-8")
    d2 <- read.csv("temp.csv", fileEncoding="latin1", stringsAsFactors=FALSE)
    expect_false(identical(unf(d), unf(d2)))
    unlink("temp.csv")
    rm("d")
    rm("d2")
})
