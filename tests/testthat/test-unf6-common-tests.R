context("UNFv6: UNF Common Tests")

test_that("Numerics", {
    expect_equal(unf6(0)$unf, 
                 unf6("+0.e+")$unf, 
                 "YUvj33xEHnzirIHQyZaHow==")
    expect_equal(unf6(1)$unf, 
                 unf6("+1.e+")$unf, 
                 "tv3XYCv524AfmlFyVOhuZg==")
    expect_equal(unf6(3.1415)$unf, 
                 unf6("+3.1415e+")$unf, 
                 "vOSZmXXXpKfQcqZ0Cuu5/w==")
    expect_equal(unf6(-300)$unf, 
                 unf6("-3.e+2")$unf, 
                 "ZTXyg54FoMfRDWZl6oWmFQ==")
    expect_equal(unf6(0.00073)$unf, 
                 unf6("+7.3e-4")$unf, 
                 "qhw3qzg3fEK0NNfoVxk4jQ==")
    expect_equal(unf6(0.0123456789012345)$unf, 
                 unf6("+1.234568e-2")$unf, 
                 "tbCwhb/PKbElzXByo6hLAw==")
})

test_that("Character strings", {
    expect_equal(unf6("A character String")$unf, "FYqU7uBl885eHMbpco1ooA==", label="Short character strings")
    expect_equal(unf6("A quite long character string, so long that the number of characters in it happens to be more than the default cutoff limit of 128.")$unf,
                 unf6("A quite long character string, so long that the number of characters in it happens to be more than the default cutoff limit of 1")$unf,
                 "/BoSlfcIlsmQ+GHu5gxwEw==", label = "Long character string")
    x1 <- "på Færøerne"
    x2 <- x3 <- "p\303\245 F\303\246r\303\270erne"
    Encoding(x3) <- "UTF-8"
    expect_equal(unf6(x1)$unf, 
                 unf6(x3)$unf,
                 "KHM6bKVaVaxWDDsmyerfDA==", label = "Non-ASCII characters in string")
    expect_equal(unf6("")$unf, "ECtRuXZaVqPomffPDuOOUg==", label = "Empty character string")
    expect_equal(unf6(NA)$unf, "cJ6AyISHokEeHuTfufIqhg==", label = "Missing value")
})

test_that("Datetimes", {
    expect_equal(unf6("2014-09-25T20:47:18")$unf, "JHTL641Otsj3glsU6X3QUQ==")
    expect_equal(unf6("2014-09-26T00:47:18Z")$unf, "wdVxpMSI5njSxL9vU9Iuxw==")
})

test_that("Boolean/logical values", {
    expect_equal(unf6(TRUE)$unf, unf6("+1.e+")$unf, "tv3XYCv524AfmlFyVOhuZg==")
    expect_equal(unf6(FALSE)$unf, unf6("+0.e+")$unf, "YUvj33xEHnzirIHQyZaHow==")
})
