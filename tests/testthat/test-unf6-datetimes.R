context("UNFv6: Dates")
test_that("Partial dates (year-only) supported", {})
test_that("Partial dates (year-month) supported", {})

context("UNFv6: Datetimes")
test_that("Examples from v6 specification",{
    expect_equal(unf6("2014-08-22T16:51:05Z"), 
                 unf6(strptime("2014-08-22T16:51:05Z", "%FT%H:%M:%SZ", tz="UTC"), timezone="UTC"))
    #expect_equal(unf6("2012-06-10T14:29:00"), 
    #             unf6(strptime("2012-06-10T14:29:00", "%FT%H:%M:%S", tz="UTC"), timezone=""))
})

test_that("UNFs differ by timezone", {
    expect_false(identical(unf6(strptime("2014-08-22T16:51:05Z", "%FT%H:%M:%SZ", tz="UTC"), timezone="UTC"), 
                 unf6(strptime("2014-08-22T16:51:05Z", "%FT%H:%M:%SZ", tz="UTC"), timezone="US/Eastern")))
    expect_false(identical(unf6(strptime("2014-08-22T16:51:05Z", "%FT%H:%M:%SZ", tz="UTC"), timezone="UTC"), 
                 unf6(strptime("2014-08-22T16:51:05Z", "%FT%H:%M:%SZ", tz="US/Eastern"), timezone="UTC")))
    
})

test_that("Correct UNF for UTC timezones", {})

test_that("Tests of `decimal_seconds` rounding parameter", {})