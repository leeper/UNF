# Universal Numeric Fingerprint #

[![Build Status](https://travis-ci.org/leeper/UNF.png?branch=master)](https://travis-ci.org/leeper/UNF)

UNF is a cryptographic hash or signature that can be used to uniquely identify (a version of) a rectangular dataset, or a subset thereof. UNF can be used, in tandem with a DOI or Handle, to form a persistent citation to a versioned dataset. A UNF signature is printed in the following form:

```
UNF:[UNF version]:[UNF header options]:[UNF hash]
```

This allows a data consumer to quickly, easily, and definitively verify an in-hand data file against a data citation or to test for the equality of two datasets, regardless of their variable order or file format. UNF is used by [The Dataverse Network](http://www.thedata.org) archiving software for data citation (making the UNF package a logical companion to the [dvn](http://cran.r-project.org/web/packages/dvn/) package). This package implements UNF versions 3 and up (current version is 6). Some details on the UNF algorithm and the R implementation thereof are included in a package vignette ("The UNF Algorithm") and details on use of UNF in data citation is available in another vignette ("Data Citation with UNF").

Please report any mismatches between this implementation and any other implementation (including Dataverse's) on [the issues page](https://github.com/leeper/UNF/issues)!

## Why UNFs? ##

While file checksums are a common strategy for verifying a file (e.g., md5 sums are available for validating R packages), they are not well-suited to being used as global signatures for a dataset. A UNF differs from an ordinary file checksum in several important ways:

1. *UNFs are format independent.* The UNF for a dataset will be the same regardless of whether the data is saved as a R binary format, SAS formatted file, Stata formatted file, etc., but file checksums will differ. The UNF is also independent of variable arrangement and naming, which can be unintentionally changed during file reading.

    
    ```r
    library("digest")
    library("UNF")
    write.csv(iris, file = "iris.csv", row.names = FALSE)
    iris2 <- read.csv("iris.csv")
    identical(iris, iris2)
    ```
    
    ```
    ## [1] TRUE
    ```
    
    ```r
    identical(digest(iris, "md5"), digest(iris2, "md5"))
    ```
    
    ```
    ## [1] FALSE
    ```
    
    ```r
    identical(unf(iris), unf(iris2))
    ```
    
    ```
    ## [1] TRUE
    ```



2. *UNFs are robust to insignificant rounding error.* This important when dealing with floating-point numeric values. A UNF will also be the same if the data differs in non-significant digits, a file checksum not.

    
    ```r
    x1 <- 1:20
    x2 <- x1 + 1e-7
    identical(digest(x1), digest(x2))
    ```
    
    ```
    ## [1] FALSE
    ```
    
    ```r
    identical(unf(x1), unf(x2))
    ```
    
    ```
    ## [1] TRUE
    ```

3. *UNFs detect misinterpretation of the data by statistical software.* If the statistical software misreads the file, the resulting UNF will not match the original, but the file checksums may match. For example, numeric values read as character will produce a different UNF than those values read in as numerics.

    
    ```r
    x1 <- 1:20
    x2 <- as.character(x1)
    identical(unf(x1), unf(x2))
    ```
    
    ```
    ## [1] FALSE
    ```

4. *UNFs are strongly tamper resistant.* Any accidental or intentional changes to data values will change the resulting UNF. Most file checksums and descriptive statistics detect only certain types of changes.

## Package Functionality ##

 - `unf`: The core `unf` function calculates the UNF signature for almost any R object for UNF algorithm versions 3, 4, 4.1, 5, or 6, with options to control the rounding of numeric values, truncation of character strings, and some idiosyncratic details of the UNFv5 algorithm as implemented by Dataverse. `unf` is a wrapper for functions `unf6`, `unf5`, `unf4`, and `unf3`, which calculate vector-level UNF signatures.
 
 - `%unf%`: `%unf%` is a binary operator that can compare two R objects, or an R object against a "UNF" class summary (e.g., as stored in a study metadata record, or returned by `unf`). The function tests whether the objects are identical and, if they are not, provides object- and variable-level UNF comparisons between the two objects, checks for difference in the sorting of the two objects, and (for dataframes) reports indices for rows seemingly present in one object but missing from the other based on row-level hashes of variables common to both dataframes. This can be used both to compare two objects in general (e.g., to see whether two dataframes differ) as well as to debug incongruent UNFs. Two UNFs can differ dramatically due to minor changes like rounding, the deletion of an observation, addition of a variable, etc., so `%unf%` provides a useful tool for looking under the hood at the differences between data objects that might produce different UNF signatures.

    
    ```r
    u <- unf(iris)
    unf(iris) %unf% u
    ```
    
    ```
    ## Objects are identical
    ## 
    ## UNF6:6oVTvlCR+F1W1HTJ/QUmkA== 
    ## 
    ## UNF6:6oVTvlCR+F1W1HTJ/QUmkA==
    ```
    
    ```r
    unf(iris) %unf% unf(iris[,1:3])
    ```
    
    ```
    ## Objects are not identical
    ## 
    ## UNF6:6oVTvlCR+F1W1HTJ/QUmkA== 
    ## Mismatched variables:
    ## Petal.Width: TN39UY6H/vRGv4ARWQTXrw==
    ## Species: Xqh76nYY3z8eTfmL1KfxaQ==
    ## 
    ## UNF6:lEajCAiTPXcxJuP+hr8Kew==
    ```
    
    ```r
    unf(iris) %unf% head(iris[,1:3])
    ```
    
    ```
    ## Objects are not identical
    ## 
    ## UNF6:6oVTvlCR+F1W1HTJ/QUmkA== 
    ## Mismatched variables:
    ## Sepal.Length: FnQvOCZE9tcn64bP78wLag==
    ## Sepal.Width: epaV+rjvURem8qIo0r9LBQ==
    ## Petal.Length: KP6tL8gFSqnG3FLJ887o/g==
    ## Petal.Width: TN39UY6H/vRGv4ARWQTXrw==
    ## Species: Xqh76nYY3z8eTfmL1KfxaQ==
    ## 
    ## UNF6:0Ppu3rquJJrYvjkDePjGbA== 
    ## Mismatched variables:
    ## Sepal.Length: yMtrQJDMuxcSay0afKLz5A==
    ## Sepal.Width: e6etgUxSU/7XccLSwNzHVQ==
    ## Petal.Length: oSk42LS4+joAOdTAr9OChQ==
    ```
