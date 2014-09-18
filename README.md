# Universal Numeric Fingerprint #

[![Build Status](https://travis-ci.org/leeper/UNF.png?branch=master)](https://travis-ci.org/leeper/UNF)

UNF is a cryptographic hash or signature that can be used to uniquely identify (a version of) a dataset, or a subset thereof. UNF is used by the [Dataverse](http://www.thedata.org) archives for data citation and this package can be used to verify a dataset against one listed available in a Dataverse study (e.g., as returned by the [dvn](http://cran.r-project.org/web/packages/dvn/) package), as well as for the citation of any rectangular data file.

This is still a draft and the present output may be unreliable, but the plan is to implement versions 3, 4, 4.1, 5, and 6 (the current version) of the UNF algorithm. Some details on the UNF algorithm and the R implementation thereof are included in a vignette ("The UNF Algorithm").

Please report any mismatches between this implementation and any other implementation (including Dataverse's) on [the issues page](https://github.com/leeper/UNF/issues)!

## Functionality ##

 - `unf`: The core `unf` function calculates the UNF signature for almost any R object for UNF algorithm versions 3, 4, 4.1, 5, or 6, with options to control the rounding of numeric values, truncation of character strings, and some idiosyncratic details of the UNFv5 algorithm as implemented by Dataverse. `unf` is a wrapper for functions `unf6`, `unf5`, `unf4`, and `unf3`, which calculate vector-level UNF signatures.
 
 - `setUNF` and `checkUNF`: `setUNF` adds a UNF signature as a "UNF" attribute to any R object and `checkUNF` can be used to compare the current UNF signature of an object against that stored attribute (e.g., to check whether the object has been modified).

 - `%unf%`: `%unf%` is a binary operator that can compare two R objects, or an R object against a "UNF" class summary (e.g., as stored in a study metadata record, or returned by `unf`). The function tests whether the objects are identical and, if they are not, provides object- and variable-level UNF comparisons between the two objects, checks for difference in the sorting of the two objects, and (for dataframes) reports indices for rows seemingly present in one object but missing from the other based on row-level hashes of variables common to both dataframes. This can be used both to compare two objects in general (e.g., to see whether two dataframes differ) as well as to debug incongruent UNFs. Two UNFs can differ dramatically due to minor changes like rounding, the deletion of an observation, addition of a variable, etc., so `%unf%` provides a useful tool for looking under the hood at the differences between data objects that might produce different UNF signatures.

## Code Examples ##

TBD
