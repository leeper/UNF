# Universal Numeric Fingerprint #

[![Build Status](https://travis-ci.org/leeper/UNF.png?branch=master)](https://travis-ci.org/leeper/UNF)

UNF is a cryptographic hash or signature that can be used to uniquely identify (a version of) a dataset, or a subset thereof. UNF is used by the [Dataverse](http://www.thedata.org) archives for data citation and this package can be used to verify a dataset against one listed available in a Dataverse study (e.g., as returned by the [dvn](http://cran.r-project.org/web/packages/dvn/) package), as well as for the citation of any rectangular data file.

## Why UNFs? ##

While file checksums are a common strategy for verifying a file (e.g., md5 sums are available for validating R packages), they are not well-suited to being used as global signatures for a dataset. A UNF differs from an ordinary file checksum in several important ways:

1. *UNFs are format independent.* The UNF for a dataset will be the same regardless of whether the data is saved as a R binary format, SAS formatted file, Stata formatted file, etc., but file checksums will differ. The UNF is also independent of variable arrangement and naming, which can be unintentionally changed during file reading.

2. *UNFs are robust to insignificant rounding error.* This important when dealing with floating-point numeric values. A UNF will also be the same if the data differs in non-significant digits, a file checksum not.

3. *UNFs detect misinterpretation of the data by statistical software.* If the statistical software misreads the file, the resulting UNF will not match the original, but the file checksums may match. For example, numeric values read as character will produce a different UNF than those values read in as numerics.

4. *UNFs are strongly tamper resistant.* Any accidental or intentional changes to data values will change the resulting UNF. Most file checksums and descriptive statistics detect only certain types of changes.

## The UNF Package ##

This is still a draft and the present output may be unreliable, but the plan is to implement versions 3, 4, 4.1, 5, and 6 (the current version) of the UNF algorithm. **Please report any mismatches between this implementation and any other implementation (including Dataverse's) on [the issues page](https://github.com/leeper/UNF/issues).**

Versions 1 and 2 were available in an earlier version of the UNF package authored by Micah Altman, which was built on custom C libraries, and is included in the version logs on GitHub. That package was orphaned by CRAN in 2009.

The current version of this package is an R implementation that relies on general implementations of the relevant hash functions provided by [digest](http://cran.r-project.org/web/packages/digest/index.html) and base64 encoding provided by [RCurl](http://cran.r-project.org/web/packages/RCurl/index.html).

The package retains the core `unf` function from the earlier versions of the UNF package, but simplifies its use considerably. The package additionally implements some new helper functions.

### `unf` ###
The core `unf` function calculates the UNF signature for almost any R object for UNF algorithm versions 3, 4, 4.1, 5, or 6, with options to control the rounding of numeric values, truncation of character strings, and some idiosyncratic details of the UNFv5 algorithm as implemented by Dataverse. `unf` is a wrapper for functions `unf6`, `unf5`, `unf4`, and `unf3`, which calculate vector-level UNF signatures.

### `setUNF` and `checkUNF` ###
`setUNF` adds a UNF signature as a "UNF" attribute to any R object and `checkUNF` can be used to compare the current UNF signature of an object against that stored attribute (e.g., to check whether the object has been modified).

### `%unf%` ###
`%unf%` is a binary operator that can compare two R objects, or an R object against a "UNF" class summary (e.g., as stored in a study metadata record, or returned by `unf`). The function tests whether the objects are identical and, if they are not, provides object- and variable-level UNF comparisons between the two objects, checks for difference in the sorting of the two objects, and (for dataframes) reports indices for rows seemingly present in one object but missing from the other based on row-level hashes of variables common to both dataframes.

This can be used both to compare two objects in general (e.g., to see whether two dataframes differ) as well as to debug incongruent UNFs. Two UNFs can differ dramatically due to minor changes like rounding, the deletion of an observation, addition of a variable, etc., so `%unf%` provides a useful tool for looking under the hood at the differences between data objects that might produce different UNF signatures.


## The UNF Algorithm ##

The UNF algorithm is described in general terms [here](http://thedata.org/book/universal-numerical-fingerprint) along with more specific (but incomplete) descriptions of the [Version 3/4](http://thedata.org/book/unf-version-3-0), [Version 5](http://thedata.org/book/unf-version-5-0) and [Version 6](http://thedata.org/book/unf-version-6) algorithms. I describe the algorithm below, noting points of ambiguity and inconsistency and how they are implemented in this package.


### Numerics ###

Round numerics to *k* digits, where the default value of *k* is 7. (Note: In UNF versions <= 5, *k* was labeled *n*.) Then, convert those numerics to a character-class string containing exponential notation in the following form:

    - A sign character
    - A single leading non-zero digit
    - A decimal point
    - Up to *k*-1 remaining digits following the decimal, no trailing zeros
    - A lowercase letter "e"
    - A sign character
    - The digits of the exponent, omitting trailing zeros
    
  Note (a): Zero can be positive ("+0.e+") or negative ("-0.e+").
  
  Note (b): `Inf`, `-Inf`, and `NaN` are represented as: "+inf", "-inf", and "+nan", respectively. At some point in time, Dataverse appeared to have handled non-finites by treating them as missing. See [here](https://redmine.hmdc.harvard.edu/issues/2960) for some notes.
    
  Note (c): The [Dataverse](http://thedata.org) implementation of UNFv5 represents zero values (and boolean FALSE) values as "+0.e-6" rather than the implied "+0.e+" (like boolean `TRUE` values: "+1.e+"). The issue is described [here](https://redmine.hmdc.harvard.edu/issues/3085).

### Character Strings ###

Truncate character strings to *l* characters, where the default value of *l* is 128. (Note: In UNF versions <= 5, *l* was labeled *k*.)

### Other Data Classes ###

Handle other types of data in the following ways.

  1. For UNF versions < 5, convert all non-numeric data to character and handle as in (2), above.
  2. For UNF versions >= 5:
  
    a. Convert boolean values to numeric (`TRUE` is "1" and `FALSE` is "0") and handle as in (1), above.

    b. Treat factors as character and handle as in (2), above.
    
    c. Treat bits (raw) variables as base64-encoded big-endian bit sequences.
    
    d. Handle dates, times, and datetimes as in (4), below.

### Dates, times, datetimes, intervals, and durations ###

Dates, times, datetimes, intervals, and durations are handled as follows:

  a. Dates are converted to character strings in the form "YYYY-MM-DD", but partial dates ("YYYY" and "YYYY-MM") are permitted.
  
  b. Times are converted to character strings using the ISO 8601 format "hh:mm:ss.fffff". "fffff" is fractions of a second and must not containing trailing zeroes (as with any numeric value, see [1], above). The time should be expressed in UTC time with a terminal "Z" character.
  
  c. Datetimes may be expressed as a concatenated date (only in the form "YYYY-MM-DD") and time, separated by "T". As an example, Fri Aug 22 12:51:05 EDT 2014 is encoded as "2014-08-22T16:51:05Z".
  
  d. Intervals are represented as two datetimes, concatenated by a "/".
  
  e. Durations, while mentioned in versions of the specification, are not supported (and were never supported by the R implementation).
    
  Note: Given the different implementation of timezones in different programming languages and software applications, UNF signatures calculated for identical datasets in different applications may differ. For example, the UNFv6 specification notes that Stata does not implement time zones, while R always assumes a timezone. The suggested work around is to convert variables to a string representation and handle as in (2), above.

### Computing the UNF ###

1.  Append all non-missing values with an end-of-line (`\n`) character and a single null byte. Represent all missing values as a string of three null bytes. (Note: At some point in time, Dataverse appeared to treat empty character strings `""` as missing values. As of UNFv6, this is explicit that a missing value `NA` is represented by only three null bytes and an empty character string `""` is represented by an end-of-line character and a null byte.)

2. Convert to Unicode bit encoding. For UNF versions < 4.1, use [UTF-32BE](http://en.wikipedia.org/wiki/UTF-32BE). For UNF versions >= 4.1, use [UTF-8](http://en.wikipedia.org/wiki/UTF-8).

3. Concatenate all values into a single byte sequence. Compute a hash on the resulting byte sequence. For UNF version 3, use [MD5](http://en.wikipedia.org/wiki/MD5). For UNF versions > 3, use [SHA256](http://en.wikipedia.org/wiki/SHA-2).

4. [Base64 encode](http://en.wikipedia.org/wiki/Base64) the resulting hash. For UNF versions >= 5, truncate the UNF by performing base64 encoding only on the leftmost 128, 192, 196, or 256 bits, where 128 bits (16 bytes) is the default.

**To aggregate multiple variables:**

1. Calculate the UNF for each variable, as above.

  Note (a): For one-variable datasets, Dataverse implements the algorithm at the variable-level only, without aggregation. Thus a UNF for a one-variable dataframe is the same as the UNF for that variable alone. The standard is ambiguous in this regard and the package copies the Dataverse implementation.
    
  Note (b): The package treats dataframes and lists identically. Matrices are coerced to dataframes before running the algorithm.

2. Sort the base64-encoded UNFs in POSIX locale order.

3. Apply the UNF algorithm to the sorted, base64-encoded UNFs, using a truncation value as large as the original, treating the UNFs as character. For UNF versions >= 5, the algorithm is applied to the truncated UNFs.

**To aggregate multiple datasets:**

1. Calculate the UNF for each dataset, as above.

2. Sort the base64-encoded UNFs in POSIX locale order.

3. Apply the UNF algorithm to the sorted, base64-encoded UNFs, using a truncation value as large as the original, treating the UNFs as character. For UNF versions >= 5, the algorithm is applied to the truncated UNFs.

    Note: Multiple datasets need to be combined based on UNFs calculated with the same version of the algorithm. Thus when calculating a study-level UNF, dataset-level UNFs need to be calculated using the same version of the algorithm. (To achieve this, Dataverse recalculates old UNFs whenever new data is added to a study.)

### Reporting the UNF ###

The UNF is intended to be used as part of a data citation, for example:

> James Druckman; Jordan Fein; Thomas Leeper, 2012, "Replication data for: A Source of Bias in Public Opinion Stability", http://hdl.handle.net/1902.1/17864 UNF:5:esVZKwuUnh5kkpDhxXKLxA==

Here, a citation to the data file includes a persistent handle URI and a UNF signature specifying a specific version of the data file available from that handle. Note the UNF is printed as with a small header indicating the algorithm version, making it easy to match any particular UNF against a data file:

    UNF:[UNF version]:[UNF hash]

In UNFv5, the header might also contain details of other parameters for non-default number rounding and character string truncation, respectively: 

    UNF:[UNF version]:[digits],[characters]:[UNF hash]

In UNFv6, the header can contain a number rounding parameter (N), a string truncation parameter (X), and a variable-level UNF hash truncation parameter (H) (in any order):

    UNF:[UNF version]:N[digits],X[characters],H[bits]:[UNF hash]

The package prints each UNF in the appropriate format, including any non-default parameters when appropriate.

### References ###

Altman, M., J. Gill and M. P. McDonald.  2003. *Numerical Issues in Statistical Computing for the Social Scientist*.  John Wiley \& Sons. http://www.hmdc.harvard.edu/numerical_issues/. *(Describes version 3 of the algorithm)*

Altman, M., \& G. King. 2007. A Proposed Standard for the Scholarly Citation of Quantitative Data. *D-Lib* 13(3/4). http://dlib.org/dlib/march07/altman/03altman.html. *(Describes a citation standard using UNFs)*

Altman, M. 2008. A Fingerprint Method for Scientiﬁc Data Veriﬁcation. In T. Sobh, editor, *Advances in Computer and Information Sciences and Engineering*, chapter 57, pages 311-316. Springer Netherlands, Netherlands. http://link.springer.com/chapter/10.1007/978-1-4020-8741-7_57 *(Describes version 5 of the algorithm)*

Data Citation Synthesis Group. 2013. Declaration of Data Citation Principles [DRAFT]. http://www.force11.org/datacitation *(Describes general principles of data citation, of which UNF is likely to be a part)*
