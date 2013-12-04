# Universal Numeric Fingerprint #

This package calculates a Universal Numeric Fingerprint (UNF) on an R data object. UNF is a crypographic hash that can be used to uniquely identify a (version of a) dataset, or a subset thereof. UNF is used by the [Dataverse](http://www.thedata.org) archives and this package can be used to verify a dataset against one listed available in a Dataverse study (e.g., as returned by the [dvn](http://cran.r-project.org/web/packages/dvn/) package).

This is still a draft and the present output may be unreliable, but the plan is to implement versions 3, 4, 4.1, and 5 (the current version) of the algorithm. Versions 1 and 2 were available in an earlier version of the UNF package authored by Micah Altman, which was built on custom C libraries, and is included in the version logs on GitHub. The current version of the UNF R package is an R implementation that relies on general implementations of the relevant hash functions provided by [digest](http://cran.r-project.org/web/packages/digest/index.html) and base64 encoding provided by [RCurl](http://cran.r-project.org/web/packages/RCurl/index.html).

## The UNF Algorithm ##

The UNF algorithm is described in general terms [here](http://thedata.org/book/universal-numerical-fingerprint) along with more specific (but incomplete) descriptions of the [Version 3/4](http://thedata.org/book/unf-version-3-0) and [Version 5](http://thedata.org/book/unf-version-5-0) algorithms. I describe the algorithm below, noting points of ambiguity and inconsistency and how they are implemented in this package.

1. Round numerics to *n* digits and truncate character strings to *k* characters.

2. For UNF versions < 5, convert all non-numeric data to character. For UNF versions >= 5, treat factor as character, boolean as numeric (see next note), date/times as ISO 8601, and bits as big-endian. (More details to come.)

3. Convert numerics to an exponential notation (see links to specific implementations for details).

    Importantly, note that the [Dataverse](http://thedata.org) implementation of UNF represents zero values (and boolean FALSE) values as "+0.e-6" rather than the implied "+0.e+" (like boolean TRUE values: "+1.e+"). The issue is described [here](https://redmine.hmdc.harvard.edu/issues/3085).

4. Handle non-finite values as special character strings (see [here](https://redmine.hmdc.harvard.edu/issues/2960) for some notes). (Dataverse appears to handle non-finites by treating them as missing.)

5. Append all non-missing values with an end-of-line (`\n`) character.

6. Convert to Unicode bit encoding. For UNF versions < 4.1, use [UTF-32BE](http://en.wikipedia.org/wiki/UTF-32BE). For UNF versions >= 4.1, use [UTF-8](http://en.wikipedia.org/wiki/UTF-8).

7. Combine all values into a single byte sequence, separating values by nul bytes.

8. Compute a hash on the resulting byte sequence. For UNF version 3, use [MD5](http://en.wikipedia.org/wiki/MD5). For UNF versions > 3, use [SHA256](http://en.wikipedia.org/wiki/SHA-2).

9. [Base64 encode](http://en.wikipedia.org/wiki/Base64) the resulting hash. For UNF version 5, truncate the UNF by performing base64 encoding only on the leftmost 16 bytes.

**To aggregate multiple variables:**

1. Calculate the UNF for each variable, as above.

    For one-variable datasets, Dataverse implements the algorithm at the variable-level only, without aggregation. Thus a UNF for a one-variable dataframe is the same as the UNF for that variable alone. The standard is ambiguous in this regard and the package copies the Dataverse implementation.
    
    The package treats dataframes and lists identically. Matrices are coerced to dataframes before running the algorithm.

2. Sort the base64-encoded UNFs in POSIX locale order.

3. Apply the UNF algorithm to the sorted, base64-encoded UNFs, using a *k* value as large as the original, treating the UNFs as character. For UNF version 5, the algorithm is applied to the truncated UNFs.

**To aggregate multiple datasets:**

1. Calculate the UNF for each dataset, as above.

2. Sort the base64-encoded UNFs in POSIX locale order.

3. Apply the UNF algorithm to the sorted, base64-encoded UNFs, using a *k* value as large as the original, treating the UNFs as character. For UNF version 5, the algorithm is applied to the truncated UNFs.

### References ###

Altman, M., J. Gill and M. P. McDonald.  2003. *Numerical Issues in Statistical Computing for the Social Scientist*.  John Wiley \& Sons. http://www.hmdc.harvard.edu/numerical_issues/. *(Describes version 3 of the algorithm)*

Altman, M., \& G. King. 2007. A Proposed Standard for the Scholarly Citation of Quantitative Data. *D-Lib* 13(3/4). http://dlib.org/dlib/march07/altman/03altman.html. *(Describes a citation standard using UNFs)*

Altman, M. 2008. A Fingerprint Method for Scientiﬁc Data Veriﬁcation. In T. Sobh, editor, *Advances in Computer and Information Sciences and Engineering*, chapter 57, pages 311-316. Springer Netherlands, Netherlands. http://link.springer.com/chapter/10.1007/978-1-4020-8741-7_57 *(Describes version 5 of the algorithm)*

Data Citation Synthesis Group. 2013. Declaration of Data Citation Principles [DRAFT]. http://www.force11.org/datacitation *(Describes general principles of data citation, of which UNF is likely to be a part)*
