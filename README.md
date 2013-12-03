# Universal Numeric Fingerprint #

This package calculates a Universal Numeric Fingerprint (UNF) on an R vector or dataframe. UNF is a crypographic hash that can be used to uniquely identify a (version of a) dataset, or a subset thereof. UNF is used by the [Dataverse](http://www.thedata.org) archives and this package can be used to verify a dataset against one listed available in a Dataverse study (e.g., as returned by the [dvn](http://cran.r-project.org/web/packages/dvn/) package).

This is still a draft and the present output may be unreliable, but the plan is to implement versions 3, 4, 4.1, and 5 (the current version) of the algorithm. Versions 1 and 2 were available in an earlier version of the UNF package authored by Micah Altman, which was built on C libraries, and is included in the version logs on GitHub. The current version of the UNF R package is a pure R implementation, which relies on general implementations of the relevant hash functions provided by [digest](http://cran.r-project.org/web/packages/digest/index.html) and base-64 encoding provided by [RCurl](http://cran.r-project.org/web/packages/RCurl/index.html).

## The UNF Algorithm ##

The UNF algorithm is described in general terms [here](http://thedata.org/book/universal-numerical-fingerprint) along with more specific (but incomplete) descriptions of the [Version 3/4](http://thedata.org/book/unf-version-3-0) and [Verion 5](http://thedata.org/book/unf-version-5-0) algorithms. 

The general flow of the algorithm is as follows (performed on each variable):

1. Round numerics to *n* digits and truncate character strings to *k* characters.

2. For versions < 5, convert all non-numeric data to character. For version >= 5, treat factor as character, boolean as numeric (see next note), date/times as ISO 8601, and bits as big-endian. (More details to come.)

3. Convert numerics to an exponential notation (see links to specific implementations for details).

    Importantly, note that the [Dataverse](http://thedata.org) implementation of UNF represents zero values (and boolean FALSE) values as "+0.e-6" rather than the implied "+0.e+" (like boolean TRUE values: "+1.e+"). The issue is described [here](https://redmine.hmdc.harvard.edu/issues/3085).

4. Handle infinites as special character strings (see [here](https://redmine.hmdc.harvard.edu/issues/2960) for some notes).

5. Append all non-missing values with an end-of-line (`\n`) character.

6. Convert to Unicode bit encoding. For versions < 4.1, use UTF-32BE. For versions >= 4.1, use UTF-8.

7. Combine all values into a single bit sequence, separating values by nul bytes.

8. Compute a hash on the resulting bit sequence. For version 3, use MD5. For versions > 3, use SHA256.

9. Base64 encode the resulting hash. For version 5, perform the base64 encoding only on the leftmost 16 bytes.

To aggregate multiple variables:

1. Calculate the UNF for each variable, as above.

2. Sort the base64-encoded UNFs in POSIX locale order.

3. Apply the UNF algorithm to the sorted, base64-encoded UNFs, using a `k` value as large as the original, treating the UNFs as character.

To aggregate multiple datasets:

1. Calculate the UNF for each dataset, as above.

2. Sort the base64-encoded UNFs in POSIX locale order.

3. Apply the UNF algorithm to the sorted, base64-encoded UNFs, using a `k` value as large as the original, treating the UNFs as character.


