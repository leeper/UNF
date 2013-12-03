# Universal Numeric Fingerprint #

This package calculates a Universal Numeric Fingerprint (UNF) on an R vector or dataframe. UNF is a crypographic hash that can be used to uniquely identify a (version of a) dataset, or a subset thereof. UNF is used by the [Dataverse](http://www.thedata.org) archives and this package can be used to verify a dataset against one listed available in a Dataverse study (e.g., as returned by the [dvn](http://cran.r-project.org/web/packages/dvn/) package).

This is still a draft and the present output may be unreliable, but the plan is to implement versions 3, 4, 4.1, and 5 (the current version) of the algorithm. Versions 1 and 2 were available in an earlier version of the UNF package authored by Micah Altman, which was built on C libraries, and is included in the version logs on GitHub. The current version of the UNF R package is a pure R implementation, which relies on general implementations of the relevant hash functions provided by [digest](http://cran.r-project.org/web/packages/digest/index.html) and base-64 encoding provided by [RCurl](http://cran.r-project.org/web/packages/RCurl/index.html).

## The UNF Algorithm ##

The UNF algorithm is described in general terms [here](http://thedata.org/book/universal-numerical-fingerprint) along with more specific (but incomplete) descriptions of the [Version 3/4](http://thedata.org/book/unf-version-3-0) and [Verion 5](http://thedata.org/book/unf-version-5-0) algorithms. 

Importantly, note that the [Dataverse](http://thedata.org) implementation of UNF represents zero values (and boolean FALSE) values as "+0.e-6" rather than the implied "+0.e+" (like boolean TRUE values: "+1.e+"). I will supply more information about the algorithm (which is rather poorly documented) in the future.
