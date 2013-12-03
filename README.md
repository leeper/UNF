# Universal Numeric Fingerprint #

This package calculates a Universal Numeric Fingerprint (UNF) on an R vector or dataframe. UNF is a crypographic hash that can be used to uniquely identify a (version of a) dataset. UNF is used by the [Dataverse](http://www.thedata.org) archives and this package can be used to verify a dataset against one listed available in a Dataverse study (e.g., as returned by the [dvn](http://cran.r-project.org/web/packages/dvn/) package).

This is still a draft, but the plan is to implement versions 3, 4, 4.1, and 5 (the current version) of the algorithm. Versions 1 and 2 were available in an earlier version of the UNF package authored by Micah Altman, which was built on C libraries. This is a pure R implementation, dependent on [digest](http://cran.r-project.org/web/packages/digest/index.html) and [RCurl](http://cran.r-project.org/web/packages/RCurl/index.html).