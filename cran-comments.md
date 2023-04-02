## Test environments

-   local R installation, R 4.2.1
-   win-builder (devel)
-   Windows Server 2022, R-devel, 64 bit
-   Fedora Linux, R-devel, clang, gfortran

## R CMD check results

0 errors | 0 warnings | 1 note

-   Found the following (possibly) invalid URLs: 
    URL: <https://doi.org/10.1002/sim.4322> 
    From: README.md NEWS.md 
    Status: 403 
    Message: Forbidden 
    
    URL: <https://doi.org/10.1177/17407745221095855> 
    From: README.md NEWS.md 
    Status: 403 
    Message: Forbidden

-   Found the following (possibly) invalid DOIs: 
    DOI: 10.1002/sim.4322 
    From: DESCRIPTION 
    Status: Forbidden 
    Message: 403 
    DOI: 10.1177/17407745221095855 
    From: DESCRIPTION inst/CITATION 
    Status: Forbidden 
    Message: 403

-   I have checked these URLs and DOIs. They seem to be correct.

## revdepcheck results

There are currently no downstream dependencies for this package.
