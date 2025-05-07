## Test environments
* local Windows 10 installation, R 4.5.0
* R-hub: windows-x86_64-release, ubuntu-gcc-release, fedora-clang-devel
* GitHub Actions (Ubuntu-latest, macOS-latest, Windows-latest)
* devtools::check(args = "--as-cran") completed with no errors/warnings/notes

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.

## Downstream dependencies
There are currently no downstream packages on CRAN that depend on this package.

## Comments
This is the first submission of the `pcir` package to CRAN.

The package provides a set of functions for calculating, comparing and visualizing the Potential Conflict Index (PCI), commonly used in conflict analysis and participatory research.

It includes:
  * `counting()`: categorizes Likert-type responses
* `pci()`: computes the Potential Conflict Index
* `bubble()`: creates visualizations of PCI scores

The package is fully documented, includes examples, and has a vignette accessible via `vignette("pcir")`.

