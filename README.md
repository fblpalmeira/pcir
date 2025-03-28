
# pcir: Potential for Conflict Index in R <a href="https://fblpalmeira.github.io/pcir/"><img src="man/figures/pcir_logo.png" alt="pcir website" align="right" height="139"/></a> (Under construction...)

## Overview

`pcir` is an R package developed to assist researchers and practitioners
in calculating, comparing, and visualizing the Potential for Conflict
Index (PCI) among stakeholders. PCI is a descriptive statistical method
designed to enhance the understanding of outcomes in human dimensions
research [(Manfredo et
al. 2003;](https://www.tandfonline.com/doi/abs/10.1080/10871200304310)
[Vaske et
al. 2010)](https://www.tandfonline.com/doi/abs/10.1080/01490401003712648).
The concepts of consensus, disagreement, and conflict are relevant
across a wide range of disciplines, including economics, political
science, psychology, sociology, and natural resource management. While
PCI can currently be calculated using software such as Excel, SPSS, and
SAS, there has been no dedicated R package available for this specific
type of analysis—until now.

The development of this package is part of my training in the [rOpenSci
Champions Program](https://ropensci.org/champions/), supported by the
Chan Zuckerberg Initiative.

Additional information:

- [Introducing rOpenSci Champions - Cohort
  2023-2024](https://ropensci.org/blog/2024/02/15/champions-program-champions-2024/)

## Theoretical approach

<img src="man/figures/likert_scales1.png"  align="center" height="400"/>

**Figure 1.** Likert scales of the Potential for Conflict Index (PCI).

## Workflow

Stages of the ‘pcir’ package:

1.  Read the data input from the interviews/ See exemple dataset
    [(Spreadsheet)]();

2.  Count the frequencies of responses within each question / Write
    [(Table
    1)](https://github.com/fblpalmeira/pcir/blob/main/data/Table1.xlsx);

3.  Calculate the potential conflict index for each question / Write
    [(Table
    2)](https://github.com/fblpalmeira/pcir/blob/main/data/Table2.xlsx);

4.  Create a bubble chart using the indices / Save [(Figure)]().

<img src="man/figures/diagrammer_pcir.png" align="center">

**Figure 2.** Workflow of the ‘pcir’ package.

## Features

- **`counting()`** summarize data by calculating counts, percentages,
  means, and standard deviations.

- **`pci()`** compute the Potential for Conflict Index from summary
  data.

- **`bubble()`** visualize PCI results using a bubble plot.

## Installation

You can install the development version of `pcir` directly from GitHub:

``` r
# Uncomment the line below if devtools is not installed
# install.packages("devtools")
devtools::install_github("fblpalmeira/pcir")
```

## Usage

Load the package if pcir is already installed.

``` r
# Load the Package:
library(pcir)
```

``` r
# Example dataset:
df1 <- data.frame(
  A = c(-1, 2, 2, 3, -1),
  B = c(-1, 2, 3, -1, 2),
  C = c(1, 2, -2, 3, -1),
  D = c(3, 2, 1, -1, -2),
  E = c(2, 3, 1, -1, -3)
  )
```

Counting function:

``` r
# The counting function summarizes data by counts, percentages, means, and standard deviations
df_count <- counting(df1)
df_count
```

PCI function:

``` r
# The pci function calculates the Potential for Conflict Index (PCI)
df_pci <- pci(df_count)
df_pci
```

Bubble plot function:

``` r
# The bubble function creates a bubble plot to visualize the PCI results
bubble_plot <- bubble(df_pci)
bubble_plot # Display the bubble plot
```

<img src="man/figures/output_pci.png">

**Figure 3.** Bubble graph illustranting the Potencial Conflict Indices.

## References

Manfredo, M., Vaske, J., Teel, T. (2003). [The potential for conflict
index: A graphic approach to practical significance of human dimensions
research](https://www.tandfonline.com/doi/abs/10.1080/10871200304310).
Human Dimensions of Wildlife, 8(3), 219-228.

Vaske, J. J., Beaman, J., Barreto, H., Shelby, L. B. (2010). [An
extension and further validation of the potential for conflict
index](https://www.tandfonline.com/doi/abs/10.1080/01490401003712648).
Leisure Sciences, 32(3), 240-254.

## Citation

``` r
# If you use the `pcir` package in your work, please cite it as follows:
citation(package = 'pcir')
```

``` r
To cite the 'pcir' package in publications, use:

  Palmeira F (2024). _pcir: Potential for Conflict Index in
  R_. R package version 0.1.0,
  <https://github.com/fblpalmeira/pcir>.

The BibTeX entry for LaTeX users is

  @Manual{,
    title = {pcir: Potential for Conflict Index in R},
    author = {Francesca Palmeira},
    year = {2024},
    note = {R package version 0.1.0},
    url = {https://github.com/fblpalmeira/pcir},
  }
```

## License

This package is licensed under the [MIT
License](https://github.com/fblpalmeira/pcir?tab=MIT-1-ov-file). See the
LICENSE file for more details.

## Contact

For any questions or inquiries, please contact Francesca Palmeira at
<francesca@alumni.usp.br>.
