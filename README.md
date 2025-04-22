
# pcir: Potential for Conflict Index in R <a href="https://fblpalmeira.github.io/pcir/"><img src="man/figures/pcir_logo.png" alt="pcir website" align="right" height="139"/></a>

## Overview

`pcir` is an R package developed to assist researchers and practitioners
in calculating, comparing, and visualizing the Potential for Conflict
Index (PCI) among stakeholders. PCI is a descriptive statistical method
designed to enhance the understanding of outcomes in human dimensions
research[(Manfredo et
al. 2003;](https://www.tandfonline.com/doi/abs/10.1080/10871200304310)
[Vaske et
al. 2010)](https://www.tandfonline.com/doi/abs/10.1080/01490401003712648).

The concepts of consensus, disagreement, and conflict are relevant
across a wide range of disciplines, including economics, political
science, psychology, sociology, and natural resource management.
Although PCI can be calculated using software such as Excel, SPSS, and
SAS, no dedicated R package existed for this analysis — until now.

This package was developed as part of my training in the [rOpenSci
Champions Program](https://ropensci.org/champions/), supported by the
Chan Zuckerberg Initiative.

Additional information:

- [Introducing rOpenSci Champions - Cohort
  2023-2024](https://ropensci.org/blog/2024/02/15/champions-program-champions-2024/)

## Theoretical approach

<img src="man/figures/likert_scales1.png"  align="center" height="400"/>

**Figure 1.** Likert scales used in the Potential for Conflict Index
(PCI).

## Workflow

Steps implemented in the ‘pcir’ package:

# 1. Read the input data from interviews – see example dataset [(Spreadsheet)]();

# 2. Count the frequency of responses for each question – see [(Table 1)]();

# 3. Calculate the PCI for each question – see [(Table 2)]();

# 4. Generate a bubble chart to visualize the results – see [(Figure)]().

<img src="man/figures/diagrammer_pcir.png" align="center">

**Figure 2.** Workflow of the ‘pcir’ package.

## Features

- `counting()`: Summarizes data by calculating counts, percentages,
  means, and standard deviations.

- `pci()`: Computes the Potential for Conflict Index from summary data.

- `bubble()`: Visualizes PCI results using a bubble plot.

## Installation

Install the development version of `pcir` directly from GitHub:

``` r
# Uncomment the line below if devtools is not installed
# install.packages("devtools")

devtools::install_github("fblpalmeira/pcir")
```

## Usage

After installation, load the package:

``` r
library(pcir)
```

Example dataset:

``` r
df1 <- data.frame(
  A = c(-1, 2, 2, 3, -1),
  B = c(-1, 2, 3, -1, 2),
  C = c(1, 2, -2, 3, -1),
  D = c(3, 2, 1, -1, -2),
  E = c(2, 3, 1, -1, -3)
  )
```

Count responses:

``` r
df_count <- counting(df1)
df_count
```

Calculate PCI:

``` r
df_pci <- pci(df_count)
df_pci
```

Visualize with a bubble plot:

``` r
# The bubble function creates a bubble plot to visualize the PCI results
bubble_plot <- bubble(df_pci)
bubble_plot # Display the bubble plot
```

<img src="man/figures/output_pci.png">

**Figure 3.** Bubble chart illustrating the Potential for Conflict
Indices.

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

If you use the `pcir` package in your work, please cite it as follows:

``` r
citation(package = 'pcir')
```

Example output:

``` r
To cite the 'pcir' package in publications, use:

  Palmeira F (2024). _pcir: Potential for Conflict Index in
  R_. R package version 0.1.0,
  <https://github.com/fblpalmeira/pcir>.

The BibTeX entry for LaTeX users is

  @Manual{,
    title = {pcir: Potential for Conflict Index in R},
    author = {Francesca Belem Lopes Palmeira},
    year = {2024},
    note = {R package version 0.1.0},
    url = {https://github.com/fblpalmeira/pcir},
  }
```

## License

This package is licensed under the [MIT
License](https://github.com/fblpalmeira/pcir?tab=MIT-1-ov-file). See the
LICENSE file for more details.

## Bug Reports

If you encounter any bugs or issues, please report them on the [GitHub
Issues](https://github.com/fblpalmeira/pcir/issues) page.

To report a bug in a GitHub repository, you need to have a GitHub
account. The account is required to interact with the repository, such
as creating issues, leaving comments, and tracking updates related to
the issue.

If you don’t have an account, you can create one for free on
[GitHub](https://github.com/join.). Once your account is created, you’ll
be able to access repositories and easily report issues.

How to Create an Issue:

- To create an issue, go to the Issues tab of your GitHub repository,
  which will be visible just below the repository name, next to Pull
  Requests.

- Click on the **New Issue** button and fill in the details about the
  bug or problem encountered.

- Users can do this at any time, and you will receive a notification
  when a new issue is created.

## Discussions

The **Discussions** section is the space for the `pcir` community to
have conversations, ask questions, and post answers without opening
issues. It’s a great place for users to interact informally, share
ideas, discuss how to use the package, and seek help with problems or
questions without the need to create a formal issue.

You can join the conversation and start discussing here: [pcir
Discussions](https://github.com/fblpalmeira/pcir/discussions)

## Contact

For any questions or inquiries, please contact Francesca Palmeira at
<francesca@alumni.usp.br>.
