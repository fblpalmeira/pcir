#' Reinstall and Test the pcir Package
#'
#' This RStudio Addin reinstalls the 'pcir' package from the local source,
#' reloads it, and runs all functions on example data for testing.
#'
#' @export
reinstall_and_test_addin <- function() {
  message("🔄 Reinstalling the 'pcir' package...")

  pkg_path <- rprojroot::find_root(rprojroot::has_file("DESCRIPTION"))

  tryCatch({
    # Detach if already loaded
    if ("pcir" %in% loadedNamespaces()) {
      message("📦 Unloading 'pcir'...")
      try(detach("package:pcir", unload = TRUE, character.only = TRUE), silent = TRUE)
    }

    # Remove installed version
    if ("pcir" %in% rownames(installed.packages())) {
      message("🧹 Removing installed version...")
      remove.packages("pcir")
    }

    # Document and install
    message("📚 Documenting with roxygen2...")
    roxygen2::roxygenise(pkg_path)

    message("📦 Installing from source...")
    devtools::install(pkg_path, upgrade = "never", quiet = TRUE)

    # Load and test
    library(pcir)
    message("✅ 'pcir' reinstalled successfully!")

    message("🧪 Running test functions on example data...")

    # Load example file from inst/extdata
    input <- system.file("extdata", "example_data.xlsx", package = "pcir")

    count_df <- counting(input)
    pci_df <- pci(count_df)
    bubble(pci_df)

    message("🎉 All functions ran successfully!")
  }, error = function(e) {
    message("❌ Error: ", e$message)
  })
}
