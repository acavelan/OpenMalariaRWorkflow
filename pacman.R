# Configure a default CRAN mirror so installs do not trigger the GUI chooser.
repos <- getOption("repos")
cran_repo <- unname(repos["CRAN"])
if (length(cran_repo) == 0 || is.na(cran_repo) || cran_repo %in% c("", "@CRAN@")) {
    repos["CRAN"] <- "https://cloud.r-project.org"
    options(repos = repos)
}

# Install pacman if it is not already available.
if (!requireNamespace("pacman", quietly = TRUE)) {
    install.packages("pacman")
}

# Load pacman.
library(pacman)
