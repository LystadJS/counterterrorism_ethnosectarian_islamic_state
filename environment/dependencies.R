# Machine-readable dependency manifest for the current project setup.
#
# This file intentionally records package names without inventing historical
# versions. Generate and commit renv.lock from a known-good environment before
# the first tagged reproducible release.

core_packages <- c(
  "tidyverse",
  "janitor",
  "mice",
  "sf",
  "broom",
  "broom.mixed",
  "skimr",
  "lubridate",
  "patchwork",
  "scales",
  "terra",
  "raster",
  "exactextractr"
)

optional_model_packages <- c(
  "brms"
)

all_documented_packages <- sort(unique(c(core_packages, optional_model_packages)))

missing_packages <- all_documented_packages[
  !vapply(
    all_documented_packages,
    requireNamespace,
    quietly = TRUE,
    FUN.VALUE = logical(1)
  )
]

if (length(missing_packages) == 0L) {
  message("All documented project packages are available.")
} else {
  message(
    "Missing documented packages: ",
    paste(missing_packages, collapse = ", ")
  )
}
