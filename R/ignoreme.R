# this function is never called, but is here just to provide dummy usage of
# the shinyjs package. This packages are used in the Shiny app which
# is not in the main R directory, and CRAN checks complain that it's in the
# DESCRIPTION Imports field if it's not used in the R directory, so this is
# just to keep CRAN checks happy.
nevercalled <- function() {
  ignored <- shinyjs::useShinyjs()
}
