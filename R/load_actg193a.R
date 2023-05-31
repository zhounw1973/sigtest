#' @name ACTG193A
#' @title Data from AIDS Clinical Trials Group 193A
#' @description The data is publicly available at https://content.sph.harvard.edu/fitzmaur/ala/cd4.txt.
#' @keywords dataset
NULL

ACTG193A <-read.table(system.file("data", "ACTG193A.txt", package = "sigtest"))
colnames(ACTG193A) <- c("id","treatment","age","gender", "week", "logCD4")


#' @export
