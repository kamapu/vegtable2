# TODO:   A function to clone Turboveg databases
# 
# Author: Miguel Alvarez
################################################################################

clone <- function(db_name, tv_home=tv.home(), stamp=TRUE, overwrite=FALSE,
        extras=FALSE, ...) {
    oldWD <- getwd()
    setwd(tv_home)
    db_description <- unlist(c(db_name, read.dbf(file.path(tv_home, "Data",
                                    db_name,"tvwin.dbf"), as.is=TRUE)[,
                            c("FLORA","DICTIONARY"), drop=FALSE]))
    # Species list
    path_species <- file.path("species", db_description["FLORA"])
    Files <- file.path(path_species, list.files(path_species, recursive=TRUE))
    # Observations
    path_observs <- file.path("Data", db_name)
    Files <- c(Files, file.path(path_observs, list.files(path_observs,
                            recursive=TRUE)))
    # Popups
    if(is.na(db_description["DICTIONARY"])) path_popups <- "popup" else
        path_popups <- file.path("popup", db_description["DICTIONARY"])
    Files <- c(Files, file.path(path_popups, list.files(path_popups,
                            recursive=TRUE)))
    # Extras
    if(extras) {
        path_extras <- file.path("extras", db_name)
        Files <- c(Files, file.path(path_extras, list.files(path_extras,
                                recursive=TRUE)))
    }
    # using stamp and don't overwrite
    if(stamp) db_name <- paste(db_name, Sys.Date(), sep="_")
    if(paste0(db_name, ".zip") %in% list.files(oldWD) & !overwrite) {
        i <- 0
        repeat{
            i <- i + 1
            if(!paste0(db_name, "_", i, ".zip") %in% list.files(oldWD)) break
        }
        db_name <- paste0(db_name, "_", i)
    }
    paste0(db_name, ".zip")
    # The clone
    zip(file.path(oldWD, db_name), Files, ...)
    setwd(oldWD)
}
