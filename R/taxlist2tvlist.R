# TODO:   Convert taxlist to turboveg-like list and protopype for export
# 
# Author: Miguel Alvarez
################################################################################

# Inverse function to TCS.replace2 (see in tvsplist, package taxlist)
TCS.replace2.back <- function(x) {
    x <- replace(x, x == "TaxonUsageID", "SPECIES_NR")
    x <- replace(x, x == "TaxonName", "ABBREVIAT")
    x <- replace(x, x == "AuthorName", "AUTHOR")
    x <- replace(x, x == "TaxonConceptID", "VALID_NR")
}

# Internal function to create LETTERCODE
get_code <- function(x) {
    x <- strsplit(x, " ", fixed=TRUE)
    x <- sapply(x, "[", c(1,2,4))
    x[2,!is.na(x[3,])] <- paste(substr(x[2,], 1, 1), substr(x[3,], 1, 1),
            sep=".")[!is.na(x[3,])]
    x[2,x[2,] %in% c("species","spec.",".sp")] <- ".SP"
    x <- toupper(paste0(substr(x[1,], 1, 4), substr(x[2,], 1, 3)))
    return(x)
}

# The function
taxlist2tvlist <- function(taxlist, ecodbase=TRUE) {
    if(class(taxlist) != "taxlist")
        stop("'taxlist' have to be of class 'taxlist'", call.=FALSE)
    if(ecodbase & ncol(taxlist@taxonTraits) > 1)
        Traits <- taxlist@taxonTraits else Traits <- NULL
    # Add synonyms
    taxlist@taxonNames$SYNONYM <- !taxlist@taxonNames$TaxonUsageID %in%
            taxlist@taxonRelations$AcceptedName
    taxlist@taxonNames$SHORTNAME <- substr(taxlist@taxonNames$TaxonName, 1, 22)
    taxlist@taxonNames$NATIVENAME <- ""
    LETTERCODE <- get_code(taxlist@taxonNames[
                    paste(taxlist@taxonRelations$AcceptedName),"TaxonName"])
    taxlist@taxonNames$LETTERCODE <- LETTERCODE[
            match(taxlist@taxonNames$TaxonConceptID,
                    taxlist@taxonRelations$AcceptedName)]
    # Final version
    taxlist <- taxlist@taxonNames
    colnames(taxlist) <- TCS.replace2.back(colnames(taxlist))
    Head <- c("SPECIES_NR","LETTERCODE","SHORTNAME","ABBREVIAT","NATIVENAME",
            "AUTHOR","SYNONYM","VALID_NR")
    Head <- c(Head, colnames(taxlist)[!colnames(taxlist) %in% Head])
    taxlist <- list(species=taxlist[,Head])
    if(!is.null(Traits)) {
        colnames(Traits)[1] <- "SPECIES_NR"
        taxlist[["ecodbase"]] <- Traits
    }
    return(taxlist)
}
