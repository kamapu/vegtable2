# TODO:   Personalized import routine for SWEA-Dataveg
# 
# Author: Miguel Alvarez
################################################################################

swea_import <- function(db, ...) {
    x <- tv2vegtable(db, ...)
    # 1: Modify presence-absence
    new_pa <- rep(1, nrow(x@samples))
    new_pa[is.na(x@samples$pr_ab)] <- NA
    x@samples$pr_ab <- new_pa
    # 2: Schultka to percent
    x@samples$percen[!is.na(x@samples$schltk)] <- as.numeric(paste(
                    x@samples$schltk[!is.na(x@samples$schltk)]))
    x@samples <- x@samples[,colnames(x@samples) != "schltk"]
    # 3: Insert default conversion
    data(braun_blanquet, envir=environment())
    x@coverconvert <- braun_blanquet
    # output
    return(x)
}
