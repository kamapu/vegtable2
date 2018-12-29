# TODO:   Function reporting communities in databases
# 
# Author: Miguel Alvarez
################################################################################

# Generic function
setGeneric("report_communities",
		function(veg, ...)
			standardGeneric("report_communities")
)

# Method for vegtable objects
setMethod("report_communities", signature(veg="vegtable"),
		function(veg, refs, filename, title, author, date,
				date_format="%d-%m-%Y", keep_rmd=TRUE, ...) {
			filename <- paste0(filename, ".Rmd")
			if(!missing(date))
				if(class(date) != "Date")
					stop("Argument 'date' has to be of class 'Date'")
			if(missing(date))
				date <- Sys.Date()
			if(class(refs)[1] != "BibEntry")
				stop("Argument 'refs' has to be of class 'BibEntry'")
			# Structure Head of text
			Head <- paste0(
					"---\n",
					paste0("title: \"", title, "\"\n"),
					paste0("author: \"", author, "\"\n"),
					paste0("date: \"", format(date, date_format), "\"\n"),
					"output: pdf_document\n",
					"---\n",
					"\n")
			# Structure the content
			comm <- aggregate(ReleveID ~ data_source + community_type,
					veg@header, length)
			comm$community_name <- with(veg@relations$community_type,
					community_name[match(comm$community_type, community_type)])
			comm$bibtexkey <- with(veg@relations$data_source,
					bibtexkey[match(comm$data_source, data_source)])
			comm <- clean_strings(comm)
			comm <- split(comm, comm$bibtexkey)
			Content <- list()
			for(i in names(comm)) {
				Content[[i]] <- paste0("**[", i,
						"]** `r capture.output(print(refs[\"", i,
				        "\"], .opts=list(bib.style=\"authoryear\")))`\n")
				for(j in 1:nrow(comm[[i]]))
					Content[[i]][j + 1] <- paste0("**",
							comm[[i]][j,"community_type"], ". ",
							comm[[i]][j,"community_name"],
							"** \\dotfill ", comm[[i]][j,"ReleveID"], "\n")
				Content[[i]][length(Content[[i]]) + 1] <- "\\vspace{5mm}"
			}
			## write(c(Head, unlist(Content)), filename)
			con <- file(filename, "wb")
			writeBin(charToRaw(do.call(paste0, list(c(Head, unlist(Content)),
											collapse="\n"))), con,
					endian="little")
			close(con)
			## compile with rmarkdown
			render(filename, encoding="UTF-8")
			if(!keep_rmd) unlink(filename)
		}
)
