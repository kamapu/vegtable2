#' @name get_description
#' 
#' @title  Get descriptions of table columns from Postgres tables
#' 
#' @description 
#' Descriptions of variables stored in PostgreSQL tables.
#' 
#' This function produces a data frame containing information on schemas,
#' tables and descriptions (i.e. comments) for every single column in tables
#' stored at a PostgreSQL database.
#' 
#' @param conn A \code{\linkS4class{PostgreSQLConnection}} object.
#' @param ... Further arguments passed among methods.
#' 
#' @return An object of class [data.frame].
#' 
#' @author Miguel Alvarez \email{malvarez@@uni-bonn.de}
#' 
#' @rdname get_description
#' 
#' @exportMethod get_description
#' 
setGeneric("get_description", function(conn, ...)
			standardGeneric("get_description"))

#' @rdname get_description
#' 
#' @aliases get_description,PostgreSQLConnection-method
#' 
setMethod("get_description", signature(conn="PostgreSQLConnection"),
		function(conn, ...) {
			SQL <- "
SELECT c.table_schema,c.table_name,c.column_name,c.data_type,c.is_nullable,c.numeric_precision,c.numeric_scale,pgd.description
FROM pg_catalog.pg_statio_all_tables as st
  inner join pg_catalog.pg_description pgd on (pgd.objoid=st.relid)
  inner join information_schema.columns c on (pgd.objsubid=c.ordinal_position
    and  c.table_schema=st.schemaname and c.table_name=st.relname);
"
			OUT <- dbGetQuery(conn, SQL, stringsAsFactors=FALSE)
			colnames(OUT)[c(1,2,3,6,7)] <- c("schema", "table", "column",
							"length", "precision")
			return(OUT)
		}
)
