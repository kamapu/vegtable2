# TODO:   Retrieving descriptions from tables in a database
# 
# Author: Miguel Alvarez
################################################################################

# Generic function
setGeneric("get_description", function(conn, ...)
			standardGeneric("get_description"))

# Method for connection-class
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
