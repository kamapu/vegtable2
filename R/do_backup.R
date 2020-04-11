#' @name do_backup
#' @rdname do_backup
#' 
#' @title Create a backup by 'pg_dump' and restore database
#' 
#' @details 
#' The function `do_backup()` is a wrapper for **pg_dump** and is creating
#' writing a dump file named by the backed up database, a time stamp (date of
#' creation) and a suffix (in the case of a further backup done at the same
#' day).
#' Naming the files is done in the same way as by [backup_object()].
#' 
#' The function `do_restore()` can restore the created backup files through
#' **pg_restore**.
#' If several backups have been accumulated in a common folder, this function is
#' able to recognize the newest one in the same way as [load_last()].
#' 
#' @param DB Character value indicating the name of the target PostgreSQL
#'     database.
#' @param b_name Character value, the name of the backup file. If not provided,
#'     the function guess the database's name and a time stamp, where the newest
#'     backup, if many in the same folder, will be used.
#' @param path_f Character value indicating the path to the folder, where the
#'     backup file will be restored. It have to be a path relative to the
#'     working directory.
#' @param fext Character value indicating the extension used for the backup
#'     file, including the leading dot.
#' @param path_psql Character value indicating the system path to PostgreSQL
#'     binaries.
#' @param username Character value indicating the database user in PostgreSQL.
#' @param password Character value, the user's password in PostgreSQL.
#' @param f_timestamp Character value indicating the format of the timestamp
#'     applied to the backup's name (see [strptime()]).
#' @param host Character value, the host name.
#' @param port Integer value, the port applied for database connection.
#' @param ... Further arguments passed to [system()].
#' 
#' @author Miguel Alvarez
#' 
#' @export do_backup
#' 
do_backup <- function(DB, path_f,  fext=".backup",
		path_psql="/opt/PostgreSQL/10/bin", username="miguel",
		password="my_password", f_timestamp="%Y%m%d", host="localhost",
		port=5432, ...) {
	# Create new name
	old_file <- list.files(file.path(getwd(), path_f, DB))
	new_file <- paste0(DB, "_",  format(Sys.Date(), f_timestamp))
	new_file <- taxlist:::add_suffix(new_file, sub(fext, "", old_file))
	new_file <- file.path(getwd(), path_f, paste0(new_file, fext))
	# Create command
	command <- paste(paste0("PGPASSWORD=\"", password, "\""),
			file.path(path_psql, "pg_dump"),
			"-U", username,
			"-h", host,
			"-p", port,
			"-F c", DB, ">", new_file)
	system(command, ...)
	message(paste0("\nDatabase '", DB, "' backed up in '",
					sub(paste0(getwd(), "/"), "", new_file), "'"))
}

#' @rdname do_backup
#' 
#' @aliases do_restore
#' 
#' @export do_restore
#' 
do_restore <- function(DB, path_f, b_name, fext=".backup",
		path_psql="/opt/PostgreSQL/10/bin", username="miguel",
		password="my_password", f_timestamp="%Y%m%d", host="localhost",
		port=5432, ...) {
	if(missing(b_name)) {
		b_name <- taxlist:::sort_backups(file.path(path_f, DB), f_timestamp,
				fext)
		b_name <-b_name$filename[nrow(b_name)]
	}
	command <- paste(paste0("PGPASSWORD=\"", password, "\""),
			file.path(path_psql, "pg_restore"),
			"-h", host,
			"-p", port,
			"-U", username,
			"-d", DB, "--clean",
			"-v", file.path(getwd(), path_f, b_name))
	system(command, ...)
	message(paste0("\nDatabase '", DB, "' restored from '",
					file.path(path_f, b_name), "'"))
}
