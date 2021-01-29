# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

library(devtools)
install_github("kamapu/vegtable2", "new_db")
install_gitlab("kamapu/dbaccess")

library(vegtable2)
library(RPostgreSQL)
library(dbaccess)

draw_badge("AF-00-006", "data-raw/swea_v.svg", link = "www.mysite.com",
		type = "version")

draw_badge("AF-00-006", "data-raw/swea_givd.svg", link = "www.mysite.com",
		type = "GIVD")

conn <- connect_db2(dbname = "veg_databases", user = "miguel")

Test <- import_swea(conn)
summary(Test)

Sam <- import_sam(conn)



