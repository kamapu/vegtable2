# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

library(devtools)
install_github("kamapu/vegtable2", "new_db")

library(vegtable2)

draw_badge("AF-00-006", "data-raw/swea_v.svg", link = "www.mysite.com",
		type = "version")
