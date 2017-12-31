
library(devtools)
create("scoreABH")
document("scoreABH")
build("scoreABH")
check("scoreABH")
library("scoreABH")
load_all("scoreABH")
install("scoreABH")

library(opencpu)
ocpu_start_server(port=8080)

(tmp <- httr::POST("http://localhost:8080/ocpu/library/scoreABH/R/get_score",
                   body = list(
                     main_driver_age = "25",
                     main_driver_gender = "'male'",
                     car_worth = "25000")))
