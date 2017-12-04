
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