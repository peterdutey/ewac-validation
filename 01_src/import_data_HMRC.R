
library(readxl)

hmrc_url <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/827833/2019_JUL_UK_Alcohol_Stats_Tables.xlsx"

temp <- tempfile()
download.file(url = hmrc_url, destfile = temp, method = "wb")


hmrc <- read_xlsx(temp, sheet = )
