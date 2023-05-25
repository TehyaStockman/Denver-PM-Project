install.packages('rvest')
install.packages('xml2')
install.packages('httr')
install.packages('qdapRegex')

library(httr)
library(xml2)
library(rvest)
library(qdapRegex)


aq_data <- read_html('https://www.colorado.gov/airquality/param_summary.aspx?parametercode=81102&seeddate=04%2f18%2f2023&export=False')

aq_data_table <- html_table(html_nodes(aq_data, replace(aq_data,'<br />',' ')))[[3]]


aq_data_table[, 2] <- unlist(lapply(rm_between(xml_find_all(aq_data, "//table/tbody/tr/td[2]"), 
                                    ">", 
                                    "<br> />", extract=TRUE), 
                         function(x) gsub("<.*?>", "", x[[3]])))


stuff <- xml_find_all(aq_data, "//body/tr[4]/")


#read webpage
htm_data <- read_html("http://www.hmdb.ca/metabolites?tf8=%E2%9C%93&filter=true&toxin=1&filter=true") 

#convert above webpage's table into a dataframe
df <- html_table(html_nodes(htm_data, "table"))[[1]]

#cleanup data in the required column
df[, 4] <- unlist(lapply(rm_between(xml_find_all(htm_data, "//table/tbody/tr/td[4]"), 
                                    ">", 
                                    "<br><br>", extract=TRUE), 
                         function(x) gsub("<.*?>", "", x[[1]])))

