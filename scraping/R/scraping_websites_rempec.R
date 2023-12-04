# web scraping using this guide: https://www.zenrows.com/blog/web-scraping-r#step-1-install-rvest

# requirements ----
# utils::install.packages("rvest")
library("rvest")

# retrieving the target of HTML ----
rempec_webpage <- xml2::read_html(site)

# selecting HTML elements and attributes ----
# the "" HTML element storing the product href
rempec_href <- rempec_webpage %>% html_nodes("div.photoAlbumEntry") %>% html_element("a") %>% html_attr("href")
# the "" HTML element and "" HTML attribute storing the product title 
rempec_title <- rempec_webpage %>% html_nodes("span.photoAlbumEntryTitle") %>% html_text2()

# converting the lists contain the scraped data into a data.frame ----
rempec_outputs <- data.frame( 
  unlist(rempec_href), 
  unlist(rempec_title)
) 

# changing the column names of the data frame before exporting it into CSV ----
names(rempec_outputs) <- c("href", "title") 

# export the data frame containing the scraped data to a CSV file ----
write.csv(rempec_outputs, file = "./rempec_outputs.csv", fileEncoding = "UTF-8", row.names = FALSE)
