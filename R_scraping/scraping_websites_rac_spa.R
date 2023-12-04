# web scraping using this guide: https://www.zenrows.com/blog/web-scraping-r#step-1-install-rvest

# requirements ----
# utils::install.packages("rvest")
library("rvest")

# retrieving the target web page ----
rac_spa_webpage <- xml2::read_html("https://www.rac-spa.org/publications#enup")

# selecting HTML elements and attributes ----
# the "h5" HTML element storing the product Scope
rac_spa_scope <- rac_spa_webpage %>% html_elements("tr") %>% html_element("h5") %>% html_text2()
# storing the product PDF and img
rac_spa_pdf1 <- rac_spa_webpage %>% html_elements("tr") %>% html_node("td:nth-child(1)") %>% html_element("a") %>% html_attr("href")
rac_spa_img1 <- rac_spa_webpage %>% html_elements("tr") %>% html_node("td:nth-child(1)") %>% html_element("img") %>% html_attr("src")
rac_spa_pdf2 <- rac_spa_webpage %>% html_elements("tr") %>% html_node("td:nth-child(2)") %>% html_element("a") %>% html_attr("href")
rac_spa_img2 <- rac_spa_webpage %>% html_elements("tr") %>% html_node("td:nth-child(2)") %>% html_element("img") %>% html_attr("src")
rac_spa_pdf3 <- rac_spa_webpage %>% html_elements("tr") %>% html_node("td:nth-child(3)") %>% html_element("a") %>% html_attr("href")
rac_spa_img3 <- rac_spa_webpage %>% html_elements("tr") %>% html_node("td:nth-child(3)") %>% html_element("img") %>% html_attr("src")
# storing the product title and year
rac_spa_title1 <- rac_spa_webpage %>% html_elements("tr") %>% html_node("td:nth-child(1)") %>% html_node("p:nth-child(1)") %>% html_element("strong") %>% html_text2()
rac_spa_year1 <- rac_spa_webpage %>% html_elements("tr") %>% html_node("td:nth-child(1)") %>% html_node("p:nth-child(2)") %>% html_text2()
rac_spa_title2 <- rac_spa_webpage %>% html_elements("tr") %>% html_node("td:nth-child(2)") %>% html_node("p:nth-child(1)") %>% html_element("strong") %>% html_text2()
rac_spa_year2 <- rac_spa_webpage %>% html_elements("tr") %>% html_node("td:nth-child(2)") %>% html_node("p:nth-child(2)") %>% html_text2()
rac_spa_title3 <- rac_spa_webpage %>% html_elements("tr") %>% html_node("td:nth-child(3)") %>% html_node("p:nth-child(1)") %>% html_element("strong") %>% html_text2()
rac_spa_year3 <- rac_spa_webpage %>% html_elements("tr") %>% html_node("td:nth-child(3)") %>% html_node("p:nth-child(2)") %>% html_text2()

# converting the lists contain the scraped data into a data.frame ----
rac_spa_outputs <- data.frame( 
  unlist(rac_spa_scope), 
  unlist(rac_spa_pdf1), 
  unlist(rac_spa_img1), 
  unlist(rac_spa_pdf2), 
  unlist(rac_spa_img2), 
  unlist(rac_spa_pdf3), 
  unlist(rac_spa_img3), 
  unlist(rac_spa_title1),
  unlist(rac_spa_year1),
  unlist(rac_spa_title2),
  unlist(rac_spa_year2),
  unlist(rac_spa_title3),
  unlist(rac_spa_year3)
) 

# changing the column names of the data frame before exporting it into CSV ----
names(rac_spa_outputs) <- c("scope", "pdf1", "img1", "pdf2", "img2", "pdf3", "img3", "title1", "year1", "title2", "year2", "title3", "year3")

# export the data frame containing the scraped data to a CSV file ----
write.csv(rac_spa_outputs, file = "./rac_spa_outputs.csv", fileEncoding = "UTF-8", row.names = FALSE)

