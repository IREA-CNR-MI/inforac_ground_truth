library(magrittr)
# web scraping using this guide: https://www.zenrows.com/blog/web-scraping-r#step-1-install-rvest

# requirements ----
# utils::install.packages("rvest")
library("rvest")

# RAC Magazine ----
# 8 records
# retrieving the target web page
cprac_webpage_mag_output <- tibble::tibble(
  title = as.character(),
  handle_PID = as.character(),
  document_link = as.character(),
  language_ISO_639_1 = as.character(),
  year = as.character(),
  image_link = as.character(),
  file_extension = as.character(),
  abstract = as.character()
)
cprac_webpage_mag <- xml2::read_html("http://www.cprac.org/en/media/scp/rac-magazine")
# selecting HTML elements and attributes
rows_num <- cprac_webpage_mag %>% 
  html_elements("div.media_item") %>% 
  html_element("h3") %>% 
  length()
for (j in 1:rows_num) {
  lang_num <- cprac_webpage_mag %>% 
    html_nodes(
      xpath = paste0(
        "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div/div[", j, "]"
      )
    ) %>% 
    html_elements("a") %>% 
    length()
  mag_row_output <- tibble::tibble(
    title = cprac_webpage_mag %>% 
      html_nodes(
        xpath = paste0(
          "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div/div[", j, "]"
        )
      ) %>% 
      html_element("h3") %>% 
      html_text2() %>% 
      rep(each = (lang_num/4)),
    handle_PID = rep("", lang_num/4),
    image_link = rep("", lang_num/4),
    file_extension = cprac_webpage_mag %>% 
      html_nodes(
        xpath = paste0(
          "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div/div[", j, "]"
        )
      ) %>% 
      html_element("span") %>%
      html_text2() %>%
      stringr::str_sub(2, 4) %>% 
      rep(each = (lang_num/4)),
    language_ISO_639_1 = cprac_webpage_mag %>% 
      html_nodes(
        xpath = paste0(
          "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div/div[", j, "]"
        )
      ) %>% 
      html_element("span") %>%
      html_elements("a") %>%
      html_text2() %>%
      .[seq(1,length(.),4)],
    document_link = paste0(
      "http://www.cprac.org",
      cprac_webpage_mag %>% 
        html_nodes(
          xpath = paste0(
            "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div/div[", j, "]"
          )
        ) %>% 
        html_elements("div.gg_fileslinks") %>% 
        html_elements("a") %>% 
        html_attr("href") %>%
        .[seq(1,length(.),4)]
    ),
    year = rep("", lang_num/4),
    abstract = rep("", lang_num/4)
  )
  cprac_webpage_mag_output <- cprac_webpage_mag_output %>% 
    dplyr::add_row(
      mag_row_output
    )
}

# RAC Progress report ----
# 17 records
# retrieving the target web page
cprac_webpage_proRep_output <- tibble::tibble(
  title = as.character(),
  handle_PID = as.character(),
  document_link = as.character(),
  language_ISO_639_1 = as.character(),
  year = as.character(),
  image_link = as.character(),
  file_extension = as.character(),
  abstract = as.character()
)
cprac_webpage_proRep <- xml2::read_html("http://www.cprac.org/en/media/progress-report")
# selecting HTML elements and attributes
rows_num <- cprac_webpage_proRep %>% 
  html_elements("div.media_item") %>% 
  html_element("h3") %>% 
  length()
for (j in 1:rows_num) {
  lang_num <- cprac_webpage_proRep %>% 
    html_nodes(
      xpath = paste0(
        "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div/div[", j, "]"
      )
    ) %>% 
    html_elements("a") %>% 
    length()
  proRep_row_output <- tibble::tibble(
    title = cprac_webpage_proRep %>% 
      html_nodes(
        xpath = paste0(
          "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div/div[", j, "]"
        )
      ) %>% 
      html_element("h3") %>% 
      html_text2() %>% 
      rep(each = (lang_num/4)),
    handle_PID = rep("", lang_num/4),
    image_link = rep("", lang_num/4),
    file_extension = cprac_webpage_proRep %>% 
      html_nodes(
        xpath = paste0(
          "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div/div[", j, "]"
        )
      ) %>% 
      html_element("span") %>%
      html_text2() %>%
      stringr::str_sub(2, 4) %>% 
      rep(each = (lang_num/4)),
    language_ISO_639_1 = cprac_webpage_proRep %>% 
      html_nodes(
        xpath = paste0(
          "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div/div[", j, "]"
        )
      ) %>% 
      html_element("span") %>%
      html_elements("a") %>%
      html_text2() %>%
      .[seq(1,length(.),4)],
    document_link = paste0(
      "http://www.cprac.org",
      cprac_webpage_proRep %>% 
        html_nodes(
          xpath = paste0(
            "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div/div[", j, "]"
          )
        ) %>% 
        html_elements("div.gg_fileslinks") %>% 
        html_elements("a") %>% 
        html_attr("href") %>%
        .[seq(1,length(.),4)]
    ),
    year = rep("", lang_num/4),
    abstract = rep("", lang_num/4)
  )
  cprac_webpage_proRep_output <- cprac_webpage_proRep_output %>% 
    dplyr::add_row(
      proRep_row_output
    )
}

# RAC SCP News ----
# sono news in html e non PDF
# http://www.cprac.org/en/media/scp-news?page=0

# RAC MedClean
# 10*15*3+8*3 = 150*3+24 = 450+24 = 474 records
# retrieving the target web page
n <- 16
cprac_webpage_medClean_output <- tibble::tibble(
  title = as.character(),
  handle_PID = as.character(),
  document_link = as.character(),
  language_ISO_639_1 = as.character(),
  year = as.character(),
  image_link = as.character(),
  file_extension = as.character(),
  abstract = as.character()
)
for (i in 0:(n-1)) {
  cprac_webpage_medClean <- xml2::read_html(paste0("http://www.cprac.org/en/media/medclean?page=", i))
  # selecting HTML elements and attributes
  rows_num <- cprac_webpage_medClean %>% 
    html_elements("div.media_item") %>% 
    html_element("h3") %>% 
    length()
  medClean_output <- tibble::tibble(
    title = cprac_webpage_medClean %>% 
      html_elements("div.media_item") %>% 
      html_element("h3") %>% 
      html_text2() %>% 
      rep(each = 3),
    handle_PID = rep("", rows_num) %>% 
      rep(each = 3),
    image_link = rep("", rows_num) %>% 
      rep(each = 3),
    file_extension = cprac_webpage_medClean %>% 
      html_elements("div.media_item") %>% 
      html_element("span") %>%
      html_text2() %>%
      stringr::str_sub(2, 4) %>% 
      rep(each = 3),
    language_ISO_639_1 = cprac_webpage_medClean %>% 
      html_elements("div.media_item") %>% 
      html_element("span") %>%
      html_elements("a") %>%
      html_text2() %>%
      .[seq(1,length(.),4)],
    document_link = paste0(
      "http://www.cprac.org",
      cprac_webpage_medClean %>% 
        html_elements("div.media_item") %>% 
        html_elements("div.gg_fileslinks") %>% 
        html_elements("a") %>% 
        html_attr("href") %>%
        .[seq(1,length(.),4)]
      ),
    year = rep("", rows_num) %>% 
      rep(each = 3),
    abstract = rep("", rows_num) %>% 
      rep(each = 3)
  )
  cprac_webpage_medClean_output <- cprac_webpage_medClean_output %>% 
    dplyr::add_row(
      medClean_output
    )
}

# RAC Business cases ----
# 130 records
# retrieving the target web page
n <- 7
cprac_webpage_busCases_output <- tibble::tibble(
  title = as.character(),
  handle_PID = as.character(),
  document_link = as.character(),
  language_ISO_639_1 = as.character(),
  year = as.character(),
  image_link = as.character(),
  file_extension = as.character(),
  abstract = as.character()
)
for (i in 0:(n-1)) {
  cprac_webpage_busCases <- xml2::read_html(paste0("http://www.cprac.org/en/media/business-cases-of-green-entrepreneurs?page=", i))
  # selecting HTML elements and attributes
  rows_num <- cprac_webpage_busCases %>% 
    html_elements("div.media_item") %>% 
    html_element("h3") %>% 
    length()
  busCases_output <- tibble::tibble(
    title = cprac_webpage_busCases %>% 
      html_elements("div.media_item") %>% 
      html_element("h3") %>% 
      html_text2() %>% 
      rep(each = 2),
    handle_PID = rep("", rows_num) %>% 
      rep(each = 2),
    image_link = rep("", rows_num) %>% 
      rep(each = 2),
    file_extension = cprac_webpage_busCases %>% 
      html_elements("div.media_item") %>% 
      html_element("span") %>%
      html_text2() %>%
      stringr::str_sub(2, 4) %>% 
      rep(each = 2),
    language_ISO_639_1 = cprac_webpage_busCases %>% 
      html_elements("div.media_item") %>% 
      html_element("span") %>%
      html_elements("a") %>%
      html_text2() %>%
      .[seq(1,length(.),4)],
    document_link = paste0(
      "http://www.cprac.org",
      cprac_webpage_busCases %>% 
        html_elements("div.media_item") %>% 
        html_elements("div.gg_fileslinks") %>% 
        html_elements("a") %>% 
        html_attr("href") %>%
        .[seq(1,length(.),4)]
    ),
    year = rep("", rows_num) %>% 
      rep(each = 2),
    abstract = rep("", rows_num) %>% 
      rep(each = 2)
  )
  cprac_webpage_busCases_output <- cprac_webpage_busCases_output %>% 
    dplyr::add_row(
      busCases_output
    )
}

# RAC General Studies ----
# 36 records
# retrieving the target web page
n <- 2
cprac_webpage_genStud_output <- tibble::tibble(
  title = as.character(),
  handle_PID = as.character(),
  document_link = as.character(),
  language_ISO_639_1 = as.character(),
  year = as.character(),
  image_link = as.character(),
  file_extension = as.character(),
  abstract = as.character()
)
for (i in 0:(n-1)) {
  cprac_webpage_genStud <- xml2::read_html(paste0("http://www.cprac.org/en/media/studies/general-studies?page=", i))
  # selecting HTML elements and attributes
  rows_num <- cprac_webpage_genStud %>% 
    html_elements("div.media_item") %>% 
    html_element("h3") %>% 
    length()
  for (j in 1:rows_num) {
    lang_num <- cprac_webpage_genStud %>% 
      html_nodes(
        xpath = paste0(
          "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div[2]/div[", j, "]"
        )
      ) %>% 
      html_elements("a") %>% 
      length()
    genStud_row_output <- tibble::tibble(
      title = cprac_webpage_genStud %>% 
        html_nodes(
          xpath = paste0(
            "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div[2]/div[", j, "]"
          )
        ) %>% 
        html_element("h3") %>% 
        html_text2() %>% 
        rep(each = (lang_num/4)),
      handle_PID = rep("", lang_num/4),
      image_link = rep("", lang_num/4),
      file_extension = cprac_webpage_genStud %>% 
        html_nodes(
          xpath = paste0(
            "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div[2]/div[", j, "]"
          )
        ) %>% 
        html_element("span") %>%
        html_text2() %>%
        stringr::str_sub(2, 4) %>% 
        rep(each = (lang_num/4)),
      language_ISO_639_1 = cprac_webpage_genStud %>% 
        html_nodes(
          xpath = paste0(
            "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div[2]/div[", j, "]"
          )
        ) %>% 
        html_element("span") %>%
        html_elements("a") %>%
        html_text2() %>%
        .[seq(1,length(.),4)],
      document_link = paste0(
        "http://www.cprac.org",
        cprac_webpage_genStud %>% 
          html_nodes(
            xpath = paste0(
              "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div[2]/div[", j, "]"
            )
          ) %>% 
          html_elements("div.gg_fileslinks") %>% 
          html_elements("a") %>% 
          html_attr("href") %>%
          .[seq(1,length(.),4)]
      ),
      year = rep("", lang_num/4),
      abstract = rep("", lang_num/4)
    )
    cprac_webpage_genStud_output <- cprac_webpage_genStud_output %>% 
      dplyr::add_row(
        genStud_row_output
      )
  }
}

# RAC Sector Studies ----
# 19 + 9*3 + 4 + 6*3 = 19+27+4+18 = 68 records
# NB il primo documento della pagina 1 si rifà non a PDF ma a pagine web
# retrieving the target web page
n <- 3
cprac_webpage_secStud_output <- tibble::tibble(
  title = as.character(),
  handle_PID = as.character(),
  document_link = as.character(),
  language_ISO_639_1 = as.character(),
  year = as.character(),
  image_link = as.character(),
  file_extension = as.character(),
  abstract = as.character()
)
for (i in 0:(n-1)) {
  cprac_webpage_secStud <- xml2::read_html(paste0("http://www.cprac.org/en/media/studies/sector-studies?page=", i))
  # selecting HTML elements and attributes
  rows_num <- cprac_webpage_secStud %>% 
    html_elements("div.media_item") %>% 
    html_element("h3") %>% 
    length()
  if (i == 0) {
    for (j in 2:rows_num) {
      lang_num <- cprac_webpage_secStud %>% 
        html_nodes(
          xpath = paste0(
            "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div[2]/div[", j, "]"
          )
        ) %>% 
        html_elements("a") %>% 
        length()
      secStud_row_output <- tibble::tibble(
        title = cprac_webpage_secStud %>% 
          html_nodes(
            xpath = paste0(
              "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div[2]/div[", j, "]"
            )
          ) %>% 
          html_element("h3") %>% 
          html_text2() %>% 
          rep(each = (lang_num/4)),
        handle_PID = rep("", lang_num/4),
        image_link = rep("", lang_num/4),
        file_extension = cprac_webpage_secStud %>% 
          html_nodes(
            xpath = paste0(
              "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div[2]/div[", j, "]"
            )
          ) %>% 
          html_element("span") %>%
          html_text2() %>%
          stringr::str_sub(2, 4) %>% 
          rep(each = (lang_num/4)),
        language_ISO_639_1 = cprac_webpage_secStud %>% 
          html_nodes(
            xpath = paste0(
              "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div[2]/div[", j, "]"
            )
          ) %>% 
          html_element("span") %>%
          html_elements("a") %>%
          html_text2() %>%
          .[seq(1,length(.),4)],
        document_link = paste0(
          "http://www.cprac.org",
          cprac_webpage_secStud %>% 
            html_nodes(
              xpath = paste0(
                "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div[2]/div[", j, "]"
              )
            ) %>% 
            html_elements("div.gg_fileslinks") %>% 
            html_elements("a") %>% 
            html_attr("href") %>%
            .[seq(1,length(.),4)]
        ),
        year = rep("", lang_num/4),
        abstract = rep("", lang_num/4)
      )
      cprac_webpage_secStud_output <- cprac_webpage_secStud_output %>% 
        dplyr::add_row(
          secStud_row_output
        )
    }
  } else {
    for (j in 1:rows_num) {
      lang_num <- cprac_webpage_secStud %>% 
        html_nodes(
          xpath = paste0(
            "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div[2]/div[", j, "]"
          )
        ) %>% 
        html_elements("a") %>% 
        length()
      secStud_row_output <- tibble::tibble(
        title = cprac_webpage_secStud %>% 
          html_nodes(
            xpath = paste0(
              "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div[2]/div[", j, "]"
            )
          ) %>% 
          html_element("h3") %>% 
          html_text2() %>% 
          rep(each = (lang_num/4)),
        handle_PID = rep("", lang_num/4),
        image_link = rep("", lang_num/4),
        file_extension = cprac_webpage_secStud %>% 
          html_nodes(
            xpath = paste0(
              "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div[2]/div[", j, "]"
            )
          ) %>% 
          html_element("span") %>%
          html_text2() %>%
          stringr::str_sub(2, 4) %>% 
          rep(each = (lang_num/4)),
        language_ISO_639_1 = cprac_webpage_secStud %>% 
          html_nodes(
            xpath = paste0(
              "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div[2]/div[", j, "]"
            )
          ) %>% 
          html_element("span") %>%
          html_elements("a") %>%
          html_text2() %>%
          .[seq(1,length(.),4)],
        document_link = paste0(
          "http://www.cprac.org",
          cprac_webpage_secStud %>% 
            html_nodes(
              xpath = paste0(
                "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div[2]/div[", j, "]"
              )
            ) %>% 
            html_elements("div.gg_fileslinks") %>% 
            html_elements("a") %>% 
            html_attr("href") %>%
            .[seq(1,length(.),4)]
        ),
        year = rep("", lang_num/4),
        abstract = rep("", lang_num/4)
      )
      cprac_webpage_secStud_output <- cprac_webpage_secStud_output %>% 
        dplyr::add_row(
          secStud_row_output
        )
    }
  }
}

# RAC Methodological Manuals ----
# 30 records
# NB il primo e l'ultimo documento della pagina 1 si rifanno non a PDF ma a pagine web
# retrieving the target web page
n <- 2
cprac_webpage_metMan_output <- tibble::tibble(
  title = as.character(),
  handle_PID = as.character(),
  document_link = as.character(),
  language_ISO_639_1 = as.character(),
  year = as.character(),
  image_link = as.character(),
  file_extension = as.character(),
  abstract = as.character()
)
for (i in 0:(n-1)) {
  cprac_webpage_metMan <- xml2::read_html(paste0("http://www.cprac.org/en/media/studies/methodological-manuals?page=", i))
  # selecting HTML elements and attributes
  rows_num <- cprac_webpage_metMan %>% 
    html_elements("div.media_item") %>% 
    html_element("h3") %>% 
    length()
  if (i == 0) {
    for (j in 2:(rows_num-1)) {
      lang_num <- cprac_webpage_metMan %>% 
        html_nodes(
          xpath = paste0(
            "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div[2]/div[", j, "]"
          )
        ) %>% 
        html_elements("a") %>% 
        length()
      metMan_row_output <- tibble::tibble(
        title = cprac_webpage_metMan %>% 
          html_nodes(
            xpath = paste0(
              "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div[2]/div[", j, "]"
            )
          ) %>% 
          html_element("h3") %>% 
          html_text2() %>% 
          rep(each = (lang_num/4)),
        handle_PID = rep("", lang_num/4),
        image_link = rep("", lang_num/4),
        file_extension = cprac_webpage_metMan %>% 
          html_nodes(
            xpath = paste0(
              "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div[2]/div[", j, "]"
            )
          ) %>% 
          html_element("span") %>%
          html_text2() %>%
          stringr::str_sub(2, 4) %>% 
          rep(each = (lang_num/4)),
        language_ISO_639_1 = cprac_webpage_metMan %>% 
          html_nodes(
            xpath = paste0(
              "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div[2]/div[", j, "]"
            )
          ) %>% 
          html_element("span") %>%
          html_elements("a") %>%
          html_text2() %>%
          .[seq(1,length(.),4)],
        document_link = paste0(
          "http://www.cprac.org",
          cprac_webpage_metMan %>% 
            html_nodes(
              xpath = paste0(
                "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div[2]/div[", j, "]"
              )
            ) %>% 
            html_elements("div.gg_fileslinks") %>% 
            html_elements("a") %>% 
            html_attr("href") %>%
            .[seq(1,length(.),4)]
        ),
        year = rep("", lang_num/4),
        abstract = rep("", lang_num/4)
      )
      cprac_webpage_metMan_output <- cprac_webpage_metMan_output %>% 
        dplyr::add_row(
          metMan_row_output
        )
    }
  } else {
    for (j in 1:rows_num) {
      lang_num <- cprac_webpage_metMan %>% 
        html_nodes(
          xpath = paste0(
            "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div[2]/div[", j, "]"
          )
        ) %>% 
        html_elements("a") %>% 
        length()
      metMan_row_output <- tibble::tibble(
        title = cprac_webpage_metMan %>% 
          html_nodes(
            xpath = paste0(
              "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div[2]/div[", j, "]"
            )
          ) %>% 
          html_element("h3") %>% 
          html_text2() %>% 
          rep(each = (lang_num/4)),
        handle_PID = rep("", lang_num/4),
        image_link = rep("", lang_num/4),
        file_extension = cprac_webpage_metMan %>% 
          html_nodes(
            xpath = paste0(
              "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div[2]/div[", j, "]"
            )
          ) %>% 
          html_element("span") %>%
          html_text2() %>%
          stringr::str_sub(2, 4) %>% 
          rep(each = (lang_num/4)),
        language_ISO_639_1 = cprac_webpage_metMan %>% 
          html_nodes(
            xpath = paste0(
              "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div[2]/div[", j, "]"
            )
          ) %>% 
          html_element("span") %>%
          html_elements("a") %>%
          html_text2() %>%
          .[seq(1,length(.),4)],
        document_link = paste0(
          "http://www.cprac.org",
          cprac_webpage_metMan %>% 
            html_nodes(
              xpath = paste0(
                "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div[2]/div[", j, "]"
              )
            ) %>% 
            html_elements("div.gg_fileslinks") %>% 
            html_elements("a") %>% 
            html_attr("href") %>%
            .[seq(1,length(.),4)]
        ),
        year = rep("", lang_num/4),
        abstract = rep("", lang_num/4)
      )
      cprac_webpage_metMan_output <- cprac_webpage_metMan_output %>% 
        dplyr::add_row(
          metMan_row_output
        )
    }
  }
}

# RAC General Leaflets ----
# 44 records
# retrieving the target web page
n <- 4
cprac_webpage_genLeaf_output <- tibble::tibble(
  title = as.character(),
  handle_PID = as.character(),
  document_link = as.character(),
  language_ISO_639_1 = as.character(),
  year = as.character(),
  image_link = as.character(),
  file_extension = as.character(),
  abstract = as.character()
)
for (i in 0:(n-1)) {
  cprac_webpage_genLeaf <- xml2::read_html(paste0("http://www.cprac.org/en/media/leaflets-and-brochures/general-leaflets?page=", i))
  # selecting HTML elements and attributes
  rows_num <- cprac_webpage_genLeaf %>% 
    html_elements("div.media_item") %>% 
    html_element("h3") %>% 
    length()
  for (j in 1:rows_num) {
    lang_num <- cprac_webpage_genLeaf %>% 
      html_nodes(
        xpath = paste0(
          "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div[2]/div[", j, "]"
        )
      ) %>% 
      html_elements("a") %>% 
      length()
    genLeaf_row_output <- tibble::tibble(
      title = cprac_webpage_genLeaf %>% 
        html_nodes(
          xpath = paste0(
            "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div[2]/div[", j, "]"
          )
        ) %>% 
        html_element("h3") %>% 
        html_text2() %>% 
        rep(each = (lang_num/4)),
      handle_PID = rep("", lang_num/4),
      image_link = rep("", lang_num/4),
      file_extension = cprac_webpage_genLeaf %>% 
        html_nodes(
          xpath = paste0(
            "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div[2]/div[", j, "]"
          )
        ) %>% 
        html_element("span") %>%
        html_text2() %>%
        stringr::str_sub(2, 4) %>% 
        rep(each = (lang_num/4)),
      language_ISO_639_1 = cprac_webpage_genLeaf %>% 
        html_nodes(
          xpath = paste0(
            "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div[2]/div[", j, "]"
          )
        ) %>% 
        html_element("span") %>%
        html_elements("a") %>%
        html_text2() %>%
        .[seq(1,length(.),4)],
      document_link = paste0(
        "http://www.cprac.org",
        cprac_webpage_genLeaf %>% 
          html_nodes(
            xpath = paste0(
              "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div[2]/div[", j, "]"
            )
          ) %>% 
          html_elements("div.gg_fileslinks") %>% 
          html_elements("a") %>% 
          html_attr("href") %>%
          .[seq(1,length(.),4)]
      ),
      year = rep("", lang_num/4),
      abstract = rep("", lang_num/4)
    )
    cprac_webpage_genLeaf_output <- cprac_webpage_genLeaf_output %>% 
      dplyr::add_row(
        genLeaf_row_output
      )
  }
}

# RAC Sectorial Leaflets ----
# 94 records
# retrieving the target web page
n <- 7
cprac_webpage_secLeaf_output <- tibble::tibble(
  title = as.character(),
  handle_PID = as.character(),
  document_link = as.character(),
  language_ISO_639_1 = as.character(),
  year = as.character(),
  image_link = as.character(),
  file_extension = as.character(),
  abstract = as.character()
)
for (i in 0:(n-1)) {
  cprac_webpage_secLeaf <- xml2::read_html(paste0("http://www.cprac.org/en/media/leaflets-and-brochures/sectorial-leaflets?page=", i))
  # selecting HTML elements and attributes
  rows_num <- cprac_webpage_secLeaf %>% 
    html_elements("div.media_item") %>% 
    html_element("h3") %>% 
    length()
  for (j in 1:rows_num) {
    lang_num <- cprac_webpage_secLeaf %>% 
      html_nodes(
        xpath = paste0(
          "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div[2]/div[", j, "]"
        )
      ) %>% 
      html_elements("a") %>% 
      length()
    secLeaf_row_output <- tibble::tibble(
      title = cprac_webpage_secLeaf %>% 
        html_nodes(
          xpath = paste0(
            "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div[2]/div[", j, "]"
          )
        ) %>% 
        html_element("h3") %>% 
        html_text2() %>% 
        rep(each = (lang_num/4)),
      handle_PID = rep("", lang_num/4),
      image_link = rep("", lang_num/4),
      file_extension = cprac_webpage_secLeaf %>% 
        html_nodes(
          xpath = paste0(
            "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div[2]/div[", j, "]"
          )
        ) %>% 
        html_element("span") %>%
        html_text2() %>%
        stringr::str_sub(2, 4) %>% 
        rep(each = (lang_num/4)),
      language_ISO_639_1 = cprac_webpage_secLeaf %>% 
        html_nodes(
          xpath = paste0(
            "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div[2]/div[", j, "]"
          )
        ) %>% 
        html_element("span") %>%
        html_elements("a") %>%
        html_text2() %>%
        .[seq(1,length(.),4)],
      document_link = paste0(
        "http://www.cprac.org",
        cprac_webpage_secLeaf %>% 
          html_nodes(
            xpath = paste0(
              "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div[2]/div[", j, "]"
            )
          ) %>% 
          html_elements("div.gg_fileslinks") %>% 
          html_elements("a") %>% 
          html_attr("href") %>%
          .[seq(1,length(.),4)]
      ),
      year = rep("", lang_num/4),
      abstract = rep("", lang_num/4)
    )
    cprac_webpage_secLeaf_output <- cprac_webpage_secLeaf_output %>% 
      dplyr::add_row(
        secLeaf_row_output
      )
  }
}

# RAC Methodological Leaflets ----
# 7 records
# retrieving the target web page
cprac_webpage_metLeaf_output <- tibble::tibble(
  title = as.character(),
  handle_PID = as.character(),
  document_link = as.character(),
  language_ISO_639_1 = as.character(),
  year = as.character(),
  image_link = as.character(),
  file_extension = as.character(),
  abstract = as.character()
)
cprac_webpage_metLeaf <- xml2::read_html(paste0("http://www.cprac.org/en/media/leaflets-and-brochures/methodological-leaflets"))
# selecting HTML elements and attributes
rows_num <- cprac_webpage_metLeaf %>% 
  html_elements("div.media_item") %>% 
  html_element("h3") %>% 
  length()
for (j in 1:rows_num) {
  lang_num <- cprac_webpage_metLeaf %>% 
    html_nodes(
      xpath = paste0(
        "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div/div[", j, "]"
      )
    ) %>% 
    html_elements("a") %>% 
    length()
  metLeaf_row_output <- tibble::tibble(
    title = cprac_webpage_metLeaf %>% 
      html_nodes(
        xpath = paste0(
          "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div/div[", j, "]"
        )
      ) %>% 
      html_element("h3") %>% 
      html_text2() %>% 
      rep(each = (lang_num/4)),
    handle_PID = rep("", lang_num/4),
    image_link = rep("", lang_num/4),
    file_extension = cprac_webpage_metLeaf %>% 
      html_nodes(
        xpath = paste0(
          "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div/div[", j, "]"
        )
      ) %>% 
      html_element("span") %>%
      html_text2() %>%
      stringr::str_sub(2, 4) %>% 
      rep(each = (lang_num/4)),
    language_ISO_639_1 = cprac_webpage_metLeaf %>% 
      html_nodes(
        xpath = paste0(
          "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div/div[", j, "]"
        )
      ) %>% 
      html_element("span") %>%
      html_elements("a") %>%
      html_text2() %>%
      .[seq(1,length(.),4)],
    document_link = paste0(
      "http://www.cprac.org",
      cprac_webpage_metLeaf %>% 
        html_nodes(
          xpath = paste0(
            "/html/body/div[1]/div/div/div[5]/div[3]/div/div[2]/div[2]/div/div/div[", j, "]"
          )
        ) %>% 
        html_elements("div.gg_fileslinks") %>% 
        html_elements("a") %>% 
        html_attr("href") %>%
        .[seq(1,length(.),4)]
    ),
    year = rep("", lang_num/4),
    abstract = rep("", lang_num/4)
  )
  cprac_webpage_metLeaf_output <- cprac_webpage_metLeaf_output %>% 
    dplyr::add_row(
      metLeaf_row_output
    )
}

# total output ----
# 908 records
cprac_webpage_output <- tibble::tibble(
  title = as.character(),
  handle_PID = as.character(),
  document_link = as.character(),
  language_ISO_639_1 = as.character(),
  year = as.character(),
  image_link = as.character(),
  file_extension = as.character(),
  abstract = as.character()
)
cprac_webpage_output <- cprac_webpage_output %>% 
  dplyr::add_row(cprac_webpage_mag_output) %>% 
  dplyr::add_row(cprac_webpage_proRep_output) %>% 
  dplyr::add_row(cprac_webpage_medClean_output) %>% 
  dplyr::add_row(cprac_webpage_busCases_output) %>% 
  dplyr::add_row(cprac_webpage_genStud_output) %>% 
  dplyr::add_row(cprac_webpage_secStud_output) %>% 
  dplyr::add_row(cprac_webpage_metMan_output) %>% 
  dplyr::add_row(cprac_webpage_genLeaf_output) %>% 
  dplyr::add_row(cprac_webpage_secLeaf_output) %>% 
  dplyr::add_row(cprac_webpage_metLeaf_output) %>% 
  dplyr::mutate(language_ISO_639_1 = dplyr::case_when(
    language_ISO_639_1 == "english" ~ "en",
    language_ISO_639_1 == "español" ~ "es",
    language_ISO_639_1 == "français" ~ "fr",
    language_ISO_639_1 == "català" ~ "ca",
    language_ISO_639_1 == "arabic" ~ "ar"
  )) %>% 
  dplyr::mutate(file_extension = dplyr::case_when(
    file_extension == "PDF" ~ "pdf"
  ))

# export the data frame containing the scraped data to a CSV file ----
write.csv(cprac_webpage_output, file = "./cprac_webpage_outputs.csv", fileEncoding = "UTF-8", row.names = FALSE)
