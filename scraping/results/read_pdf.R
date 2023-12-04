library(magrittr)
# import csv file ----
unepMapDocs_pdf <- vroom::vroom("listOfDocuments_complete.csv", ",", escape_double = FALSE, trim_ws = TRUE) %>%
  dplyr::select(c(1:7)) %>%
  dplyr::mutate(id = dplyr::row_number(), .before = title) %>%
  dplyr::mutate(filename = basename(document_link)) %>% 
  dplyr::filter(file_extension == "pdf") %>% 
  dplyr::mutate(pdf_id = dplyr::row_number())

npad <- as.character(max(unepMapDocs_pdf$id)) %>% stringr::str_length()
pad_it <- function(x){
  paste0("UNEP_", stringr::str_pad(x, npad, "left", "0"))
}

unepMapDocs_pdf %<>% dplyr::mutate(filenameid = pad_it(id))
 
# download the pdf file and keep the errors ----
errs <- c()
for (i in unepMapDocs_pdf$pdf_id) {
  
  file_name <- paste0(
    unepMapDocs_pdf$filenameid[i],
    ".",
    unepMapDocs_pdf$file_extension[i]
  )
  
  tryCatch(
    download.file(
      url = unepMapDocs_pdf$document_link[i],
      destfile = paste0("PDFs/", file_name),
      mode = "wb",
      quiet = FALSE
    ),
    error = function(e) {
      errs <<- c(errs, paste0("", i))
    }
  )
}

liness <- as.integer(errs)
unepMapDocs_pdf$error <- c(NA, "Error")[(seq_len(nrow(unepMapDocs_pdf)) %in% liness) + 1]
# View(unepMapDocs_pdf)

# store the results ----
saveRDS(unepMapDocs_pdf, file = "unepMapDocs_pdf.rds")

# reading the corpus and transformation to text ----
# require(tm)
indir <- "PDFs"
outdir <- "pdf2txt"
if(!dir.exists(outdir))
  dir.create("pdf2txt")
# unepMapDocs_pdf <- readRDS(file = "unepMapDocs_pdf.rds")

processingFiles <- unepMapDocs_pdf %>%
  dplyr::filter(
    language_ISO_639_1 == "en",
    is.na(error)
  )

textifyExecution <- lapply(
  processingFiles$filenameid,
  FUN = function(pdfname) {
    errs_pdf_text <- "OK"
    tryCatch(
      expr = {
        extracted <- pdftools::pdf_text(paste0(indir, "/", pdfname, ".pdf"))
        dd1 <- paste0(extracted, collapse = " \n ")
        cat(dd1, file = paste0(outdir, "/", pdfname, ".txt"))
      },
      error=function(err){
        errs_pdf_text <<- err
      })
    return(c(pdfname, errs_pdf_text))
  }) 

result <- textifyExecution %>% 
  purrr::map_dfr(.f = function(el){
    fname = el[[1]]
    msg = el[[2]]
    return(data.frame(filenameid = fname, execmsg = msg))
}) %>%
  dplyr::as_tibble()

unepMapDocs_pdf %<>%
  dplyr::left_join(result) %>%
  dplyr::mutate(error_download = error, error_txt_write = execmsg, .keep = "unused")

saveRDS(unepMapDocs_pdf, file = "unepMapDocs_pdf_final.rds")
unepMapDocs_pdf <- readRDS(file = "unepMapDocs_pdf_final.rds")
utils::write.csv(unepMapDocs_pdf, "unepMapDocs_pdf_final.csv", row.names = FALSE)
