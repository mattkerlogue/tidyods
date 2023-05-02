# ODS XML file handling

# unzip and ods file and get content xml
unzip_ods_xml <- function(ods_file) {

  temp_dir <- tempdir()

  # if remote file location download the file to temp_dir
  if (grepl("^((http|ftp)s?|sftp)://", ods_file)) {
    cli::cli_progress_step("Reading remote file")
    utils::download.file(
      url = ods_file,
      destfile = file.path(temp_dir, basename(ods_file)),
      quiet = TRUE
    )
    ods_file <- file.path(temp_dir, basename(ods_file))
  } else if (!file.exists(ods_file)) {
    cli::cli_abort("{.file {ods_file}} does not exist")
  }

  if (tolower(tools::file_ext(ods_file) != "ods")) {
    cli::cli_abort("{.file {ods_file}} does not have a valid extension, .ods")
  }

  zip::unzip(ods_file, files = "content.xml", overwrite = TRUE,
             junkpaths = TRUE, exdir = temp_dir)

  return(file.path(temp_dir, "content.xml"))

}

# read the extracted XML file
extract_ods_xml <- function(ods_file) {

  xml_file <- unzip_ods_xml(ods_file)

  if (!file.exists(xml_file)){
    cli::cli_abort("Error in unzip procedure")
  }

  return(xml2::read_xml(xml_file))

}
