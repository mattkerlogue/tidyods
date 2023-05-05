# ODS XML file handling

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

extract_ods_xml <- function(ods_file) {

  check_xml_memory(ods_file)

  xml_file <- unzip_ods_xml(ods_file)

  if (!file.exists(xml_file)){
    cli::cli_abort("Error in unzip procedure")
  }

  return(xml2::read_xml(xml_file))

}

# check whether {xml2} is going to have problems reading the file
check_xml_memory <- function(path, verbose = FALSE) {

  zip_files <- zip::zip_list(path)

  content_size <- zip_files$uncompressed_size[zip_files$filename == "content.xml"]

  # libxml2 requires memory approx 4 times the size of the file
  xml2_req <- content_size * 4

  sys_mem <- ps::ps_system_memory()

  avail_mem <- sys_mem$avail

  if (xml2_req > avail_mem) {
    pretty_need <- prettyunits::pretty_bytes(xml2_req)
    pretty_avail <- prettyunits::pretty_bytes(avail_mem)
    cli::cli_abort(c(
      "ODS file is too large to process",
      "i" = "ODS XML is estimated to need {pretty_need} of memory",
      "x" = "Available system memory is estimated at {pretty_avail}"
    ))
  }

  if (verbose) {
    pretty_need <- prettyunits::pretty_bytes(xml2_req)
    pretty_avail <- prettyunits::pretty_bytes(avail_mem)
    cli::cli({
      cli::cli_alert_info("ODS XML is estimated to need {pretty_need} of memory")
      cli::cli_alert_success("Available system memory is estimated at {pretty_avail}")
    })
  }

  return(invisible(TRUE))

}

