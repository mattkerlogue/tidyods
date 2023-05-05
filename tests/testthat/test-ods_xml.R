example_file <- system.file("extdata", "basic_example.ods", package = "tidyods")
excel_file <- system.file("extdata", "basic_example_excel.ods", package = "tidyods")

test_that("XML extraction (LibreOffice)", {
  expect_silent(example_ods <- extract_ods_xml(example_file))
  expect_type(example_ods, "list")
  expect_s3_class(example_ods, c("xml_document", "xml_node"), exact = TRUE)
  expect_length(example_ods, 2)
  expect_named(example_ods, c("node", "doc"))
  expect_equal(xml2::xml_length(example_ods), 4)
})

test_that("XML namespace (LibreOffice)", {
  example_ods <- extract_ods_xml(example_file)
  expect_silent(example_ns <- xml2::xml_ns(example_ods))
  expect_type(example_ns, "character")
  expect_s3_class(example_ns, "xml_namespace", exact = TRUE)
  expect_length(example_ns, 35)
  expect_named(
    example_ns,
    c("calcext",
      "chart",
      "css3t",
      "dc",
      "dom",
      "dr3d",
      "draw",
      "drawooo",
      "field",
      "fo",
      "form",
      "formx",
      "grddl",
      "loext",
      "math",
      "meta",
      "number",
      "of",
      "office",
      "ooo",
      "oooc",
      "ooow",
      "presentation",
      "rpt",
      "script",
      "style",
      "svg",
      "table",
      "tableooo",
      "text",
      "xforms",
      "xhtml",
      "xlink",
      "xsd",
      "xsi")
  )
  expect_equal(
    as.character(example_ns),
    c("urn:org:documentfoundation:names:experimental:calc:xmlns:calcext:1.0",
      "urn:oasis:names:tc:opendocument:xmlns:chart:1.0",
      "http://www.w3.org/TR/css3-text/",
      "http://purl.org/dc/elements/1.1/",
      "http://www.w3.org/2001/xml-events",
      "urn:oasis:names:tc:opendocument:xmlns:dr3d:1.0",
      "urn:oasis:names:tc:opendocument:xmlns:drawing:1.0",
      "http://openoffice.org/2010/draw",
      "urn:openoffice:names:experimental:ooo-ms-interop:xmlns:field:1.0",
      "urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0",
      "urn:oasis:names:tc:opendocument:xmlns:form:1.0",
      "urn:openoffice:names:experimental:ooxml-odf-interop:xmlns:form:1.0",
      "http://www.w3.org/2003/g/data-view#",
      "urn:org:documentfoundation:names:experimental:office:xmlns:loext:1.0",
      "http://www.w3.org/1998/Math/MathML",
      "urn:oasis:names:tc:opendocument:xmlns:meta:1.0",
      "urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0",
      "urn:oasis:names:tc:opendocument:xmlns:of:1.2",
      "urn:oasis:names:tc:opendocument:xmlns:office:1.0",
      "http://openoffice.org/2004/office",
      "http://openoffice.org/2004/calc",
      "http://openoffice.org/2004/writer",
      "urn:oasis:names:tc:opendocument:xmlns:presentation:1.0",
      "http://openoffice.org/2005/report",
      "urn:oasis:names:tc:opendocument:xmlns:script:1.0",
      "urn:oasis:names:tc:opendocument:xmlns:style:1.0",
      "urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0",
      "urn:oasis:names:tc:opendocument:xmlns:table:1.0",
      "http://openoffice.org/2009/table",
      "urn:oasis:names:tc:opendocument:xmlns:text:1.0",
      "http://www.w3.org/2002/xforms",
      "http://www.w3.org/1999/xhtml",
      "http://www.w3.org/1999/xlink",
      "http://www.w3.org/2001/XMLSchema",
      "http://www.w3.org/2001/XMLSchema-instance")
  )
  expect_equal(xml2::xml_name(example_ods, example_ns), "office:document-content")
})

test_that("XML extraction (Excel)", {
  expect_silent(excel_ods <- extract_ods_xml(excel_file))
  expect_type(excel_ods, "list")
  expect_s3_class(excel_ods, c("xml_document", "xml_node"), exact = TRUE)
  expect_length(excel_ods, 2)
  expect_named(excel_ods, c("node", "doc"))
  expect_equal(xml2::xml_length(excel_ods), 3)
})

test_that("XML namespace (Excel)", {
  excel_ods <- extract_ods_xml(excel_file)
  expect_silent(excel_ns <- xml2::xml_ns(excel_ods))
  expect_type(excel_ns, "character")
  expect_s3_class(excel_ns, "xml_namespace", exact = TRUE)
  expect_length(excel_ns, 15)
  expect_named(
    excel_ns,
    c("dc",
      "dr3d",
      "dr3d1",
      "dr3d2",
      "dr3d3",
      "draw",
      "fo",
      "number",
      "of",
      "office",
      "style",
      "svg",
      "table",
      "text",
      "xlink")
  )
  expect_equal(
    as.character(excel_ns),
    c("http://purl.org/dc/elements/1.1/",
      "urn:oasis:names:tc:opendocument:xmlns:dr3d:1.0",
      "urn:oasis:names:tc:opendocument:xmlns:dr3d:1.0",
      "urn:oasis:names:tc:opendocument:xmlns:dr3d:1.0",
      "urn:oasis:names:tc:opendocument:xmlns:dr3d:1.0",
      "urn:oasis:names:tc:opendocument:xmlns:drawing:1.0",
      "urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0",
      "urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0",
      "urn:oasis:names:tc:opendocument:xmlns:of:1.2",
      "urn:oasis:names:tc:opendocument:xmlns:office:1.0",
      "urn:oasis:names:tc:opendocument:xmlns:style:1.0",
      "urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0",
      "urn:oasis:names:tc:opendocument:xmlns:table:1.0",
      "urn:oasis:names:tc:opendocument:xmlns:text:1.0",
      "http://www.w3.org/1999/xlink")
  )
  expect_equal(xml2::xml_name(excel_ods, excel_ns), "office:document-content")
})
