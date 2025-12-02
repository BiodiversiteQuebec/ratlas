#' Function to insert datasets in Atlas.
#' Formatting should follow https://eml.ecoinformatics.org/schema/
#'
#' This function is designed to interface with a web API deployed with PostgREST
#' Function is not exported, as it is meant for internal use only.
#' It can be called like so: ratlas:::post_dataset()
#'
#' EML parameters:
#' @param title `character`. Name of the dataset.
#' @param creator `list`. List of responsibleParty objects created with EML::set_responsibleParty().
#' @param id Optional. `character`. UUID of the dataset. If not provided, a new UUID will be generated.
#' @param pub_date Optional. `character`. Publication date of the dataset in "YYYY-MM-DD" format.
#' @param abstract Optional. `character`. Abstract of the dataset.
#' @param keyword_set Optional. `list`. List of keywordSet objects created manually.
#' @param distribution Optional. `list`. List of list distribution objects created manually.
#' @param alternate_identifier Optional. `list`. List of alternateIdentifier `character`. DOI is declared here.
#' @param licensed Optional. `list`. List of license informations created manually.
#' @param associated_party Optional. `list`. List of responsibleParty objects created manually.
#' @param contact `list`. List of responsibleParty objects created with EML::set_responsibleParty().
#' @param metadata_provider Optional. `list`. List of responsibleParty objects created with EML::set_responsibleParty().
#' @param additional_info Optional. `list`. List of additional information about the dataset created with EML::set_TextType() to a markdown tag.
#' @param language Optional. `character`. Language of the dataset.
#' @param publisher Optional. `list`. List of responsibleParty objects created with EML::set_responsibleParty().
#' @param coverage Optional. `list`. List of coverage objects created with EML::set_coverage().
#' @param methods Optional. `jsonb`. Methods section created with EML::set_methods() or manually.
#' @param additional_metadata Optional. `character`. Citation string, nested in metadata, citation.
#'
#' Non-EML parameters:
#' @param data_type `character`. Type of data, example: occurrence, time series, etc.
#' @param source_eml_url Optional. `character`. URL of the source EML.
#' @param source_alias `character`. Internal alias to recognize the dataset.
#' @param shareable_data `boolean`. Default `FALSE`, if `TRUE` it means we have the license to share the data.
#' @param schema obligatory api, as function resides in this schema on the backend.

#' @return newly generated dataset UUID.
#' @keywords internal

post_dataset <- function(
  title,
  creator,
  id = NULL,
  pub_date = NULL,
  abstract = NULL,
  keyword_set = NULL,
  distribution = NULL,
  alternate_identifier = NULL,
  licensed = NULL,
  associated_party = NULL,
  contact,
  metadata_provider = NULL,
  additional_info = NULL,
  language = NULL,
  publisher = NULL,
  coverage = NULL,
  methods = NULL,
  additional_metadata = NULL,
  data_type,
  source_eml_url = NULL,
  source_alias,
  shareable_data = FALSE,
  schema = "api"
) {
  # Validate id is id if provided
  if (!is.null(id)) {
    if (!grepl("^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$", id)) {
      stop("The provided id is not a valid UUID.")
    }
  } else {
    id <- uuid::UUIDgenerate()
  }

  # Build EML
  eml <- list(
    packageId = id,
    system = "uuid",
    dataset = list(
      title = title,
      creator = creator,
      pubDate = pub_date,
      abstract = abstract,
      keywordSet = keyword_set,
      distribution = distribution,
      alternateIdentifier = alternate_identifier,
      licensed = licensed,
      associatedParty = associated_party,
      contact = contact,
      metadataProvider = metadata_provider,
      additionalInfo = additional_info,
      language = language,
      publisher = publisher,
      coverage = coverage,
      methods = methods
    ),
    additionalMetadata = additional_metadata
  )

  EML::eml_validate(eml)

  # Write to temp file
  tmpfile <- tempfile(fileext = ".xml")
  EML::write_eml(eml, tmpfile)

  # Read back as valid XML string
  eml_xml <- paste(readLines(tmpfile, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  # Call your db_call_function
  result <- db_call_function(
    name = "dataset_insert_eml",
    schema = schema,
    eml_input = eml_xml,
    methods = methods,
    data_type = data_type,
    source_eml_url = source_eml_url,
    source_alias = source_alias,
    shareable_data = shareable_data
  )

  return(result)

}