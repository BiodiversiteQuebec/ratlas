guess_schema <- function(endpoint) {
  row <- get_gen(endpoint, limit = 1)
  # try(row["id"] <- NULL)
  # for (name in names(row)) {
  #   if (is.na(row[[name]])) {
  #     row[name] <- NULL
  #   }list
  # }
  schema <- lapply(row, class)
  return(schema)
}

#' @export
schema.tables <- list(
    taxa = list(
        id = list(type = "integer"),
        scientific_name = list(type = "character"),
        rank = list(type = "enum_ranks"),
        valid = list(type = "logical"),
        valid_taxa_id = list(type = "integer"),
        gbif = list(type = "integer"),
        col = list(type = "integer"),
        family = list(type = "character"),
        species_gr = list(type = "enum_sp_categories"),
        authorship = list(type = "character"),
        updated_at = list(type = "date"),
        qc_status = list(type = "enum_qc"),
        exotic = list(type = "logical")
    ),
    datasets = list(
        id = list(type = "integer"),
        original_source = list(type = "character"),
        org_dataset_id = list(type = "character"),
        creator = list(type = "character"),
        title = list(type = "character"),
        publisher = list(type = "character"),
        modified = list(type = "character"),
        keywords = list("character"),
        abstract = list(type = "character"),
        type_sampling = list(type = "character"),
        type_obs = list(type = "enum_type_observation"),
        intellectual_rights = list(type = "character"),
        license = list(type = "character"),
        owner = list(type = "character"),
        methods = list(type = "character"),
        open_data = list(type = "logical"),
        exhaustive = list(type = "logical"),
        direct_obs = list(type = "logical"),
        centroid = list(type = "logical")
    ),
    observations = list(
        id = list(type = "integer"),
        org_parent_event = list(type = "character"),
        org_event = list(type = "character"),
        org_id_obs = list(type = "character"),
        id_datasets = list(type = "integer", int_range = c(1, 500)),
        year_obs = list(type = "integer"),
        month_obs = list(type = "integer"),
        day_obs = list(type = "integer"),
        time_obs = list(type = "time"),
        id_taxa = list(type = "integer", int_range = c(1, 4365)),
        id_variables = list(type = "integer", int_range = c(1, 9)),
        obs_value = list(type = "numeric"),
        issue = list(type = "logical"),
        geom = list(type = "geometry")
    ),
    variables = list(
        id = list(type = "integer"),
        name = list(type = "character"),
        unit = list(type = "character"),
        description = list(type = "character")
    ),
    obs_efforts = list(
        id_obs = list(type = "integer", int_range = c(1, 60)),
        id_efforts = list(type = "integer")
    ),
    efforts = list(
        id = list(type = "integer"),
        id_variables = list(type = "integer", int_range = c(1, 26)),
        effort_value = list(type = "numeric")
    )
)