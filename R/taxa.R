# TODO : numeric col, gbif

#' @export

Taxa <- function(id = NULL,
                 scientific_name,
                 rank,
                 valid,
                 valid_taxa_id = NULL,
                 authorship,
                 gbif = NULL,
                 col = NULL,
                 family = NULL,
                 species_gr,
                 qc_status = NULL,
                 exotic = NULL) {
    out <- as.list(environment())
    class(out) <- "Taxa"
    return(out)
}

#' @export

Taxa.from_global_names <- function(names) {
    ref <- get_taxa_global_names(names)

    out <- list()

    # Source ids and their parameter name ordered by preference
    source_ids <- c(
        "col" = 1,
        "gbif" = 11
    )

    for (i in length(names)) {
        data_source_ids <- sapply(
            ref[[i]]$preferredResults,
            function(v) v$dataSourceId
        )

        # Find first source_id in list to match available data source
        preffered_indx <- na.omit(match(source_ids, data_source_ids))[[1]]
        ref_source <- ref[[i]]$preferredResults[[preffered_indx]]
        ref_param <- names(source_ids)[
            data_source_ids[preffered_indx] == source_ids
        ]

        classification <- strsplit(
            ref_source$classificationPath, "|",
            fixed = TRUE
        )[[1]]
        names(classification) <- strsplit(
            ref_source$classificationRanks, "|",
            fixed = TRUE
        )[[1]]
        taxa <- Taxa(
            scientific_name = ref_source$matchedCanonicalSimple,
            rank = tail(names(classification), 1),
            family = classification["family"],
            valid = ref_source$recordId == ref_source$currentRecordId,
            authorship = stringr::str_match(
                ref_source$matchedName, "\\((.*?)\\)"
            )[2],
            species_gr = get_group(classification),
            qc_status = get_qc_status(ref_source$matchedCanonicalSimple),
            exotic = is_exotic(ref_source$matchedCanonicalSimple)
        )
        if (!taxa$valid) {
            taxa$valid_taxa_id <- ref_source$currentRecordId
        }
        taxa[ref_param] <- ref_source$recordId
        return(taxa)
    }
}

#' get_group
#' Find a match between classification of the taxon and the group object

get_group <- function(classification) {
    searched_taxas <- classification[
        which(names(classification) == "kingdom"):
        which(names(classification) == "family")
    ]

    # Select lower integer to get higher match (kingdom = 1 and family is the higher integer)
    # *** Sometimes a taxon can represent more than one phylum.
    # For example, Acanthocephala is a genus of "Arthropoda" phylum AND a phylum.
    # In that case, we want the higher rank (The phylum Acantocephala) ***

    out_group <- NULL
    for (taxa in searched_taxas) {
        for (i in 1:length(taxa_groups)) {
            if (taxa %in% taxa_groups[[i]]) {
                out_group <- names(taxa_groups[i])
            }
        }
    }

    return(out_group)
}

is_exotic <- function(scientific_name) {
    out <- scientific_name %in% taxa_exotic
    return(out)
}

get_qc_status <- function(scientific_name) {
    status_index <- match(taxa_status$scientific_name, scientific_name) %>%
        na.omit()
    if (length(status_index) > 0) {
        out <- taxa_status[status_index, "qc_status"]
    } else {
        out <- ""
    }
    return(out)
}