taxa_groups <- list(
    "Amphibiens" = "Amphibia",
    "Oiseaux" = "Aves",
    "Mammifères" = "Mammalia",
    "Reptiles" = "Reptilia",
    "Poissons" = c(
        "Actinopterygii", "Cephalaspidomorphi", "Elasmobranchii",
        "Holocephali", "Myxini", "Sarcopterygii"
    ),
    "Tuniciers" = c("Appendicularia", "Ascidiacea", "Thaliacea"),
    "Céphalocordés" = "Leptocardii",
    "Arthropodes" = "Arthropoda",
    "Autres_invertébrés" = c(
        "Acanthocephala", "Annelida", "Brachiopoda", "Bryozoa",
        "Cephalorhyncha", "Chaetognatha", "Cnidaria", "Ctenophora",
        "Cycliophora", "Dicyemida", "Echinodermata", "Entoprocta",
        "Gastrotricha", "Gnathostomulida", "Hemichordata", "Micrognathozoa",
        "Mollusca", "Myxozoa", "Nematoda", "Nematomorpha", "Nemertea",
        "Onychophora", "Orthonectida", "Phoronida", "Placozoa",
        "Platyhelminthes", "Porifera", "Rotifera", "Sipuncula",
        "Tardigrada", "Xenacoelomorpha"
    ),
    "Autres_taxons" = c(
        "Archaea", "Bacteria", "Chromista", "Protozoa", "Viruses"
    ),
    "Mycètes" = "Fungi",
    "Angiospermes" = c("Liliopsida", "Magnoliopsida"),
    "Conifères" = "Pinopsida",
    "Cryptogames_vasculaires" = c("Lycopodiopsida", "Polypodiopsida"),
    "Autres_gymnospermes" = c("Cycadopsida", "Ginkgoopsida", "Gnetopsida"),
    "Algues" = c("Charophyta", "Chlorophyta", "Rhodophyta"),
    "Bryophytes" = "Bryophyta",
    "Autres_plantes" = c("Anthocerotophyta", "Glaucophyta", "Marchantiophyta")
)


taxa_exotic <- read.csv(
    'data-raw//exotic_sp.csv', stringsAsFactors=FALSE, header = FALSE
    )[,1]
taxa_status <- read.csv('data-raw//sp_qc_status.csv', stringsAsFactors=FALSE)

usethis::use_data(taxa_exotic, taxa_status, taxa_groups,
    overwrite = TRUE, internal = TRUE)
