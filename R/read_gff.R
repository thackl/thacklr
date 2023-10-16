#' Read a .gff file
#'
#' Uses `rtracklayer::import`.
#'
#' @param gff_file to read
#' @param sources only return features from these sources
#' @param types only return features of these types, e.g. gene, CDS, ...
#' @export
#' @return tibble
read_gff <- function(gff_file, sources=NULL, types=NULL){
  if (!requireNamespace("rtracklayer", quietly = TRUE)) {
    stop("Reading .gffs requires package 'rtracklayer' to be installed.",
         call. = FALSE)
  }

  gff <- as_tibble(rtracklayer::import(gff_file)) %>%
    mutate_if(is.factor, as.character) %>%
    mutate_if(is_AsIs, unAsIs) %>%
    rename(seq_id=1, feature_id=ID, name=Name, parent_id=Parent) %>%
    # parent_id is a list col, because one feature can have multiple parents -
    # solve this by "cloning" the feature
    unnest(parent_id, keep_empty=TRUE) %>%
    mutate(gene_id=ancestor(.))

  if(!is.null(sources)) gff <- filter(gff, `source` %in% sources)
  if(!is.null(types)) gff <- filter(gff, type %in% types)
  rlang::inform("Features read:")
  print(dplyr::count(gff, `source`, type))
  gff
}

#' Compute the ancestor of a gff feature
#'
#' Bubble up the feature_id -> parent_id associations until the top-level ID is
#' found. This is the ancestor_id, shared by all associated features.
ancestor <- function(.data, kid = feature_id, parent = parent_id){
  data <- select(.data, kid={{kid}}, parent={{parent}})
  map_chr(split(data, seq_len(nrow(data))),
    function(r) {
      p <- r$parent
      if(is.na(p)) return(r$kid)
      g <- data$parent[data$kid == p]
      if(is.na(g)) return(p)
      if(length(g)>1)
        rlang::abort("parents with multiple grantparent not yet supported")
      if(!is.na(data$parent[data$kid == g])){
        rlang::abort("deep ancestries (>3 levels) are not supported")
      }
      g
    })
}

is_AsIs <- function(x) inherits(x, "AsIs")

unAsIs <- function(x) {
  if("AsIs" %in% class(x))
    class(x) <- class(x)[-match("AsIs", class(x))]
  x
}
