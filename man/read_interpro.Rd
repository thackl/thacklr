% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_interpro.R
\name{read_interpro}
\alias{read_interpro}
\title{Read InterProScan .tsv files.}
\source{
https://github.com/ebi-pf-team/interproscan/wiki/OutputFormats
}
\usage{
read_interpro(file)
}
\arguments{
\item{file}{Either a path to a file, a connection, or literal data
(either a single string or a raw vector).

Files ending in \code{.gz}, \code{.bz2}, \code{.xz}, or \code{.zip} will
be automatically uncompressed. Files starting with \verb{http://},
\verb{https://}, \verb{ftp://}, or \verb{ftps://} will be automatically
downloaded. Remote gz files can also be automatically downloaded and
decompressed.

Literal data is most useful for examples and tests. To be recognised as
literal data, the input must be either wrapped with \code{I()}, be a string
containing at least one new line, or be a vector containing at least one
string with a new line.

Using a value of \code{\link[readr:clipboard]{clipboard()}} will read from the system clipboard.}
}
\value{
tibble
}
\description{
Columns are:
  1. protein_id - Protein Accession (e.g. P51587)
  2. protein_md5 - Sequence MD5 digest (e.g. 14086411a2cdf1c4cba63020e1622579)
  3. protein_length - Sequence Length (e.g. 3418)
  4. analysis - Analysis (e.g. Pfam / PRINTS / Gene3D)
  5. signature_id - Signature Accession (e.g. PF09103 / G3DSA:2.40.50.140)
  6. signature_desc - Signature Description (e.g. BRCA2 repeat profile)
  7. start - Start location
  8. end - Stop location
  9. score - is the e-value (or score) of the match reported by member database method (e.g. 3.1E-52)
  10. status - is the status of the match (T: true)
  11. date - is the date of the run
  12. interpro_id - (InterPro annotations - accession (e.g. IPR002093) - optional column; only displayed if -iprlookup option is switched on)
  13. interpro_desc - (InterPro annotations - description (e.g. BRCA2 repeat) - optional column; only displayed if -iprlookup option is switched on)
  14. go_terms - (GO annotations (e.g. GO:0005515) - optional column; only displayed if --goterms option is switched on)
  15. pathway_terms - (Pathways annotations (e.g. REACT_71) - optional column; only displayed if --pathways option is switched on)
}
\details{
Because `readr::read_tsv` expects a fixed number of columns, but interpro columsn 12-15 are optional, warnings like 15 cols expected, 11 cols seen should be ignored.
}
