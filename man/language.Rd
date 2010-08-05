\name{language}
\alias{parse_IETF_language_tag}
\title{Parse IETF Language Tag}
\description{
  Extract language, script, region and variant subtags from IETF
  language tags.
}
\usage{parse_IETF_language_tag(x, expand = FALSE)}
\arguments{
  \item{x}{a character vector with IETF language tags.}
  \item{expand}{a logical indicating whether to expand subtags into
    their description(s).}
}
\details{
  Internet Engineering Task Force (IETF) language tags are defined by
  IETF BCP 47, which is currently composed by the normative RFC 5646
  (\url{http://tools.ietf.org/html/rfc5646}) and RFC 4647
  (\url{http://tools.ietf.org/html/rfc4646}), along with the normative
  content of the IANA Language Subtag Registry regulated by these RFCs.
  These tags are used in a number of modern computing standards.

  Each language tag is composed of one or more \dQuote{subtags}
  separated by hyphens.  For the basic format currently supported, the
  subtags occur as follows:
  \itemize{
    \item a language subtag
    \item an optional script subtag
    \item an optional region subtag
    \item optional variant subtags
  }
  Language subtags are mainly derived from ISO 639-1 and ISO 639-2,
  script subtags from ISO 15924, and region subtags from ISO 3166-1
  alpha-2 and UN M.49.  (See package \pkg{ISOcodes} for more information
  about these standards.)  Variant subtags are not derived from any
  standard.  The Language Subtag Registry
  (\url{http://www.iana.org/assignments/language-subtag-registry}),
  maintained by the Internet Assigned Numbers Authority (IANA), lists
  the current valid public subtags.

  See \url{http://en.wikipedia.org/wiki/IETF_language_tag} for more
  information.

  Note that in particular so-called grandfathered and private use tags
  are currently not supported.
}
\value{
  If \code{expand} is false, a character matrix with 4 columns named
  \code{"Language"}, \code{"Script"}, \code{"Region"}, and
  \code{"Variant"}, giving the corresponding subtags (or \code{NA} if
  these were missing from the language tag).  Otherwise, a character
  frame with the same 4 columns giving the corresponding descriptions
  (which may be multiple), and the gives tags as row names.
}
\examples{
## German as used in Switzerland:
parse_IETF_language_tag("de-CH")
## Serbian written using Latin script as used in Serbia and Montenegro:
parse_IETF_language_tag("sr-Latn-CS")
## Spanish appropriate to the UN Latin American and Caribbean region:
parse_IETF_language_tag("es-419")
## All in one:
parse_IETF_language_tag(c("de-CH", "sr-Latn-CS", "es-419"))
parse_IETF_language_tag(c("de-CH", "sr-Latn-CS", "es-419"),
                        expand = TRUE)
}
\keyword{utilities}
