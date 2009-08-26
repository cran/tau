## ceeboo 2008

## <NOTE>
## Currently the approach to counting of word sequences is a bad
## workaround. As R has a character cache this could be implemented
## using pointer comparisons and prefix trees, but the latter would be
## inefficient compared to using hash tables. However, the R API does
## not provide the necessary interfaces to its hash table code.
## </NOTE>

textcnt <- 
function(x, n = 3L, split = "[[:space:][:punct:][:digit:]]+",
         tolower = TRUE, marker = "_", words = NULL, lower = 0L,
         method = c("ngram", "string", "prefix", "suffix"),
         recursive = FALSE, persistent = FALSE, verbose = FALSE,
         decreasing = FALSE)
{
    n <- as.integer(n)
    if (is.list(x)) {
        if (recursive) {
            if (persistent) {
                for (z in x[-length(x)])
                    textcnt(z, n, split, tolower, marker, words, lower,
                            method, recursive, persistent, verbose)
                x <- x[length(x)]
                persistent <- FALSE
            } else
                return (lapply(x, textcnt, n, split, tolower, marker, words,
                            lower, method, recursive, persistent, verbose))
        }
    } else {
        if (is.null(x)) return (x)
        x <- list(x)
    }
    method <- match.arg(method)
    if (!is.null(split))
        x <- lapply(lapply(x, strsplit, split), unlist)
    if (tolower)
        x <- lapply(x, tolower)
    if (!is.null(words))
        x <- .Call("R_copyTruncate", x, words)
    x <- if (method == "ngram") {
        ## add marker at both ends
        x <- lapply(x, function(x) gsub("^(?!$)|$(?<!^)", marker, x,
                                        perl = TRUE))
        .Call("R_utf8CountNgram", x, n, lower, verbose, persistent)
    } else {
        ## preprocess 
        if (method == "string" && n > 1L)
            x <- lapply(x, function(x) 
                unlist(lapply(.Call("R_copyToNgram", x, n), paste,
                    collapse = " ")))
        .Call("R_utf8CountString", x, n, lower,
              match(method, c("string", "prefix", "suffix")) - 1L, 
              verbose, persistent)
    }
    if (!is.null(x)) {
        if (decreasing)
            sort(x, decreasing = TRUE) -> x
        class(x) <- "textcnt"
    }
    x
}
