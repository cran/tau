## ceeboo 2008

tokenize <-
function(x, lines = FALSE, eol = "\n")
{
    if (!is.character(x))
        stop("'x' not a character")
    if (lines)
        x <- gsub("$", eol, x, perl = TRUE)
    # protect infix punctuation
    x <- unlist(strsplit(x, "\\b(?<!^|\\w[[:punct:]])(?![[:punct:]]\\w)",
                perl = TRUE))
    # split non-words
    i <- grep("^\\W+$", x, perl = TRUE)
    if (length(i)) {
        x <- as.list(x)
        x[i] <- lapply(x[i], strsplit, "\\B", perl = TRUE)
        return (unlist(x))
    }
    x
}

remove_stopwords <-
function(x, words, lines = FALSE)
{
    if (!is.character(x))
        stop("'x' not a character")
    if (!is.character(words))
        stop("'words' not a character")
    if (lines) {
        ## FIXME perl prefers partial matches
        words <- paste("\\<(", paste(words, collapse = "|"),
            ")\\>", sep = "", collapse = "")
        x <- gsub(words, "", x, ignore.case = TRUE)
        ## FIXME remove ""?
    } else {
        i <- match(tolower(x), words)
        x <- x[is.na(i)]
    }
    x
}
