##' Table notes
##'
##' Prepare notes about standard errors and statistical significance
##'
##' Table notes are part of the tabular environment and may be based on the
##' content of the table itself. For example, the \code{stars} argument to
##' \code{\link{apsrtable}} determines whether one or many levels of
##' statistical significance are indicated in the output. The \code{stars.note}
##' function creates text to place in such a note.
##'
##' By default the output uses the notation \eqn{ * p <.05} and the example
##' below shows a replacement function that states, \dQuote{significant at
##' \code{lev} percent.}.
##'
##' To access variables in the call to \code{apsrtable} from functions in
##' \code{notes}, include the arugment \code{env} in any custom functions. This
##' is the \code{apsrtable} call environment.
##'
##' Remember, to escape characters in Latex output, backslashes have to be
##' doubled in R character strings.
##'
##' @rdname notefunctions
##' @aliases notefunctions se.note stars.note pval.note
##' @param env The environment of the \code{apsrtable()} call, because note
##' functions may need to make use of some variables such as \code{lev} or
##' \code{digits}.
##' @return A character string to place within the tabular environment in
##' footnotesize beneath other output.
##' @author Michael Malecki <malecki at wustl.edu>
##' @examples
##'
##' ### Custom note function
##'
##' signif.pct <- function(env) {
##'   paste("$^*$ significant at", evalq(lev,envir=env)*100, "percent")
##' }
##' ### Continue the example from apsrtable
##' \dontrun{
##' apsrtable(lm.D90, lm.D9, glm.D9, digits=1, align="left",
##'           stars=1, lev=0.05, model.counter=0, order="rl",
##'           notes=list(se.note, signif.pct,
##'             "Plant weight data from the lm() example" )
##' 	 )
##' }
##'
"se.note" <- function(env) {
    note <- paste(ifelse( evalq(se,envir=env) != "vcov","Robust s","S"),
                  "tandard errors in parentheses",
                  ifelse(evalq(se,envir=env)=="both",
                         paste("\\\\\n\\multicolumn{",
                               evalq(nmodels,envir=env)+1,"}{l}{",
                               'Na\\"ive standard errors in brackets',
                               collapse="",sep=""),
                         "" ) ,sep="")
    return(note)
}

## Added pval support
"pval.note" <- function(env) {
    note <- paste(ifelse(evalq(se,envir=env) != "vcov", "Robust ", ""),
                  "$p$ values in parentheses",sep="")
    return(note)
}

"stars.note" <- function(env) {
    paste(ifelse(evalq(stars,envir=env)=="default",
                 paste("$^\\dagger$ significant at $p<.10$; $^* p<.05$; $^{**} p<.01$; $^{***} p<.001$"),
                 paste("$^*$ indicates significance at $p<",
                       evalq(lev,envir=env),"$")))
}


## ADM <admartin@wustl.edu> requested sanitizing coefnames 2010-03-10
## this function taken from print.xtable v1.5-6.
sanitize <- function(str) {
    result <- str
    result <- gsub("\\\\","SANITIZE.BACKSLASH",result)
    result <- gsub("$","\\$",result,fixed=TRUE)
    result <- gsub(">","$>$",result,fixed=TRUE)
    result <- gsub("<","$<$",result,fixed=TRUE)
    result <- gsub("|","$|$",result,fixed=TRUE)
    result <- gsub("{","\\{",result,fixed=TRUE)
    result <- gsub("}","\\}",result,fixed=TRUE)
    result <- gsub("%","\\%",result,fixed=TRUE)
    result <- gsub("&","\\&",result,fixed=TRUE)
    result <- gsub("_","\\_",result,fixed=TRUE)
    result <- gsub("#","\\#",result,fixed=TRUE)
    result <- gsub("^","\\verb|^|",result,fixed=TRUE)
    result <- gsub("~","\\~{}",result,fixed=TRUE)
    result <- gsub("SANITIZE.BACKSLASH","$\\backslash$",
                   result,fixed=TRUE)
    return(result)
}
