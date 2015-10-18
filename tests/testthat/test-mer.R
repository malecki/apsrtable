context("Given a lme4 merMod")
does_not_throw_error <- function (regexp = NULL) {
    function(expr) {
        res <- try(force(expr), TRUE)
        error <- inherits(res, "try-error")
        if (error) {
            return(expectation(FALSE, "code did generate an error"))
        }
        if (!is.null(regexp)) {
            matches(regexp)(res)
        }
        else {
            expectation(TRUE, "")
        }
    }
}

hide <- capture.output(example("lmer", package="lme4", echo=FALSE))

expect_that(apsrtable(fm1,lev=0), does_not_throw_error())
