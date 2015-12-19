\name{Get thread numbers}
\alias{get_active_thread_no}
\alias{get_archived_thread_no}

\title{
Retrieve numbers of threads accessible through the API
}
\usage{
get_active_thread_no(boards, tries = 5, url_format = "https://a.4cdn.org/\%s/catalog.json")
get_archived_thread_no(boards, tries = 5, url_format = "https://a.4cdn.org/\%s/archive.json")
}

\arguments{
  \item{boards}{
Character vector of boards to search.
}
  \item{tries}{
Number of times to try before moving onto the next board. Internet is fickle,
4chan even more so.
}
  \item{url_format}{
Single character string of the url used for the query.  \code{board} will be
substituted for \code{\%s}.  This is an argument in case the API changes after
the package is no longer maintained.
}
}
\details{

}
\value{
Named list of numeric vectors, one for each board specified, containing the available thread numbers. \code{get_active_thread_no} returns those threads still on the board's pages, and \code{get_archived_thread_no} returns those threads in the archive.
}
\note{
Threads that were active at the start of a script may be pruned by the end.  This is especially true for fast boards, like /b/ and /v/.
}
\seealso{
\code{\link{read_thread}},
\code{\link{thread_to_table}}
}
\examples{
## A named list is always returned, so subsetting must be done.
first_circle_of_hell <- get_active_thread_no("b")[["b"]]
Sys.sleep(30)
second_circle_of_hell <- get_active_thread_no("b")[["b"]]

cat(length(setdiff(first_circle_of_hell, second_circle_of_hell)),
    "/b/ threads have been promoted to purgatory.",
    "\\nI bet you hate me for making your session sleep 30 seconds.\\n")
}