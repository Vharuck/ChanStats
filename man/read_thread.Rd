\name{read_thread}
\alias{read_thread}

\title{
Get lists of post data from the API.
}
\description{
Query the API for post data from a list of threads.
}
\usage{
read_thread(board, number, tries = 5)
}
\arguments{
  \item{board}{
Character vector of board names (abbreviated).
}
  \item{number}{
Character vector (or coercible) of thread numbers from the corresponding board in \code{board}.
}
  \item{tries}{
Number of times to try each request until success.  The internet is fickle, 4chan even more so.
}
}
\details{
Like with most vectorized R functions, \code{board} and \code{number} will be made the same length.  This function will then go through each pair of \code{board} and \code{number} and read all the posts in that thread, returning the list version of its JSON as part of a larger list of all the threads.
}
\value{
A list of threads, which are in turn a list of posts.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{get_active_thread_no}},
\code{\link{get_archived_thread_no}},
\code{\link{thread_to_table}}
}
\examples{
## Just to get the point across about getting the stuff out of lists.
an_active <- get_active_thread_no("an")

## an_active is not a numeric vector. It is a list.  It needs subsetted.
an_thread_no <- an_active[["an"]]
first_thread <- an_thread_no[1]
an_thread <- read_thread("an", first_thread)

## an_thread is not a list of posts.
length(an_thread)
## You need to subset it to get to that list.
thread_length <- length(an_thread[[1]])
cat("The first thread number on /an/ is", thread_length, "posts long.\\n")

## So why the Russian doll approach?  Because these are vectorized functions.
alpha_boards <- c("a", "adv", "an", "asp")
alpha_boards_active <- get_active_thread_no(alpha_boards)
first_alpha_thread <- sapply(alpha_board_active, FUN = "[", 1)
alpha_posts <- read_thread(alpha_boards, first_alpha_threads)

## Now the lists of threads are separate, since they are separate threads.
}