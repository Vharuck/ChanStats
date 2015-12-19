\name{count_replies}
\alias{count_replies}

\title{
Find the number of quoting replies for each post.
}
\description{
Return the count of replies in the given comments for each of the given post numbers.
}
\usage{
count_replies(no, com)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{no}{
Numeric vector of post numbers, like its namesake field.
}
  \item{com}{
Character vector of UNALTERED comments, like its namesake field.
}
}
\details{
This function returns the number of post comments in \code{com} that quotelink each post number in \code{no}.  This is done with regular expression matching the HTML tags in \code{com}, so don't clean it up beforehand.  Also, different boards have different numbers, so don't mix them together.

The counts reflect the number of quoting comments, not the number of quotes.  So a comment that includes 10 quotelinks to one number in \code{no} is only counted once.
}
\value{
Numeric vector with the number of quotelinking comments for each of \code{no}, in the same order.
}
\examples{
## gif threads are guaranteed to have quotelinks from saucefags
gif_archive <- get_archived_thread_no("gif")

## Remember the general law of this package: it's not broken, it's a list.
gif_thread_one <- read_thread("gif", gif_archive[["gif"]][1])
gif_table <- thread_to_table(gif_thread_one[[1]], json_4mat)

gif_table[, list(no, com, replies = count_replies(no, com))]
}