\name{assign_thread_no}
\alias{assign_thread_no}
\title{
Determine the thread number for posts.
}
\description{
Quick thing to match thread posts to the original post.
}
\usage{
assign_thread_no(no, resto)
}
\arguments{
  \item{no}{
Numeric vector (or coercible) with post numbers.  Basically the same thing as
the namesake field.
}
  \item{resto}{
Numeric vector (or coercible) with "RE:s to" numbers.  Basically the same thing
as the namesake field.
}
}
\details{
If \code{no} and \code{resto} are not the same length, they will be extended to
the length of the longer one.  So it's best to give it equal-lengthed vectors.
}
\value{
Numeric vector of thread numbers.
}
\examples{
## The middle one is the OP.
fake_no <- c(897235, 897230, 897257)
fake_resto <- c(897230, 0, 897230)

assign_thread_no(fake_no, fake_resto)
}