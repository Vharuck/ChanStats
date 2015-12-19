\name{ChanStats-package}
\alias{ChanStats-package}
\alias{ChanStats}
\docType{package}
\title{
Functions to get and munge data from the 4chan API
}
\description{
Experience the thrill of social media big data paired with over a thousand
flavors of the word "fag!"  Retrieve splendidly complex lists of lists from the
4chan JSON API for active or archived boards, then convert them into monstrously
huge data tables!  If you wanted data frames, then you're free to convert them
after benefitting from the performance.
}
\details{
\tabular{ll}{
Package: \tab ChanStats\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-01-23\cr
License: \tab GPL 3 (same as R)\cr
}
This package includes functions to interact with the 4chan API to get
information about the boards, see what threads are active or stored in the
archive, convert thread JSON objects into lists, turn those lists into
data.tables, and do a bit of data-munging on the those.
}
\author{
4chan Stats Anon

Maintainer: ChanStats Anon <4ChanStats@gmail.com>
}
\references{
Read the 4chan API documentation.  This package only gets you the data.  The
data itself is described at this link:
https://github.com/4chan/4chan-API
}
~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R ~~
~~ documentation directory ~~
\keyword{ package }
\seealso{
\code{\link{data.table}}\cr
\code{\link{rjson}}
}
