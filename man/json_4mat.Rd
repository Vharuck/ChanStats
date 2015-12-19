\name{json_4mat}
\alias{json_4mat}
\docType{data}
\title{
Details on the post fields returned by the 4chan API.
}
\usage{data("json_4mat")}
\format{Named list, with the names being the field names as
described by the 4chan API and the values being a character vector of the
class(es) of that field.  Note that integers are stored as numeric.  R may be
interpreted and dynamic, but class can still screw you over.

Currently, every field has one class.  But \code{json_4mat} is still a list, not a character
vector, in case something changes down the road.
}
\details{
Used by \code{thread_to_table} for creating standard fields.  If a change comes
along and this package is no longer maintained, you will have to provide your
own format object like this to keep it working.

You can also add your own fields to this before supplying it to the functions.
\code{thread_to_table} will then simply add blank columns with those field names
and classes to the resulting data.table.
}
\references{
https://github.com/4chan/4chan-API
}
\seealso{
\code{\link{thread_to_table}}
}
\examples{
data(json_4mat)

## Which fields are numeric?
names(json_4mat[json_4mat == "numeric"])
}
\keyword{datasets}
