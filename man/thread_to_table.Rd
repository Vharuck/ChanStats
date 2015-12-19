\name{thread_to_table}
\alias{thread_to_table}

\title{
Turn a list of post data into a data.table
}
\description{
The final function in the collection process.
}
\usage{
thread_to_table(thread, field_classes = json_4mat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{thread}{
A list of post data.  This is called \code{thread} because the usual value is a
list of post data from a single thread, such as an element in the returned value
from \code{read_thread}.  However, any similar list in which each member is a
list of field values should work.
}
  \item{field_classes}{
A named list, where the names are field names and the values are the
corresponding vector classes of those fields.
}
}
\details{
Turns a list of post details into a data table containing all the fields in
json_4mat, populated by post data as available. Use this function on each item
in a list returned by read_thread, with lapply or something similar.

If a post does not have details for one of the fields, the value in that post's
row will be set to default value for the field's class (e.g., the default
character value is \code{""}, and the default numeric value is \code{0}).

\code{read_thread} actually returns a list of threads, which are in turn a list
of post details, so that cannot be given directly to this function.  Instead,
give it to one of the \code{lapply} family with \code{thread_to_table} as
\code{FUN}.
}
\value{
A data table of post data with a column for each value of json_4mat.
}
\note{
Yes, this would be possible to do with data.frames only.  However, consider the
following.
\enumerate{
\item Data.tables are much, much more efficient, especially since I am filling in a
table on row at a time.  Data.frames are copied every time a change is made, but
data.tables can modify in memory without copying, so the difference is huge.
\item The post tables are huge, and data.table makes working in them easier and
quicker.
\item I use data.tables, and I don't want to maintain two sets of code.
}
}
\seealso{
\code{\link{json_4mat}},
\code{\link{read_thread}}
}
\examples{
## Again, be aware of lists.
sci_archive <- get_archived_thread_no("sci")
raw_threads <- read_thread("sci", sci_archive[["sci"]][1:5])

sci_tables <- lapply(raw_threads, FUN = thread_to_table, json_4mat = json_4mat)

## sci_tables is a list of data.tables, since it's the result of lapply.
class(sci_tables)
sapply(sci_tables, class)

## So if you want a single table, use rbindlist from the data.table package.
bigsci <- rbindlist(sci_tables)
}