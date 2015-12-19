\name{flatten_json}
\alias{flatten_json}

\title{
Make a JSON list nothing but a bunch of atomic vectors.
}
\description{
Take a JSON list, then flatten out every list in that list.
}
\usage{
flatten_json(json)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{json}{
List representation of a JSON object, as returned by \code{fromJSON} in the
rjson package.
}
}
\details{
Names for "flattened" list items are created as "[name of the list]_[name of the
item in the list]". If the items aren't named, then the suffix will just be a
number.
}
\value{
A list of only atomic vectors.
}
\examples{
## Used in query_boards to flatten the "cooldowns" sublist:
boards_json <- fromJSON(file = "http://a.4cdn.org/boards.json")[["boards"]]
boards_json <- lapply(X = boards_json, FUN = flatten_json)
}
