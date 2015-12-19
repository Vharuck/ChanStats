\name{decode_html}
\alias{decode_html}
\title{
Take strings formatted as HTML and reverts them to nice printable forms
}
\description{
Replaces all those special HTML character sequences with what they really mean.
}
\usage{
decode_html(x, html_dictionary = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Character vector of HTML-encoded strings.
}
  \item{html_dictionary}{
Named character vector, where the names are the HTML keys and the values are the corresponding characters.  If NULL, the default one is used (see Details).
}
}
\details{
At heart, this function takes the regular expressions in the names of \code{html_dictionary} and replaces them with the corresponding values.
}
\value{
A character vector of the unencoded comments.

The default dictionary changes the following patterns:
\item{PATTERN}{REPLACEMENT}
\item{<br( /)?>}{\\n}
\item{&quot;}{"}
\item{&amp;}{&}
\item{&lt;}{<}
\item{&gt;}{>}
\item{&#ddd;}{The unicode character with number ddd}
}

\examples{
## Note how it mysteriously fucks up the Yen.
shitposting <- "&gt;2015<br>&gt;implying you don&#039;t eat shit &amp; pocky for &#165;300"
cat(decode_html(shitposting), "\\n")
}
