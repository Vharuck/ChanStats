\name{remove_html_tags}
\alias{remove_html_tags}
\title{
Remove HTML tags from character strings
}
\description{
Remove 
}
\usage{
remove_html_tags(x, tags)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Character vector of text to remove tags from.
}
  \item{tags}{
Character vector of HTML tag words to use for deletion.  If this is not
supplied, the default list is used (see Details).
  }
}
\details{
An HTML tag is of the form "<[tag id] blah blah blah>", sometimes with a forward
slash (/) before the tag id.  Tags are simply removed, not replaced with
anything.  So if they signify formatting (like <br>), don't expect that
formatting in the result.

You can specificy \code{tags} if you only want
to remove certain ones, or if new tags are introduced after this package's
maintenance wanes.  If you don't supply \code{tags}, the following default list
is used:

!--, !Doctype, a, abbr, acronym, address, applet, area, article, aside, audio,
b, base, basefont, bdi, bdo, big, blockquote, body, br, button, canvas, caption,
center, cite, code, col, colgroup, datalist, dd, del, details, dfn, dialog, dir,
div, dl, dt, em, embed, fieldset, figcaption, figure, font, footer, form, frame,
frameset, h[1-6], head, header, hgroup, hr, html, i, iframe, img, input, ins,
kbd, keygen, label, legend, li, link, main, map, mark, menu, menuitem, meta,
meter, nav, noframes, noscript, object, ol, optgroup, option, output, p, param,
pre, progress, q, rp, rt, ruby, s, samp, script, section, select, small, source,
span, strike, strong, style, sub, summary, sup, table, tbody, td, textarea,
tfoot, th, thead, time, title, tr, track, tt, u, ul, var, video, wbr 
}
\value{
Character vector of the untagged versions of the elements of \code{x}.
}

\seealso{
\code{\link{decode_html}} to replace <br> with a newline character and otherwise
clean HTML strings up some more.  If you want to run both functions, do it as
\code{remove_html_tags(decode_html(x))}.
}
\examples{
sample_post <- paste0("<a href=&quot;/v/thread/280611436#p280623721&quot; class=&quot;quotelink&quot;>",
                      "&gt;&gt;280623721</a><br><span class=&quot;quote&quot;>",
                      "&gt;They do look like Japanese boyband members</span>",
                      "<br><br>Japanese boyband members look like this now? ",
                      "Wow you really are going all out.")
remove_html_tags(sample_post)
}