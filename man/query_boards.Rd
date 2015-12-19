\name{query_boards}
\alias{query_boards}
\title{
Get details about all the boards
}
\description{
Create a table of information for each of the boards, such as whether it's a
work-safe board, whether it's archived, how many threads are stored on each
page, and so on.
}
\usage{
query_boards(info_url = "http://a.4cdn.org/boards.json")
}
\arguments{
  \item{info_url}{
The API url for this information.  It's only included here as an argument in the chance that it changes one day after this package is no longer maintained.
}
}
\details{
This function should be the first step for any collection process.
}
\value{
A data.table, with a row for each board and columns giving the values of different board aspects.  If the JSON of a board did not include a value for a field, then it's set as the default value for that atomic vector class (i.e., a
non-given numeric is set to 0).  Because the API doesn't describe any of these fields, here are my best guesses (with the vector class in the beginning):
\item{board}{(character) Board abbreviation}
\item{title}{(character) Longer name of the board}
\item{ws_board}{(numeric) 1 = Work-safe, 0 = NSFW}
\item{per_page}{(numeric) Number of threads shown on each page}
\item{pages}{(numeric) Number of pages}
\item{max_filesize}{(numeric) In bytes}
\item{max_webm_filesize}{(numeric) In bytes}
\item{max_comment_chars}{(numeric) How many characters can be in a post comment}
\item{bump_limit}{(numeric) After this many posts, a thread can no longer be bumped}
\item{image_limit}{(numeric) After this many images, no more can be posted to the thread}
\item{is_archived}{(numeric) 1 = this board is archived, 0 = this board is not}
\item{cooldowns_threads}{(numeric) How long an IP has to wait between creating threads, in seconds}
\item{cooldowns_replies}{(numeric) How long an IP has to wait between posting, in seconds}
\item{cooldowns_images}{(numeric) How long an IP has to wait between posting an image, in seconds}
\item{cooldowns_replies_intra}{(numeric) How long an IP has to wait between posting in the same thread, in seconds}
\item{cooldowns_images_intra}{(numeric) How long an IP has to wait between posting images in the same thread, in seconds}
\item{spoilers}{(numeric) 1 = spoilers can be used on this board, 0 = spoilers can not be used}
\item{custom_spoilers}{(numeric) 1 = there are custom spoilers on this board, 0 = nein}
\item{user_ids}{(numeric) 1 = poster IDs are displayed, 0 = secret}
\item{code_tags}{(numeric) 1 = comments can use code tags, 0 = they cannot}
\item{country_flags}{(numeric) 1 = country flags of posters' IP locations are displayed, 0 = one world, one love}
\item{math_tags}{(numeric) 1 = comments can use math tags, 0 = not $300k starting}
}

\examples{
board_info <- query_boards()

## Find out which boards are work-safe
board_info[ws_board == 1, board]

## See the different combinations of image and bump limits.
plot(image_limit ~ bump_limit, data = board_info)
}