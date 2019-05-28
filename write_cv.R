# generate CV
devtools::load_all()

content = "inst/content/dfg/muenchow_dfg.yaml"
content = "inst/content/muenchow_full/muenchow_full_vita.yaml"
style = system.file("style/dl-vita.sty", package = "vita")
out = "pdf/"
# If you want to scrape the WOS, you need to figure out the sid for your
# query... haven't found a completetely automated way so far, hence, go to the
# WOS, query au=muenchow,j and copy the sid from the resulting url (grrrr)

# it seems, that get_pub_record() no longer works for WOS...
# well, I didn't like the solution in any case. If you really need it again,
# just copy and paste from WOS...
# build_cv(content, style, out, pub_score = TRUE, sid = "F4vpP3iHDTxP21bPh3j",
#          clean = FALSE)

build_cv(content, style, out, pub_score = FALSE, clean = TRUE)
# If you want to scrape the WOS, you need to figure out the sid for your
# query... haven't found a completetely automated way so far, hence, go to the
# WOS, query au=muenchow,j and copy the sid from the resulting url (grrrr)

# it seems, that get_pub_record() no longer works for WOS...
# well, I didn't like the solution in any case. If you really need it again,
# just copy and paste from WOS...
# build_cv(content, style, out, pub_score = TRUE, sid = "F4vpP3iHDTxP21bPh3j",
#          clean = FALSE)

