# generate CV
devtools::load_all()

content = "inst/content/muenchow_full_vita.yaml"
style = system.file("style/dl-vita.sty", package = "vita")
out = "pdf/"
# If you want to scrape the WOS, you need to figure out the sid for your
# query... haven't found a completetely automated way so far, hence, go to the
# WOS, query au=muenchow,j and copy the sid from the resulting url (grrrr)
build_cv(content, style, out, pub_score = TRUE, sid = "C5a9zLFedsAog9BGOBX",
         clean = TRUE)
