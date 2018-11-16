# generate CV
devtools::load_all()

content = "inst/content/muenchow_full_vita.yaml"
style = system.file("style/dl-vita.sty", package = "vita")
out = "pdf/"
build_cv(content, style, out, pub_score = TRUE, clean = TRUE)


