# generate CV
devtools::load_all()
# content <- "D:/programming/R/cv/inst/content/muenchow_full_vita.yaml"
content <- system.file("content/muenchow_full_vita.yaml", package = "vita")
# content <-  system.file("content/muenchow_vita_en.yaml", package = "vita")
# content <- "D:/programming/R/cv/inst/content/muenchow_vita_en.yaml"
style <- system.file("style/dl-vita.sty", package = "vita")
out <- "D:/programming/R/cv/out/"
build_cv(content, style, out, clean = TRUE)
