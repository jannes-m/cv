# TO DO:
# - do not load the vita package but source all the functions of this file!!!
# - update documentation
# - git version control -> upload to your git account
# - rewrite format_ functions (one function should do it all)
# - build package


#' Processes a specified YAML file to create outputs
#'
#' @param section a character giving the name of the section to
#' process.  There must be a corresponding \code{section.yaml} file in
#' the input directory and \code{format_section} function in the
#' formats file.  The function should take a YAML parsed list as input
#' and returned a list of lines in the correct format.
#' @param input_dir the input directory (no trailing /)
#' @param output_dir the output directory (no trailing /)
#' @param format a character giving a file name of the format
#' \code{format_functions.r}.  The default is "tex"
#'
#' @return NULL
process_yaml <- function(section, inp, out, format = "tex") {

    inp <- file.path(inp, paste0(section, ".yaml"))
    out <- file.path(out, paste0(section, ".", format))

    if (!file.exists(inp)) {
      stop(sprintf("Unable to find input file '%s'", inp))
    }

    message(paste("processing yaml file ", inp))
    data <- yaml::yaml.load_file(inp)
    # mhh, couldn't we write a general format_...-function?

    # make sure that something like format_workshops_short also works
    f <- paste0("format_", gsub("_.*", "", section))
    lines <- eval(call(f, data))
    # delete previous versions, if necessary
    if (file.exists(out)) {
      suppressWarnings(file.remove(out))
    }
    # write the tex file
    invisible(lapply(lines, write, file = out, append = TRUE))
}


#' Creates a section break
#'
#' Creates a section break in LaTeX comments
#' @param name the name of the section
#' @param width the column width
#' @return a character of separate lines
section_break <- function(name, width = 80) {
    c(paste0(c("% ", rep("-", width - 2)), collapse=""),
      sprintf("%% %s", name),
      paste0(c("% ", rep("-", width - 2)), collapse=""))
}

#' Tidies lines for LaTeX.  Currently only escapes ampersands
#'
sanitize <- function(l) {
   stringr::str_replace(l, "\\&", "\\\\&")
}

#' Formats the address for printing
#'
#' @param address a list of address lines.
#' @return a character including lines breaks for TeX
format_address <- function(address) {
    top_lines <- paste0(address[1:3], collapse="\\\\")
    postcode <-
      sprintf(paste0("\\vspace{-0.04in}\\addfontfeature",
                     "{Numbers={Proportional, Lining}}%s"),
                     address$postcode)
    paste0(c(top_lines, postcode), collapse="\\\\")
}

#' @title Build a CV using latex
#' @details \code{build_cv} builds a CV using a predefined style and YAML files
#'   which contain positions held, employments, projects worked on, etc.
#' @param content Path to a YAML file which contains all your personal
#'   information as well as the structure of your CV.
#' @param style Path to the latex style you wish to apply.
#' @param out The directory where your output should be stored (without trailing
#'   forward or back slash).
#' @param pub_score If TRUE, the publications section will start with an
#'   overview of the current publication record including number of
#'   publications, h- and h10-factor. For more information, see
#'   [get_pub_record].
#' @param ... triple dot, here mainly used for the sid parameter of [get_pub_record()]
#' @param clean If \code{TRUE}, all files but the output pdf will be deleted
#'   from the output directory.
build_cv <- function(content, style, out = NULL, pub_score = TRUE,
                     clean = TRUE, ...) {
  # create the output directory, if necessary
  if (is.null(out)) {
    out <- getwd()
    out <- paste(out, "out", sep = "/")
  }
  suppressWarnings(dir.create(out))
  # check if content file exists
  if (!file.exists(content)) {
    stop(sprintf("Unable to find content file '%s'", content))
  }

  message("Processing YAML files...", appendLF = FALSE)
  # load the config file
  config <- yaml::yaml.load_file(content)

  # build the document header first
  header <- with(config, c(
    sprintf("\\title{%s}", person$title),
    sprintf("\\name{%s %s}", person$first_name, person$last_name),
    sprintf("\\postnoms{%s}", person$postnoms),
    sprintf("\\address{%s}", format_address(person$address)),
    sprintf("\\www{%s}", person$web),
    sprintf("\\email{%s}", person$email),
    sprintf("\\tel{%s}", person$tel),
    sprintf("\\subject{%s}", person$subject)))
  # escape ampersands (&) for our latex output
  header <- sanitize(header)
  # then add the bibliography stuff
  bibinfo <-
    with(config$publications,
         c(sprintf("\\bibliography{%s}", tools::file_path_sans_ext(bib_file)),
           unlist(lapply(1:length(sections), function(i) {
             sprintf("\\addtocategory{%s}{%s}",
                     names(sections)[i],
                     paste0(sections[[i]], collapse=", "))
    }))))

  # then add the other sections
  sections <- lapply(config$sections, function(x) {
    if (x$file == "BIBTEX") {
      # insert publications
      c("\\begin{publications}",
        lapply(names(config$publications$sections),
               function(x) sprintf("\\printbib{%s}", x)),
        "\\end{publications}", "")

    } else {
      file_root <- tools::file_path_sans_ext(x$file)
      # process the YAML file
      process_yaml(section = file_root,
                   inp = dirname(content),
                   out = out)

      ## Return the lines
      c(sprintf("\\section*{%s}", x$title),
        sprintf("\\input{%s}", file_root),
        "")
    }
  })
  sections <- unlist(sections)
  ind = grep("begin\\{publications", sections)

  insert = ""
  if (pub_score) {
    rec = get_pub_record(...)
    insert =
      paste0("So far, I have published ", rec$n_journal,
            " peer-reviewed journal articles which ",
            "were cited ", rec$wos_tc, " times (", rec$wos_tc_wsc,
            " times excluding self-citations) according ",
            "to the Web of Science and ", rec$total_cites,
            " times when consulting Google Scholar. The h-index is ",
            rec$h_index, ".\\linebreak\\linebreak")
  }

  sections = c(sections[1:ind],
               insert,
               sections[(ind + 1):length(sections)])
  # sections[grepl("software", sections)] <-
  #   "\\section{Other}\\printbib{software}"

  # build the entire document
  lines <- c("\\documentclass[english, 11pt, a4paper]{article}",
             sprintf("\\usepackage{%s}",
                     tools::file_path_sans_ext(basename(style))),
             "",
             section_break("Personal information"),
             header,
             "",
             section_break("Publications info"),
             bibinfo,
             "",
             "\\begin{document}",
             # we need to add all citations again. In fact, in dl-vita.sty the
             # nocite command is already executed and it has worked before but
             # now no longer and we need to add the command here again
             "\\nocite{*}",
             "\\maketitle",
             "",
             "born in Dresden, Germany, on 09/29/1981.",
             sections,
             "\\end{document}")


  message("done")

  message(sprintf("Copying source files to '%s'...", out), appendLF = FALSE)
  ## Copy in the bib file and package
  files = list.files(path = c(dirname(style),
                               dirname(content)),
                      full.names = TRUE)

  file.copy(files, to = out, overwrite = TRUE, copy.mode = TRUE)
  # make author bold
  bib = grep(".bib$", dir(out, full.names = TRUE), value = TRUE)
  make_bold(bib = bib,
            output = bib)
  # replace colon in posts, we need to escape a colon in yaml files with ''
  replace_colon(file.path(out, "posts.tex"))

  # create the tex file in the right place
  outfile <- paste0(c(tools::file_path_sans_ext(basename(content)), ".", "tex"),
                    collapse = "")
  outfile <- file.path(out, outfile)

  if (file.exists(outfile)) {
    suppressWarnings(file.remove(outfile))
  }
  invisible(lapply(lines, write, file = outfile, append = TRUE))
  message("done")
  tmp_dir <- getwd()
  setwd(out)
  system(paste("xelatex", gsub("yaml", "tex", basename(content))))
  # run biber to add the literature
  system(paste("biber", tools::file_path_sans_ext(basename(content))))
  # compile the pdf again to make the literature appear in the text
  system(paste("xelatex", gsub("yaml", "tex", basename(content))))

  # clean up
  if (clean) {
    file.remove(dir()[!grepl(".pdf", dir())])
  }
  setwd(tmp_dir)

  # and first it worked, and then again it complains that one should use
  # xelatex...
  # texi2dvi("C:/Users/pi37pat/Desktop/outdir/muenchow_full_vita.tex", pdf = TRUE)
  # run biber to add the literature
  # system("biber muenchow_full_vita")
  # compile the pdf again to make the literature appear in the text
  # texi2dvi("C:/Users/pi37pat/Desktop/outdir/muenchow_full_vita.tex",
  #          pdf = TRUE)


}

format_talks <- function(l) {
    tmp <- l[[1]]
    ## Sort by year order
    ord <- order(unlist(lapply(tmp, function(x) x$year)), decreasing = TRUE)
    tmp <- tmp[ord]
    month <- ""
    # lines <- lapply(tmp, function(x) {
    #     with(x, sprintf("\\ind  %s %s: \\textit{%s}. %s, %s.\n",
    #                     year, authors, title, event, city))
    # })
    lines <- lapply(tmp, function(x) {
      x$authors = gsub("Muenchow, J.", "\\\\textbf{Muenchow, J.}",
                       x$authors)
      with(x, sprintf("\\ind %s (%s): \\textit{%s}. %s, %s.\n",
                      authors, year, title, event, city))
    })
    lines
}
format_workshops <- format_talks

format_affiliations <- function(l) {
    tmp <- l[[1]]

    ## Sort by year order
    ord <- order(unlist(lapply(tmp, function(x) x$start)), decreasing = TRUE)
    tmp <- tmp[ord]

    lines <- lapply(tmp, function(x) {
        with(x, sprintf("\\ind %d--%s.  %s, \\emph{%s}.\n", start, end, status,
                        org))
    })
    return(lines)
}

format_education <- function(l) {

    tmp <- l[[1]]

    ## Sort by year order
    ord <- order(unlist(lapply(tmp, function(x) x$year)), decreasing = TRUE)
    tmp <- tmp[ord]

    lines <- lapply(tmp, function(x) {

        post <- ""

        if ("thesis" %in% names(x))
            post <- paste0(post, sprintf("\\emph{%s}", x$thesis))


        if ("award" %in% names (x))
            post <- paste(post, sprintf("%s", x$award))

        if (nchar(post)>0) post <- paste0(post, ".")

        with(x,
             ifelse(end == "",
                    sprintf("\\ind %d. %s, %s. %s\n", year, degree, university, post, award),
                    sprintf("\\ind %d--%s.  {\\addfontfeature{Numbers={Proportional, Lining}}%s. %s}.\n",
                            year, end,  degree, university)))
     # with(x, sprintf("\\ind %d. %s, %s. %s\n", year, degree, university, post, award))
    })
    return(lines)
}



format_grants <- function(l) {
    tmp <- l[[1]]

    ## Sort by year order
    ord <- order(unlist(lapply(tmp, function(x) x$start)), decreasing = TRUE)
    tmp <- tmp[ord]

    lines <- lapply(tmp, function(x) {
        if (x$start == x$end) {
            with(x, sprintf("\\ind %s. %s, %s. %s (%s).\n",
                            start, title, funder, role, value))
        } else {
            with(x, sprintf("\\ind %s--%s. %s, %s. %s (%s).\n",
                            start, end, title, funder, role, value))
        }
    })
    return(lines)
}


format_awards <- function(l) {
    tmp <- l[[1]]

    ## Sort by year order
    ord <- order(unlist(lapply(tmp, function(x) x$start)), decreasing = TRUE)
    tmp <- tmp[ord]

    lines <- lapply(tmp, function(x) {
        if (x$start == x$end) {
            with(x, sprintf("\\ind %s. %s, %s.\n",
                            start, title, other))
        } else {
            with(x, sprintf("\\ind %s--%s. %s, %s.\n",
                            start, end, title, other))
        }
    })

    return(lines)
}

format_service <- function(l) {
    tmp <- l[[1]]

    lines <- lapply(tmp, function(x) {
        topline <- list()
        topline <- sprintf("\\subsection*{%s}\n", x$context)
        roles <- list()
        if (x$context == "Academic"){
            for(i in 1:length(x$roles)){
                r <- x$roles[[i]]
                roles[[i]] <- paste0("\\ind ", r$start,
                                    ifelse(!is.null(r$end), paste0("--", r$end), ""),
                                     ". ", r$title, ". ", "\n")
            }
        } else if (grepl("Workshops", x$context)) {
            for(i in 1:length(x$roles)){
                r <- x$roles[[i]]
                roles[[i]] <- paste0("\\ind ", r$date, ". ", r$role, ". ",  r$title, ". ", r$location, ". \n")
            }

        } else if (x$context == "Consultancy") {
            for(i in 1:length(x$roles)){
                r <- x$roles[[i]]
                roles[[i]] <- paste0("\\ind ", r$start, ifelse(!is.null(r$end), paste0("--", r$end), ""), ". ", r$role,".\n")
            }

        } else {
            roles[[1]] <- paste(unlist(x$roles), collapse = ", ")
            roles [[1]] <- paste0(roles[[1]], ".")
        }

        c(list(topline), roles)
    })

    return(unlist(lines))
}

format_posts <- function(l) {
    tmp <- l[[1]]

    lines <- lapply(tmp, function(x) {
        topline <- sprintf("\\subsection*{%s}\n", x$employer)

        roles <- lapply(x$roles, function(x) {
            if ("end" %in% names(x)) {
              if (grepl("enumerate|itemize", x$title)) {
                with(x, sprintf("\\ind %s--%s.  %s\n", start, end, title))
              } else {
                with(x, sprintf("\\ind %s--%s.  %s.\n", start, end, title))
              }
            } else {
                with(x, sprintf("\\ind %s.  %s.\n", start, title))
            }
        })

        c(list(topline), roles)
    })

    return(unlist(lines))
}

format_students <- function(l) {
      lines <- mapply(function(data, nm) {

        tmp_fun <- function(tmp) {
          # Sort by year order
          ord <- order(unlist(lapply(tmp, function(x) x$start)),
                       decreasing = TRUE)
          tmp <- tmp[ord]
          lapply(tmp, function(x) {
            ifelse(is.null(x$note),
                   with(x, sprintf("\\ind %d--%s.  %s, \\emph{%s}.\n",
                                   start, end, name, title)),
                   with(x, sprintf("\\ind %d--%s.  %s, \\emph{%s} (%s).\n",
                                   start, end, name, title, note)))
          })

        }
        lines <- unlist(tmp_fun(data))
        c(sprintf("\\subsection*{%s}\n", nm),
          lines)

      }, l, names(l))
      as.character(unlist(lines))
    }

# format_students <- function(l) {
#     tmp <- l[[1]]
#
#     # Sort by year order
#     ord <- order(unlist(lapply(tmp, function(x) x$start)), decreasing = TRUE)
#     tmp <- tmp[ord]
#
#     lines <- lapply(tmp, function(x) {
#         with(x, sprintf("\\ind %d--%s.  %s, \\emph{%s}.\n", start, end, name, title))
#     })
#
#     lines
# }

format_teaching <- function(l) {

    tmp <- l[[1]]

    ## Sort by year order
    ord <- order(unlist(lapply(tmp, function(x) x$start)), decreasing = TRUE)
    tmp <- tmp[ord]
    end <- ""

    lines <- lapply(tmp, function(x) {
        with(x,
             ifelse(end == "",
                    sprintf("\\ind %d.  {\\addfontfeature{Numbers={Proportional, Lining}}%s}.\n", start, title),
                    sprintf("\\ind %d--%s.  {\\addfontfeature{Numbers={Proportional, Lining}}%s}.\n", start, end, title)))
    })

    return(lines)
}

format_visits <- function(l) {

  tmp <- l[[1]]

  ## Sort by year order
  # ord <- order(unlist(lapply(tmp, function(x) x$start)), decreasing = TRUE)
  # tmp <- tmp[ord]
  end <- ""

  lines <- lapply(tmp, function(x) {
    with(x,
         sprintf(
           paste0("\\ind %s--%s.  {\\addfontfeature{Numbers={Proportional,",
                  " Lining}}%s. %s}.\n"),
           start, end, title, place))
  })

  return(lines)
}

format_language <- function(l) {
  tmp <- l[[1]]

  lines <- lapply(tmp, function(x) {
    topline <- sprintf("\\ind \\textit{%s}: %s.\n", x$language, x$roles[[1]]$title)
    topline
  })
  lines
}

format_computing <- function(l) {
  tmp <- l[[1]]

  lines <- lapply(tmp, function(x) {
    topline <- sprintf("\\ind \\textit{%s}: %s.\n", x$language, x$roles[[1]]$title)
    topline
  })
  lines
}

make_bold = function(bib,
                     pattern = "(Muenchow, Jannes)|(Muenchow, J.)",
                     bold = "\\\\textbf{Muenchow, J.}",
                     output) {
  ll = readLines(bib)
  ll = gsub(pattern, bold, ll)
  writeLines(ll, output)
}

replace_colon = function(tex_file) {
  ll = readLines(tex_file)
  ll = gsub("':'", ":", ll)
  writeLines(text = ll, con = tex_file)
}

