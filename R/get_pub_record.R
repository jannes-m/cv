#' @title Get Jannes publication record
#' @param sid Provide manually a SID until you have found a better solution,
#'   i.e. you have to go to the WOS and look for AU=muenchow,j and then copy the
#'   SID from the resulting page, grrrrr
#' @details Scrape publication record from FSU homepage, WOS and Google Scholar
#' @return A list containing Jannes publication record
#' @importFrom RCurl getURL
#' @importFrom XML xmlRoot xpathSApply htmlParse xmlValue
#' @importFrom httr GET
#' @importFrom RCurl getURL
#' @importFrom scholar get_profile
get_pub_record = function(sid) {

  # Google Scholar
  jm = get_profile("Slq94Y4AAAAJ")
  # number of citations
  jm$total_cites
  jm$h_index
  jm$i10_index

  # WOS
  # number of publications
  res = rwos::wos_search(sid, "AU=Muenchow, J")
  # number of peer-reviewed journal_articales
  n_journal = res$results
  # create manually a citation report for Jannes Muenchow and copy the
  # corresponding URL address

  url = paste0("http://apps.webofknowledge.com/CitationReport.do?",
               "product=UA&search_mode=CitationReport&SID=",
               sid,
               "&page=1&cr_pqid=4&viewType=summary")
  # browseURL(url)
  # doc = getURL(url)
  doc = httr::GET(url)
  doc = htmlParse(doc)
  root = xmlRoot(doc)
  # names(root)
  # WOS h-index
  x_path = "//em[@id='H_INDEX' and @class='commafy']"
  wos_h = xpathSApply(root[["body"]], x_path, xmlValue)
  # WOS TC
  wos_tc = xpathSApply(root[["body"]], "//span[@id='GRAND_TOTAL_TC2']",
                       xmlValue)
  # WOS TC without self citations
  x_path = paste0("//span[@ctmp = get_pub_recordlass='lowerspan' and . = contains(., ",
                  "'Without self citations')]/following-sibling::em")
  wos_tc_wsc = xpathSApply(root[["body"]], x_path, xmlValue)

  # return your result
  res = list(n_journal = n_journal, wos_h = wos_h, wos_tc = wos_tc,
             wos_tc_wsc = wos_tc_wsc)
  c(res, jm[c("total_cites", "h_index", "i10_index")])
}

#' @title Counting the total number of WOS citations and self citations of
#'   Jannes' publications.
#' @details The function takes quite a while. The total number of WOS citations
#'   is correct. However, the total number of self citations is only almost
#'   correct. This is for two reasons. First, I am only using the surname of my
#'   co-authors to find out if they were also co-authors of the citing
#'   publications. This is especially problematic with very common surnames such
#'   as Smith or Meyer. Secondly, scraping the citing articles only returns the
#'   first three authors of the citing papers. This could be enhanced but would
#'   make the function even slower.
#' @importFrom XML xmlRoot xpathSApply htmlParse xmlValue
#' @importFrom httr GET
#' @importFrom rows wos_authenticate wos_search wos_retrieve

count_cits = function() {
  sid = rwos::wos_authenticate()
  res = rwos::wos_search(sid, "AU=Muenchow, J")
  pubs = rwos::wos_retrieve(res)
  # co-authors
  co_auth = unique(gsub(",.*", "", unlist(strsplit(pubs$authors,
                                                   split = " \\| "))))
  count = plyr::ldply(pubs$uid, function(x) {
    url = paste0("http://gateway.isiknowledge.com/gateway/Gateway.cgi?",
                 "GWVersion=2&SrcAuth=ResearchSoft&SrcApp=EndNote&DestLinkType",
                 "=FullRecord&DestApp=WOS&KeyUT=")
    url = paste0(url, x)
    doc = GET(url)
    doc = htmlParse(doc)
    root = xmlRoot(doc)
    # tc per article
    xpath = "//span[@class='large-number']"
    # take the first large number (= times cited)
    tc = xpathSApply(root[["body"]], xpath, xmlValue)[1]
    sc = 0
    # if the article has been cited, count how often I did it myself
    if (tc > 0) {
      # extract href to citing articles
      xpath = "//a[@class='snowplow-citation-network-times-cited-count-link']"
      extr_url = xpathSApply(root[["body"]], xpath, xmlGetAttr, "href")
      url_2 = paste0("http://apps.webofknowledge.com", extr_url)
      doc_2 = GET(url_2)
      doc_2 = htmlParse(doc_2)
      root_2 = xmlRoot(doc_2)
      xpath = "//div/span[@class='label' and . = 'By: ']/parent::div"
      out = xpathSApply(root_2[["body"]], xpath, xmlValue)
      # self citations

      # The chosen approch to estimate self citations ist not perfect because I
      # am just using the surnames of my co-authors which might be problematic
      # in the case of very common surnames such as Smith or Meyer. Secondly,
      # scraping the citing articles homepage only returns the first three
      # authors of a paper...
      sc = sum(grepl(paste(co_auth, collapse = "|"), out))
    }
    res = data.frame(tc = as.numeric(as.character(tc)),
                     sc = as.numeric(as.character(sc)))
    colSums(res)
  })
}
