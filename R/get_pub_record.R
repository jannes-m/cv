#' @title Get Jannes publication record
#' @details Scrape publication record from FSU homepage, WOS and Google Scholar
#' @return A list containing Jannes publication record
#' @importFrom RCurl getURL
#' @importFrom XML xmlRoot xpathSApply htmlParse xmlValue
#' @importFrom httr GET
#' @importFrom RCurl getURL
#' @importFrom scholar get_profile
get_pub_record = function() {

  # number of peer-reviewed journal_articales
  url = "https://www.geographie.uni-jena.de/en/Muenchow.html"
  hp = getURL(url)
  doc = htmlParse(hp)
  root = xmlRoot(doc)
  # names(root)

  # find out how many peer-reviewd publications have been published
  x_path = paste0("//strong[text() = 'Journal articles (peer-reviewed)']/",
                  "following::ul[1]/li")
  n_journal = length(xpathSApply(root[["body"]], x_path) )
  # same result
  # xpathSApply(root[["body"]],
  #             "//h2[text()='Selected publications']/following::ul[2]/li")

  # Google Scholar
  jm = get_profile("Slq94Y4AAAAJ")
  # number of citations
  jm$total_cites
  jm$h_index
  jm$i10_index

  # WOS
  # create manually a citation report for Jannes Muenchow and copy the
  # corresponding URL address
  url = paste0("http://apps.webofknowledge.com/CitationReport.do?product=WOS&",
               "search_mode=CitationReport&SID=D5zcKLAwVgT37lnrMfl&page=1&",
               "cr_pqid=1&viewType=summary&colName=WOS")
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
  x_path = paste0("//span[@class='lowerspan' and . = contains(., ",
                  "'Without self citations')]/following-sibling::em")
  wos_tc_wsc = xpathSApply(root[["body"]], x_path, xmlValue)

  # return your result
  res = list(n_journal = n_journal, wos_h = wos_h, wos_tc = wos_tc,
             wos_tc_wsc = wos_tc_wsc)
  c(res, jm[c("total_cites", "h_index", "i10_index")])
}
