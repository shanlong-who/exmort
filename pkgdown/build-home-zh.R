# Generate a static Chinese homepage from the rendered pkgdown layout.
library(xml2)
library(yaml)

build_home_zh <- function(destination) {
  home <- file.path(destination, "index.html")
  english <- read_html(home, encoding = "UTF-8")
  chinese <- read_html(home, encoding = "UTF-8")
  dictionary <- unlist(read_yaml("pkgdown/home-zh.yml"), use.names = TRUE)
  base.url <- sub("/?$", "/", read_yaml("_pkgdown.yml")$url)
  zh.url <- paste0(base.url, "zh/")

  # Translate individual text nodes so mixed headings, links and R code retain their markup.
  nodes <- xml_find_all(chinese, "//body//text()[normalize-space() and not(ancestor::pre or ancestor::script)]")
  keys <- trimws(xml_text(nodes))
  matches <- keys %in% names(dictionary)
  in.content <- xml_find_all(chinese, "//main//text()[normalize-space() and not(ancestor::pre)]")
  words <- trimws(xml_text(in.content))
  untranslated <- setdiff(words[grepl("[A-Za-z]", words)], c(names(dictionary), "exmort", "English"))
  if (length(untranslated)) stop("Missing homepage translations: ", paste(untranslated, collapse = ", "))
  xml_set_text(nodes[matches], unname(dictionary[keys[matches]]))
  for (attribute in c("aria-label", "alt", "title", "placeholder", "content")) {
    nodes <- xml_find_all(chinese, sprintf("//*[@%s]", attribute))
    keys <- trimws(xml_attr(nodes, attribute))
    matches <- keys %in% names(dictionary)
    xml_set_attr(nodes[matches], attribute, unname(dictionary[keys[matches]]))
  }
  xml_set_text(xml_find_first(chinese, "//title"), "exmort · 全因死亡与超额死亡计算器")
  xml_set_attr(xml_root(chinese), "lang", "zh-CN")

  # The translated homepage shares the root website's documentation and assets.
  for (attribute in c("href", "src", "data-search-index")) {
    nodes <- xml_find_all(chinese, sprintf("//*[@%s]", attribute))
    paths <- xml_attr(nodes, attribute)
    relative <- nzchar(paths) & !grepl("^([a-zA-Z][a-zA-Z0-9+.-]*:|[/#?])", paths)
    xml_set_attr(nodes[relative], attribute, paste0("../", paths[relative]))
  }
  xml_set_attr(xml_find_first(chinese, "//a[contains(@class,'navbar-brand')]"), "href", "./")
  xml_set_attr(xml_find_first(chinese, "//img[@class='em-chart']"), "src", "../monthly-example-zh.png")
  switch.en <- xml_find_first(chinese, "//nav[@class='em-language']/a[@hreflang='en']")
  switch.zh <- xml_find_first(chinese, "//nav[@class='em-language']/a[@hreflang='zh-CN']")
  xml_set_attr(switch.en, "href", "../")
  xml_set_attr(switch.en, "aria-current", NULL)
  xml_set_attr(switch.zh, "href", "./")
  xml_set_attr(switch.zh, "aria-current", "page")

  zh.dir <- file.path(destination, "zh")
  dir.create(zh.dir, showWarnings = FALSE)
  version <- read.dcf("DESCRIPTION", fields = "Version")[[1L]]
  development <- length(strsplit(version, ".", fixed = TRUE)[[1L]]) == 4L
  for (language in c("en", "zh-CN")) {
    page <- if (language == "en") english else chinese
    path <- if (language == "en") home else file.path(zh.dir, "index.html")
    if (development) {
      label <- if (language == "en") "Development" else "开发版"
      xml_set_text(xml_find_first(page, "//nav//small[contains(@class,'nav-text')]"), paste(label, version))
    }
    head <- xml_find_first(page, "//head")
    xml_remove(xml_find_all(head, "link[@rel='canonical' or (@rel='alternate' and @hreflang)]"))
    xml_add_child(head, "link", rel = "canonical", href = if (language == "en") base.url else zh.url)
    xml_add_child(head, "link", rel = "alternate", hreflang = "en", href = base.url)
    xml_add_child(head, "link", rel = "alternate", hreflang = "zh-CN", href = zh.url)
    write_html(page, path, encoding = "UTF-8")
  }
  sitemap <- file.path(destination, "sitemap.xml")
  if (file.exists(sitemap)) {
    doc <- read_xml(sitemap)
    if (!zh.url %in% xml_text(xml_find_all(doc, "//*[local-name()='loc']"))) {
      xml_add_child(xml_add_child(xml_root(doc), "url"), "loc", zh.url)
      write_xml(doc, sitemap)
    }
  }
}
