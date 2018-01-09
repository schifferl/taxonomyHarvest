library(magrittr)
library(dplyr)
library(rvest)
library(readr)

archaea <- "https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?mode=Undef&name=Archaea&lvl=30&srchmode=1&keep=1&unlock"
bacteria <- "https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?mode=Undef&id=2&lvl=30&srchmode=1&keep=1&unlock"
eukaryota <- "https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?mode=Undef&name=Eukaryota&lvl=30&srchmode=1&keep=1&unlock"
viroids <- "https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?mode=Undef&name=Viroids&lvl=30&srchmode=1&keep=1&unlock"
viruses <- "https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?mode=Undef&name=Viruses&lvl=30&srchmode=1&keep=1&unlock"

grep_id <- function(href) {
  regexpr("(?!id=)[0-9]+(?=&lvl)", href, perl = TRUE) %>%
    regmatches(href, .)
}

taxonomy_index <- function(level, title) {
  title_length <- length(title)
  title_index <- seq_len(title_length)
  empty_index <- rep_len(0, title_length)
  taxonomy_positions <- grep(level, title)

  for(i in title_index) {
    for(j in taxonomy_positions) {
      if(i >= j) {
        empty_index[i] <- j
      }
    }
  }

  empty_index
}

taxonomy_data <- function(kingdom) {
  kingdom %<>%
    tolower()

  kingdoms <- c("archaea", "bacteria", "eukaryota", "viroids", "viruses")

  stopifnot(kingdom %in% kingdoms)

  case_when(
    grepl(kingdom, "archaea") ~ archaea,
    grepl(kingdom, "bacteria") ~ bacteria,
    grepl(kingdom, "eukaryota") ~ eukaryota,
    grepl(kingdom, "viroids") ~ viroids,
    grepl(kingdom, "viruses") ~ viruses
  ) %>%
  read_html(options = "HUGE") %>%
    html_nodes("a[title!='no rank']") %>% {
      href <- html_attr(., "href")
      title <- html_attr(., "title")
      text <- html_text(.)
      data_frame(href, title, text)
    } %>%
    filter(!is.na(title)) %>%
    mutate(taxonomy_id = grep_id(href)) %>%
    mutate(taxonomy_id = as.integer(taxonomy_id)) %>%
    mutate(kingdom = taxonomy_index("superkingdom", title)) %>%
    mutate(phylum = taxonomy_index("phylum", title)) %>%
    mutate(class = taxonomy_index("class", title)) %>%
    mutate(order = taxonomy_index("order", title)) %>%
    mutate(family = taxonomy_index("family", title)) %>%
    mutate(genus = taxonomy_index("genus", title)) %>%
    mutate(species = taxonomy_index("species", title)) %>%
    mutate(phylum = ifelse(phylum > kingdom, phylum, NA)) %>%
    mutate(class = ifelse(class > phylum, class, NA)) %>%
    mutate(order = ifelse(order > class, order, NA)) %>%
    mutate(family = ifelse(family > order, family, NA)) %>%
    mutate(genus = ifelse(genus > family, genus, NA)) %>%
    mutate(species = ifelse(species > genus, species, NA)) %>%
    mutate(kingdom = ifelse(!is.na(kingdom), extract(text, kingdom), kingdom)) %>%
    mutate(phylum = ifelse(!is.na(phylum), extract(text, phylum), phylum)) %>%
    mutate(class = ifelse(!is.na(class), extract(text, class), class)) %>%
    mutate(order = ifelse(!is.na(order), extract(text, order), order)) %>%
    mutate(family = ifelse(!is.na(family), extract(text, family), family)) %>%
    mutate(genus = ifelse(!is.na(genus), extract(text, genus), genus)) %>%
    mutate(species = ifelse(!is.na(species), extract(text, species), species)) %>%
    select(taxonomy_id, kingdom, phylum, class, order, family, genus, species) %>%
    distinct(kingdom, phylum, class, order, family, genus, species, .keep_all = TRUE) %>% {
      if(nrow(.) >= 7) {
        first_row <- slice(., 1)
        other_rows <- slice(., (min(which(!is.na(species))) - 5):length(species))
        bind_rows(first_row, other_rows)
      } else {
        .
      }
    }
}

# archaea_data <- taxonomy_data("archaea")
# bacteria_data <- taxonomy_data("bacteria")
# eukaryota_data <- taxonomy_data("eukaryota")
# viroids_data <- taxonomy_data("viroids")
# viruses_data <- taxonomy_data("viruses")

# write_csv(archaea_data, "~/archaea_data.csv")
# write_csv(bacteria_data, "~/bacteria_data.csv")
# write_csv(eukaryota_data, "~/eukaryota_data.csv")
# write_csv(viroids_data, "~/viroids_data.csv")
# write_csv(viruses_data, "~/viruses_data.csv")
