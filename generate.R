# ============================================================
# ShoalBase static species page generator
# Reads the live Google Sheet and writes one HTML file per
# species to /public, plus an index page.
# Reuses the same data-cleaning logic as the Shiny app.
# ============================================================

library(dplyr)
library(readr)
library(lubridate)
library(tidyr)
library(rlang)
library(countrycode)
library(memoise)
library(jsonlite)
library(rfishbase)
library(htmltools)

# ---- Config ----
sheet_csv_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vScflrcxVjeIuYkR8ipKw6IIo32vBHf4PWmMt9AwcUMvvS_XrW2FsO7yAeXftZAiqxWD9xeDsdW98CJ/pub?gid=1314366351&single=true&output=csv"
shiny_app_url <- "https://shaunkillen.shinyapps.io/shoalshare_1/"
site_url      <- "https://species.shoalbase.org"   # change if you pick a different subdomain
output_dir    <- "public"

# ---- Valid value lists (from the Shiny app) ----
valid_life_stage     <- c("egg/embryo","larval","juvenile","adult","spawning","not sure")
valid_group_size     <- c("solitary","pairing","small groups (3-10)","medium groups (10-100)",
                          "large groups (>100)","large groups (100-1000)","huge groups (1000+)",
                          "huge groups (>1000)","changing group sizes (fission-fusion)","unsure")
valid_social_system  <- c("solitary","pairing","shoaling","schooling","colony",
                          "aggregation (non spawning)","aggregation (spawning)",
                          "courtship display/mating","parental care","unsure")
valid_habitat_type   <- c("river/stream","lake/pond","brackish","coastal marine","benthic marine",
                          "benthopelagic marine","pelagic marine","deep sea","coral reef","tidepool")
valid_evidence_type  <- c("personal observation","published peer-reviewed paper","thesis",
                          "unpublished data","book / field guide","online resource (website or database)")

# ---- Country cleaning (same as app) ----
clean_country <- function(x) {
  if (is.null(x)) return(x)
  x <- trimws(x); x[x == ""] <- NA
  xl <- tolower(x)
  x[xl %in% c("uk","u.k.","united kingdom","england","scotland","wales","northern ireland","great britain","gb")] <- "UK"
  x[xl %in% c("usa","u.s.","u.s.a.","united states","united states of america")] <- "USA"
  x[xl %in% c("brasil")] <- "Brazil"
  needs <- !is.na(x) & !x %in% c("UK","USA")
  if (any(needs)) {
    cc <- suppressWarnings(countrycode::countrycode(x[needs], "country.name", "country.name"))
    x[which(needs)[!is.na(cc)]] <- cc[!is.na(cc)]
  }
  x
}

# ---- Wikipedia image (first JPEG, reused from app logic, simplified) ----
fetch_wiki_image <- memoise::memoise(function(species) {
  if (is.na(species) || species == "") return(NULL)
  url <- paste0("https://en.wikipedia.org/w/api.php?action=query&format=json",
                "&prop=pageimages&piprop=original|thumbnail&pithumbsize=500",
                "&redirects=1&titles=", utils::URLencode(species, reserved = TRUE))
  tryCatch({
    resp <- readLines(url, warn = FALSE, encoding = "UTF-8")
    p <- jsonlite::fromJSON(paste(resp, collapse = ""), simplifyVector = FALSE)
    pg <- p$query$pages[[1]]
    if (!is.null(pg$thumbnail$source)) pg$thumbnail$source
    else if (!is.null(pg$original$source)) pg$original$source
    else NULL
  }, error = function(e) NULL)
})

# ---- Helpers ----
split_multi <- function(x, valid) {
  if (all(is.na(x))) return(character(0))
  v <- unlist(strsplit(x[!is.na(x) & x != ""], ","))
  v <- trimws(v); v <- v[v != "" & v %in% valid]
  sort(unique(v))
}

slugify <- function(x) {
  s <- tolower(x); s <- gsub("[^a-z0-9]+", "-", s); gsub("^-|-$", "", s)
}

esc <- function(x) htmltools::htmlEscape(x)

# ---- Load data ----
cat("Reading Google Sheet...\n")
df <- readr::read_csv(sheet_csv_url, show_col_types = FALSE)
names(df) <- trimws(names(df))

rename_map <- c(
  species_scientific = "Species (scientific name)",
  species_common     = "Species (common name)",
  life_stage         = "Life stage observed",
  country            = "Country of observation",
  group_size         = "Group size observed",
  social_system      = "Social system observed",
  evidence_type      = "Evidence type",
  habitat_type       = "Habitat type (of species in wild)",
  permission_display = "Permission to display this record publicly"
)
rename_map <- rename_map[rename_map %in% names(df)]
df <- dplyr::rename(df, !!!rename_map)

df$species_scientific <- trimws(gsub("\\s+", " ", as.character(df$species_scientific)))
df <- dplyr::filter(df, !is.na(species_scientific), species_scientific != "")

# ---- Taxonomy ----
cat("Loading rfishbase taxonomy...\n")
tax <- tryCatch(as.data.frame(rfishbase::load_taxa()), error = function(e) NULL)

get_tax <- function(sp) {
  if (is.null(tax)) return(list(family = NA, order = NA, class = NA))
  cs <- intersect(c("Species","species"), names(tax))[1]
  row <- tax[tax[[cs]] == sp, ]
  if (nrow(row) == 0) return(list(family = NA, order = NA, class = NA))
  list(
    family = row[[intersect(c("Family","family"), names(tax))[1]]][1],
    order  = row[[intersect(c("Order","order"),   names(tax))[1]]][1],
    class  = row[[intersect(c("Class","class"),   names(tax))[1]]][1]
  )
}

# ---- HTML template ----
page_template <- function(title, description, body_html, canonical, jsonld = "") {
  paste0('<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>', esc(title), '</title>
<meta name="description" content="', esc(description), '">
<link rel="canonical" href="', esc(canonical), '">
<meta property="og:title" content="', esc(title), '">
<meta property="og:description" content="', esc(description), '">
<meta property="og:url" content="', esc(canonical), '">
<meta property="og:type" content="article">
', jsonld, '
<style>
body { font-family: system-ui,-apple-system,"Segoe UI",sans-serif; background:#fef5d7; color:#000; margin:0; }
.container { max-width: 900px; margin: 0 auto; padding: 30px 20px 60px; }
h1 { font-style: italic; color: #2c3e50; margin-bottom: 4px; }
h1 .common { font-style: normal; font-weight: 400; color:#7a4a2b; font-size: 0.6em; }
.summary { background: #FFE3CC; border: 2px solid #F57627; border-radius: 12px; padding: 18px 22px; margin: 20px 0; }
.summary table { width: 100%; border-collapse: collapse; }
.summary td { padding: 5px 0; vertical-align: top; }
.summary td.label { color: #D45F14; font-weight: 600; width: 190px; }
.photo { text-align: center; margin: 20px 0; }
.photo img { max-width: 100%; max-height: 350px; border-radius: 10px; border: 2px solid #F57627; box-shadow: 0 4px 12px rgba(0,0,0,0.15); }
.btn { display: inline-block; padding: 12px 22px; border-radius: 8px; color: #fff; text-decoration: none; font-weight: 600; margin: 4px 6px 4px 0; }
.btn-primary { background: #F57627; font-size: 16px; }
.btn-sec { background: #097FC8; font-size: 14px; }
a { color: #D45F14; }
.footer { margin-top: 40px; font-size: 13px; color: #7a4a2b; border-top: 1px solid #e8d4b5; padding-top: 16px; }
nav a { margin-right: 16px; font-weight: 500; }
</style>
</head>
<body>
<div class="container">
<nav><a href="/">All species</a> <a href="', esc(shiny_app_url), '">Interactive app</a></nav>
', body_html, '
<div class="footer">ShoalBase is a living, community-contributed database of fish social behaviour. Data is added and curated continuously — the content of this page reflects submissions as of ', format(Sys.Date(), "%d %B %Y"), '. <a href="', esc(shiny_app_url), '">Explore the full interactive dataset</a>.</div>
</div>
</body>
</html>')
}

# ---- Generate one page per species ----
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

species_list <- sort(unique(df$species_scientific))
cat(sprintf("Generating %d species pages...\n", length(species_list)))

index_rows <- list()

for (sp in species_list) {
  sub <- df[df$species_scientific == sp, ]
  n_records <- nrow(sub)

  common <- sub$species_common[!is.na(sub$species_common) & sub$species_common != ""]
  common <- if (length(common)) common[1] else NA

  social   <- split_multi(sub$social_system,  valid_social_system)
  groups   <- split_multi(sub$group_size,     valid_group_size)
  stages   <- split_multi(sub$life_stage,     valid_life_stage)
  habitats <- split_multi(sub$habitat_type,   valid_habitat_type)
  evidence <- split_multi(sub$evidence_type,  valid_evidence_type)
  countries <- sort(unique(na.omit(clean_country(
    trimws(unlist(strsplit(sub$country[!is.na(sub$country)], ",")))
  ))))

  tx <- get_tax(sp)
  img <- fetch_wiki_image(sp)

  title <- if (!is.na(common)) paste0(sp, " (", common, ") | ShoalBase") else paste0(sp, " | ShoalBase")
  desc_parts <- c(
    paste0("Social behaviour, group sizes, and habitat for ", sp,
           if (!is.na(common)) paste0(" (", common, ")") else "", "."),
    if (length(social))   paste0("Social systems: ", paste(social, collapse = ", "), ".") else NULL,
    if (length(habitats)) paste0("Habitats: ", paste(habitats, collapse = ", "), ".") else NULL,
    paste0(n_records, " record", if (n_records == 1) "" else "s", " in ShoalBase.")
  )
  description <- paste(desc_parts, collapse = " ")
  canonical <- paste0(site_url, "/", slugify(sp), ".html")

  jsonld <- sprintf('<script type="application/ld+json">%s</script>',
    jsonlite::toJSON(list(
      "@context" = "https://schema.org",
      "@type" = "Taxon",
      "name" = sp,
      "alternateName" = if (!is.na(common)) common else NULL,
      "parentTaxon" = if (!is.na(tx$family)) tx$family else NULL,
      "taxonRank" = "species",
      "url" = canonical
    ), auto_unbox = TRUE, null = "null"))

  fmt <- function(x) if (length(x) == 0) "<em>not reported</em>" else esc(paste(x, collapse = ", "))

  photo_html <- if (!is.null(img)) sprintf('<div class="photo"><img src="%s" alt="%s"></div>', esc(img), esc(sp)) else ""

  body <- paste0(
    '<h1>', esc(sp),
    if (!is.na(common)) paste0(' <span class="common">', esc(common), '</span>') else "",
    '</h1>',
    '<p>ShoalBase has <strong>', n_records, ' record', if (n_records == 1) "" else "s", '</strong> for this species',
    if (length(countries)) paste0(", reported from ", length(countries), " ", if (length(countries) == 1) "country" else "countries") else "",
    '.</p>',
    photo_html,
    '<div class="summary"><table>',
    '<tr><td class="label">Social systems</td><td>', fmt(social), '</td></tr>',
    '<tr><td class="label">Group sizes</td><td>', fmt(groups), '</td></tr>',
    '<tr><td class="label">Habitats</td><td>', fmt(habitats), '</td></tr>',
    '<tr><td class="label">Life stages</td><td>', fmt(stages), '</td></tr>',
    '<tr><td class="label">Evidence sources</td><td>', fmt(evidence), '</td></tr>',
    '<tr><td class="label">Countries</td><td>', fmt(countries), '</td></tr>',
    if (!is.na(tx$family)) paste0('<tr><td class="label">Family</td><td>', esc(tx$family), '</td></tr>') else "",
    if (!is.na(tx$order))  paste0('<tr><td class="label">Order</td><td>', esc(tx$order), '</td></tr>') else "",
    '</table></div>',
    '<p><a class="btn btn-primary" href="', esc(shiny_app_url), '?species=',
    utils::URLencode(sp, reserved = TRUE), '">Explore interactively on ShoalBase \u2192</a></p>',
    '<p>',
    '<a class="btn btn-sec" href="https://www.fishbase.se/summary/', esc(gsub(" ", "-", sp)), '.html">FishBase</a>',
    '<a class="btn btn-sec" href="https://www.gbif.org/species/search?q=', utils::URLencode(sp, reserved = TRUE), '">GBIF</a>',
    '<a class="btn btn-sec" href="https://www.inaturalist.org/taxa/search?q=', utils::URLencode(sp, reserved = TRUE), '">iNaturalist</a>',
    '</p>'
  )

  html <- page_template(title, description, body, canonical, jsonld)
  writeLines(html, file.path(output_dir, paste0(slugify(sp), ".html")), useBytes = TRUE)

  index_rows[[sp]] <- list(
    sci = sp, common = common, n = n_records, slug = slugify(sp),
    family = tx$family, order = tx$order
  )
}

# ---- Index page ----
cat("Writing index page...\n")
rows_html <- vapply(index_rows, function(r) {
  paste0('<tr><td><a href="', r$slug, '.html"><em>', esc(r$sci), '</em></a>',
         if (!is.na(r$common)) paste0(' <span style="color:#7a4a2b">(', esc(r$common), ')</span>') else "",
         '</td><td>', if (!is.na(r$family)) esc(r$family) else "—",
         '</td><td>', if (!is.na(r$order)) esc(r$order) else "—",
         '</td><td style="text-align:right">', r$n, '</td></tr>')
}, character(1))

index_body <- paste0(
  '<h1 style="font-style:normal">ShoalBase species index</h1>',
  '<p>', length(index_rows), ' species currently in the database. ',
  'Click any species for details, or use the <a href="', esc(shiny_app_url),
  '">interactive app</a> to explore the full dataset.</p>',
  '<table style="width:100%; border-collapse:collapse; background:#fff; border-radius:8px; overflow:hidden;">',
  '<thead><tr style="background:#F57627; color:#fff;">',
  '<th style="padding:10px; text-align:left">Species</th>',
  '<th style="padding:10px; text-align:left">Family</th>',
  '<th style="padding:10px; text-align:left">Order</th>',
  '<th style="padding:10px; text-align:right">Records</th>',
  '</tr></thead><tbody>',
  paste(rows_html, collapse = "\n"),
  '</tbody></table>'
)

writeLines(
  page_template(
    "ShoalBase species index",
    paste0("Browse all ", length(index_rows), " fish species in ShoalBase, a community database of fish social behaviour."),
    index_body,
    paste0(site_url, "/")
  ),
  file.path(output_dir, "index.html"),
  useBytes = TRUE
)

# ---- Sitemap ----
cat("Writing sitemap...\n")
sitemap <- paste0('<?xml version="1.0" encoding="UTF-8"?>\n<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">\n',
  '<url><loc>', site_url, '/</loc></url>\n',
  paste(vapply(index_rows, function(r) paste0('<url><loc>', site_url, '/', r$slug, '.html</loc></url>'), character(1)), collapse = "\n"),
  '\n</urlset>')
writeLines(sitemap, file.path(output_dir, "sitemap.xml"), useBytes = TRUE)

cat(sprintf("Done. %d species pages + index + sitemap written to %s/\n", length(index_rows), output_dir))
