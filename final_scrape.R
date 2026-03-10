# =============================================================================
# dark_souls_scraper.R
#
# PURPOSE: Scrape item descriptions from the Dark Souls Fextralife wiki in
#          a single pipeline, staying entirely in memory until the final
#          output CSV is written.
#
# TO DO: Add the item locations to the scraped data.
#
# PIPELINE:
#   Stage 1: Scrape group URLs from the Equipment & Magic main page
#   Stage 2: Scrape the subcategory URLs from each group
#   Stage 3: Scrape individual item URLs from each subcategory page
#   Stage 4: Scrape item descriptions from each individual item page
#   Output: .csv file with all scraped data
#            data/dark_souls_items.csv
#            Columns: item_name, group, subcategory, url, path,  description, scrape_status
#                 
# =============================================================================

# =============================================================================
# PACKAGES
# =============================================================================
library(xml2)
library(httr)
library(tidyverse)

# =============================================================================
# DEFINING THE BASE URLs
# =============================================================================
BASE_URL      <- "https://darksouls.wiki.fextralife.com"
EQUIPMENT_URL <- paste0(BASE_URL, "/Equipment+&+Magic")

# =============================================================================
# CHECKING PERMISSIONS
# this fetches and displays the robots.txt file of the wiki
# then the user has to verify and confirm scraping is allowed to continue
# scraping is allowed at the moment, but it should be checked anytime the code
# is run in case permissions change
# =============================================================================
cat("CHECKING ROBOTS.TXT...\n\n")

robots_txt <- GET(paste0(BASE_URL, "/robots.txt")) |> content(as = "text")
cat(robots_txt)
cat("\n")

confirm <- readline("Proceed with scrape? (y/n): ")

if (tolower(confirm) != "y") {
  stop("SCRAPE ABORTED.")
}

cat("\nPROCEEDING WITH SCRAPE\n\n")

# =============================================================================
# SETTING USER AGENT
# a descriptive user agent helps the website owner understand our purpose
# =============================================================================
set_config(
  user_agent("Mozilla/5.0 (compatible; DarkSoulsLoreScraper/1.0; Academic project)")
)

# =============================================================================
# STAGE 1: Scrape group URLs from the Equipment & Magic main page (excluding upgrades)
#          EXPECTED OUTPUT: 6 group URLs
# =============================================================================

# -----------------------------------------------------------------------------
# STAGE 1: SCRAPING GROUP URLs
# -----------------------------------------------------------------------------
cat("STAGE 1: SCRAPING GROUP URLs\n")

# pauses between each request. this avoids overwhelming
Sys.sleep(2) 

# accessing the "starting page"
equipment_page <- read_html(GET(EQUIPMENT_URL) |> content(as = "text"))

# there are 6 main groups of items we want to scrape: armor, shields, weapons,
# magic, rings and items. we want to scrape their names and urls

# first we locate the nodes of our desired links through a common unique pattern:
# <a> tags with class wiki_link that sit inside an <h3> tag anywhere 
# within the div with id wiki-content-block
group_links <-
  equipment_page |>
  xml_find_all("//div[@id='wiki-content-block']//h3//a[@class='wiki_link']")

# now we extract the nodes' contents: (group_name and href) into a tibble
# href is the relative path of the link. joined with the base url, it gives us 
# the group's url
# removing the "upgrades" row from the results because it's not useful for us
group_urls_tb <- tibble(
  group_name = xml_text(group_links),
  href = xml_attr(group_links, "href")) |>
  filter(href != "/Upgrades") |> 
  mutate(url = paste0(BASE_URL, href)) 
  # adds URL column by combining the base URL with the relevant href

cat("GROUP URLs FOUND:\n")
print(group_urls_tb) 
cat(sprintf("\nTOTAL GROUPS: %d\n\n", nrow(group_urls_tb)))

# -----------------------------------------------------------------------------
# STAGE 2: SCRAPING SUBCATEGORY URLs
# Three page layouts are handled at Level 2:
#
#   Layout A - Tab layout (Armor, Shields, Magic, Items):
#     Subgroup links are in buttons that use class "btn-default fullwidth" 
#     inside the first div[@align='center'].
# 
#   Layout B - Gallery layout (Weapons):
#     Subcategories are wiki_link anchors inside col-sm-2 divs within
#     row gallery divs that are direct children of wiki-content-block
#     AND appear before the h2 titlearea header.
#
#   Layout C - Direct category (Rings):
#     The group page itself is the only category. Detected when neither
#     Layout A or Layout B returns any results.
# -----------------------------------------------------------------------------
cat("STAGE 2: SCRAPING SUBCATEGORY URLs\n")
get_subcategory_urls <- function(group_name, group_url) {
  # shows what group is being scraped currently
  cat(sprintf("  FETCHING SUBCATEGORIES FOR: %s\n", group_name))
  Sys.sleep(2)
  
# fetches the group page as a html node tree to start from there
page <- read_html(GET(group_url) |> content(as = "text"))

# saves the group's relative path again, we will need it later
group_href <- str_remove(group_url, fixed(BASE_URL))


# CHECKING FOR LAYOUT A: tab-based pages (Armor, Shields, Magic, Items)

# we target only the first div[@align='center'] to avoid picking up a 
# second group of tabs further down the page
tabs <- xml_find_all(
  page,
  "(//div[@align='center'])[1]//a[contains(@class,'btn-default fullwidth')]"
)
  
# in the "items" page, the first tab has a different styling, "btn-success fullwidth"
# so we need a special pattern for it
active_tab <- xml_find_all(
  page,
  "//a[contains(@class,'btn-success fullwidth')]"
)
  
# if "tabs" found any results (confirming we're on a tab-based page), it 
# combines the href from both types of tags into a single vector
# and then returns a tibble with the group name, relative hrefs and full 
# URLs for all subcategories

# As one of the tabs leads back to the group's main page (i.e. "all shields"),
# the group's own href was scraped too and needs to be filtered out

  if (length(tabs) > 0) {
    hrefs <- c(
      xml_attr(active_tab, "href"),
      xml_attr(tabs, "href")
    )
    hrefs <- hrefs[hrefs != group_href]
    cat(sprintf("    (tab layout, %d subcategories found)\n", length(hrefs)))
    return(tibble(
      group = group_name,
      href = hrefs,
      full_url = paste0(BASE_URL, hrefs)
    ))
  }

# if something is returned, the function stops and loops to the next group
# if not, it checks for other layouts
  
# CHECKING FOR LAYOUT B: gallery-based pages (Weapons)

# searches for wiki_link anchors inside col-sm-2 divs within row gallery divs 
# that are direct children of wiki-content-block AND have at least one <h2> element following them
  gallery_nodes <- xml_find_all(
    page,
    "//div[@id='wiki-content-block']/div[contains(@class,'row gallery')][count(following-sibling::h2)>0]//div[contains(@class,'col-sm-2')]//a[@class='wiki_link']"
  )
  
# If gallery_nodes found any results (confirming we're on a gallery-based page), 
# it returns a tibble with the group name, relative hrefs and full URLs for all 
# subcategory links found
  if (length(gallery_nodes) > 0) {
    cat(sprintf("    (gallery layout, %d subcategories found)\n", length(gallery_nodes)))
    return(tibble(
      group    = group_name,
      href     = xml_attr(gallery_nodes, "href"),
      full_url = paste0(BASE_URL, xml_attr(gallery_nodes, "href"))
    ))
  }
  
# CHECKING FOR LAYOUT C: direct category page (Rings)
# when neither the tab nor gallery layout is detected, the function returns 
# a tibble with just the group's own URL as the single subcategory, treating 
# the group page itself as the final page
# currently only applies to Rings
  cat(sprintf("    (no subcategories found, treating page as direct category)\n"))
  tibble(
    group    = group_name,
    href     = group_href,
    full_url = group_url
  )
}
# now we call the defined function:
# map2_dfr() loops over the group names and URLs simultaneously, applying 
# our function to each pair and combining all the results into one data frame
all_subcats <- map2_dfr(
  group_urls_tb$group_name,
  group_urls_tb$url,
  get_subcategory_urls
) |>
# adds subcategory column to tibble based off href names
  mutate(
    subcategory = href |>
      str_remove("^/") |>
      str_replace_all("\\+", " ")
  )

cat("\nSUBCATEGORY URLs FOUND:\n")
print(all_subcats, n = Inf)
cat(sprintf("\nTOTAL SUBCATEGORIES SCRAPED: %d\n\n", nrow(all_subcats)))

# =============================================================================
# STAGE 3: SCRAPING INDIVIDUAL ITEM URLs FROM EACH SUBCATEGORY PAGE
# Two page layouts are handled:
#
#   Layout A - table-based (most pages):
#     Items are in wiki_table(s). Stats tables are identified by header
#     keywords and skipped. Column selection checks td[1] first and falls
#     back to td[2] if td[1] contains no text links.
#
#   Layout B - gallery-based (i.e. Armor):
#     Items are in col-sm-2 divs inside a row gallery div.
#
# Upgrade variants (e.g. "Pyromancy Flame (Upgraded)") are excluded as
# they are states of existing items rather than distinct items and have
# no descriptions on the wiki.
# =============================================================================
cat("STAGE 3: SCRAPING INDIVIDUAL ITEM URLs\n")

# defines the scrape_item_links function which takes three arguments — the group 
# name, subcategory name, and relative path
# then pauses for 2 seconds, constructs the full URL, and prints a progress message 
# showing the current group, subcategory and URL being scraped
scrape_item_links <- function(group_name, subcategory_name, path) {
  
  Sys.sleep(2) # 2 second pause
  
  url <- paste0(BASE_URL, path) # constructing URL
  cat(sprintf("  [%-22s > %-20s] %s\n", group_name, subcategory_name, url)) 
  # progress message
  
# tryCatch is R's error handling mechanism -  if the page fetches successfully 
# it continues normally
# but if anything goes wrong it issues a warning with the error message and returns 
# NULL instead of crashing the entire script
# we can either leave the tryCatch() parts in or take them out, it just means 
# the script will crash if there is an error
  page <- tryCatch( 
    read_html(url),
    error = function(e) {
      warning(sprintf("Could not fetch %s: %s", url, e$message))
      return(NULL)
    }
  )
  
  if (is.null(page)) return(data.frame())
  
# This searches the fetched page for all <table> elements whose class contains 
# wiki_table (the wiki's standard table format)
# stores them all as a collection so we can loop over them in the next step.
  all_tables <- xml_find_all(page, "//table[contains(@class,'wiki_table')]")
  
# checks whether any wiki_table elements were found on the page, if none were 
# found it means the page uses a gallery layout instead of a table layout
# and moves on to layout B check
  if (length(all_tables) == 0) {
# Layout B: gallery-based (e.g. Unique Armor)
    cat(sprintf("    (no table found, trying gallery layout)\n")) 
# this is also just a text output that is't necessarily needed
# searches the page for wiki_link anchors inside col-sm-2 divs 
# (the gallery structure used), 
# then extracts the href attribute and visible text from each link into separate 
# vectors hrefs and names
# these are used later to build the output data frame.
    gallery_links <- xml_find_all(
      page,
      "//div[contains(@class,'col-sm-2')]//a[contains(@class,'wiki_link')]"
    )
    hrefs <- xml_attr(gallery_links, "href")
    names <- xml_text(gallery_links, trim = TRUE)
    
# opens the table layout branch and initialises two empty character vectors 
# hrefs and names these act as containers that will be filled up as we loop 
# over each table on the page, accumulating all the item links and names 
# found across multiple tables
} else {

# Layout A: table-based
    hrefs <- character(0)
    names <- character(0)
    
# opens the loop that iterates over each table found on the page, and for each 
# table extracts the text from the first header cell (<th>) in the first row
# this header text is then used in the next step to identify and skip stats 
# tables that were accidentally being scraped otherwise
# The .// at the start of the XPath means "search anywhere within the current 
# table" rather than search the whole page
    for (tbl in all_tables) {
      first_header <- xml_find_first(tbl, ".//tr[1]//th[1]") |>
        xml_text(trim = TRUE)
      
      # skip stats tables identified by header keywords
      if (!is.na(first_header) && str_detect(first_header, regex(
        "defense|reduction|requirements|bonus|attack|parameter",
        ignore_case = TRUE
      ))) {
        next
      }
      
# extracts item links from each table by checking both the first and second columns: 
# td1 gets all wiki_link anchors from the first column and td2 from the second
# td1_names extracts the text from the first column links, and best then selects 
# whichever column contains actual text (at least 2 characters)
# this handles the inconsistency across pages where some tables have the item name 
# in the first column while others have an image in the first column and the name 
# in the second
# selected links and names are then appended to the hrefs and names vectors using c()
      td1       <- xml_find_all(tbl, ".//tr/td[1]//a[@class='wiki_link']")
      td2       <- xml_find_all(tbl, ".//tr/td[2]//a[@class='wiki_link']")
      td1_names <- xml_text(td1, trim = TRUE)
      best      <- if (any(nchar(td1_names) >= 2)) td1 else td2
      hrefs     <- c(hrefs, xml_attr(best, "href"))
      names     <- c(names, xml_text(best, trim = TRUE))
    }
  }
  
# this prints a warning message identifying which subcategory had no results 
# and returns an empty data frame
# again, we can get rid of the message if we want and also get rid of the empty 
# data frame creation but it means
# if there is an issue the script will just crash
  if (length(hrefs) == 0) {
    warning(sprintf("No item name links found for: %s", subcategory_name))
    return(data.frame())
  }
  
# strips the base URL prefix from any hrefs that might contain it, ensuring all 
# hrefs are stored as relative paths
# fixed() ensures the base URL is treated as a literal string rather than a 
# regular expression
  hrefs <- str_remove(hrefs, fixed(BASE_URL))
  
# Filter to exclude upgrade variants
  is_item_link <- nchar(names) >= 2 &
    !str_detect(names, regex("upgraded", ignore_case = TRUE))
  
  hrefs <- hrefs[is_item_link]
  names <- names[is_item_link]
  
  if (length(hrefs) == 0) return(data.frame())
  
# defining the final dataframe
  data.frame(
    item_name   = names,
    group       = group_name,
    subcategory = subcategory_name,
    url         = paste0(BASE_URL, hrefs),
    path        = hrefs,
    stringsAsFactors = FALSE
  )
}

# announces the start of the scraping
cat(sprintf("COLLECTING SUBCATEGORY URLs\n\n", nrow(all_subcats)))

# loops over all three columns of the named list simultaneously, passing each 
# set of values as group_name, subcategory_name and path to the function, and 
# combines all the returned data frames into one
#  distinct() then removes any duplicate URLs that may have appeared across 
# multiple subcategory pages
# arrange() sorts the final result alphabetically by group, then subcategory, then item name
all_items <- pmap_dfr(
  list(
    group_name       = all_subcats$group,
    subcategory_name = all_subcats$subcategory,
    path             = all_subcats$href
  ),
  scrape_item_links
) |>
  distinct(url, .keep_all = TRUE) |>
  # remove duplicates
  arrange(group, subcategory, item_name)

# announces end of scraping
cat(sprintf("\nFINISHED. TOTAL ITEM URLS COLLECTED: %d\n\n", nrow(all_items)))
# shows breakdown of scrape
all_items |> count(group, subcategory) |> as.data.frame() |> print()

# =============================================================================
# STAGE 4: SCRAPING ITEM DESCRIPTIONS
# Two strategies are tried in order:
#
#   Strategy 1 - blockquote:
#     Most items have their description inside a <blockquote> tag within
#     wiki-content-block. xml_text() captures the full text regardless of
#     whether it is wrapped in <p>, <em>, or both.
#
#   Strategy 2 - quoted paragraph:
#     Some items (certain armor pieces, some weapons) have their description
#     in a plain <p> tag starting with a quote character, with no blockquote.
# =============================================================================
cat("STAGE 4: SCRAPING ITEM DESCRIPTIONS\n")

extract_description <- function(page) {
  
# Strategy 1: blockquote
# searches the page for the first <blockquote> element within wiki-content-block, 
# which is the HTML structure used by most item pages to contain the in-game description text
  blockquote <- xml_find_first(
    page,
    "//div[@id='wiki-content-block']//blockquote"
  )
  
# If a blockquote was found (i.e. it is not NA), this extracts the full text 
# content from it, with trim = TRUE removing leading and trailing whitespace 
# and str_squish() collapsing any internal whitespace into single spaces
# returns a list with the description text and a status of "ok_blockquote"
# to indicate which strategy successfully found the description
  if (!is.na(blockquote)) {
    desc <- xml_text(blockquote, trim = TRUE) |> str_squish()
    return(list(desc = desc, status = "ok_blockquote"))
  }

# Strategy 2: <p> tag starting with a quote character
# searches the page for the first <p> element within wiki-content-block 
# whose text starts with a quote character "
# the normalize-space(.) part strips any leading whitespace from the 
# text before checking, ensuring we don't miss paragraphs where the first 
# quote character is preceded by a space.
  desc_p <- xml_find_first(
    page,
    "//div[@id='wiki-content-block']//p[starts-with(normalize-space(.), '\"')]"
  )
  
# Same structure as the blockquote check — if a matching <p> tag was found, 
# it extracts and cleans the text and returns it with a status of "ok_p_tag"
  if (!is.na(desc_p)) {
    desc <- xml_text(desc_p, trim = TRUE) |> str_squish()
    return(list(desc = desc, status = "ok_p_tag"))
  }
  
# if neither strategy found a description, it returns a list with NA as the 
# description and "not_found" as the status.
  return(list(desc = NA_character_, status = "not_found"))
}

# creates the function to scrape the location
# the location info sits inside a <ul> tag directly below a <h3> tag that contains
# one of these words: find, location or acquired
# since we don't have a ignore caps function in xml_find_all, we will find all 
# <h3> tags and filter them with str_detect

extract_location <- function(page) {
  all_h3s <- xml_find_all(page, "//h3")
  
  target_index <- which(str_detect(
    xml_text(all_h3s), 
    regex("find|location|acquired", ignore_case = TRUE)
  ))
  
  if (length(target_index) > 0) {
    target_node <- all_h3s[target_index[1]]
    
    # Combined XPath to grab all <li> items directly [cite: 32, 36, 39]
    li_nodes <- xml_find_all(target_node, "./following-sibling::ul[1]/li")
    
    if (length(li_nodes) > 0) {
      # Extract and clean text [cite: 50]
      li_texts <- xml_text(li_nodes, trim = TRUE) %>% str_squish()
      
      # Step A: Add a dot if there isn't one (Regex: if it doesn't end in .)
      # The pattern "[.]$" looks for a literal dot at the end of the string.
      li_texts <- ifelse(str_detect(li_texts, "[.]$"), 
                         li_texts, 
                         paste0(li_texts, "."))
      
      # Step B: Format as [1] - text1. [2] - text2.
      formatted_locs <- paste0("[", seq_along(li_texts), "] - ", li_texts, 
                               collapse = " ")
      
      return(list(loc = formatted_locs, status = "ok_loc_list"))
    }
  }
  
  return(list(loc = NA_character_, status = "not_found")) [cite: 53]
}

# stores the total number of items as n so it can be referenced throughout 
# the scraping loop, then prints a progress message to the console showing how 
# many item pages are about to be scraped.
n <- nrow(all_items)
cat(sprintf("SCRAPING DESCRIPTIONS FOR %d ITEMS...\n\n", n)) # another progress signpost

# Scrape all descriptions and add directly to all_items
# map_dfr() here is looping over the numbers 1 to n (one number per item), 
# and for each number i it pulls out that row from all_items and prints a 
# progress message showing which item we're currently scraping
# map_dfr() automatically combines all the results into one data frame at the end
all_descriptions <- map_dfr(
  300:350,
  function(i) {
    item <- all_items[i, ]
    cat(sprintf("[%d/%d] %-40s", i, n, substr(item$item_name, 1, 40)))
    
    # 2 second pause again
    Sys.sleep(2) 
    
# fetches and parses the individual item page, using tryCatch() to handle any errors
# if the page loads successfully it continues normally, but if anything goes 
# wrong it prints the error message to the console and returns NULL instead of crashing the script
# it is  the same pattern as earlier  but here we construct the full URL on the 
# fly using paste0(BASE_URL, item$path) rather than having it pre-built
    page <- tryCatch(
      read_html(GET(paste0(BASE_URL, item$path)) |> content(as = "text")),
      error = function(e) {
        cat(sprintf(" ERROR: %s\n", e$message))
        return(NULL)
      }
    )
    
# If the page failed to fetch (i.e. tryCatch() returned NULL), this prints 
# [fetch_error] to the console to flag the problem, then returns the item row 
# with NA as the description and "fetch_error" as the status
# like above e need to decide if we want to keep this so the script won't
# crash or just take out and allow crashes
    if (is.null(page)) {
      cat(" [fetch_error]\n") # displays that there was an error fetching the description
      return(item |> mutate(description = NA_character_, scrape_status = "fetch_error"))
    }
    
# calls extract_description() on the successfully fetched page, prints the
# status (e.g. [ok_blockquote], [ok_p_tag], or [not_found]) to the console so
# we can monitor progress, then returns the item row with two new columns added 
# description containing the extracted text and scrape_status indicating which
# strategy found it
    
    extracted_desc <- extract_description(page)
    extracted_loc  <- extract_location(page)
    
    cat(sprintf(" [Desc: %s | Loc: %s]\n", extracted_desc$status, extracted_loc$status))
    
    item |> mutate(
      description = extracted_desc$desc,
      scrape_status = extracted_desc$status,
      location = extracted_loc$loc,
      loc_status = extracted_loc$status
    )
  }
)

cat("FINISHED SCRAPING ITEM DESCRIPTIONS\n")

# =============================================================================
# OUTPUT: WRITE FINAL CSV
# =============================================================================

cat("FINAL DATA\n")

# what the scrape has scraped
cat(sprintf("TOTAL ROWS: %d\n", nrow(all_descriptions)))
cat(sprintf("FOUND: %d (%.0f%%)\n",
            sum(!is.na(all_descriptions$description)),
            100 * mean(!is.na(all_descriptions$description))))
# for identifying problems with the scrape
cat(sprintf("NOT FOUND: %d\n", sum(all_descriptions$scrape_status == "not_found")))
cat(sprintf("FETCH ERRORS: %d\n", sum(all_descriptions$scrape_status == "fetch_error")))

dir.create("data", showWarnings = FALSE) 
#creates a data folder to save .csv to -> do we have to chage this for the final product?
write_csv(all_descriptions, "data/dark_souls_items.csv")
cat(sprintf("\nSAVED: data/dark_souls_items.csv\n"))



# # WANDERER HOOD
# # xpath
# //*[@id="wiki-content-block"]/ul[2]
# 
# # find h3 tag with one of these words and then an ul inside
# 
# # SMALL LEATHER SHIELD
# # xpath
# //*[@id="wiki-content-block"]/ul[1]
# # need to filter out "video location timestamp"
# 
# # DAGGER
# # xpath
# //*[@id="wiki-content-block"]/ul[1]
# 
# # MOONLIGHT BUTTERFLY HORN
# # xpath
# //*[@id="wiki-content-block"]/ul[1]
# 
# # SOUL ARROW
# # xpath
# //*[@id="wiki-content-block"]/ul[2]
# 
# # TINY BEING'S RING
# # xpath
# //*[@id="wiki-content-block"]/ul[2]
# 
# # STANDARD ARROW
# # xpath
# //*[@id="wiki-content-block"]/ul[2]

# locations are lists, we need to separate into "location 1", "location 2" etc


all_descriptions <- read_csv("sagradas_escrituras_ds1.csv")

# 1. Cargar y filtrar

resultados <- all_descriptions %>%
  filter(str_detect(description, regex("Havel", ignore_case = TRUE)))

# 2. Mostrar en formato "párrafo" legible
for(i in 1:nrow(resultados)) {
  cat(paste0("--- ITEM: ", resultados$item_name[i], " ---\n"))
  cat(resultados$description[i], "\n\n")
}



