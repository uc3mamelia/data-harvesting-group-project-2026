# Dark Souls Wiki Scraper

## What is Dark Souls

Dark Souls is a dark fantasy action role-playing game series developed by FromSoftware and published by Bandai Namco Entertainment. The first game of the series was released in 2011, receiving immense critical acclaim and it is still widely considered a masterpiece in the industry.

## What does the Web Scraper Do

The web scraper, written in R code, creates a database of the items collected in the game, the item's wiki URL, the item's category, the description players are given when they find the item, and the location of the item in the game. 

It pulls this information from the Wiki, beginning on this page https://darksouls.wiki.fextralife.com/Equipment+&+Magic. It scrapes the URLs from that page, then the sub-category URLs (for example, the magic page is divided into Sorceries, Pyromancies, and Miracles), then each item URL (example: https://darksouls.wiki.fextralife.com/Soul+Arrow), and from that page scrapes the item name, description, and location. The category and subcategory are derived from the category and subcategory URLs and where the item lives in the site.

## Dependencies

The code uses the packages xml2, httr, and tidyverse. These can be installed using install.packages("[package name]"). 

- xml2 allows searching on XML paths.
- httr provides functions like GET() and POST().
- tidyverse allows manipulation of the gathered data to clean it up and make it readable, utilizing functions like mutate(), str_detect(), and distinct().
- base package R provides functions like cat() to print command-line messages, and basic functions like nrow() to count number of rows.

## How to Use

If needed, install the packages and then comment the lines. Run each section of code at a time for best results. 

## Expected Output

A csv file with the following columns:
