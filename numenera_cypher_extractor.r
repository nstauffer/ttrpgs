### Extracts cyphers (or artifacts, with some tweaking) from sourcebooks for Monte Cook Game's Numenera
### This requires you to already have the sourcebooks, so as to respectfully not violate the fan use policy
### https://www.montecookgames.com/fan-support/fan-use-policy/
### As currently written, this is set up for Sir Arthour's Technology Compendium. Filenames/locations are not
### written to be stored for easy generalization, so tread carefully!
###-----------------------------------------------------------------

### Just installing and attaching packages. I abandoned streamlining via tidyr because hacky
### solutions were faster for me to implement. Feel free to improve them!
install.packages("dplyr")
install.packages("tm")
#install.packages("tidyr")
install.packages("stringr")
require(tm)
require(dplyr)
#require(tidyr)
require(stringr)

### Before you start! This is to convert a sourcebook to .txt
### The rest of the script assumes that you've done this and then made a separate .txt file that contains
### just the cyphers chapter(s)
## Download and install pdftotxt from
## ftp://ftp.foolabs.com/pub/xpdf/xpdfbin-win-3.03.zip
# exe <- "C:/Program Files (x86)/xpdfbin-win-3.04/bin64/pdftotext.exe"
# pdf <- "C:/Users/NS/Documents/Numenera/Numenera-Ninth_World_Guidebook.pdf"
# exe <- "C:/Program Files (x86)/xpdfbin-win-3.04/bin64/pdftotext.exe"
# system(paste("\"", exe, "\" \"", pdf, "\"", sep = ""), wait = F)
 
## Read in the textfile containing just the chunk of the document that listed cyphers
## My method was to copy and paste in notepad to cut the .txt down to the relevant chapter[s]
cyphers.arthour <- "C:/Users/NS/Documents/Numenera/cyphers.txt" %>% readLines()
 
## Remove all the cruft lines that don't contain information about the cyphers
cyphers.arthour <- cyphers.arthour[cyphers.arthour != ""]
cyphers.arthour <- cyphers.arthour[!grepl("TECHNOLOGY COMPENDIUM", cyphers.arthour)]
cyphers.arthour <- cyphers.arthour[!grepl("SIR ARTHOUR\'S GUIDE TO THE NUMENERA CYPHERS", cyphers.arthour)]
cyphers.arthour <- cyphers.arthour[!grepl("Search terms:", cyphers.arthour)]
cyphers.arthour <- cyphers.arthour[is.na(cyphers.arthour %>% as.numeric())]
 
## Start a data table with these things and prime the id number column because each cypher is getting an id
cyphers.raw <- data.frame(cyphers.arthour, stringsAsFactors = F)
cyphers.raw$id <- NA
 
## Go through and any line that has "Level:" in it gets an id number (the row number it occurs in) because
## every cypher has that line and it's easy to identify with a grep. The previous line should always be the
## cypher name, so it gets the same number
for(n in 1:nrow(cyphers.raw)){
  if (grepl("Level:", cyphers.raw$cyphers.arthour[n])){
    cyphers.raw$id[n] <- n
    cyphers.raw$id[n-1] <- n
  }
}
 
## This just takes all the other lines and gives them the id of the previous line that had an id because they probably belong
## that cypher (when there're tables of detonation effects and things) but need some kind of home regardless
for(n in 1:nrow(cyphers.raw)){
  if (is.na(cyphers.raw$id[n])){
    cyphers.raw$id[n] <- {
      cyphers.raw$id[cyphers.raw$id < n] %>% na.omit() %>% as.vector %>% last()
    }
  }
}
 
## Making it tidy. Each id number gets a row with a column for each line associated with it. Yes, it's a mess, but it works
cyphers.tidier <- cyphers.raw %>%
  group_by(id) %>%
  summarize(
    line.1 = nth(cyphers.arthour, 1),
    line.2 = nth(cyphers.arthour, 2),
    line.3 = nth(cyphers.arthour, 3),
    line.4 = nth(cyphers.arthour, 4),
    line.5 = nth(cyphers.arthour, 5),
    line.6 = nth(cyphers.arthour, 6),
    line.7 = nth(cyphers.arthour, 7),
    line.8 = nth(cyphers.arthour, 8),
    line.9 = nth(cyphers.arthour, 9),
    line.10 = nth(cyphers.arthour, 10),
    line.11 = nth(cyphers.arthour, 11),
    line.12 = nth(cyphers.arthour, 12),
    line.13 = nth(cyphers.arthour, 13),
    line.14 = nth(cyphers.arthour, 14),
    line.15 = nth(cyphers.arthour, 15),
    line.16 = nth(cyphers.arthour, 16),
    line.17 = nth(cyphers.arthour, 17),
    line.18 = nth(cyphers.arthour, 18),
    line.19 = nth(cyphers.arthour, 19),
    line.20 = nth(cyphers.arthour, 20)
  )
 
## Honestly, I don't know if these are still necessary as primers
cyphers.tidier$wearable <- NA
cyphers.tidier$usable <- NA
cyphers.tidier$internal <- NA
cyphers.tidier$effect <- NA
 
## The real sh*t. This grabs the qualities of the cypher and writes them into a column that matches that name
for (n in 1:nrow(cyphers.tidier)){
  ## line.2 is where things like the level and effect can be found usually, so this makes a vector of strings, each representing
  ## a word
  contents <- str_split(cyphers.tidier$line.2[n], " ") %>% as.vector()
  contents <- contents[[1]]
  ## Identifying the locations of things like "Level:" and "Effect:" in the vector
  header.locs <- grep(":", contents)
  ## Getting the names of those qualities, so I have both the quality and the location in the vector
  header.names <- contents[header.locs] %>% str_replace_all(":", "") %>% str_to_lower()
  ## I know that they always list level first, so this creates a string from all the strings in the vector located between
  ## "Level:" and the next quality, whatever it might be
  cyphers.tidier$level[n] <- paste(contents[2:(header.locs[2] - 1)], collapse = " ")
  ## This goes through the rest of the qualities in identified by the grep() above and writes them into columns using their
  ## names. There are a lot of weird false positives here because the .pdf wasnt formatted for this use
  for (x in 2:length(header.locs)){
    if (x < length(header.locs)){
      cyphers.tidier[n, header.names[x]] <- paste(contents[(header.locs[x] + 1):(header.locs[x + 1] - 1)], collapse = " ")
    } else {
      cyphers.tidier[n, header.names[x]] <- paste(contents[(header.locs[x] + 1):length(contents)], collapse = " ")
    }
  }
}
 
## So, there were lots of lines beyond the one that starts with "Level:" that sometimes have relevant information
## This takes all the other lines and combines them into one string, stripping out all the NAs and trimming whitespace
for (n in 1:nrow(cyphers.tidier)){
  cyphers.tidier$additional[n] <- paste(cyphers.tidier[n, 4:21], collapse = " ") %>% gsub("NA", "", .) %>% str_trim()
}
 
## Maybe I should have a name field?
cyphers.tidier$name <- cyphers.tidier$line.1
 
## Put everything into a nice, cleanish data frame. These would definitely need to be changed for artifacts
cyphers <- cyphers.tidier[,c("name", "level", "usable", "wearable", "internal", "effect", "additional")]

## Writing out the data table for final editing and formatting
write.csv(cyphers, "C:/Users/NS/Documents/Numenera/cypher_table_uncorrected.csv")
