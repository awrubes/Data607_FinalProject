library(rvest)
library(stringr)
library(tidyverse)
library(dplyr)
library(xml2)
library(httr)
library(wdman)
library(RSelenium)
library(netstat)
library(purrr)


#artist dataframe to store collected data
artists_df <- tibble(
  ArtistName = character(),
  Medium = character(),
  ArtworkTitle = character(),
  Dimensions = character(),
  PriceEstimate = character(),
  SaleDate = character(),
  SalePrice = character(),
  AuctionHouse = character()
)

#example, though this can be converted to a full list of artist names
artists <- c("Jackson Pollock")

#web scraping using rselenium

selenium_object <- selenium(retcommand = T, check = F)
binman::list_versions("chromedriver")

rD <- rsDriver(browser = "firefox", port = free_port(), verbose = FALSE, geckover = NULL)
remDr <- rD[["client"]]

login_url <- "https://www.artprice.com/identity"
remDr$navigate(login_url)

# input field selectors
username_field <- remDr$findElement(using = 'css selector', "input[class='e2e-login-input']")
password_field <- remDr$findElement(using = 'css selector', "input[class='e2e-pwd-input']")
login_button <- remDr$findElement(using = 'css selector', "button[class='e2e-login-submit-btn']")

#login credentials
username_field$sendKeysToElement(list("abwrubel@gmail.com"))
password_field$sendKeysToElement(list("Tigger123$%"))

#login to artprice
login_button$clickElement()

Sys.sleep(5) 

safe_extract <- function(artwork, css_selector) {
  tryCatch({
    element <- artwork$findElement(using = 'css selector', css_selector)
    text <- element$getElementText()
    return(text[[1]])
  }, error = function(e) {
    return(NA)
  })
}

waitForElement <- function(remDr, css_selector, timeout = 10) {
  for (i in seq_len(timeout)) {
    if (length(remDr$findElements(using = 'css selector', css_selector)) > 0) {
      return(TRUE)
    } else {
      Sys.sleep(1)
    }
  }
  stop("Timeout waiting for element")
}

#remDr$findElement(using = 'css selector', "input[id='universal-search-input']")
#search_input <- remDr$findElement(using = 'css selector', "input[id='universal-search-input']")
#search_input$clearElement()
#Sys.sleep(5)  # wait for any JS reactions to the clear action
#Sys.sleep(5)


artist_data_text <- purrr::imap(all_artists_data, function(artist_data, artist_name) {
  paste("Artist: ", artist_name, "\n", 
        sapply(artist_data, function(texts) {
          paste("Artwork Details: ", paste(texts, collapse = " | "), "\n\n")
        }),
        collapse = "\n")
})

# The folder path
folder_path <- file.path(Sys.getenv("HOME"), "Desktop", "ArtistData")

# Create the folder if it doesn't exist
if (!dir.exists(folder_path)) {
  dir.create(folder_path, recursive = TRUE)
}


save_artist_data <- function(artist_data, artist_name, folder_path) {
  # Create a file name
  file_name <- file.path(folder_path, paste0(gsub(" ", "_", artist_name), "_data.txt"))
  
  # Convert artist's data to a readable string format
  artist_data_text <- sapply(artist_data, function(texts) {
    paste("Artwork Details: ", paste(texts, collapse = " | "), "\n\n")
  }, simplify = FALSE)
  
  # Combine all artwork details into one large string
  final_text <- paste(artist_data_text, collapse = "\n")
  
  # Write to a text file named after the artist
  writeLines(final_text, file_name)
}

max_artworks <- 5 
all_artists_data <- list()
for (artist_name in artists) {

  remDr$navigate("https://www.artprice.com/search")
  Sys.sleep(5)
  
  #enter name of artist
  remDr$findElement(using = 'css selector', "input[id='universal-search-input']")
  search_input <- remDr$findElement(using = 'css selector', "input[id='universal-search-input']")
  search_input$clearElement()
  Sys.sleep(5)  # wait for any JS reactions to the clear action
  search_input$sendKeysToElement(list(artist_name, "\uE007")) 
  Sys.sleep(5)
  
  artist_links <- remDr$findElements(using = 'css selector', '.items .item .r1 a')
  print(length(artist_links)) 
  if (length(artist_links) > 0) {
    for (link in artist_links) {
      link_text <- link$getElementText()[[1]]
      cat("Found Link Text:", link_text, "\n")  # Output found text for debugging
      
      # Convert both texts to lower case to ensure case insensitive comparison
      if (tolower(link_text) == tolower(artist_name)) {
        link$clickElement()  # Click on the correct link
        Sys.sleep(10) # Allow artist page to load
        break
      }
    }
  } else {
    cat("No links found. Check the selector or the page for changes.")
    next
  }
  
  print(artist_name)
  
  #look at past auctions
  if (waitForElement(remDr, "a[id='sln_ps']")) {
    past_auctions <- remDr$findElement(using = 'css selector', "a[id='sln_ps']")
    past_auctions$clickElement()
  } else {
    message("Element not found")
  }
  
  open_filter <- remDr$findElement(using ='css selector', '.common-dropdown.right.common-drop-down-to-validate')
  open_filter$clickElement()
  Sys.sleep(3)
  auction_asc <- remDr$findElement(using ='css selector', "label[for='sort-radio-datesale_asc']")
  auction_asc$clickElement()
  Sys.sleep(3)
  apply_button<- remDr$findElement(using = 'css selector', ".btn.btn-primary.pull-right")
  apply_button$clickElement()
  
  artwork_divs <- remDr$findElements(using = 'css selector', '.lots-list.lots-square .lot-container .lot.lot-square')
  print(length(artwork_divs))
  
  all_texts <- list()
  
  for (i in seq_len(min(length(artwork_divs), max_artworks))) {
    artwork <- artwork_divs[[i]]
    # Fetch all lot data blocks
    lot_data_elements <- artwork$findElements(using = 'css selector', '.lot-datas-block')
    texts <- sapply(lot_data_elements, function(element) element$getElementText()[[1]], simplify = TRUE, USE.NAMES = FALSE)
    
    # Remove duplicates
    unique_texts <- unique(texts)
    
    # Debugging output
    print(paste("Iteration:", i))
    print("Texts before removing duplicates:")
    print(texts)
    print("Texts after removing duplicates:")
    print(unique_texts)
    
    # Store texts
    all_texts <- c(all_texts, paste(unique_texts, collapse = " | "))  # Using unique_texts instead of texts
    Sys.sleep(5)
    
  }
  
  Sys.sleep(5)
  
  open_filter <- remDr$findElement(using ='css selector', '.common-dropdown.right.common-drop-down-to-validate')
  open_filter$clickElement()
  Sys.sleep(5)
  auction_desc <- remDr$findElement(using ='css selector', "label[for='sort-radio-datesale_desc']")
  auction_desc$clickElement()
  Sys.sleep(5)
  apply_button<- remDr$findElement(using = 'css selector', ".btn.btn-primary.pull-right")
  apply_button$clickElement()
  
  Sys.sleep(3)
  
  artwork_divs <- remDr$findElements(using = 'css selector', '.lots-list.lots-square .lot-container .lot.lot-square')
  print(length(artwork_divs))
  
  for (i in seq_len(min(length(artwork_divs), max_artworks))) {
    artwork <- artwork_divs[[i]]
    # Fetch all lot data blocks
    lot_data_elements <- artwork$findElements(using = 'css selector', '.lot-datas-block')
    texts <- sapply(lot_data_elements, function(element) element$getElementText()[[1]], simplify = TRUE, USE.NAMES = FALSE)
    
    # Remove duplicates
    unique_texts <- unique(texts)
    
    # Debugging output
    print(paste("Iteration:", i))
    print("Texts before removing duplicates:")
    print(texts)
    print("Texts after removing duplicates:")
    print(unique_texts)
    
    # Store texts
    all_texts <- c(all_texts, paste(unique_texts, collapse = " | "))   # Using unique_texts instead of texts
    
  }
  all_artists_data[[artist_name]] <- all_texts
  save_artist_data(all_texts, artist_name, folder_path)
  Sys.sleep(10)
}

#stop the session
remDr$close()
rD[["server"]]$stop()


#once you have the text files in the folder, will need to iterate through text files and use regex to process data
#adding to dataframe







