## ----------------------------------------------------------------------------------------------------------------------
set.seed(124)


## ----------------------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------------------
# Generate a new cipher by permuting the letters of the alphabet
generate_cipher <- function() sample(letters, replace = FALSE)

# Encode a text using a cipher
encode_text <- function(text, cipher){
  chartr(x = text,
         old = paste(letters, collapse = ""),
         new = paste(cipher, collapse = ""))
}

# Decode a text given a cipher
decode_text <- function(ciphered_text, cipher){
  chartr(x = ciphered_text,
         old = paste(cipher, collapse = ""),
         new = paste(letters, collapse = ""))
}


## ----------------------------------------------------------------------------------------------------------------------
plaintext <- "AV AL T QWVS AJRWGKOJV XJ OHOZS GTLXJ VX LWIIXZV T KZXVPOZ'L RPTZTRVOZ AJ PAL TKLOJRO OYWTNNS TL VPXWDP PO EOZO IZOLOJV; JXV VX ZOHANO PAG KOPAJQ PAL KTRU JXZ LWMMOZ AV VX KO QXJO KS XVPOZL, EAVPXWV WLAJD OHOZS JOROLLTZS TVVOGIV VX IZOHOJV AV"

# Create and store the cipher
true_cipher <- generate_cipher()


## ----------------------------------------------------------------------------------------------------------------------
# Encode the plaintext
ciphertext <- encode_text(plaintext, true_cipher)


## ----------------------------------------------------------------------------------------------------------------------
# Decode the ciphertext
decoded_text <- decode_text(ciphertext, true_cipher)

## ----------------------------------------------------------------------------------------------------------------------
war_and_peace <- readr::read_file("https://www.gutenberg.org/cache/epub/2600/pg2600.txt")


## ----------------------------------------------------------------------------------------------------------------------
war_and_peace <-
  war_and_peace |>
  stringr::str_to_lower() |>
  gsub(pattern = "[^A-Za-z ]+", replacement = "", x=_) |>
  stringi::stri_trans_general(id = "Latin-ASCII")


## ----------------------------------------------------------------------------------------------------------------------
test_string <- "hello there"

starting_indices <- 1 : (nchar(test_string) - 1)
ending_indices <- starting_indices + 1


## ----------------------------------------------------------------------------------------------------------------------


## ----------------------------------------------------------------------------------------------------------------------


## ----------------------------------------------------------------------------------------------------------------------


## ----------------------------------------------------------------------------------------------------------------------
break_into_two_chars <- function(text){
  starting_indices <- 1 : (nchar(text) - 1)
  ending_indices <- starting_indices + 1
  return(stringi::stri_sub(text,
                           from = starting_indices,
                           to = ending_indices))
}


## ----------------------------------------------------------------------------------------------------------------------
war_and_peace_2_characters <- break_into_two_chars(war_and_peace)


## ----------------------------------------------------------------------------------------------------------------------


## ----------------------------------------------------------------------------------------------------------------------
# Ten most common two-character combinations
probability_table <-
  table(war_and_peace_2_characters) / length(war_and_peace_2_characters)


## ----------------------------------------------------------------------------------------------------------------------


## ----------------------------------------------------------------------------------------------------------------------


## ----------------------------------------------------------------------------------------------------------------------


## ----------------------------------------------------------------------------------------------------------------------
get_prob_two_char <- function(two_char){
  prob_from_table <- probability_table[two_char]
  
  if (is.na(prob_from_table)) {
    return(1 / length(war_and_peace_2_characters))
  } else{
    return(prob_from_table)
  }
}


## ----------------------------------------------------------------------------------------------------------------------


## ----------------------------------------------------------------------------------------------------------------------


## ----------------------------------------------------------------------------------------------------------------------
get_log_lik_text <- function(text){
  text |>
  break_into_two_chars() |>
  purrr::map_dbl(get_prob_two_char) |>
  log() |>
  sum()
}


## ----------------------------------------------------------------------------------------------------------------------


## ----------------------------------------------------------------------------------------------------------------------
swap <- function(x){
  # Select two distinct indices
  rand_indices <- sample(1:length(x), size = 2, replace=FALSE)
  element_1 <- x[rand_indices[1]]
  element_2 <- x[rand_indices[2]]
  
  x[rand_indices[1]] <- element_2
  x[rand_indices[2]] <- element_1
  
  return(x)
}


## ----------------------------------------------------------------------------------------------------------------------
ciphered_text <- "LIVITCSWPIYVEWHEVSRIQMXLEYVEOIEWHRXEXIPFEMVEWHKVSTYLXZIXLIKIIXPIJVSZEYPERRGERIMWQLMGLMXQERIWGPSRIHMXQEREKIETXMJTPRGEVEKEITREWHEXXLEXXMZITWAWSQWXSWEXTVEPMRXRSJGSTVRIEYVIEXCVMUIMWERGMIWXMJMGCSMWXSJOMIQXLIVIQIVIXQSVSTWHKPEGARCSXRWIEVSWIIBXVIZMXFSJXLIKEGAEWHEPSWYSWIWIEVXLISXLIVXLIRGEPIRQIVIIBGIIHMWYPFLEVHEWHYPSRRFQMXLEPPXLIECCIEVEWGISJKTVWMRLIHYSPHXLIQIMYLXSJXLIMWRIGXQEROIVFVIZEVAEKPIEWHXEAMWYEPPXLMWYRMWXSGSWRMHIVEXMSWMGSTPHLEVHPFKPEZINTCMXIVJSVLMRSCMWMSWVIRCIGXMWYMX"

ciphered_text <-
  ciphered_text |>
  stringr::str_to_lower() |>
  gsub(pattern = "[^A-Za-z ]+", replacement = "", x=_) |>
  stringi::stri_trans_general(id = "Latin-ASCII")

current_cipher <- generate_cipher()

i <- 0
for (iter in 1:1e7) {

  proposed_cipher <- swap(current_cipher)

  decoded_text_proposed <- decode_text(ciphered_text,
                              cipher = proposed_cipher)

  decoded_text_current <- decode_text(ciphered_text,
                              cipher = current_cipher)

  proposed_log_lik <- get_log_lik_text(decoded_text_proposed)
  current_log_lik <- get_log_lik_text(decoded_text_current)

  acceptance_probability <- min(1, exp(proposed_log_lik - current_log_lik))
  accept <- sample(c(TRUE, FALSE), size=1, prob = c(acceptance_probability, 1-acceptance_probability))

  if (accept) {
    current_cipher <- proposed_cipher
    print(glue::glue("Iteration {i}: {decoded_text_proposed}"))
    i <- i + 1
  }
}

