## -----------------------------------------------------------------------------------------------------------------------
set.seed(127)


## -----------------------------------------------------------------------------------------------------------------------
chartr(
  x = "hello",
  old = "abcdefghijklmnopqrstuvwxyz",
  new = "defghijklmnopqrstuvwxyzabc"
)


## -----------------------------------------------------------------------------------------------------------------------
letters |> paste(collapse = "")

## -----------------------------------------------------------------------------------------------------------------------
sample(letters) |> paste(collapse = "")


## -----------------------------------------------------------------------------------------------------------------------
# Generate a new cipher by permuting the letters of the alphabet
generate_cipher <- function() sample(letters,
                                     replace = FALSE)

# Encode a text using a cipher
encode_text <- function(text, cipher) {
  chartr(
    x = text,
    old = paste(letters, collapse = ""),
    new = paste(cipher, collapse = "")
  )
}

# Decode a text given a cipher
decode_text <- function(ciphered_text, cipher) {
  chartr(
    x = ciphered_text,
    old = paste(cipher, collapse = ""),
    new = paste(letters, collapse = "")
  )
}


## -----------------------------------------------------------------------------------------------------------------------
plaintext <- "do dee at ale ang of ale aoscme atvnsc t ahng of alhee iaegi roys ale gtdtae ao atg ta alhee os ale aeeal do dee at ile yti do gdtns do ns ale wohsnsc iatsrnsc fomh feea aes ns ose iopv ile yti dodt ns idtpvi ile yti roddu ta iplood ile yti rodohei os ale roaaer dnse bma ns wu thwi ile yti tdytui dodnat rnr ile ltke t ghepmhioh ile rnr nsreer ile rnr ns gonsa of ftpa alehe wncla ltke bees so dodnat ta tdd ltr n soa doker ose imwweh ts nsnantd cnhd plndr ns t ghnsperow bu ale iet ol yles tboma ti wtsu uethi befohe dodnat yti bohs ti wu tce yti alta imwweh uom pts tdytui pomsa os t wmhreheh foh t ftspu ghoie iaude dtrnei tsr cesadewes of ale qmhu ejlnbna smwbeh ose ni ylta ale iehtgli ale wninsfohwer inwgde sobde ynscer iehtgli eskner doov ta alni atscde of alohsi"

# Create and store the cipher
true_cipher <- generate_cipher()
print(true_cipher)


## -----------------------------------------------------------------------------------------------------------------------
# Encode the plaintext
ciphertext <- encode_text(plaintext, true_cipher)
print(ciphertext)


## -----------------------------------------------------------------------------------------------------------------------
# Decode the ciphertext
decoded_text <- decode_text(ciphertext, true_cipher)
print(decoded_text)


## -----------------------------------------------------------------------------------------------------------------------
factorial(26)


## -----------------------------------------------------------------------------------------------------------------------
factorial(26) / (1.1 * 10^18)


## -----------------------------------------------------------------------------------------------------------------------
ciphertext


## -----------------------------------------------------------------------------------------------------------------------
war_and_peace <- readr::read_file("https://www.gutenberg.org/cache/epub/2600/pg2600.txt")


## -----------------------------------------------------------------------------------------------------------------------
war_and_peace <-
  war_and_peace |>
  stringr::str_to_lower() |>
  gsub(pattern = "[^A-Za-z ]+", replacement = "", x=_) |>
  stringi::stri_trans_general(id = "Latin-ASCII")


## -----------------------------------------------------------------------------------------------------------------------
test_string <- "hello there"

starting_indices <- 1 : (nchar(test_string) - 1)
ending_indices <- starting_indices + 1


## -----------------------------------------------------------------------------------------------------------------------
starting_indices


## -----------------------------------------------------------------------------------------------------------------------
ending_indices


## -----------------------------------------------------------------------------------------------------------------------
stringi::stri_sub(test_string,
                  from = starting_indices,
                  to = ending_indices)


## -----------------------------------------------------------------------------------------------------------------------
break_into_two_chars <- function(text){
  starting_indices <- 1 : (nchar(text) - 1)
  ending_indices <- starting_indices + 1
  return(stringi::stri_sub(text,
                           from = starting_indices,
                           to = ending_indices))
}


## -----------------------------------------------------------------------------------------------------------------------
war_and_peace_2_characters <- break_into_two_chars(war_and_peace)


## -----------------------------------------------------------------------------------------------------------------------
head(war_and_peace_2_characters)


## -----------------------------------------------------------------------------------------------------------------------
# Ten most common two-character combinations
probability_table <-
  table(war_and_peace_2_characters) / length(war_and_peace_2_characters)


## -----------------------------------------------------------------------------------------------------------------------
probability_table |> head(20)


## -----------------------------------------------------------------------------------------------------------------------
probability_table |>
  sort(decreasing = TRUE) |>
  head(10)


## -----------------------------------------------------------------------------------------------------------------------
# Get the probability of "ou"
probability_table["ou"]


## -----------------------------------------------------------------------------------------------------------------------
get_prob_two_char <- function(two_char){
  prob_from_table <- probability_table[two_char]
  
  if (is.na(prob_from_table)) {
    return(1 / length(war_and_peace_2_characters))
  } else{
    return(prob_from_table)
  }
}


## -----------------------------------------------------------------------------------------------------------------------
# Try a combination in War and Peace
get_prob_two_char("ou")
probability_table["ou"]

# Try a combination not in War and Peace
get_prob_two_char("qq")
probability_table["qq"]


## -----------------------------------------------------------------------------------------------------------------------
sample_text <- "this is a text"
sample_text_two_char <- break_into_two_chars(sample_text)

score <- 
  purrr::map_dbl(sample_text_two_char, get_prob_two_char) |>
  prod()

score


## -----------------------------------------------------------------------------------------------------------------------
get_log_lik_text <- function(text){
  text |>
  break_into_two_chars() |>
  purrr::map_dbl(get_prob_two_char) |>
  log() |>
  sum()
}


## -----------------------------------------------------------------------------------------------------------------------
get_log_lik_text("This is English text")

get_log_lik_text("fghr gh wghdfrf etfs")


## -----------------------------------------------------------------------------------------------------------------------
swap <- function(x){
  # Select two distinct indices
  rand_indices <- sample(1:length(x), size = 2, replace=FALSE)
  element_1 <- x[rand_indices[1]]
  element_2 <- x[rand_indices[2]]
  
  x[rand_indices[1]] <- element_2
  x[rand_indices[2]] <- element_1
  
  return(x)
}

## -----------------------------------------------------------------------------------------------------------------------
plaintext <- "do dee at ale ang of ale aoscme atvnsc t ahng of alhee iaegi roys ale gtdtae ao atg ta alhee os ale aeeal do dee at ile yti do gdtns do ns ale wohsnsc iatsrnsc fomh feea aes ns ose iopv ile yti dodt ns idtpvi ile yti roddu ta iplood ile yti rodohei os ale roaaer dnse bma ns wu thwi ile yti tdytui dodnat rnr ile ltke t ghepmhioh ile rnr nsreer ile rnr ns gonsa of ftpa alehe wncla ltke bees so dodnat ta tdd ltr n soa doker ose imwweh ts nsnantd cnhd plndr ns t ghnsperow bu ale iet ol yles tboma ti wtsu uethi befohe dodnat yti bohs ti wu tce yti alta imwweh uom pts tdytui pomsa os t wmhreheh foh t ftspu ghoie iaude dtrnei tsr cesadewes of ale qmhu ejlnbna smwbeh ose ni ylta ale iehtgli ale wninsfohwer inwgde sobde ynscer iehtgli eskner doov ta alni atscde of alohsi"

# Generate a random cipher to be the true cipher
true_cipher <- generate_cipher()

# Encode the plaintext
ciphered_text <- encode_text(text = plaintext,
                             cipher = true_cipher)

# Create another random cipher to be the starting cipher for the Markov Chain
current_cipher <- generate_cipher()

# A counter to track how many decoded texts have been accepted
i <- 0

for (iter in 1:50000) {
  
  # Propose a new cipher by swapping two letters in the current cipher
  proposed_cipher <- swap(current_cipher)
  
  # Text decoded from the proposal cipher
  decoded_text_proposed <- decode_text(ciphered_text,
                              cipher = proposed_cipher)
  
  # Text decoded from the current cipher
  decoded_text_current <- decode_text(ciphered_text,
                              cipher = current_cipher)
  
  # Log-likelihood of the decoded text from the proposal cipher
  proposed_log_lik <- get_log_lik_text(decoded_text_proposed)
  
  # Log-likelihood of the decoded text from the current cipher
  current_log_lik <- get_log_lik_text(decoded_text_current)
  
  # Acceptance probability of the proposal, defined by the Metropolis algorithm
  # Remember that subtraction on the log-scale is division on the probability
  # scale. We exponentiate to get back to the probability scale.
  acceptance_probability <- min(1, exp(proposed_log_lik - current_log_lik))
  
  # Accept or not with probability given by `acceptance_probability`
  accept <- sample(c(TRUE, FALSE),
                   size=1,
                   prob = c(acceptance_probability,
                            1-acceptance_probability))
  
  # IF we accept the proposal, set the proposal cipher as the current cipher
  # ELSE, go on to the next iteration
  if (accept) {
    current_cipher <- proposed_cipher
    
    # Print the text as decoded by the current cipher
    print(glue::glue("Iteration {i}: {decoded_text_proposed}"))
    
    # Increment the counter so that we can keep track of acceptances
    # This is just for printing the output
    i <- i + 1
  }
}


## -----------------------------------------------------------------------------------------------------------------------
## plaintext <- "to be or not to be that is the question whether tis nobler in the mind to suffer the slings and arrows of outrageous fortune or to take arms against a sea of troubles"
## 
## true_cipher <- generate_cipher()
## ciphered_text <- encode_text(text = plaintext,
##                              cipher = true_cipher)
## 
## current_cipher <- generate_cipher()
## 
## i <- 0
## 
## for (iter in 1:50000) {
## 
##   proposed_cipher <- swap(current_cipher)
## 
##   decoded_text_proposed <- decode_text(ciphered_text,
##                               cipher = proposed_cipher)
##   decoded_text_current <- decode_text(ciphered_text,
##                               cipher = current_cipher)
## 
##   proposed_log_lik <- get_log_lik_text(decoded_text_proposed)
##   current_log_lik <- get_log_lik_text(decoded_text_current)
## 
##   acceptance_probability <- min(1, exp(proposed_log_lik - current_log_lik))
## 
##   accept <- sample(c(TRUE, FALSE),
##                    size=1,
##                    prob = c(acceptance_probability,
##                             1-acceptance_probability))
## 
##   if (accept) {
##     current_cipher <- proposed_cipher
##     print(glue::glue("Iteration {i}: {decoded_text_proposed}"))
##     i <- i + 1
##   }
## }

