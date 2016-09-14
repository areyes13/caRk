carkness <- function(text, percent = NULL) {
  if(is.null(percent)) {
    percent = 0.2
    message('Setting cark level at 20%')
  }
  
  if(percent < 0 | percent > 1) {
    stop("'percent' must be between 0 and 1")
  }
  n <- nchar(text)
  lambda <- n * percent
  rpois(1, lambda)
}

carkening <- function(text, carkN){
  if(carkN == 0) {
    message('Wow, no typos! Very impressive!')
  }
  
  for(i in 1:carkN) {
    letter <- round(runif(1, 1, nchar(text)), 0)
    rand <- round(runif(1, 1, 26), 0)
    a <- substr(text, 1, letter-1)
    b <- substr(text, letter+1, nchar(text))
    
    text <- paste0(a, letters[rand], b)
  }
  return(text)
}

carker <- function(text, percent = NULL) {
  carkN <- carkness(text, percent)
  message(paste0(carkN, ' letters will be carked'))
  val <- carkening(text, carkN)
  val
}

text <- "hello im clark lameson and i am a butt"
carker(text)
carker(text, .1)

