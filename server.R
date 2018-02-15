#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tm)
library(ngramrr)

# Loading functions
corpus_pre <- function(corpus) {
  
  # Remove punctuation
  corpus_pro <- tm_map(corpus, removePunctuation)
  
  # Remove numbers
  corpus_pro <- tm_map(corpus_pro, removeNumbers)
  
  # Change to lowercase.
  corpus_pro <-
    tm_map(corpus_pro, content_transformer(tolower))
  
  # Strip whitespace
  corpus_pro <- tm_map(corpus_pro, stripWhitespace)
  
  # Stem
  # corpus_pro <- tm_map(corpus_pro, stemDocument)
  
  # Remove stopwords
  corpus_pro <-
    tm_map(corpus_pro, removeWords, stopwords("en"))
  
  # Return value.
  return(corpus_pro)
  
}
backoffmodel_katz <- function(phrase) {
  
  if (typeof(phrase) == "character") {
    
    mod_trigram <- function(tokens) {
      
      key <- function(tokens) {
        paste(
          tail(
            tokens,
            n = 2
          )[1],
          tail(
            tokens,
            n = 2
          )[2]
        )
      }
      
      # find matches and count
      matchesCnt <- function(phrase) {
        sapply(
          names(
            which(
              sapply(
                Terms(tdm_trigram),
                function(terms) {
                  grepl(
                    phrase,
                    paste(
                      strsplit(
                        terms, split = " "
                      )[[1]][1],
                      strsplit(
                        terms, split = " "
                      )[[1]][2]
                    ),
                    ignore.case = TRUE
                  )
                }
              )
            )
          ),
          function(match) sum(tm_term_score(tdm_trigram, match))
        )
      }
      
      # The most common match
      comm_match <- function(phrase) {
        matches <- matchesCnt(phrase)
        if (length(matches) > 0) {
          tail(
            strsplit(
              names(
                head(
                  which(matches == max(matches)),
                  n = 1
                )
              )
              , split = " ")[[1]],
            n = 1
          )
        } else mod_tdm_bigram(tail(inp_corpus, n = 1))
      }
      
      return(
        comm_match(key(tokens))
      )
      
    }
    
   mod_tdm_bigram <- function(token) {
      
      # find matches and their count
      matchesCnt <- function(phrase) {
        sapply(
          names(
            which(
              sapply(
                Terms(tdm_bigram),
                function(terms) {
                  grepl(
                    phrase,
                    strsplit(
                      terms, split = " "
                    )[[1]][1],
                    ignore.case = TRUE
                  )
                }
              )
            )
          ),
          function(match) sum(tm_term_score(tdm_bigram, match))
        )
      }
      
      # find the last word of the most frequent match
      comm_match <- function(phrase) {
        matches <- matchesCnt(phrase)
        if (length(matches) > 0) {
          tail(
            strsplit(
              names(
                head(
                  which(matches == max(matches)),
                  n = 1
                )
              )
              , split = " ")[[1]],
            n = 1
          )
        } else mod_unigram(tail(inp_corpus, n = 1))
      }
      
      return(
        comm_match(token)
      )
      
    }
    
    mod_unigram <- function(token) {
      
      associations <-
        findAssocs(tdm_unigram, token, corlimit = .99)[[1]]
      if (length(associations) > 0) {
        names(sample(which(associations == max(associations)), 1))
      } else return("will")
      
    }
    
    # preprocess phrase
   inp_corpus <-
      VCorpus(
        VectorSource(phrase),
        list(reader = PlainTextDocument)
      )
   inp_corpus <- corpus_pre(inp_corpus)
   inp_corpus <- scan_tokenizer(inp_corpus[[1]][[1]][1])
    
    return(
      if (length(inp_corpus) >= 2) {
        mod_trigram(inp_corpus)
      } else if (length(inp_corpus) == 1) {
       mod_tdm_bigram(inp_corpus)
      } else return("will")
    )
    
  } else {
    stop("non-character or null input")
  }
  
}

# Load term-document matrices
load("ngrams.RData")

# Main function
shinyServer(
  function(input, output) {
    
    output$phrase <-
      renderText(
        {
          if (input$predictButton == 0) "in progress ..."
          else input$phrase
        }
      )
    
    
    output$word <-
      renderText(
        {
          if (input$predictButton == 0) "in progress ..."
          else backoffmodel_katz(input$phrase)
        }
      )
    
  }
)