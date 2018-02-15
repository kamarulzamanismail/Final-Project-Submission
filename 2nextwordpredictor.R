
download_data_file <- function(location, data_file) {

    if (!file.exists(data_file)) {

        download.file(
            url = location,
            destfile = data_file,
            method = "curl"
        )

    }

}



uncompress_data_file <- function(location, data_file) {

    if (!dir.exists(location)) {

        unzip(data_file)

    }

}


sample_data_file <- function() {

    if (!file.exists("./final/de_DE/de_DE.blogs.sample.txt")) {

        system(
            "
            cd ./final/de_DE;
            gshuf -n 1000 de_DE.blogs.txt > de_DE.blogs.sample.txt;
            gshuf -n 1000 de_DE.news.txt > de_DE.news.sample.txt;
            gshuf -n 1000 de_DE.twitter.txt > de_DE.twitter.sample.txt;
            rm de_DE.blogs.txt;
            rm de_DE.news.txt;
            rm de_DE.twitter.txt;
            cd ..;
            cd ./en_US;
            gshuf -n 1000 en_US.blogs.txt > en_US.blogs.sample.txt;
            gshuf -n 1000 en_US.news.txt > en_US.news.sample.txt;
            gshuf -n 1000 en_US.twitter.txt > en_US.twitter.sample.txt;
            rm en_US.blogs.txt;
            rm en_US.news.txt;
            rm en_US.twitter.txt;
            cd ..;
            cd ./fi_FI;
            gshuf -n 1000 fi_FI.blogs.txt > fi_FI.blogs.sample.txt;
            gshuf -n 1000 fi_FI.news.txt > fi_FI.news.sample.txt;
            gshuf -n 1000 fi_FI.twitter.txt > fi_FI.twitter.sample.txt;
            rm fi_FI.blogs.txt;
            rm fi_FI.news.txt;
            rm fi_FI.twitter.txt;
            cd ..;
            cd ./ru_RU;
            gshuf -n 1000 ru_RU.blogs.txt > ru_RU.blogs.sample.txt;
            gshuf -n 1000 ru_RU.news.txt > ru_RU.news.sample.txt;
            gshuf -n 1000 ru_RU.twitter.txt > ru_RU.twitter.sample.txt;
            rm ru_RU.blogs.txt;
            rm ru_RU.news.txt;
            rm ru_RU.twitter.txt;
            cd ..; cd ..;
            "
        )

    }

}




read_data <- function(language) {

    if (language == "de") {

        if (!exists("corpus")) {

            VCorpus(
                DirSource("./final/de_DE"),
                readerControl = list(reader = readPlain, language  = "de")
            )

        }

    } else if (language == "en") {

        if (!exists("corpus")) {

            VCorpus(
                DirSource("./final/en_US"),
                readerControl = list(reader = readPlain)
            )

        }

    } else if (language == "fi") {

        if (!exists("corpus")) {

            VCorpus(
                DirSource("./final/fi_FI"),
                readerControl = list(reader = readPlain, language  = "fi")
            )

        }

    } else if (language == "ru") {

        if (!exists("corpus")) {

            VCorpus(
                DirSource("./final/ru_RU"),
                readerControl = list(reader = readPlain, language  = "ru")
            )

        }

    }

}



corpus_pre <- function(corpus) {

    # Remove punctuation from text.
    corpus_pro <- tm_map(corpus, removePunctuation)

    # Remove numbers from text.
    corpus_pro <- tm_map(corpus_pro, removeNumbers)

    # Convert text to lowercase.
    corpus_pro <-
        tm_map(corpus_pro, content_transformer(tolower))

    # Strip whitespace from text.
    corpus_pro <- tm_map(corpus_pro, stripWhitespace)

    # Stem the text.
    # corpus_pro <- tm_map(corpus_pro, stemDocument)

    # Remove stopwords.
    corpus_pro <-
        tm_map(corpus_pro, removeWords, stopwords("en"))

    # Return value.
    return(corpus_pro)

}




create_ngram <- function(corpus, n) {

    if (n == 1) {

        TermDocumentMatrix(corpus)

    } else {

        tdm2(corpus, ngmin = n, ngmax = n)

    }

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

            # find matches and their count
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
                } else mod_bigram(tail(inp_corpus, n = 1))
            }

            return(
                comm_match(key(tokens))
            )

        }

        mod_bigram <- function(token) {

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
                mod_bigram(inp_corpus)
            } else mod_unigram(inp_corpus)
        )

    } else {
        stop("non-character or null input")
    }

}


# # Calls of Main function 
#
# # loading packages
# library("tm")
# library("ngramrr")
#
# download_data_file(
#     "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip",
#     "Coursera-SwiftKey.zip"
# )
#
# uncompress_data_file("final", "Coursera-SwiftKey.zip")
#
# sample_data_file()
#
# corpus <- read_data("en")
#
# cleaned_corpus <- corpus_pre(corpus)
#
# tdm_unigram <- create_ngram(cleaned_corpus, 1)
# tdm_bigram <- create_ngram(cleaned_corpus, 2)
# tdm_trigram <- create_ngram(cleaned_corpus, 3)
#
# # save the n-grams
# if (!file.exists("./ngrams.RData")) {
#
#     save(tdm_unigram, tdm_bigram, tdm_trigram, file = "./ngrams.RData")
#     # load("./ngrams.RData")
#
# }

