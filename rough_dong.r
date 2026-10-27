#library(tidyverse)
library(stringr)
library(tm)
library(SnowballC)
library(wordcloud)
library(fs)
library(purrr)

# filename <- "../transcripts/AD-FREE_Episode_022_-_LIVE_in_Melbourne_Dave_Thornton_Nick_Cody_Cameron_James.af00ce.txt"
# x1_raw <- scan(filename, "", sep = "\n")

input_filenames <- dir_ls("../transcripts/", glob = "*.txt")

input_list <- map(input_filenames, ~ scan(.x, what = "", sep = "\n"))

input_vector <- map_chr(input_filenames, ~ paste(scan(.x, what = "", sep = "\n"), collapse = "\n")) %>%
  strsplit("\n") %>%
  unlist()

names(input_vector) <- NULL

# for quick testing
x1 <- head(input_vector)
x1 <- input_vector

spoken_lines <- x1 |> str_remove("^[^:]+: ") |> tolower()


sms_corpus <- VCorpus(VectorSource(spoken_lines))
print(sms_corpus)
inspect(sms_corpus[1:2])
as.character(sms_corpus[[1]])

sms_corpus_clean <- tm_map(sms_corpus,
                           content_transformer(tolower))
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)
sms_corpus_clean <- tm_map(sms_corpus_clean,
                           removeWords, stopwords())
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)
# debatable whether stemming should be done.
#sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)

wordcloud(sms_corpus_clean, min.freq = 42, random.order = FALSE)


# make the matrix
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)


# frequency
group_index <- rep(1, nrow(sms_dtm))

# Find the most frequent terms across all documents combined
findMostFreqTerms(sms_dtm, 69, INDEX = group_index)
