library(devtools)
if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")
}

devtools::install_github("bradleyboehmke/harrypotter")

library(tidyverse)      # data manipulation & plotting
library(stringr)        # text cleaning and regular expressions
library(tidytext)       # provides additional text mining functions
library(harrypotter)

install.packages(("tidytext"))

library(tidytext)

text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

text
library(dplyr)
text_df <- data_frame(line = 1:4, text = text)

text_df

text_df %>%
  unnest_tokens(word, text)

text_df %>%
  unnest_tokens(word, text)


library(janeaustenr)
library(dplyr)
library(stringr)

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()

x <- original_books
head(x, 20)


tidy_books <- original_books %>%
  unnest_tokens(word, text)

tidy_books
