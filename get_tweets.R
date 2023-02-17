library(jsonlite)
library(data.table)
library(tidyverse)
library(glue)
library(here)
library(academictwitteR)
library(quanteda)
library(quanteda.sentiment)
library(quanteda.textmodels)
library(quanteda.textplots)
library(quanteda.textstats)

here <- here()

cs_key <- Sys.getenv("google_key")
cx <- Sys.getenv("google_cx")
# First, use the google API to do searches of Chinese ambassador twitter
# accounts in like 10 countries

countries <- c("United States", "Australia", "United Kingdom", "France", "Mexico", "India", "Japan", "South Korea", "Germany", "Canada")
outdir <- glue("{here}/data/json")

getAmbassadorHandles <- function(country) {
    # country <- countries[1]
    # Testing above.

    search_term <- glue("Chinese PRC ambassador to {country} twitter account")
    search_term <- str_replace_all(search_term, " ", "%20")
    country_outfile <- tolower(str_replace_all(country, "\\W+", "_"))
    json_out <- glue("{outdir}/{country_outfile}.json")

    if (file.exists(json_out)) {
        return()
    }

    url <- glue("https://customsearch.googleapis.com/customsearch/v1?cx={cx}&q={search_term}&key={cs_key}")
    try(res <- read_json(url))
    if (!exists("res")) {
        print("URL didn't work")
        return()
    }
    res1 <- map_df(res$items, ~ as.data.table(.x))
    rownums <- nrow(res1)
    jsonlite::write_json(res1, json_out)
}
walk(countries, ~ getAmbassadorHandles(.x))

json_files <- list.files(outdir, full.names = T)

getTwitterHandles <- function(json) {
    # file <- json_dir_both$json_dir_files[1]
    # json <- json_files[2]
    # Testing above.

    json_in <- read_json(json)
    handle <- json_in %>%
        tibble(result = json_in) %>%
        unnest_wider(result) %>%
        distinct(htmlTitle, .keep_all = T) %>%
        filter(str_detect(link, "https://twitter.com")) %>%
        pluck("link")

    country <- str_replace_all(basename(json), ".json", "")
    return(data.table(country, handle))
}

df_handles <- map_df(json_files, ~ getTwitterHandles(.x))
df_handles[, handle := str_remove_all(handle, "\\?.*$")]
df_handles[, handle := str_remove_all(handle, "^.*?\\.com\\/")][]

# Now, use the twitter API to grab the full tweet history of all these accounts.
getTweetArchives <- function(handle) {
    # handle <- df_handles$handle[1]
    print(glue("Getting tweets for {handle}..."))
    # Testing above
    academictwitteR::get_all_tweets(
        users = handle,
        start_tweets = "2020-01-01T00:00:00Z",
        end_tweets = "2022-02-15T00:00:00Z",
        data_path = glue("./data/tweets/{handle}"),
        n = 2000
    )
}

walk(df_handles$handle, ~ getTweetArchives(.x))

# Now, we have a bunch of json files in ./data/tweets/
parseTweets <- function(json_dir) {
    # json_dir <- json_dirs[1]
    print(json_dir)
    # Testing above
    handle_f <- basename(json_dir)
    country <- df_handles[handle == handle_f]$country[1]

    # Use this ref: https://cran.r-project.org/web/packages/academictwitteR/vignettes/academictwitteR-tidy.html
    tweet_text <- bind_tweets(json_dir, output_format = "tidy") %>%
        select(text)

    tweet_timestamp <- bind_tweets(json_dir, output_format = "tidy") %>%
        select(created_at) %>%
        mutate(created_at = lubridate::ymd_hms(created_at))

    return(data.table(country, handle = handle_f, tweet_text, tweet_timestamp))
}
json_dirs <- list.dirs("./data/tweets", recursive = F)

possibly_parseTweets <- possibly(parseTweets, otherwise = NULL)
df_tweets <- map_df(json_dirs, ~ possibly_parseTweets(.x))

corp_tweets <- quanteda::corpus(df_tweets)

dfmat_tweets <- quanteda::tokens(corp_tweets) %>%
    dfm()
# summary(dfmat_tweets)
summary(corp_tweets)