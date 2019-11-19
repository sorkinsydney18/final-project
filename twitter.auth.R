consumer_key <- "1G8amEhpNj0qkbU34l9oa1FFa"
consumer_secret <- "y14jSk4QQFZOXLcU4ogH3w0bYfRtTlNJlguUKDYfYRH36YvAAW"
app_name <- "gov_1005_proj"

token <- create_token(app_name, consumer_key, consumer_secret)

write_rds(token, path = "shiny_files/twitter_token.rds")
saveRDS(token, path_to_token)

env_var <- paste0("TWITTER_PAT=", path_to_token)

cat(env_var, file = file.path(path.expand("~"), ".Renviron"), 
    fill = TRUE, append = TRUE)

readRenviron("~/.Renviron")
