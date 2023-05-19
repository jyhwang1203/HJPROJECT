#install.packages("shinydashboard")
ipak <- function(pkg){ 
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 
pkg <-c("remotes","chatgpt","httr","stringr","PerformanceAnalytics","reshape","openai")
ipak(pkg)
Sys.setenv(OPENAI_API_KEY = api_key)
api_key <- "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" # Don't share this! 😅


# Calls the ChatGPT API with the given prompt and returns the answer
ask_chatgpt <- function(prompt) {
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", api_key)),
    content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-3.5-turbo",
      messages = list(list(
        role = "user",
        content = prompt
      ))
    )
  )
  str_trim(content(response)$choices[[1]]$message$content)
}

cat(ask_chatgpt("What do you think about R language?"))
cat(comment_code("for (i in 1:10) {\n  print(i ** 2)\n}"))

cat(complete_code("# A function to square each element of a vector\nsquare_each <- function("))


# Set your OpenAI API key
openai_key <- "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" # Don't share this! 😅
set_openai_key(openai_key)

# Create a prompt
prompt <- "What is the capital of France?"

# Generate a response
response <- completions(prompt = prompt, engine = "davinci", max_tokens = 50)

# Print the response
cat(response$choices[[1]]$text)