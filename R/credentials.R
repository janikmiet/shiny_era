## Example of making the Credentials data


## Access Credentials -----
# https://datastorm-open.github.io/shinymanager/articles/SQLite_Admin.html
# https://github.com/datastorm-open/shinymanager/issues/25
# define some credentials (you can also use sqlite database)
credentials <- data.frame(
  user = c("User1", "User2", "User3", "User4"),
  password = c("password1", "password12", "password13", "password14"),
  stringsAsFactors = FALSE
)
write.table(credentials, "credentials.txt", row.names = FALSE, col.names = TRUE)

