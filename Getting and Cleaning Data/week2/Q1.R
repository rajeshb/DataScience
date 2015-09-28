library(httr)

oauth_endpoints("github")

myapp <- oauth_app("github",
	key = "865133a77d934f39c6a6",
	secret = "05dc40b855960483acd694435eecaab9bb981d75")

github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/jtleek/repos", gtoken)
stop_for_status(req)
repoContent <- content(req)

for (repo in repoContent) {
	if (repo$name == "datasharing") {
		print(repo$created_at)
	}
}
