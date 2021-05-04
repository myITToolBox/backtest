install.packages('rsconnect')

rsconnect::setAccountInfo(name='henrique-anatole', token='E0250C32822A43625968EB68DCF2E6B2', secret='3j6I49GROqkJuQKYh+BXy62rUvF5PJllQzDVkLY9')

library(rsconnect)
rsconnect::deployApp('path/to/your/app')