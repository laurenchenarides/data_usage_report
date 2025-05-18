install.packages('rsconnect')
library(rsconnect)

rsconnect::setAccountInfo(name='laurenchenarides', token='619BA0A32D0ED67ACE7B573988FFA321', secret='VDRlZUpVUW4asvgNmo/H/mDE/zYu1j3ZfyOOVAsg')

rsconnect::deployApp("C:/Users/lachenar/OneDrive - Colostate/Documents/GitProjectsWithR/shinyapps/questionnaire.qmd")

