# 2016 Political Sentiment Tracking using Twitter

# Clear the previously used libraries

rm(list=ls())

# Load the required R libraries
library(twitteR)
library(ROAuth)
library(httr)
library(plyr)
library(stringr)
library(ggplot2)
library(gmodels)
library(e1071)

#-----------------------------------------------------------------------------------------------

Twitter Keys

#-----------------------------------------------------------------------------------------------

# Connect to Twitter API #

setwd(workingDirectory)                                                                         # Set the working directory

# Windows users, download the curl Cert and save it at your default R folder
download.file(url='http://curl.haxx.se/ca/cacert.pem', destfile='cacert.pem')

authenticate <- OAuthFactory$new(consumerKey=polConsumerKey,
                                 consumerSecret=polConsumerSecret,
                                 requestURL=polRequestURL,
                                 accessURL=polAccessURL,
                                 authURL=polAuthURL)

# Access the Twitter API
setup_twitter_oauth(polConsumerKey,
                    polConsumerSecret,
                    access_token = accessToken,
                    access_secret = accessSecret)

save(authenticate, file="twitter authentication.Rdata")

#----------

# Functions #

# Function to conduct initial data cleaning
clean.tweets <- function(politician) {
  # Remove retweets from the list
  politician = strip_retweets(politician,strip_manual=TRUE,strip_mt=TRUE)
  # Move tweets from the list into a data frame
  politician = twListToDF(politician)
  # Use iconv to solve the UTF encoding problem
  politician$text <- iconv(politician$text, 'UTF-8', 'ASCII')
  # Remove URL references
  politician$text = gsub("http://t.co/[a-z,A-Z,0-9]*{8}", "", politician$text)
  politician$text = gsub("https://t.co/[a-z,A-Z,0-9]*{8}", "", politician$text)
  # Remove @screename references
  politician$text = gsub("@[a-z,A-Z]*", "", politician$text)
  # Remove #hashtags
  politician$text = gsub("#[a-z,A-Z]*", "", politician$text)
  # Remove numbers
  politician$text = gsub("[[:digit:]]", "", politician$text)
  # Remove punctuation
  politician$text = gsub('[[:punct:]]', '', politician$text)
  # remove unnecessary spaces
  politician$text = gsub("[ \t]{2,}", "", politician$text)
  politician$text = gsub("^\\s+|\\s+$", "", politician$text)
  # Change to lower case.
  politician$text <- tolower(politician$text)
  # Remove records with NAs in the text field.
  politician <- politician[!is.na(politician$text),]
  # Remove duplicate entries.
  politician <- politician[!duplicated(politician$text),]
}

# Function to clean the tweets and then check their sentiment
score.sentiment = function(sentences, pos.words, neg.words,.progress='none') {       
  # We have a vector of sentences; plyr will handle a list or a vector as an "l"  
  # We want a simple array ("a") of scores back, so we use "l" + "a" + "ply" = "laply" 
  
  good.smiley <- c(":)")
  bad.smiley <- c(":(",";)",":'",":P") 
  
  scores = laply(sentences, function(sentence, pos.words, neg.words) {  
    # clean up sentences with R's regex-driven global substitute, gsub()  
    sentence = gsub(":)", 'awsum', sentence)
    sentence = gsub('[[:punct:]]', '', sentence)  
    sentence = gsub('[[:cntrl:]]', '', sentence)  
    sentence = gsub('\\d+', '', sentence)   
    # Split into words. str_split is in the stringr package  
    word.list = str_split(sentence, '\\s+')  
    # Sometimes a list() is one level of hierarchy too much  
    words = unlist(word.list)  
    # Compare our words to the dictionaries of positive & negative terms 
    pos.matches = match(words, pos.words)  
    neg.matches = match(words, neg.words)  
    # match() returns the position of the matched term or NA  
    # We just want a TRUE/FALSE:  
    pos.matches = !is.na(pos.matches)  
    neg.matches = !is.na(neg.matches)  
    # TRUE/FALSE will be treated as 1/0 by sum()  
    score = sum(pos.matches) - sum(neg.matches)  
    return(score)  
  }, pos.words, neg.words, .progress=.progress )  
  scores.df = data.frame(score=scores, text=sentences)  
  return(scores.df)  
} 

# Calculate standard error of the mean
stdErr <- function(x){sqrt(var(x)/length(x))}

#----------

# Load sentiment word lists
hu.liu.pos = scan(paste(workingDirectory, '/positive-words.txt', sep = ""), what='character', comment.char=';')  # Positive words.
hu.liu.neg = scan(paste(workingDirectory, '/negative-words.txt', sep = ""), what='character', comment.char=';')  # Negative words.

# Create and (if necessary) add words to list
pos.words = c(hu.liu.pos, "awsum", "stand with", "justice", "equality", "voting for")
neg.words = c(hu.liu.neg, "wtf","murderer","no", "withholding", "fooled")

#----------

# Get tweets and score them for each politician

numTwt = 3000              # number of tweets to request from each query
rType = "recent"           # return type
keepCols <- c("text")      # columns kept in the written files

# Clinton
clinton.list <- searchTwitter("#hillaryclinton OR #imwithhillary OR #hillarysoprogressive", n=numTwt, lang = "en",
                              resultType=rType)
clinton <- clean.tweets(clinton.list)
# Remove unused columns
clinton <- clinton[keepCols]
# Write data to a politician's .csv file
write.csv(clinton, file = paste(polDirectory, "Clinton.csv", sep = ""), row.names = TRUE)
# Score Clinton's tweets.
clinton.scores = score.sentiment(clinton$text, pos.words, neg.words, .progress='text')
# Write a .csv file that shows the score for each tweet.
write.csv(clinton.scores, file = paste(polDirectory, "Clinton_Scores.csv", sep = ""), row.names = TRUE)

# Sanders
sanders.list <- searchTwitter("#berniesanders OR #feelthebern", n=numTwt, lang = "en", resultType=rType)
sanders <- clean.tweets(sanders.list)
# Remove unused columns
sanders <- sanders[keepCols]
# Write data to a politician's .csv file
write.csv(sanders, file = paste(polDirectory, "Sanders.csv", sep = ""), row.names = TRUE)
# Score Sanders' tweets.
sanders.scores = score.sentiment(sanders$text, pos.words, neg.words, .progress='text')
# Write a .csv file that shows the score for each tweet
write.csv(sanders.scores, file = paste(polDirectory, "Sanders_Scores.csv", sep = ""), row.names = TRUE)

# Add names of the politicans to the scores data
clinton.scores$Pol = 'Hillary Clinton'
sanders.scores$Pol = 'Bernie Sanders'

#----------

# Collect the results

# Extract the positive tweets
clinton.scores.pos = subset(clinton.scores, clinton.scores$score > 0)
sanders.scores.pos = subset(sanders.scores, sanders.scores$score > 0)

# Extract the neutral tweets
clinton.scores.neu = subset(clinton.scores, clinton.scores$score == 0)
sanders.scores.neu = subset(sanders.scores, sanders.scores$score == 0)

# Extract the negative tweets
clinton.scores.neg = subset(clinton.scores, clinton.scores$score < 0)
sanders.scores.neg = subset(sanders.scores, sanders.scores$score < 0)

# Breakout polarity numbers for each politician
clinton.pol.pos <- nrow(clinton.scores.pos)
clinton.pol.neu <- nrow(clinton.scores.neu)
clinton.pol.neg <- nrow(clinton.scores.neg)

sanders.pol.pos <- nrow(sanders.scores.pos)
sanders.pol.neu <- nrow(sanders.scores.neu)
sanders.pol.neg <- nrow(sanders.scores.neg)

#----------

# Visualizations

# Individual politician numerical results
table(clinton.scores$score)
table(sanders.scores$score)

# Individual politician histograms
ggplot(clinton.scores, aes(x = score)) + geom_histogram(binwidth = 1, fill = "white", colour = "black") + xlab(clinton.scores$Pol)
ggplot(sanders.scores, aes(x = score)) + geom_histogram(binwidth = 1, fill = "white", colour = "black") + xlab(sanders.scores$Pol)

# Summarize the scores by tweet for each politician
all.scores = rbind(clinton.scores, sanders.scores)

# Numerical results for all politicians
table(all.scores$score, all.scores$Pol)

# Plot histograms for all politicians
ggplot(data=all.scores) + geom_histogram(mapping=aes(x=score, fill=Pol), binwidth=1) +
  facet_grid(Pol~.) +                         # Make a separate plot for each team.
  theme_bw() + scale_fill_brewer()            # Plain display, nicer colors.

# Plot distribution of polarity by politician (bar charts)

# Summarize polarity breakout for each politician
clinton.polarity = as.data.frame(rbind(clinton.pol.pos, clinton.pol.neu, clinton.pol.neg))
sanders.polarity = as.data.frame(rbind(sanders.pol.pos, sanders.pol.neu, sanders.pol.neg))

# Change column name.
colnames(clinton.polarity) <-"Count"
colnames(sanders.polarity) <-"Count"

# Change row names
rownames(clinton.polarity) <- c('Positive','Neutral','Negative')
rownames(sanders.polarity) <- c('Positive','Neutral','Negative')

# Plot polarity by team politician
ggplot(clinton.polarity, aes(x = rownames(clinton.polarity), y = Count, fill = rownames(clinton.polarity))) + 
  geom_bar(stat="identity", colour = "black") + scale_fill_brewer(palette="Pastel1") +
  labs(x="Polarity Categories", y="Number of Tweets") +
  ggtitle("Polarity Analysis of Tweets about\nHillary Clinton") +
  theme(plot.title = element_text(, size = 16, lineheight=.8, face="bold"), legend.title=element_blank())

ggplot(sanders.polarity, aes(x = rownames(sanders.polarity), y = Count, fill = rownames(sanders.polarity))) + 
  geom_bar(stat="identity", colour = "black") + scale_fill_brewer(palette="Pastel1") +
  labs(x="Polarity Categories", y="Number of Tweets") +
  ggtitle("Polarity Analysis of Tweets about\nBernie Sanders") +
  theme(plot.title = element_text(, size = 16, lineheight=.8, face="bold"), legend.title=element_blank())

#----------

# Statistics #

# Democrats #

# Classical tests (Individual) #

# Smmaries
summary(clinton.scores$score)
summary(sanders.scores$score)

# Shapiro-Wilk normality test
shapiro.test(clinton.scores$score)
shapiro.test(sanders.scores$score)

# Skewness
skewness(clinton.scores$score)
skewness(sanders.scores$score)

# Kurtosis
kurtosis(clinton.scores$score)
kurtosis(sanders.scores$score)

# Classical tests (Multiple) #

# F test to compare two variances
var.test(clinton.scores$score,sanders.scores$score)

# Bartlett test of homogeneity of variances
bartlett.test(score ~ Pol, data = democrats.mean)

# Student t-test
t.test(clinton.scores$score,sanders.scores$score)

# Population sentiment means testing using ANOVA #

# Create data frames for means testing
clinton.mean.prep <- subset(clinton.scores, select = c(score, Pol))
sanders.mean.prep <- subset(sanders.scores, select = c(score, Pol))

# Combine Democrats into one data frame
democrats.mean = rbind(clinton.mean.prep, sanders.mean.prep)

# Box plot (with means as diamonds) comparing Democrats
ggplot(democrats.mean, aes(x = Pol, y = score)) + geom_boxplot() + stat_summary(fun.y = "mean", geom = "point",
                                                                                shape = 23, size = 3, fill = "lightblue")

democrats.anova <- aov(score ~ Pol, data = democrats.mean)   # ANOVA Model
summary(democrats.anova)                                     # ANOV table

# Check the model table (means) to compare means across politicians.
model.tables(democrats.anova, type = 'means')
# Check the model table (effects) to compare the difference between politicians' means and the overall mean.
model.tables(democrats.anova, type = 'effects')

# Check the significance of pairwise mean differences between Democrats.
democratic.mean.comparisons <- TukeyHSD(democrats.anova)
democratic.mean.comparisons
plot(democratic.mean.comparisons, las = 1)















