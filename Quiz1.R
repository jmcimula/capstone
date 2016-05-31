#In the en_US twitter data set, if you divide the number of lines where the word "love" (all lowercase) occurs by the number of lines the word "hate" (all #lowercase) occurs, about what do you get?

filename <- "en_US.twitter.txt"
conn <- file(filename, open="r",blocking = TRUE)
linn <- readLines(conn,skipNul = TRUE)

love <- grep ("love",linn)
hate <- grep ("hate",linn)

love/hate

#The one tweet in the en_US twitter data set that matches the word "biostats" says what?
biostats <- grep ("biostats",linn)
linn[biostats]

#How many tweets have the exact characters "A computer once beat me at chess, but it was no match for me at kickboxing". (I.e. the line matches those #characters exactly.)
string <- "A computer once beat me at chess, but it was no match for me at kickboxing"
tweet <- grep (string, linn)
length(tweet)