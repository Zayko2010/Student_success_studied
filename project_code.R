library(RColorBrewer)
library(corrplot)

# ----- Reading and Preparing the data.
df1 <- read.csv('student-por.csv', header = TRUE)
df2 <- read.csv('student-mat.csv', header = TRUE)

df <- rbind.data.frame(df1, df2)
dm = data.matrix(df)

summary(df)

windowsFonts(times = windowsFont("Times New Roman")) 
par(family = "times", font = 2, font.lab = 2, font.axis = 2)

# ----- Pie chart of students by age.
counts = table(as.vector(df['sex']))
piepercent <- paste(round(100*counts/sum(counts), 1), '%', sep = ' ')

col <- rev(brewer.pal(2, "BuPu"))
pie(counts, labels = piepercent, main = 'Students By Gender', col = col)
legend("topright", c("Female","Male"), cex = 0.8, fill = col)


# ----- Bar graph of students by age
counts = table(as.vector(df['age']))

col <- rev(brewer.pal(8, "BuPu"))
barplot(counts, main = 'Students By Age', col = col, ylim = c(0, 300))


# ----- Correlation plot of the correlation matrix
corel = cor(dm)
corrplot(corel, method = "square", lower.col = "black")



# ----- Bar graph of students by travel time
adrs = as.vector(df['address'])
tt = as.vector(df['traveltime'])
sub = c(tt, adrs)
counts = table(sub)
counts

col <- rev(brewer.pal(4, "BuPu"))
barplot(counts, beside = TRUE, main = 'Travel time to/from school', col = col, ylim = c(0, 600), xlab = 'Grouped by resident area. (R: Rural, U: Urban)')
legend("topright", legend=c('<  15 min', '15-30 min', '30-60 min', '> 60 min'), cex = 0.8, fill = col)


# ----- Density plot of absences by area of residence
rural_data <- df[ which(df$address=='R'), ]
den = density(rural_data$absences)
plot(den, main = 'Distribution of student absences in rural areas')
polygon(den, col="red", border="black")
 

urban_data <- df[ which(df$address=='U'), ]
den = density(urban_data$absences)
plot(den, main = 'Distribution of student absences in urban areas')
polygon(den, col="red", border="black")


# ----- Pie chart of students by study time.
counts = table(as.vector(df['studytime']))
piepercent <- paste(round(100*counts/sum(counts), 1), '%', sep = ' ')

col <- rev(brewer.pal(4, "BuPu"))
pie(counts, labels = piepercent, main = 'Students by study time', col = col)
legend("topleft", c('<2 hours', '2 to 5 hours', '5 to 10 hours', '>10 hours'), cex = 0.8, fill = col)


# ----- Boxplot of study time vs grades
col <- rev(brewer.pal(4, "BuPu"))
boxplot(G1~studytime, data = df, ylab='First period grade', xlab='Study Time', col = col, names=c('<2 hours', '2 to 5 hours', '5 to 10 hours', '>10 hours'))


col <- rev(brewer.pal(4, "BuPu"))
boxplot(G2~studytime, data = df, ylab='Second period grade', xlab='Study Time', col = col, names=c('<2 hours', '2 to 5 hours', '5 to 10 hours', '>10 hours'))


col <- rev(brewer.pal(4, "BuPu"))
boxplot(G3~studytime, data = df, ylab='Final Grade G3', xlab='Study Time', col = col, names=c('<2 hours', '2 to 5 hours', '5 to 10 hours', '>10 hours'))


# ----- Pie chart of students by health status
counts = table(as.vector(df['health']))
piepercent <- paste(round(100*counts/sum(counts), 1), '%', sep = ' ')

col <- brewer.pal(5, "BuPu")
pie(counts, labels = piepercent, main = 'Students By Health status', col = col)
legend("topright", c("Very Bad","Bad","Neutral", "Good", "Very Good"), cex = 0.8, fill = col)


# ----- Boxplot of health status vs grades
col <- rev(brewer.pal(5, "BuPu"))
boxplot(G1~health, data = df, ylab='First period grade', xlab='Health status', col = col, names=c("Very Bad","Bad","Neutral", "Good", "Very Good"))


col <- rev(brewer.pal(5, "BuPu"))
boxplot(G2~health, data = df, ylab='Second period grade', xlab='Health status', col = col, names=c("Very Bad","Bad","Neutral", "Good", "Very Good"))


col <- rev(brewer.pal(5, "BuPu"))
boxplot(G3~health, data = df, ylab='Final Grade G3', xlab='Health status', col = col, names=c("Very Bad","Bad","Neutral", "Good", "Very Good"))


# ----- Pie chart of students by weekday alcohol consumption
counts = table(as.vector(df['Dalc']))
piepercent <- paste(round(100*counts/sum(counts), 1), '%', sep = ' ')

col <- brewer.pal(5, "BuPu")
pie(counts, labels = piepercent, main = 'Students by weekday alcohol consumption', col = col)
legend("topright", c("Very Low","Low","Medium", "High", "Very High"), cex = 0.8, fill = col)



# ----- Boxplot of weekday alcohol consumption vs grades
col <- rev(brewer.pal(5, "BuPu"))
boxplot(G1~Dalc, data = df, ylab='First period grade', xlab='Weekday alcohol consumption', col = col, names=c("Very Low","Low","Medium", "High", "Very High"))


col <- rev(brewer.pal(5, "BuPu"))
boxplot(G2~Dalc, data = df, ylab='Second period grade', xlab='Weekday alcohol consumption', col = col, names=c("Very Low","Low","Medium", "High", "Very High"))


col <- rev(brewer.pal(5, "BuPu"))
boxplot(G3~Dalc, data = df, ylab='Final Grade G3', xlab='Weekday alcohol consumption', col = col, names=c("Very Low","Low","Medium", "High", "Very High"))



# ----- Pie chart of students by weekend alcohol consumption
counts = table(as.vector(df['Walc']))
piepercent <- paste(round(100*counts/sum(counts), 1), '%', sep = ' ')

col <- rev(brewer.pal(5, "BuPu"))
pie(counts, labels = piepercent, main = 'Students by weekend alcohol consumption', col = col)
legend("topright", c("Very Low","Low","Medium", "High", "Very High"), cex = 0.8, fill = col)



# ----- Boxplot of weekend alcohol consumption vs grades
col <- rev(brewer.pal(5, "BuPu"))
boxplot(G1~Walc, data = df, ylab='First period grade', xlab='Weekend alcohol consumption', col = col, names=c("Very Low","Low","Medium", "High", "Very High"))


col <- rev(brewer.pal(5, "BuPu"))
boxplot(G2~Walc, data = df, ylab='Second period grade', xlab='Weekend alcohol consumption', col = col, names=c("Very Low","Low","Medium", "High", "Very High"))


col <- rev(brewer.pal(5, "BuPu"))
boxplot(G3~Walc, data = df, ylab='Final Grade G3', xlab='Weekend alcohol consumption', col = col, names=c("Very Low","Low","Medium", "High", "Very High"))


# ----- Pie chart of students by desire for higher education
counts = table(as.vector(df['higher']))
piepercent <- paste(round(100*counts/sum(counts), 1), '%', sep = ' ')

col <- rev(brewer.pal(2, "BuPu"))
pie(counts, labels = piepercent, main = 'Students by desire for higher education', col = col)
legend("topright", c("No","Yes"), cex = 0.8, fill = col)



# ----- Boxplot of higher education desire vs grades
col <- rev(brewer.pal(2, "BuPu"))
boxplot(G1~higher, data = df, ylab='First period grade', xlab='Desire for a higher education', col = col, names=c("No","Yes"))


col <- rev(brewer.pal(2, "BuPu"))
boxplot(G2~higher, data = df, ylab='Second period grade', xlab='Desire for a higher education', col = col, names=c("No","Yes"))


col <- rev(brewer.pal(2, "BuPu"))
boxplot(G3~higher, data = df, ylab='Final Grade G3', xlab='Desire for a higher education', col = col, names=c("No","Yes"))

