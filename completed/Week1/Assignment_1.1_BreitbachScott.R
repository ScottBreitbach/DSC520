# Week: 1
# Assignment: ASSIGNMENT 1.1
# Name: Breitbach, Scott
# Date: 2020-06-05


# 1. What are the observational units in this study?
"The observational units are the two sections: Regular and Sports"


# 2. Identify the variables mentioned in the narrative paragraph 
#    and determine which are categorical and quantitative?
"The scores (and count) variable is quantitative, the grades variable and
the section are categorical"


# 3. Create one variable to hold a subset of your data set that contains 
#    only the Regular Section and one variable for the Sports Section.
scores_df <- read.csv('data/scores.csv', stringsAsFactors = TRUE)
sportssection_df <- scores_df[scores_df$Section == "Sports",]
regularsection_df <- scores_df[scores_df$Section == "Regular",]
scores <- read_csv('data/scores.csv')
sportssection <- scores[scores$Section == "Sports",]
regularsection <- scores[scores$Section == "Regular",]
# NOTE: I wasn't sure which method was preferred so I did both.


# 4. Use the Plot function to plot each Sections scores and the number of 
#    students achieving that score. Use additional Plot Arguments to label 
#    the graph and give each axis an appropriate label. Once you have 
#    produced your Plots answer the following questions:
plot(sportssection_df$Score, sportssection_df$Count, 
     main = "Sports Section Scores", xlab = "Score", ylab = "Count", 
     xlim = c(200, 400))
plot(regularsection_df$Score, regularsection_df$Count, 
     main = "Regular Section Scores", xlab = "Score", ylab = "Count", 
     xlim = c(200, 400))


# 4. a. Comparing and contrasting the point distributions between the two 
#       sections, looking at both tendency and consistency: Can you say 
#       that one section tended to score more points than the other?  
#       Justify and explain your answer.
"The Sports class scores have a larger range and are more dispersed / less
consistent, with a range of 195. The plot of the scores also appears to be 
bimodal.
The Regular class scores have a cleaner frequency distribution, which appears 
to be negatively skewed and with a range of 115.
For these reasons, I would say the Regular class tended to score more points
than the Sports class."


#    b. Did every student in one section score more points than every 
#       student in the other section? If not, explain what a statistical 
#       tendency means in this context.
ggplot(scores, aes(Score, Count, color = Section)) + geom_point() +
ggtitle("Class Scores")

"There was a significant overlap in scores between the two sections so you
cannot say that every student in one section scored more points than every
student in the other section. In statistics, tendency (or central tendency)
is a general measure of the center of the data, whether through mean, median,
mode, or range. Having discussed mode and range in the previous question, I
will focus primarily on mean and median here.
As seen below, the Regular section has a higher mean (335) than both the 
combined sections (321.7) and the Sports section (306.9) as well as a higher
median (335, 330, and 312.5, respectively.
This, along with the mode and range, tells us that the scores in the Regular
section tended to be more consistently higher than the scores in the Sports
section, which means that taking the Regular section, one would have a higher
probability of scoring higher than if taking the Sports section."

"** Combined Sections:"
"Mean: 321.7273"
(sum(scores$Score * scores$Count))/(sum(scores$Count))
"Median: 330"
with(scores, median(rep.int(Score, Count)))

"** Sports:"
"Mean: 306.9231"
(sum(sportssection$Score * sportssection$Count))/(sum(sportssection$Count))
"Median: 312.5"
with(sportssection, median(rep.int(Score, Count)))
"Mode: 285, 335"

"** Regular:"
"Mean: 335"
(sum(regularsection$Score * regularsection$Count))/(sum(regularsection$Count))
"Median: 335"
with(regularsection, median(rep.int(Score, Count)))
"Mode: 350"


#    c. What could be one additional variable that was not mentioned in 
#       the narrative that could be influencing the point distributions 
#       between the two sections?
"A potential variable not mentioned in the narrative could be the time of day.
If one of the sections is in the early morning or perhaps right after lunch,
it could have an effect on attendance or attention span of the students, which
has the potential to have an effect on test scores."




