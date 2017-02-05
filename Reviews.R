# Load Data
setwd("D:/Data Science/Data Incubator/DataSet")

reviews <- read.csv("Reviews.csv")

reviewsUSS <- subset(reviews, Attraction == "Universal Studios")
reviewsUSS$sentiment <- 0
reviewsUSS[reviewsUSS$Reviewer.Rating == 5 | reviewsUSS$Reviewer.Rating == 4, "sentiment"] <- 1 
reviewsUSS_a <- aggregate(reviewsUSS$Id, list(reviewsUSS$Review.Month), length)
names(reviewsUSS_a)[names(reviewsUSS_a) == "x"] <- "Total"
temp <- aggregate(reviewsUSS$sentiment, list(reviewsUSS$Review.Month), sum)
names(temp)[names(temp) == "x"] <- "Positive"
reviewsUSS_a <- merge(reviewsUSS_a, temp, by.x = "Group.1", by.y = "Group.1")
reviewsUSS_a$Negative <- 0
reviewsUSS_a$Negative <- reviewsUSS_a$Total - reviewsUSS_a$Positive 
reviewsUSS_a$Positive_precent <- 0
reviewsUSS_a$Positive_precent <- reviewsUSS_a$Positive / reviewsUSS_a$Total * 100
names(reviewsUSS_a)[names(reviewsUSS_a) == "Group.1"] <- "Period"

library(ggplot2)

ggplot(reviewsUSS_a) + 
        geom_line(aes(x=Period, y=Total, color="Total")) + 
        geom_line(aes(x=Period, y=Positive, col="Positive")) +
        geom_line(aes(x=Period, y=Negative, col="Negative")) + 
        scale_color_discrete(name="Legend") + labs(list(x = "Period", y = "Number of Reviews", title="Universal Studio, Singapore - Reviews"))

### Gardens by the Bay

reviewsGBB <- subset(reviews, Attraction == "Gardens by the Bay")
reviewsGBB$sentiment <- 0
reviewsGBB[reviewsGBB$Reviewer.Rating == 5 | reviewsGBB$Reviewer.Rating == 4, "sentiment"] <- 1 
reviewsGBB_a <- aggregate(reviewsGBB$Id, list(reviewsGBB$Review.Month), length)
names(reviewsGBB_a)[names(reviewsGBB_a) == "x"] <- "Total"
temp <- aggregate(reviewsGBB$sentiment, list(reviewsGBB$Review.Month), sum)
names(temp)[names(temp) == "x"] <- "Positive"
reviewsGBB_a <- merge(reviewsGBB_a, temp, by.x = "Group.1", by.y = "Group.1")
reviewsGBB_a$Negative <- 0
reviewsGBB_a$Negative <- reviewsGBB_a$Total - reviewsGBB_a$Positive 
reviewsGBB_a$Positive_precent <- 0
reviewsGBB_a$Positive_precent <- reviewsGBB_a$Positive / reviewsGBB_a$Total * 100
names(reviewsGBB_a)[names(reviewsGBB_a) == "Group.1"] <- "Period"

library(ggplot2)

ggplot(reviewsGBB_a) + 
        geom_line(aes(x=Period, y=Total, color="Total")) + 
        geom_line(aes(x=Period, y=Positive, col="Positive")) +
        geom_line(aes(x=Period, y=Negative, col="Negative")) + 
        scale_color_discrete(name="Legend") + labs(list(x = "Period", y = "Number of Reviews", title="Gardens by the Bay, Singapore - Reviews"))

# Comparision
reviews_BOTH <- merge(reviewsUSS_a, reviewsGBB_a, by.x = "Period", by.y = "Period")
ggplot(reviews_BOTH) + 
        geom_line(aes(x=Period, y=Positive_precent.x, color="Universal Studios, Singapore")) + 
        geom_line(aes(x=Period, y=Positive_precent.y, col="Gardens by the Bay")) +
        scale_color_discrete(name="Legend") + labs(list(x = "Period", y = "% Positive Reviews", title="Universal Studios, Singapore Vs. Gardens by the Bay"))

# Analyze reasons for Poor Reviews
reviews_poor <- reviews[reviews$Reviewer.Rating < 4, c("Attraction", "Reviewer.Rating", "Reviewer.Level", "Reason.for.Poor.Rating", "Reviewer.Type")]
reviews_poorUSS <- reviews_poor[reviews_poor =="Universal Studios",]
reviews_poorUSS_a <- aggregate(Attraction ~ Reason.for.Poor.Rating + Reviewer.Type, data = reviews_poorUSS, length)

ggplot(data=reviews_poorUSS, aes(x = Reason.for.Poor.Rating,
                         fill = Reviewer.Rating, color = "Reviewer.Rating")) +
        geom_bar() +
        scale_x_discrete("Reason for Poor Rating") +
        scale_y_continuous("Count") +
        facet_grid(. ~ Reviewer.Type)

ggplot(data=reviews_poorUSS, aes(x = factor(Reason.for.Poor.Rating),
                         fill = Reviewer.Rating)) +
        geom_bar() +
        scale_x_discrete("Reason for Poor Rating") +
        scale_y_continuous("Count") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        facet_grid(. ~ Reviewer.Type)

ggplot(data=reviews_poorUSS, aes(x = factor(Reviewer.Type),
                                 fill = Reviewer.Rating)) +
        geom_bar() +
        scale_x_discrete("Reviewer Type") +
        scale_y_continuous("Count") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        facet_grid(. ~ Reason.for.Poor.Rating)
