library(ggplot2)
library(patchwork)


######
###
# Histogram for the Nov.1973-Mar.1975 Recession
pool <- -0.334008
black <- -0.966556
white <- -0.1722803

groups <- c(pool, black, white)
groups <- data.frame(groups, "race" = c("All", "Black", "White"))

plots <- function(title = "Need Title"){
ggplot(groups) +
  geom_col(mapping = aes(x = race, y = groups, fill = race), show.legend = FALSE) +
    ylim(-3, .3) +
  ylab("") +
  xlab("Demographic") +
  ggtitle(title) +
  scale_fill_manual(values = c(All = "#CC6666", Black = "#9999CC", White = "9900CC")) +
  theme_classic()
}

r1 <- plots("Nov. 1973 - Mar. 1975 Recession")


###
# Histogram for the 1980-1982 Recession
pool <- -0.114883864
black <- -0.5561215
white <- -0.062987133

groups <- c(pool, black, white)
groups <- data.frame(groups, "race" = c("All", "Black", "White"))

r2 <- plots(title = "Jan. 1980 - Nov. 1982 Recession")




###
# A Histogram for the July 1990 to March 1991 Recession
pool <- -0.521343862
black <- -0.189413386
white <- -0.328859189

groups <- c(pool, black, white)
groups <- data.frame(groups, "race" = c("All", "Black", "White"))

r3 <- plots(title = "Jul. 1990 - Mar. 1991 Recession")





###
# A Histogram for the Mar. 2001 - Nov. 2001 Recession
pool <- -0.291108569
black <- -0.90753402
white <- -0.275512803
hispanic <- -0.910550862

groups <- c(pool, black, white, hispanic)
groups <- data.frame(groups, "race" = c("All", "Black", "White", "Hispanic"))

plots2 <- function(title = "Need Title"){
  ggplot(groups) +
    geom_col(mapping = aes(x = race, y = groups, fill = race), show.legend = FALSE) +
    ylim(-3, .3) +
    ylab("") +
    xlab("Demographic") +
    ggtitle(title) +
    scale_fill_manual(values = c(All = "#CC6666", Black = "#9999CC", Hispanic = "#66CC99", White = "9900CC")) +
    theme_classic()
}

r4 <- plots2("Mar. 2001 - Nov. 2001 Recession")



###
# A Histogram for the Dec. 2007 - Jun. 2009 Recession
pool <- -0.480316844
black <- -0.461765842
white <- -0.475579663
hispanic <- -0.424652423

groups <- c(pool, black, white, hispanic)
groups <- data.frame(groups, "race" = c("All", "Black", "White", "Hispanic"))

r5 <- plots2("Dec. 2007 - Jun. 2009 Recession")





###
# A Histogram for the Current Recession
pool <- -1.268298659
black <- -2.831470812
white <- -1.156569587
hispanic <- -1.804078664
asian <- -1.042452782

groups <- c(pool, black, white, hispanic, asian)
groups <- data.frame(groups, "race" = c("All", "Black", "White", "Hispanic", "Asian"))

plots3 <- function(title = "Need Title"){
  ggplot(groups) +
    geom_col(mapping = aes(x = race, y = groups, fill = race), show.legend = FALSE) +
    ylim(-3, .3) +
    ylab("") +
    xlab("Demographic") +
    ggtitle(title) +
    scale_fill_manual(values = c(All = "#CC6666", Asian = "666600", Black = "#9999CC", Hispanic = "#66CC99", White = "9900CC")) +
    theme_classic()
}

r6 <- plots3("Feb. 2020 - Present Recession")





























######
###
# Histograms for the Nov. 1970 - Nov. 1973 Expansion
pool <- -0.313718934
black <- 0.297963843
white <- -0.258659168

groups <- c(pool, black, white)
groups <- data.frame(groups, "race" = c("All", "Black", "White"))

e1 <- plots("Nov. 1970 - Nov. 1973 Expansion")


###
# Histograms for the Mar. 1975 - Jan. 1980 Expansion
pool <- -0.042604123
black <- 0.018467764
white <- -0.053113506

groups <- c(pool, black, white)
groups <- data.frame(groups, "race" = c("All", "Black", "White"))

e2 <- plots("Mar. 1975 - Jan. 1980 Expansion")




###
# Histogram for Nov 1982 - Jul 1990 Expansion
pool <- -0.062777654
black <- -0.005515637
white <- -0.054245149

groups <- c(pool, black, white)
groups <- data.frame(groups, "race" = c("All", "Black", "White"))

e3 <- plots("Nov. 1982 - Jul. 1990 Expansion")





###
# Histogram for Mar. 1991 - Mar. 2001 Expansion
pool <- -0.19039043
black <- -0.381197706
white <- -0.180419628
hispanic <- 0.289133031

groups <- c(pool, black, white, hispanic)
groups <- data.frame(groups, "race" = c("All", "Black", "White", "Hispanic"))

e4 <- plots2("Mar. 1991 - Mar. 2001 Expansion")





###
# Histogram for Nov. 2001 - Dec. 2007 Expansion
pool <- -0.095650128
black <- 0.020410545
white <- -0.087232776
hispanic <- 0.148815605

groups <- c(pool, black, white, hispanic)
groups <- data.frame(groups, "race" = c("All", "Black", "White", "Hispanic"))

e5 <- plots2("Nov. 2001 - Dec. 2007 Expansion")





###
# Histogram for Jun. 2009 - Feb. 2020 Expansion
pool <- -0.11132957
black <- -0.053823164
white <- -0.104032944
hispanic <- -0.183760838
asian <- 0.112700473

groups <- c(pool, black, white, hispanic, asian)
groups <- data.frame(groups, "race" = c("All", "Black", "White", "Hispanic", "Asian"))

plots3 <- function(title = "Need Title"){
  ggplot(groups) +
    geom_col(mapping = aes(x = race, y = groups, fill = race), show.legend = FALSE) +
    ylim(-3, .3) +
    ylab("") +
    xlab("Demographic") +
    ggtitle(title) +
    scale_fill_manual(values = c(All = "#CC6666", Asian = "666600", Black = "#9999CC", Hispanic = "#66CC99", White = "9900CC")) +
    theme_classic()
}

e6 <- plots3("Jun. 2009 - Feb. 2020 Expansion")

















######
# Perhaps I should change the scales...?

###
# Expansion histograms for all races
poole1 <- -0.313718934
poole2 <- -0.042604123
poole3 <- -0.062777654
poole4 <- -0.19039043
poole5 <- -0.095650128
poole6 <- -0.11132957

groups <- c(poole1, poole2, poole3, poole4, poole5, poole6)
groups <- data.frame(groups, "Expansion" = c("Nov.1970 - Nov.1973", "Mar.1975 - Jan. 1980",
                                             "Nov. 1982 - Jul. 1990", "Mar. 1991 - Mar. 2001",
                                             "Nov. 2001 - Dec. 2007", "Jun. 2009 - Feb. 2020"))

groups$Expansion <- factor(groups$Expansion, levels = c("Nov.1970 - Nov.1973", "Mar.1975 - Jan. 1980",
                                                        "Nov. 1982 - Jul. 1990", "Mar. 1991 - Mar. 2001",
                                                        "Nov. 2001 - Dec. 2007", "Jun. 2009 - Feb. 2020"))


plots4 <- function(title = "Need Title", episode = "Demographic"){
  ggplot(groups) +
    geom_col(mapping = aes(x = Expansion, y = groups, fill = Expansion), show.legend = FALSE) +
    ylim(-3, .3) +
    ylab("") +
    xlab(episode) +
    ggtitle(title) +
    scale_fill_manual(values = c("#CC6666", "666600", "#9999CC", "#66CC99", "9900CC", "violetred")) +
    theme_classic()
}




ae <- plots4(title = "All Men", episode = "Expansion")






###
# Expansion histograms for Whites
whitee1 <- -0.258659168
whitee2 <- -0.053113506
whitee3 <- -0.054245149
whitee4 <- -0.180419628
whitee5 <- -0.087232776
whitee6 <- -0.104032944

groups <- c(whitee6, whitee5, whitee4, whitee3, whitee2, whitee1)
groups <- data.frame(groups, "Expansion" = c("Nov.1970 - Nov.1973", "Mar.1975 - Jan. 1980",
                                             "Nov. 1982 - Jul. 1990", "Mar. 1991 - Mar. 2001",
                                             "Nov. 2001 - Dec. 2007", "Jun. 2009 - Feb. 2020"))

groups$Expansion <- factor(groups$Expansion, levels = c("Nov.1970 - Nov.1973", "Mar.1975 - Jan. 1980",
                                                        "Nov. 1982 - Jul. 1990", "Mar. 1991 - Mar. 2001",
                                                        "Nov. 2001 - Dec. 2007", "Jun. 2009 - Feb. 2020"))

we <- plots4(title = "White Men", episode = "Expansion")










###
# Expansion histograms for Blacks
blacke1 <- 0.297963843
blacke2 <- 0.018467764
blacke3 <- -0.005515637
blacke4 <- -0.381197706
blacke5 <- 0.020410545
blacke6 <- -0.053823164

groups <- c(blacke1, blacke2, blacke3, blacke4, blacke5, blacke6)
groups <- data.frame(groups, "Expansion" = c("Nov.1970 - Nov.1973", "Mar.1975 - Jan. 1980",
                                             "Nov. 1982 - Jul. 1990", "Mar. 1991 - Mar. 2001",
                                             "Nov. 2001 - Dec. 2007", "Jun. 2009 - Feb. 2020"))

groups$Expansion <- factor(groups$Expansion, levels = c("Nov.1970 - Nov.1973", "Mar.1975 - Jan. 1980",
                                                        "Nov. 1982 - Jul. 1990", "Mar. 1991 - Mar. 2001",
                                                        "Nov. 2001 - Dec. 2007", "Jun. 2009 - Feb. 2020"))

be <- plots4(title = "Black Men", episode = "Expansion")








###
# Expansion histograms for Hispanics
na1 <- 0
na2 <- 0
na3 <- 0
hispanice1 <- 0.289133031
hispanice2 <- 0.148815605
hispanice3 <- -0.183760838


groups <- c(na1, na2, na3, hispanice1, hispanice2, hispanice3)
groups <- data.frame(groups, "Expansion" = c("Nov. 1973 - Mar. 1975",
                                             "Jan. 1980 - Nov. 1982", "Jul. 1990 - Mar. 1991",
                                             "Mar. 2001 - Nov. 2001", "Dec. 2007 - Jun. 2009", "Feb. 2020 - Present"))

groups$Expansion <- factor(groups$Expansion, levels = c("Nov. 1973 - Mar. 1975",
                                                        "Jan. 1980 - Nov. 1982", "Jul. 1990 - Mar. 1991",
                                                        "Mar. 2001 - Nov. 2001", "Dec. 2007 - Jun. 2009", "Feb. 2020 - Present"))


plots5 <- function(title = "Need Title"){
  ggplot(groups) +
    geom_col(mapping = aes(x = Expansion, y = groups, fill = Expansion), show.legend = FALSE) +
    ylim(-3, .3) +
    ylab("") +
    xlab("Demographic") +
    ggtitle(title) +
    scale_fill_manual(values = c("#66CC99", "9900CC", "violetred")) +
    theme_classic()
}



he <- plots4(title = "Hispanic Men", episode = "Expansion")

















######
# This section makes histograms for each race by recession

###
# Histograms for all races
poolr1 <- -0.334008
poolr2 <- -0.114883864
poolr3 <- -0.521343862
poolr4 <- -0.291108569
poolr5 <- -0.480316844
poolr6 <- -1.268298659

groups <- c(poolr1, poolr2, poolr3, poolr4, poolr5, poolr6)
groups <- data.frame(groups, "Expansion" = c("Nov. 1973 - Mar. 1975",
                                             "Jan. 1980 - Nov. 1982", "Jul. 1990 - Mar. 1991",
                                             "Mar. 2001 - Nov. 2001", "Dec. 2007 - Jun. 2009", "Feb. 2020 - Present"))

groups$Expansion <- factor(groups$Expansion, levels = c("Nov. 1973 - Mar. 1975",
                                                        "Jan. 1980 - Nov. 1982", "Jul. 1990 - Mar. 1991",
                                                        "Mar. 2001 - Nov. 2001", "Dec. 2007 - Jun. 2009", "Feb. 2020 - Present"))

ar <- plots4(title = "All Men", episode = "Recession")







###
# Histograms for White men
whiter1 <- -0.1722803
whiter2 <- -0.062987133
whiter3 <- -0.328859189
whiter4 <- -0.275512803
whiter5 <- -0.475579663
whiter6 <- -1.156569587

groups <- c(whiter1, whiter2, whiter3, whiter4, whiter5, whiter6)
groups <- data.frame(groups, "Expansion" = c("Nov. 1973 - Mar. 1975",
                                             "Jan. 1980 - Nov. 1982", "Jul. 1990 - Mar. 1991",
                                             "Mar. 2001 - Nov. 2001", "Dec. 2007 - Jun. 2009", "Feb. 2020 - Present"))

groups$Expansion <- factor(groups$Expansion, levels = c("Nov. 1973 - Mar. 1975",
                                                        "Jan. 1980 - Nov. 1982", "Jul. 1990 - Mar. 1991",
                                                        "Mar. 2001 - Nov. 2001", "Dec. 2007 - Jun. 2009", "Feb. 2020 - Present"))

wr <- plots4(title = "White Men", episode = "Recession")







###
# Histograms for Black men
blackr1 <- -0.966556
blackr2 <- -0.5561215
blackr3 <- -0.189413386
blackr4 <- -0.90753402
blackr5 <- -0.461765842
blackr6 <- -2.831470812

groups <- c(blackr1, blackr2, blackr3, blackr4, blackr5, blackr6)
groups <- data.frame(groups, "Expansion" = c("Nov. 1973 - Mar. 1975",
                                             "Jan. 1980 - Nov. 1982", "Jul. 1990 - Mar. 1991",
                                             "Mar. 2001 - Nov. 2001", "Dec. 2007 - Jun. 2009", "Feb. 2020 - Present"))

groups$Expansion <- factor(groups$Expansion, levels = c("Nov. 1973 - Mar. 1975",
                                                        "Jan. 1980 - Nov. 1982", "Jul. 1990 - Mar. 1991",
                                                        "Mar. 2001 - Nov. 2001", "Dec. 2007 - Jun. 2009", "Feb. 2020 - Present"))

br <- plots4(title = "Black Men", episode = "Recession")









###
# Histograms for Hispanic Men
na1 <- 0
na2 <- 0
na3 <- 0
hispanicr1 <- -0.910550862
hispanicr2 <- -0.424652423
hispanicr3 <- -1.804078664

groups <- c(na1, na2, na3, hispanicr1, hispanicr2, hispanicr3)
groups <- data.frame(groups, "Expansion" = c("Nov. 1973 - Mar. 1975",
                                             "Jan. 1980 - Nov. 1982", "Jul. 1990 - Mar. 1991",
                                             "Mar. 2001 - Nov. 2001", "Dec. 2007 - Jun. 2009", "Feb. 2020 - Present"))

groups$Expansion <- factor(groups$Expansion, levels = c("Nov. 1973 - Mar. 1975",
                                                        "Jan. 1980 - Nov. 1982", "Jul. 1990 - Mar. 1991",
                                                        "Mar. 2001 - Nov. 2001", "Dec. 2007 - Jun. 2009", "Feb. 2020 - Present"))

hr <- plots4(title = "Hispanic Men", episode = "Recession")






















######
# This section provides more recession data for White men and all races

### Histograms for all men
poolr1 <- 0.041761142
poolr2 <- 0.057181259
poolr3 <- -0.353710246
poolr4 <- -0.056145619
poolr5 <- -0.225162763
poolr6 <- -0.334008
poolr7 <- -0.114883864
poolr8 <- -0.521343862
poolr9 <- -0.291108569
poolr10 <- -0.480316844
poolr11 <- -1.268298659

groups <- c(poolr1, poolr2, poolr3, poolr4, poolr5, poolr6, poolr7, poolr8, poolr9, poolr10, poolr11)
groups <- data.frame(groups, "Expansion" = c("Nov. 1948 - Oct. 1949", "Jul. 1953 - May 1954", "Aug. 1957 - Apr. 1958", "Apr. 1960 - Feb. 1961",
                                             "Dec. 1969 - Nov. 1970", "Nov. 1973 - Mar. 1975",
                                             "Jan. 1980 - Nov. 1982", "Jul. 1990 - Mar. 1991",
                                             "Mar. 2001 - Nov. 2001", "Dec. 2007 - Jun. 2009", "Feb. 2020 - Present"))

groups$Expansion <- factor(groups$Expansion, levels = c("Nov. 1948 - Oct. 1949", "Jul. 1953 - May 1954", "Aug. 1957 - Apr. 1958", "Apr. 1960 - Feb. 1961",
                                                        "Dec. 1969 - Nov. 1970", "Nov. 1973 - Mar. 1975",
                                                        "Jan. 1980 - Nov. 1982", "Jul. 1990 - Mar. 1991",
                                                        "Mar. 2001 - Nov. 2001", "Dec. 2007 - Jun. 2009", "Feb. 2020 - Present"))



plots6 <- function(title = "Need Title", episode = "Add thing here"){
  ggplot(groups) +
    geom_col(mapping = aes(x = Expansion, y = groups, fill = Expansion), show.legend = FALSE) +
    ylim(-3, .3) +
    ylab("") +
    xlab(episode) +
    ggtitle(title) +
    scale_fill_manual(values = c("cyan1", "hotpink2", "wheat2", "darkgoldenrod2", "burlywood4", "#CC6666", "666600", "#9999CC", "#66CC99", "9900CC", "violetred")) +
    theme_classic()
}



ar2 <- plots6(title = "All Races", episode = "Recession")














### Histograms for White men
na1 <- 0
na2 <- 0
whiter1 <- -0.171292286
whiter2 <- 0.018830358
whiter3 <- -0.258763894
whiter4 <- -0.1722803
whiter5 <- -0.062987133
whiter6 <- -0.328859189
whiter7 <- -0.275512803
whiter8 <- -0.475579663
whiter9 <- -1.156569587

groups <- c(na1, na2, whiter1, whiter2, whiter3, whiter4, whiter5, whiter6, whiter7, whiter8, whiter9)
groups <- data.frame(groups, "Expansion" = c("Nov. 1948 - Oct. 1949", "Jul. 1953 - May 1954", "Aug. 1957 - Apr. 1958", "Apr. 1960 - Feb. 1961",
                                             "Dec. 1969 - Nov. 1970", "Nov. 1973 - Mar. 1975",
                                             "Jan. 1980 - Nov. 1982", "Jul. 1990 - Mar. 1991",
                                             "Mar. 2001 - Nov. 2001", "Dec. 2007 - Jun. 2009", "Feb. 2020 - Present"))

groups$Expansion <- factor(groups$Expansion, levels = c("Nov. 1948 - Oct. 1949", "Jul. 1953 - May 1954", "Aug. 1957 - Apr. 1958", "Apr. 1960 - Feb. 1961",
                                                        "Dec. 1969 - Nov. 1970", "Nov. 1973 - Mar. 1975",
                                                        "Jan. 1980 - Nov. 1982", "Jul. 1990 - Mar. 1991",
                                                        "Mar. 2001 - Nov. 2001", "Dec. 2007 - Jun. 2009", "Feb. 2020 - Present"))



plots7 <- function(title = "Need Title", episode = "Add Recession/Expansion Here"){
  ggplot(groups) +
    geom_col(mapping = aes(x = Expansion, y = groups, fill = Expansion), show.legend = FALSE) +
    ylim(-3, .3) +
    ylab("") +
    xlab(episode) +
    ggtitle(title) +
    scale_fill_manual(values = c("cyan1", "hotpink2", "wheat2", "darkgoldenrod2", "burlywood4", "#CC6666", "666600", "#9999CC", "#66CC99", "9900CC", "violetred")) +
    theme_classic()
}



wr2 <- plots7(title = "White Men", episode = "Recession")























######
# This section provides more expansion data for White men and all races

### Histograms for all men
poole1 <- 0.218774202
poole2 <- -0.043186796
poole3 <- 0.013437992
poole4 <- -0.112022226
poole5 <- -0.313718934
poole6 <- -0.042604123
poole7 <- -0.062777654
poole8 <- -0.19039043
poole9 <- -0.095650128
poole10 <- -0.11132957

groups <- c(poolr1, poolr2, poolr3, poolr4, poolr5, poolr6, poolr7, poolr8, poolr9, poolr10)
groups <- data.frame(groups, "Expansion" = c("Oct. 1949 - Jul. 1953", "May 1954 - Aug. 1957",
                                             "Apr. 1958 - Apr. 1960", "Feb. 1961 - Dec. 1969",
                                             "Nov. 1970 - Nov. 1973", "Mar. 1975 - Jan. 1980",
                                             "Nov. 1982 - Jul. 1990", "Mar. 1991 - Mar. 2001",
                                             "Nov. 2001 - Dec. 2007", "Jun. 2009 - Feb.2020"))

groups$Expansion <- factor(groups$Expansion, levels = c("Oct. 1949 - Jul. 1953", "May 1954 - Aug. 1957",
                                                        "Apr. 1958 - Apr. 1960", "Feb. 1961 - Dec. 1969",
                                                        "Nov. 1970 - Nov. 1973", "Mar. 1975 - Jan. 1980",
                                                        "Nov. 1982 - Jul. 1990", "Mar. 1991 - Mar. 2001",
                                                        "Nov. 2001 - Dec. 2007", "Jun. 2009 - Feb.2020"))

ae2 <- ggplot(groups) +
    geom_col(mapping = aes(x = Expansion, y = groups, fill = Expansion), show.legend = FALSE) +
    ylim(-3, .3) +
    ylab("") +
    xlab("Expansion") +
    ggtitle("All Men") +
    scale_fill_manual(values = c("hotpink2", "wheat2", "darkgoldenrod2", "burlywood4", "#CC6666", "666600", "#9999CC", "#66CC99", "9900CC", "violetred")) +
    theme_classic()






### Histograms for White men
na1 <- 0
na2 <- 0
whitee1 <- -0.036589268
whitee2 <- -0.082709297
whitee3 <- -0.258659168
whitee4 <- -0.053113506
whitee5 <- -0.054245149
whitee6 <- -0.180419628
whitee7 <- -0.087232776
whitee8 <- -0.104032944

groups <- c(na1, na2, whitee1, whitee2, whitee3, whitee4, whitee5, whitee6, whitee7, whitee8)
groups <- data.frame(groups, "Expansion" = c("Oct. 1949 - Jul. 1953", "May 1954 - Aug. 1957",
                                             "Apr. 1958 - Apr. 1960", "Feb. 1961 - Dec. 1969",
                                             "Nov. 1970 - Nov. 1973", "Mar. 1975 - Jan. 1980",
                                             "Nov. 1982 - Jul. 1990", "Mar. 1991 - Mar. 2001",
                                             "Nov. 2001 - Dec. 2007", "Jun. 2009 - Feb.2020"))

groups$Expansion <- factor(groups$Expansion, levels = c("Oct. 1949 - Jul. 1953", "May 1954 - Aug. 1957",
                                                        "Apr. 1958 - Apr. 1960", "Feb. 1961 - Dec. 1969",
                                                        "Nov. 1970 - Nov. 1973", "Mar. 1975 - Jan. 1980",
                                                        "Nov. 1982 - Jul. 1990", "Mar. 1991 - Mar. 2001",
                                                        "Nov. 2001 - Dec. 2007", "Jun. 2009 - Feb.2020"))

we2 <- ggplot(groups) +
  geom_col(mapping = aes(x = Expansion, y = groups, fill = Expansion), show.legend = FALSE) +
  ylim(-3, .3) +
  ylab("") +
  xlab("Expansion") +
  ggtitle("White Men") +
  scale_fill_manual(values = c("hotpink2", "wheat2", "darkgoldenrod2", "burlywood4", "#CC6666", "666600", "#9999CC", "#66CC99", "9900CC", "violetred")) +
  theme_classic()


######
# Removing unwanted variables
rm(asian)
rm(black)
rm(blacke1)
rm(blacke2)
rm(blacke3)
rm(blacke4)
rm(blacke5)
rm(blacke6)
rm(hispanic)
rm(hispanice1)
rm(hispanice2)
rm(hispanice3)
rm(pool)
rm(poole1)
rm(poole2)
rm(poole3)
rm(poole4)
rm(poole5)
rm(poole6)
rm(poole7)
rm(poole8)
rm(poole9)
rm(poole10)
rm(white)
rm(whitee1)
rm(whitee2)
rm(whitee3)
rm(whitee4)
rm(whitee5)
rm(whitee6)
rm(whitee7)
rm(whitee8)
rm(whitee9)
rm(poolr1)
rm(poolr2)
rm(poolr3)
rm(poolr4)
rm(poolr5)
rm(poolr6)
rm(poolr7)
rm(poolr8)
rm(poolr9)
rm(poolr10)
rm(poolr11)
rm(hispanicr1)
rm(hispanicr2)
rm(hispanicr3)
rm(blackr1)
rm(blackr2)
rm(blackr3)
rm(blackr4)
rm(blackr5)
rm(blackr6)
rm(whiter1)
rm(whiter2)
rm(whiter3)
rm(whiter4)
rm(whiter5)
rm(whiter6)
rm(whiter7)
rm(whiter8)
rm(whiter9)
rm(groups)
rm(na1)
rm(na2)
rm(na3)
rm(plots)
rm(plots2)
rm(plots3)
rm(plots4)
rm(plots5)
rm(plots6)
rm(plots7)
gc()







######
### The histograms in chronological order (1970 - 2020)
e1 + r1 + e2 + r2 +
e3 + r3 + e4 + r4 +
e5 + r5 + e6 + r6 +
  plot_annotation(title = "Changes in Chronological Order",
                  subtitle = "Average % Change Per Month")





### The expansion histograms for each race, minus Asians (1970 - 2020)
ae / we / be / he +
  plot_annotation(title = "Changes by Expansion",
                  subtitle = "Average % Change Per Month")







### The recession histograms for each race, minus Asians (1970 - 2020)
ar / wr / br / hr +
  plot_annotation(title = "Changes by Recession",
                  subtitle = "Average % Change Per Month")









### The expansion histograms for white men and all races
ae2 / we2 + plot_annotation(title = "Changes by Expansion, All Races and White Men",
                            subtitle = "Average % Change Per Month")









### The recession histograms for white men and all races
ar2 / wr2 + plot_annotation(title = "Changes by Recession, All Races and White Men",
                                        subtitle = "Average % Change Per Month")


