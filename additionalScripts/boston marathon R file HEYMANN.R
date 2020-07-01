#Danielle Heymann
#CSCI 688 Linear Regression
#Boston Marathon Project
#R Script for visualizations

install.packages("PerformanceAnalytics")
install.packages("corrplot")
install.packages("psych")
install.packages("ggplot2")
install.packages("GGally")



b_data_a <- read.csv('C:\\Users\\dheym\\OneDrive\\Documents\\WilliamMary19-20\\LinearRegression\\boston_data_A.csv', header = TRUE)
  
b_select_a <- subset(b_data_a, select = c("Y..Official.Time",  "X1..Age",            "X2..5K.Pace",        "X3..Half.Pace",         "X4..Gender",        
                  "X5..Wave..Elite",    "X6..Wave..One",      "X7..Wave..Two",      "X8..Wave..Three" ))
names(b_select_a)

names(b_select_a) <- c("Y", "X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8")

#Using PerformanceAnalytics library :
library("PerformanceAnalytics")
chart.Correlation(b_select_a, histogram = T, pch= 19 )

bostoncor <- cor(b_select_a)

library("corrplot")
corrplot(bostoncor)

library(psych)
pairs.panels(b_select_a[,-5], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE, # show correlation ellipses
             main = "Correlation Matrix"
)
library(GGally)
library(ggplot2)
ggpairs(b_select_a, columns = c("Y", "X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8"),
        title = "Bivariate analysis of Boston Marathon 2017 Performance Factors",
        upper = list(continuous = wrap("cor",
                                       size = 3)),
                  
        lower = list(
          continuous = wrap("smooth",
                            alpha = 0.3,
                            size = 0.1))
)

criterias_A <- read.csv('C:\\Users\\dheym\\OneDrive\\Documents\\WilliamMary19-20\\LinearRegression\\selectioncriteriaboston1.csv', header = TRUE)
names(criterias_A)
names(criterias_A) <- c("p", "R-Squ", "R-Squ-Adj", "PRESS", "R-Squ-Pred", "Cp", "S", "AICp", "BIC",  "Cond.No")

criterias_A <- criterias_A[c(2:16), ]

#plotting genders impact on 5K vs half splits
library(ggplot2)
ggplot(b_data_a, aes(x= X3..Half.Pace, y= X2..5K.Pace, color =M.F)) + 
  geom_point(shape = 21, alpha = .3)+
  geom_smooth() +    
  labs(title="5K Pace vs Half Marathon Pace WRT Gender", y = "X2: 5K Pace (min./km)", x = "X3: Half Pace (min./km)") + guides(fill=guide_legend(title="Gender")) 

#plotting genders impact on finish time
library(ggplot2)
ggplot(b_data_a, aes(x = Y..Official.Time, color = M.F, fill = M.F, alpha = .1)) + 
  geom_dotplot(method = "histodot",binwidth = 1.5, stackgroups = TRUE, binpositions = "all") +
    
  labs(title="Finish Times Density Plot WRT Gender", y = "Count", x = "Y, Official Time (min.)") + guides(fill=guide_legend(title="Gender")) 


#plotting genders impact on finish time
library(ggplot2)
ggplot(b_data_a, aes(x = Y..Official.Time, color = M.F, fill = M.F, alpha = .1)) + 
  geom_dotplot(method = "histodot",binwidth = 2.2, binpositions = "all") +
  labs(title="Finish Times Density Plot WRT Gender", y = "Count", x = "Y, Official Time (min.)") + guides(fill=guide_legend(title="Gender")) 




ggplot(criterias_A, aes(x = p, y = R-Squ-Pred)) + geom_point() +  
  labs(title="Variables Selection Criteria",x= "p", y = "R-Squ-Pred") 
 
  