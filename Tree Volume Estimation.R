library(readr)

scatterData <- read_csv("~/Downloads/scatterData.csv")

x<- scatterData$`height (m)`
y<- scatterData$height_uav_fr
z<- scatterData$height_uav_sat

grps <- as.factor(scatterData$species)

#Plot
library(scatterplot3d)
scatterplot3d(x, y, z, pch = 16)

#Change color by groups
# add grids and remove the box around the plot
# Change axis labels: xlab, ylab and zlab

colors <- c("#999999", "#E69F00", "#56B4E9")
scatterplot3d(x, y, z, pch = 16, color = colors[grps],
              grid = TRUE, box = FALSE, xlab = "Ground Measured Height", 
              ylab = "Friday UAV Measured Height", zlab = "Saturday UAV Measured Height")
library("car")
scatterplot(`height (m)` ~ height_uav_fr, data = scatterData,
            smoother = FALSE, grid = FALSE, frame = FALSE,
            
            id.cex = 0.7, id.col = "steelblue",
            ellipse = FALSE)

library(ggplot2)
Specis <- scatterData$species
fri <- ggplot(scatterData, aes(x = scatterData$height_uav_fr, y = scatterData$`height (m)`)) +
  geom_point(aes(color = Specis)) +
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1)
fri + 
  labs(
    title = "Scatter Plot of:",
    subtitle = "Ground Measured Height & UAV Measured Height on Friday",
    x = "UAV Measured Height on Friday",
    y = "Ground Measured Height"
  )

sat <- ggplot(scatterData, aes(x = scatterData$height_uav_sat, y = scatterData$`height (m)`)) +
  geom_point(aes(color = Specis)) +
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1)
sat + 
  labs(
    title = "Scatter Plot of:",
    subtitle = "Ground Measured Height & UAV Measured Height on Saturday",
    x = "UAV Measured Height on Saturday",
    y = "Ground Measured Height"
  )

fri_and_sat <- ggplot(scatterData, aes(x = scatterData$height_uav_sat, y = scatterData$height_uav_fr)) +
  geom_point(aes(color = Specis)) +
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1)
fri_and_sat + 
  labs(
    title = "Scatter Plot of:",
    subtitle = "Friday and Saturday UAV Measured Height",
    x = "UAV Measured Height on Saturday",
    y = "UAV Measured Height on Friday"
  )


#T TEST

#t-test statistisc value can be calculated using the following formula:
  
 # t=ms/n‾√

#where,

#m is the mean differences
#n is the sample size (i.e., size of d).
#s is the standard deviation of d


#We can compute the p-value corresponding to the absolute value of the t-test statistics (|t|) for the degrees of freedom (df): df=n−1.

#If the p-value is inferior or equal to 0.05, we can conclude that the difference between the two paired samples are significantly different.

cut <- c(12.23371,7.47312,10.57347,8.56789,10.96574,6.73942,6.85781,7.98548,9.85439,4.87826,5.34769,9.74521,13.84912,12.74629,9.63381,7.25739,10.14987,4.54734,5.49438)
uncut <- c(11.00451,7.22424,10.57287,8.8894,10.98461,6.70922,6.67272,13.10659,9.05444,4.56567,5.63562,2.60473,13.94238,11.37646,9.72344,7.16638,9.65563,6.82342,5.50634)

combined_data <- data.frame(
  group = rep(c("Uncut- Friday", "Cut- Saturday"), each = 19),
  height = c(uncut, cut)
)
print(combined_data)

# Compute summary statistics by groups:

library("dplyr")

group_by(combined_data, group) %>%
  summarise(
    count = n(),
    mean = mean(height, na.rm = TRUE),
    sd = sd(height, na.rm = TRUE)
  )

# A tibble: 2 x 4
# group         count  mean    sd
#* <chr>         <int> <dbl> <dbl>
#  1 Cut- Saturday    19  8.68  2.73
#2 Uncut- Friday    19  8.49  2.97

# Plot weight by group and color by group

library("ggpubr")

ggboxplot(combined_data, x = "group", y = "height", 
          color = "group", palette = c("#00AFBB", "#E7B800"),
          order = c("Uncut- Friday", "Cut- Saturday"),
          ylab = " Uncut Trees Height - Friday", xlab = "Cut Trees Height - Saturday")

# box plot shows the decrease in tree height from uncut to cut

#paired data graph

before_cutting <- subset(combined_data,  group == "Uncut- Friday", height,
                 drop = TRUE)

after_cutting <- subset(combined_data,  group == "Cut- Saturday", height,
                drop = TRUE)
# Plot paired data

library(PairedData)

pd <- paired(before_cutting, after_cutting)
plot(pd, type = "profile") + theme_bw()

# paired data graph shows a decrease in height of two trees, but also shows an increase in height of 3 trees

# Preleminary test to check paired t-test assumptions

# Assumption 1: Are the two samples paired?
# Yes, since the data have been collected from measuring twice the height of the same trees
# Assumption 2: Is this a large sample?
# No, because n < 30. Since the sample size is not large enough (less than 30), we need to check whether the differences of the pairs follow a normal distribution.

# Shapiro-Wilk normality test
# Null hypothesis: the data are normally distributed
# Alternative hypothesis: the data are not normally distributed

#Visual methods
#Density plot and Q-Q plot can be used to check normality visually.

#Density plot: the density plot provides a visual judgment about whether the distribution is bell shaped.

library("ggpubr")

ggdensity(cut, 
          main = "Density plot of Cut trees Height - Friday",
          xlab = "Height")

ggdensity(uncut, 
          main = "Density plot of Uncut trees Height - Saturday",
          xlab = "Height")

ggqqplot(cut)
ggqqplot(uncut)

# As all the points fall approximately along this reference line, we can assume normality

shapiro.test(cut)

# W = 0.9661, p-value = 0.6967
shapiro.test(uncut)
# W = 0.98046, p-value = 0.9475
# compute the difference
d <- with(combined_data, 
          height[group == "Uncut- Friday"] - height[group == "Cut- Saturday"])
# Shapiro-Wilk normality test for the differences


# From the output, the p-value is greater than the significance level 0.05 implying that the distribution of the differences (d) are not significantly different from normal distribution. In other words, we can assume the normality.


# Compute paired samples t-test

res <- t.test(uncut, cut, paired = TRUE)
res

#Paired t-test

#data:  uncut and cut
#t = -0.38562, df = 18, p-value = 0.7043
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -1.2494903  0.8619409
#sample estimates:
#  mean of the differences 
#-0.1937747 

# The p-value of the test is 0.7043, which is greater than the significance level alpha = 0.05. We can then accept the null hypothesis and conclude that the average height of the trees before cutting is not significantly different from the average height after cutting with a p-value = 6.210^{-9}.

library(forestmangr)
data("exfm7")
exfm7

# Calculate the volume with bark using the Smalian method:
smalianwb(exfm7,"di_wb", "hi", "TREE")

# Using pipes:
library(dplyr)

exfm7 %>% 
  group_by(TREE) %>% 
  smalianwb("di_wb", "hi")