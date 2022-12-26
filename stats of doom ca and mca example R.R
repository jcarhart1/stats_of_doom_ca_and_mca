

## Language Topics Discussed

# A reanalysis of color terms and categories



library(reticulate)


## Basic Color Terms
# Berlin and Kay (1969) proposed a theory about our linguistic interpretations of colors, mainly that color vocabulary falls into universal categories:
  

## Simple Correspondence Analysis


# Data is from the Corpus of Contemporary American English (COCA)
# Counts of adjective use of color terms


install.packages("Rling")
# this will not work. Have to download from https://github.com/levshina/Rling/blob/master/Rling_1.0.tar.gz and install

library(Rling)

data(colreg)

head(colreg)

write.csv(colreg, "C:\\code_personal_use\\stats_of_doom_ca_and_mca\\col_reg.csv", row.names=FALSE)


# Chi-square analyses tell us if specific category frequencies are different than we might expect.
# Let's look at a simple combination to understand the math behind chi-square.
cs_example = colreg[1:2, 1:2]

cs_example


## Expected values
cs_example_e = cs_example

rows = rowSums(cs_example)

columns = colSums(cs_example)

cs_example_e[1,1] = rows[1]*columns[1]/sum(cs_example)
cs_example_e[1,2] = rows[1]*columns[2]/sum(cs_example)
cs_example_e[2,1] = rows[2]*columns[1]/sum(cs_example)
cs_example_e[2,2] = rows[2]*columns[2]/sum(cs_example)

cs_example_e


## Using chisq.test
cs_test = chisq.test(cs_example)
cs_test$expected

cs_example

# plug the data into the chi-sq formula (which is similar conceptually to the sd formula)
# chi-sq: sum(o - e)^2 / e 
# std dev: sqrt(sum(xi - xmean)^2 / n)

sum((cs_example - cs_example_e)^2 / cs_example_e)

cs_test$statistic


## Chi-Square - What Next?

# This test doesn't tell you *what* was different though, much like ANOVA
# The way to know what cells were higher/lower than expected would be to use standardized residuals

## Residuals 

# Residuals are (O-E)/sqrt(E), whereas standardized residuals are standardized format akin to z-scores (O-E)/ sqrt(var(residuals))

#(cs_example - cs_example_e) /sqrt(cs_example_e)
cs_test$residuals 

#(cs_example - cs_example_e) /sqrt(var(residuals))
cs_test$stdres


## Mosaic Plots
# A visualization of the standardized residuals from a chi-square type analysis
# The box size is related to the observed cell size 
# Coloring is shaded based on direction and strength of the residuals


## Mosaic Plots - Small Example
mosaicplot(colreg[1:2, 1:2], #data frame
           las = 2, #axis label style (perpendicular)
           shade = T, #color in the boxes
           main = "Register Variation")



## Mosaic Plot - Full Data
mosaicplot(colreg, #data frame
           las = 2, #axis label style (perpendicular)
           shade = T, #color in the boxes
           main = "Register Variation")


##########################################################################################################################################

## Simple Correspondence Analysis 
# Identifies systematic relationships between variables in low dimensional space
# Similar to multidimensional scaling, principle components analysis, exploratory factor analysis

library(ca)
sca_model = ca(colreg)


## What's in the output?
summary(sca_model)



## Inertia
# Top part is the table of inertias, which explain how much variation is accounted for by each dimension
# These are similar to eigenvalues that we will cover more with factor analyses 
# Try to represent the relationship between variables in as few dimensions as possible
# Here we see that the first two dimensions capture 97% of the variance
# And the third dimension captures all the variance 



## Visualize the Dimensions
plot(sca_model)



## Key Differences

# How are these plots different than the ones we've been making?

######## Terms are close together if they have similar frequency counts
######## This means the rows have similar *profiles* - rather than similar relationships to a latent variable 

# The distances on the map are a representation of the $\chi^2$ values of each row/column to the average profile



## Some other interesting notes
# Press is close to green-red because of the political orientation for these terms and proper names (Red Cross/Green Bay Packers)
# Fiction is likely close to the later color terms because of the requirement to "paint a picture" for readers
# Appears academics and spoken speech are pretty boring in their use of color terms



## 3D Plots in R
plot3d.ca(sca_model, #model
       labels = c(1,1)) #see both row and column labels


## A Category Example
# Is there a difference between the categories for *stuhl* (chair) and *sessel* (armchair)?
# Gipper (1959) had subjects name pictures of chairs to determine their relative frequencies
# The difference appeared to be that chairs are functional, while armchairs are about comfort

## The Data
# Data was coded from an online shopping place based on their text descriptions and other chair related variables 


data(chairs)

head(chairs)

write.csv(chairs, "C:\\code_personal_use\\stats_of_doom_ca_and_mca\\chairs.csv", row.names=FALSE)


## Multiple Correspondence Analysis
library(FactoMineR)

mca_model = MCA(chairs[ , -c(1:3)], #dataset minus the first 3 columns
                graph = FALSE)


summary(mca_model)


## Plot the MCA
plot(mca_model, cex = .7, 
     col.var = "black",  #color the variable names
     col.ind = "gray")
    # invis = "ind") #color the indicators



## How Useful are the Variables?
dimdesc(mca_model)


## Interpretation
# r-squared values represent the variables association with the dimension
# p-value represent the strength of that association
# Then the `$category` section represents the directionality of the relationship
# If this value is positive, shows on the right hand side of plot, representing a positive coefficient (and vice versa)
  

## Overall interpretation
# First dimension seems to represent comfort chairs versus not
# Second dimensions seems to represent functionality (work versus home)
# Third is harder to understand 
# Appears to separate chairs into three categories:
# Comfortable relaxation chairs
# Comfortable adjustable chairs for work
# Multifunctional chairs for the house



## Using the chair label
# We did not use the type of chair that is found in column 3 of our dataset
# We can map it onto our analysis using it as a supplementary variable 


## Running that analysis
mca_model2 = MCA(chairs[ , -c(1,2)], 
                 quali.sup = 1, #supplemental variable
                 graph = FALSE)



## Plot that analysis
plot(mca_model2, invis = "ind", col.var = "darkgray", col.quali.sup = "black")
#the invis turned off the individual points


## Examine the prototypes
plotellipses(mca_model2, 
             keepvar = 1, #use column 1 to label
             label = "quali")



## Interpretation
# These confidence ellipses do not overlap, so we could consider the prototypes distinct entities 
# We can also create a more traditional 95% CI type interval


## 95% Ellipses 
# Now you can see that the categories themselves overlap a lot, so likely a 
# representation of the fuzzy boundaries that categories appear to have. 
plotellipses(mca_model2, 
             means = F,
             keepvar = 1, #use column 1 to label
             label = "quali")


## The plot
plotellipses(mca_model2, 
             means = F,
             keepvar = 1, #use column 1 to label
             label = "quali")



## But what about inertia?
mca_model2$eig


## Inertia part 2 (use this one!!!)
mca_model3 = mjca(chairs[ , -c(1:3)])

summary(mca_model3)



## Further work
# From here, you could take the dimension scores `mca_model2$ind$coord` and use them to predict the categories or other variables
# This analysis would tell you how good at representing their categories each dimension does 

## Summary

# We applied new models to basic color terms and category groupings
# We learned how to do simple and multiple correspondence analysis