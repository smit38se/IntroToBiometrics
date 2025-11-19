#November 17 Question 2



Populations of the Baltimore checkerspot butterfly have two hostplants north of Maryland (state insect)
 The two hostplants are the native white turtlehead & introduced English plantain. In spring 2012/2013, 
 I collected caterpillars in the field & placed them in “bug dorms” like cages, over one of each two hostplants.  
Use larvalsurv.csv to run a model to determine 
if postdiapause (spring) larval survival differs between larvae reared on the two host plants.
-	Year = year
-	DormID = Dorm ID
-	larv_orig = prediapause host plant
-	dorm_loc = host plant in dorm (postdiapause host plant)
#-	N.emerged = number adults
#-	N.Init = initial number of larvae

#a. Remove two rows of data where host plant depleted (see the Notes column).
   # What code did you use?



#fixedbelow
# 1. Read the csv into R as a data frame


caterpillar <- read.csv("/Users/billymorgan/Downloads/BCBpostdialarvsurv.csv")
# 3. Drop the two "host depleted; DROP" rows
piller <- caterpillar %>%
  filter(Notes != "host depleted; DROP")

b.  	What distribution will you use to model these data and why?

#binomial because because its discrete and also theres a set number of trials with a defined limit.

c. 	Run a model with fixed effects of larval origin, dorm location, and year (as a factor). Include a random effect of the dorm in which caterpillars were reared as a random effect. Make sure that dorms with the same numeric ID but from different years are not treated as the same!

library(lme4)

 #make Year a factor
piller$Year <- factor(piller$Year)

#make a unique dorm ID for each year + dormID combo
piller$Dorm <- interaction(piller$Year, piller$DormID)

#binomial mixed model
larv_mod <- glmer(
  cbind(N.emerged, N.init - N.emerged) ~ larv_orig + dorm_loc + Year +
    (1 | Dorm),
  data = piller,
  family = binomial
)





Does survival differ between larvae reared on the two different host plants? Why or why not? Support your answer with a statistical test.


