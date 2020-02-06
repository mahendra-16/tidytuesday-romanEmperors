
library(tidyverse)
library(lubridate)
library(ggfittext)



emperors <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-13/emperors.csv")
View(emperors)

# let's calculate the number or years roman emperors ruled 
ruled_by_years <- (emperors$reign_end - emperors$reign_start)/dyears(1)

# if we view it than we find that all the numbers are in fraction
ruled_by_years

# but there is one problem if we see the first one it's in minus it might be an error
emp <- select(emperors, reign_start, reign_end)

# so let's check the data once again 
View(emp)

# we can see that it has mistakely inter changed.
emp[1,]

# we correct the number which was wrongly mentioned in the first row
reign_order <- c("0014-08-19","0026-01-16")

emp[1,] <- reign_order
emp[1,]
View(emp)

emperors[ , -3]

reign <- cbind(emperors[, c(-9, -10)], emp)
View(reign)

new_emperors <- select(reign, name,dynasty,reign_start, reign_end, rise)

View(new_emperors)

# ceiling function round the fraction number to the nearest possible number
ceiling(new_emperors$years_rules)

# we are tried to check how many years each roman emperors ruled 
new_emperors <- mutate(new_emperors, years_rules = ceiling((reign_end - reign_start)/dyears(1)))
View(new_emperors)

# plot 
ggplot(new_emperors, aes(x = reorder(name, years_rules), y = years_rules, colour = rise)) + 
  geom_point(size = 2) + coord_flip() +
  geom_text(aes(label = years_rules), hjust=2, colour="Red", size = 2.5) + 
  ylab("No. of years they ruled") + xlab("Roman emperor")



# ploting causes and killer of roman emperors
new_emp2 <- select(reign, name, reign_start,reign_end,rise, cause, killer)
View(new_emp2)

new_emp2 <- mutate(new_emp2, years_rules = ceiling((reign_end - reign_start)/dyears(1)))
View(new_emp2)

ggplot(new_emp2, aes(factor(cause), "", fill = killer, label = paste(name, "reignYears",years_rules))) + 
  geom_col(position = "stack") +
  geom_fit_text(position = "stack", reflow = TRUE, min.size = 3) + 
  theme(axis.title.y = element_text()) + xlab("Cause of Emperors Death")



