# load data
setwd("~/Desktop/ids_702/assignment")
dt = read.csv("../data/mushrooms.csv", stringsAsFactors = TRUE)



# imports
library(ggplot2)
library(dplyr)

# useful info
# https://www.usask.ca/biology/fungi/glossary.html

# EDA
# -------

table(dt$class)

# habitat v class
habitat.df = as.data.frame(table(dt$class, dt$habitat))
colnames(habitat.df) = c("class", "habitat", "count")

ggplot(habitat.df, aes(x = reorder(habitat, -count), y = count, fill = class)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_brewer(palette="Blues") +
  theme_classic()

table(dt$class, dt$habitat)
prop.table(table(dt$class, dt$habitat), 2)

# most observation in woods and grasses, least observation for waste and meadows,
# paths has the most percentage of poisonous, followed by urban, leaves
# all mushrooms in waste are edible, meadows have the second highest percentage for edibility

# population v class
table(dt$class, dt$population)
ggplot(dt, aes(x = population, fill = class)) +
  geom_bar()

# several are most likely to be poisonous
# all abundant, numerous are edible

# spore-print-color v class
table(dt$spore.print.color, dt$class)
prop.table(table(dt$class, dt$spore.print.color), 2)

ggplot(dt, aes(x = spore.print.color, fill = class)) +
  geom_bar()

# all buff, orange, yellow are edible (all 48 obs)
# all green are poisonous
# chocolate, and white are more likely to be poisonous
# black, brown are more likely to be edible

# ring-type v class 

table(dt$ring.type, dt$class)
prop.table(table(dt$class, dt$ring.type), 2)

ggplot(dt, aes(x = ring.type, fill = class)) +
  geom_bar()

# most pendant and all flaring are edible
# all large are poisonous

# ring-number v class

table(dt$ring.number, dt$class)
prop.table(table(dt$class, dt$ring.number), 2)

ggplot(dt, aes(x = ring.number, fill = class)) +
  geom_bar()

# all none are poisonous
# most two are edible

# veil-color v class -- only one 

table(dt$veil.color, dt$class)
prop.table(table(dt$class, dt$veil.color), 2)

ggplot(dt, aes(x = veil.color, fill = class)) +
  geom_bar()

# all brown, orange are edible (96 obs each)
# all yellow are poisonous but only 8 obs
# white are slightly more likely to be edible

# veil.type v class (remove this? only one type)

table(dt$veil.type, dt$class)
prop.table(table(dt$class, dt$veil.color), 2)

ggplot(dt, aes(x = veil.color, fill = class)) +
  geom_bar()

# stalk.color.below.ring v class

table(dt$stalk.color.below.ring, dt$class)
prop.table(table(dt$class, dt$veil.color), 2)

ggplot(dt, aes(x = veil.color, fill = class)) +
  geom_bar()

# all buff, cinnamon, yellow (24 obs) are poisonous
# all red, gray, orange are edible
# brown, pink are more likely to be poisonous


# stalk.color.above.ring v class

table(dt$stalk.color.above.ring, dt$class)
prop.table(table(dt$class, dt$stalk.color.above.ring), 2)

ggplot(dt, aes(x = stalk.color.above.ring, fill = class)) +
  geom_bar()

# all buff, cinnamon, yellow (8 obs only) are poisonous
# brown, pink are more likely to be poisonous
# red, gray, orange are edible
# red are less likely poisonous

# finding are similar to the stalk.color.below.ring

# stalk-surface-below-ring v class

table(dt$stalk.surface.below.ring, dt$class)
prop.table(table(dt$class, dt$stalk.surface.below.ring), 2)

ggplot(dt, aes(x = stalk.surface.below.ring, fill = class)) +
  geom_bar()

# fibrous, smooth, scaly are more likely edible
# silky are more likely to be poisonous

# stalk.surface.above.ring v class (same conclusion as stalk-surface-below-ring)

table(dt$stalk.surface.above.ring, dt$class)
prop.table(table(dt$class, dt$stalk.surface.above.ring), 2)

ggplot(dt, aes(x = stalk.surface.below.ring, fill = class)) +
  geom_bar()

# fibrous, smooth, scaly are more likely edible
# silky are more likely to be poisonous

# stalk.root v class (missing value - need imputation??)

table(dt$stalk.root, dt$class)
prop.table(table(dt$class, dt$stalk.root), 2)

ggplot(dt, aes(x = stalk.root, fill = class)) +
  geom_bar()

# all rooted are edible
# club, equal are more likely to be edible
# ? are more likely to be poisonous

# stalk.shape v class

table(dt$stalk.shape, dt$class)
prop.table(table(dt$class, dt$stalk.shape), 2)

ggplot(dt, aes(x = stalk.shape, fill = class)) +
  geom_bar()

# enlarging are more likely to be poisonous

# gill.color v class

table(dt$gill.color, dt$class)
prop.table(table(dt$class, dt$stalk.root), 2)

ggplot(dt, aes(x = stalk.root, fill = class)) +
  geom_bar()

# all black, green (24 obs) are poisonous
# red, orange are edible 
# gray, chocolate are more likely to be poisonous
# others are more likely to be edible

# gill.size v class

table(dt$gill.size, dt$class)
prop.table(table(dt$class, dt$gill.size), 2)

ggplot(dt, aes(x = gill.size, fill = class)) +
  geom_bar()

# broad are more edible
# narrow are more poisonous

# gill.spacing v class

table(dt$gill.spacing, dt$class)
prop.table(table(dt$class, dt$gill.spacing), 2)

ggplot(dt, aes(x = gill.spacing, fill = class)) +
  geom_bar()

# crowded are more edible

# gill.attachment v class

table(dt$gill.attachment, dt$class)
prop.table(table(dt$class, dt$gill.attachment), 2)

ggplot(dt, aes(x = gill.attachment, fill = class)) +
  geom_bar()

# attached are more edible
# free is slightly more likely to be edible

# odor v class (ODOR SEEMS TO BE STRONG INDICATOR)

table(dt$odor, dt$class)
prop.table(table(dt$class, dt$odor), 2)

ggplot(dt, aes(x = odor, fill = class)) +
  geom_bar()

# all almond, anise are edible
# none are more likely to be edible
# all creosote, foul, pungent, spicy, fishy are poisonous

# bruises v class

table(dt$bruises, dt$class)
prop.table(table(dt$class, dt$bruises), 2)

ggplot(dt, aes(x = bruises, fill = class)) +
  geom_bar()

# None bruise are more likely to be poisonous
# bruises are more to be edible

# cap.color v class

table(dt$cap.color, dt$class)
prop.table(table(dt$class, dt$cap.color), 2)

ggplot(dt, aes(x = cap.color, fill = class)) +
  geom_bar()

# buff are more poisonous
# cinnamon are more edible
# all green, purple are edible (only 16 obs each)

# cap.surface v class

table(dt$cap.surface, dt$class)
prop.table(table(dt$class, dt$cap.surface), 2)

ggplot(dt, aes(x = cap.surface, fill = class)) +
  geom_bar()

# all gray are poisonous (4 obs)
# fibrous are more edible

# cap.shape v class

table(dt$cap.shape, dt$class)
prop.table(table(dt$class, dt$cap.shape), 2)

ggplot(dt, aes(x = cap.shape, fill = class)) +
  geom_bar()

# bell are more edible
# conical are poisonous (only 4 obs)
# knobbed are more poisonous
# all sunken are edible (32 obs)
# convex are more edible


ggplot(dt, aes(spore.print.color, group = class, fill = class)) +
  geom_bar(color = "black", position = "dodge") +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.3, position = position_dodge(0.9)) +
  facet_wrap(~stalk.shape) +
  scale_fill_brewer(palette="Blues", labels = c("edible", "poisonous")) +
  theme_classic() +
  labs(title = "Cap Surface v. Class by Gill Size")


ggplot(dt, aes(bruises, group = class, fill = class)) +
  geom_bar(color = "black", position = "dodge") +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.3, position = position_dodge(0.9)) +
  facet_wrap(~gill.color) +
  scale_fill_brewer(palette="Blues", labels = c("edible", "poisonous")) +
  theme_classic() +
  labs(title = "Cap Surface v. Class by Gill Size")
