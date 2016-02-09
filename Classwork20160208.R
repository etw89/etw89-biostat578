data("iris")
iris

head(iris)
iris[100,]
iris$Species
iris[, "Species"]
iris$Species[100:105]

TF <- iris$Species == "versicolor"

which(TF)
iris[iris$Species == "versicolor",]

library(dplyr)

head(iris %>% select(Sepal.Width, Species))


iris %>% select(Sepal.Width, Species) %>% 
  filter(Species == "versicolor") %>%
  summarise(mean.sepal.width = mean(Sepal.Width))



iris %>% select(Sepal.Width, Species) %>% 
  group_by(Species) %>%
  summarise(mean.sepal.width = mean(Sepal.Width))


iris %>%
  select(Sepal.Width, Species) %>%
  arrange(Sepal.Width) %>%
  summarise(tenth.smallest = Sepal.Width[10])


iris %>% select(Species)

iris %>% select(Sepal.Width) %>% filter(Sepal.Width > mean(Sepal.Width)) %>% 
  summarise(flowerland = length(Sepal.Width)) -> flowerlady


##How many flowers are there in each species of Iris?

iris %>%select(Species, Sepal.Width)%>%
  group_by(Species) %>%
  summarise(number = length(Sepal.Width))
# ANSWER: 3

## How many flowers are there in sepal width greater than the overall mean?
iris %>% select(Sepal.Width) %>% 
  filter(Sepal.Width > mean(Sepal.Width)) %>% 
  summarise(flowerland = length(Sepal.Width))

# ANSWER: 67


#Of the flowers with sepal width, how many belong to each species?

iris %>% select(Species, Sepal.Width) %>% 
  filter(Sepal.Width > mean(Sepal.Width)) %>% 
  group_by(Species) %>%
  summarise(number = n())
# ANSWER:    setosa     42; versicolor      8; virginica     17



