#Reading in the data
library(tidyverse)

cristina1 = read.csv(file = "/Users/KeatonWilson/Documents/Projects/melanin-extraction/data/cristina_poly1.csv")
cristina2 = read.csv(file = "/Users/KeatonWilson/Documents/Projects/melanin-extraction/data/cristina_poly2.csv")
colnames(cristina1) = c("ID", "r1", "r2", "r3", "r4", "r5", "x", "Sum")
colnames(cristina2) = c("ID", "r1", "r2", "r3", "r4", "r5", "x", "Sum")

cristina1 = cristina1[, -7]
cristina2 = cristina2[, -7]

cristina = bind_rows(cristina1, cristina2)
cristina = cristina[-c(16, 32), -c(8:10)]
cristina = as.tbl(cristina)

write.csv(cristina, file = "/Users/KeatonWilson/Documents/Projects/melanin-extraction/data/cristina.csv")

ggplot(cristina, aes(x = r1, y = Sum)) + 
  geom_point(size = 3, alpha = 0.7) + 
  theme_classic() +
  stat_smooth(method = "lm") +
  xlab("Round 1 Melanin Concentration") +
  ylab("Summed total Melanin Concentration")


lm1 = lm(Sum ~ r1, data = cristina)
summary(lm1)


#A few of the plots Goggy suggested - we have to make the data 'tidy' first and do some manipulations

#Step 1 is lengthening the data
cristina_long = gather(cristina, key = round, value = melanin, -Sum, -ID)

ggplot(cristina_long, aes(x = melanin, y = Sum, color = factor(round))) +
  geom_point(size = 3, alpha = 0.7) +
  theme_classic()

#OK, so this is interesting, you're getting less and less information with each extraction
#about the relationship, let's try and graph this a different way, by doing the cumulative sum.

#Let's generate a data frame of the R1+R2 values

cristinar1r2 = cristina_long %>%
  filter(round == "r1" | round == "r2") %>%
  group_by(ID) %>%
  summarize(r1r2 = sum(melanin))

#Generate a data frame of R1 + R2 + R3 values

cristinar1r3 = cristina_long %>%
  filter(round == "r1" & round == "r2" & round == "r3") %>%
  group_by(ID) %>%
  summarize(r1r3 = sum(melanin))

#Generate a data frame of R1 + R2 + R3 + R4 values

cristinar1r4 = cristina_long %>%
  filter(round == "r1" | round == "r2" | round == "r3" | round == "r4") %>%
  group_by(ID) %>%
  summarize(r1r4 = sum(melanin))

#Generate a data frame of R1 thru R5, summed (this should have an R^2 of 1)

cristinar1r5 = cristina_long %>%
  group_by(ID) %>%
  summarize(r1r5 = sum(melanin))

cristina_new = as.tibble(bind_cols(cristinar1r2, cristinar1r3, cristinar1r4, cristinar1r5)) %>%
  select(-ID1, -ID2, -ID3)

r1 = cristina_long %>%
  filter(round == "r1") %>%
  select(melanin, Sum)

cristina_new = bind_cols(cristina_new, r1)


#Changing back to a long format
cristina_new = cristina_new %>%
  rename(r1 = melanin)
cristina_new_long = gather(cristina_new, key = roundID, value = melanin, -Sum, -ID)

cristina_new_long %>%
  filter(roundID == "r1r5") %>%
ggplot(aes(x = melanin, y = Sum)) +
  geom_point() +
  theme_classic()

#Looks weird - something went awry (trying a different approach)
Fig.1 = cristina %>%
  mutate(r1r2 = (r1 + r2),
         r1r3 = (r1 + r2 + r3), 
         r1r4 = (r1 + r2 + r3 + r4), 
         r1r5 = (r1 + r2 + r3 + r4 + r5)) %>%
  select(-(r2:r5)) %>%
  gather(key = roundID, value = melanin, -ID, -Sum) %>%
    ggplot(aes(x = melanin, y = Sum, color = factor(roundID))) +
    geom_point() +
    geom_smooth(method = "lm", alpha = 0.2) +
    theme_classic() +
    scale_color_discrete(name = "Round",
                        breaks = c("r1", "r1r2", "r1r3", "r1r4", "r1r5"),
                        labels = c("Round 1", "Rounds 1 & 2", "Rounds 1-3", "Rounds 1-4",
                                   "All Rounds")) + 
    xlab("Melanin") +
    ylab("Total Melanin")

ggsave(plot = Fig.1, "output/Fig.1.pdf", device = "pdf", width = 10, height = 10, units = "in")


#Now let's extract each R2 from the models.
library(broom)
cristina_lms = cristina %>%
  mutate(r1r2 = (r1 + r2),
         r1r3 = (r1 + r2 + r3), 
         r1r4 = (r1 + r2 + r3 + r4), 
         r1r5 = (r1 + r2 + r3 + r4 + r5)) %>%
  select(-(r2:r5)) %>%
  gather(key = roundID, value = melanin, -ID, -Sum) %>%
  group_by(roundID) %>%
  do(roundfit = lm(Sum ~ melanin, data = .))

#Extract R^2s
cristina_R2s = glance(cristina_lms, roundfit)

Fig.2 = ggplot(cristina_R2s, aes(x = roundID, y = adj.r.squared, group = 1)) +
  geom_point() +
  geom_path() + 
  theme_classic() + 
  scale_x_discrete(labels = c("Round 1", "Rounds 1 & 2", "Rounds 1-3", "Rounds 1-4",
                              "All Rounds")) +
  xlab("") +
  ylab("Adjusted R-Squared")

ggsave(plot = Fig.2, "output/Fig.2.pdf", device = "pdf", width = 10, height = 10, units = "in")

  
  

