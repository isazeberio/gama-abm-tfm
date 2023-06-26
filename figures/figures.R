# Code to recreate the plots from my master's thesis and calculate the ABM parameter's value
# The script does not follow the order in which the figures are presented in the paper
# 1. Voters figures: London and Great Britain
# 2. Media figures
# 3. London map
# 4. Simulation outcome figures
# The code to recreate the London map is in the final part of the script

library(tidyverse)
library(RColorBrewer)
library(readr)

# London data --------------------------------------------------------------

# This data is used as input for the proportion of each type of voter in the simulation 
# and to create the 3 scenarios within 
# 1. Majority of Remain 
# 2. Majority of Leave
# 3. Tie
# The latter did not exactly occur
# we created a third scenario to account for the variations in the margin of Remain victory


# Upload the data from the London Brexit referendum 
# Available at https://data.london.gov.uk/dataset/eu-referendum-results

london <- read_csv("EU-referendum-london.csv")

# Plot difference in vote between the UK and London
# Extract the Leave and Remain percentages for London
london_percentages <- london %>%
  filter(Region=="London") %>% 
  summarise(Pct_Leave = mean(Pct_Leave),
            Pct_Remain = mean(Pct_Remain)) %>% 
  mutate(Location="London")
# Extract the Leave and Remain percentages for the rest of the UK regions
uk_percentages <- london %>%
  group_by(Region) %>%
  summarise(Pct_Leave = mean(Pct_Leave),
            Pct_Remain = mean(Pct_Remain)) %>%
  summarise(Pct_Leave = mean(Pct_Leave),
            Pct_Remain = mean(Pct_Remain)) %>% 
  mutate(Location="UK")

# Combine the London and UK data frames
comparison <- rbind(london_percentages, uk_percentages)

# Reshape the data into long format
comparison_long <- comparison %>%
  pivot_longer(cols = c(Pct_Leave, Pct_Remain), names_to = "Vote", 
               values_to = "Percentage")

# Create the bar plot
# In the paper this is Figure 5. Comparison of Leave and Remain Vote: London vs UK
p0 <- ggplot(comparison_long, aes(x = Percentage, y = Location, fill = Vote)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = c("#E60000", "#003883"),
                    labels = c("Leave","Remain")) + 
  xlab("") +
  ylab("") +
  theme_bw() +
  geom_text(aes(label = paste0(round(Percentage), "%")), 
           color = "white", position = position_fill(vjust = 0.5)) 

ggsave("plot0.png", plot = p0)

# Filter by region to work only with data from London 
london <-  london %>% filter(Region=="London") 
tibble(london)

# Turnout Percentage by borough
# In the paper this is Figure 12. Turnout by Borough
p1 <-ggplot(london, aes(x = reorder(Area, -Pct_Turnout), y = Pct_Turnout)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  # geom_text(aes(label = paste0(Pct_Turnout, "%")), vjust = -0.5, 
  #          color = "white", size = 3, position = position_fill(vjust = 0.5)) +
  xlab("Borough") +
  ylab("Turnout Percentage") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8)) +
  coord_cartesian(ylim = c(0, 100))

# The commented code allows you to add the exact turnout percentage in each bar

ggsave("plot1.png", plot = p1)


# Create a new column that classifies boroughs by the majority position: Remain, Leave, Tie
london <- london %>% mutate(Position = case_when(Pct_Leave>Pct_Remain~"Leave",
                                            Pct_Remain<=55~"Tie",
                                            Pct_Remain>Pct_Leave~"Remain",))

# We will use the average vote in each category to create the prototype scenarios for the simulation 
prototypes <- london %>% group_by(Position) %>% summarize(avg_remain=mean(Pct_Remain),
                                            avg_leave=mean(Pct_Leave),
                                            n=n())
library(kableExtra)
# In the paper we use this information to create Table 4. Design of the Simulation and Parameter Values
prototypes %>% 
  kbl() %>%
  kable_classic(full_width = F, html_font = "Times New Roman")


# Table boroughs and the position that won
lnd <-  london %>% select(Area,Pct_Remain, Pct_Leave)
lnd <- lnd %>%
  arrange(desc(Pct_Remain))

lnd %>%
  kbl() %>%
  kable_classic(full_width = F, html_font = "Times New Roman") %>%
  kable_styling() %>%
  row_spec(1:(nrow(lnd)), background = "#ffb3b3") %>% 
  row_spec(nrow(lnd) - 4:nrow(lnd), background = "white") 


# Table boroughs Remain
remain.counties <- filter(lnd, Pct_Remain > Pct_Leave)
remain.counties %>% 
  kbl() %>%
  kable_classic(full_width = F, html_font = "Times New Roman")

# Table boroughs Leave
leave.counties <- filter(lnd, Pct_Remain < Pct_Leave)
leave.counties %>% 
  kbl() %>%
  kable_classic(full_width = F, html_font = "Times New Roman")




# GB data -----------------------------------------------------------------

# British Election Survey 
# Fieldhouse, E., J. Green, G. Evans, J. Mellon, C. Prosser, J. Bailey, R.De Geus, H. Schmitt, and C. Van Der Eijk. 2020. 
# “British ElectionStudy, 2014-2023: Combined Internet Panel.” UK Data Service. https://doi.org/10.5255/UKDA-SN-8202-2.
# Wave 7: 2016 pre-local election wave of the 2014-2023 British Election Study Internet Panel 
# Wave 8: The 2016 EU Referendum campaign wave of the 2014-2023 British Election Study Internet Panel 
# Wave 9: 2016 post-referendum wave of the 2014-2023 British Election Study Internet Panel 

# Upload the data
library(haven)
bes_w7 <- read_sav("bes_w7") #Read the SPSS file   
bes_w8 <- read_sav("bes_w8")
bes_w9 <- read_sav("bes_w9")

# Extract variable names from each database
vars1 <- names(bes_w7)
vars2 <- names(bes_w8)
vars3 <- names(bes_w9)

# Remove conflicting values 
bes_w7 <- zap_labels(bes_w7)
bes_w8 <- zap_labels(bes_w8)
bes_w9 <- zap_labels(bes_w9)

# Identify variables present in all three datasets
common_vars <- intersect(intersect(vars1, vars2), vars3)

# Create a new dataset with only the common variables
combined_bes <- bind_rows(
  mutate(select(bes_w7, all_of(common_vars)), wave = 7),
  mutate(select(bes_w8, all_of(common_vars)), wave = 8),
  mutate(select(bes_w9, all_of(common_vars)), wave = 9)
)

# During the development of the study, we explored the following variables:
bes_df <- combined_bes %>%
  dplyr::select(wave,id,turnoutUKGeneral,partyIdStrength,euRefVote,britishness,
                scottishness,welshness,englishness,europeanness,trustMPs,likeCameron,
                likeCorbyn,likeFarron,likeFarron,likeSturgeon,likeWood,likeFarage,
                likeBennett,likeJohnson,likeGove,likeOsborne,riskPoverty,likeCon,
                likeLab,likeLD,likeSNP,likePC,likeUKIP,likeGrn,changeEconomy,
                EUIntegrationSelf,
                socialIdentityGlobalLeave,socialIdentityGlobalRemain,satDemUK,satDemEU,
                immigSelf,ptvCon,ptvLab,ptvLD,ptvSNP,ptvPC,ptvUKIP,ptvGrn,euID,euID1,euID2,
                euID3,euID4,euID5,euID6,euID7,education,edlevel,small_mii_cat,ns_sec,
                country,gender,p_past_vote_2015,p_past_vote_2017,p_eurefvote,satDemUK,mii,
                mii_cat)

# 9999 category represents missing value so we changed it to NA
bes_df[bes_df == 9999] <- NA

library(forcats)
# Most Important Issue for the UK citizens
# convert to regular factor
bes_df$small_mii_cat <- as_factor(bes_df$small_mii_cat)

# Re-code the factor levels
bes_df$small_mii_cat <- recode(bes_df$small_mii_cat,
                               "1" = "Europe",
                               "2" = "Immigration",
                               "3" = "Economy",
                               "4" = "Health",
                               "5" = "Terrorism",
                               "7" = "Inequality",
                               "8" = "Environment",
                               "9" = "Austerity/spending",
                               "10" = "Negativity",
                               "11" = "Other lib-auth",
                               "12" = "Other Left-right",
                               "13" = "Other")
# Color vector
colors <- colorRampPalette(c("#045275", "#089099", "#7ccba2", "#f7feae"))

# Use the factor function to re-label the values and save them in a new variable called vote_leave
bes_df$vote_leave <- factor(bes_df$p_eurefvote)

# Recode the factor labels for Leave and Remain 
bes_df$vote_leave <- recode(bes_df$vote_leave,
                                "1" = "Leave",
                                "0" = "Remain")

# Wave 7
arguments_w7 <- bes_df %>% 
  filter(wave==7) %>% 
  group_by(small_mii_cat, vote_leave) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  drop_na() 

# Figure 2. MII Wave 7, Pre-Referendum
p2 <- ggplot(arguments_w7, 
       aes(x = reorder(small_mii_cat, n), y = n, #small_mii_cat
           fill=vote_leave)) +
  geom_col(position = "dodge", alpha = 0.8) +
  labs(x = "Most important issue", y = "Count", fill = "Referendum vote") +
  scale_fill_manual(values = c("#003883", "#E60000")) +
  theme_bw() +
  coord_flip() 

ggsave("plot2.png", plot = p2)

# Wave 8
arguments_w8 <- bes_df %>% 
  filter(wave==8) %>% 
  group_by(small_mii_cat, vote_leave) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  drop_na() 

# Figure 3. MII Wave 8, During the Referendum
p3 <- ggplot(arguments_w8, 
            aes(x = reorder(small_mii_cat, n), y = n, 
                fill=vote_leave)) +
  geom_col(position = "dodge", alpha = 0.8) +
  labs(x = "Most important issue", y = "Count", fill = "Referendum vote") +
  scale_fill_manual(values = c("#003883", "#E60000")) +
  theme_bw() +
  coord_flip() 

ggsave("plot3.png", plot = p3)

# Wave 9
arguments_w9 <- bes_df %>% 
  filter(wave==9) %>% 
  group_by(small_mii_cat, vote_leave) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  drop_na() 

# Figure 4. MII Wave 9, Post-Referendum
p4 <- ggplot(arguments_w9, 
             aes(x = reorder(small_mii_cat, n), y = n, 
                 fill=vote_leave)) +
  geom_col(position = "dodge", alpha = 0.8) +
  labs(x = "Most important issue", y = "Count", fill = "Referendum vote") +
  scale_fill_manual(values = c("#003883", "#E60000")) +
  theme_bw() +
  coord_flip() 

ggsave("plot4.png", plot = p4)



# Media data --------------------------------------------------------------

# This data is used to understand the partisan print newspapers to which London citizens were exposed
# and for to simulate the reach each newspaper had.

# Data from Levy, D., Aslan, B., & Bironza, D. (2016). UK press coverage of the EU Referendum. 
# https://reutersinstitute.politics.ox.ac.uk/our-research/uk-press-coverage-eu-referendum
# Period analysed: February 20, 2016 – June 21, 2016

press <- haven::read_sav("prime_risj") # stata file

# Define a named vector of labels
# Nine national daily newspapers 
labels <- c("The Times", "The Daily Telegraph", "The Sun", "Daily Mirror", 
            "Daily Mail", "The Guardian", "Daily Express", "Financial Times (UK Edition)", 
            "The Daily Star")
names(labels) <- c(51101, 51102, 51103, 51104, 51105, 51106, 51108, 51110, 51112)

# Replace the values in press$medium with the corresponding labels
press$medium <- factor(press$medium, labels = labels)

# Labels for the referendum position
poisition_labels <- c("No disposition", "Pro-Remain", "Pro-Leave", "Mixed/Undecided")
names(poisition_labels) <- c(0, 1, 2, 3)

# Replace the values in Position and art_position (article) with the corresponding labels
press$Position <- factor(press$Position, labels = poisition_labels)

press$art_position <- factor(press$art_position, labels = poisition_labels)

# We do the same for the labels of the topics
topic_labels <- c("EU Referendum/Brexit is the main topic", "Passage about EU Referendum in article about other topics")
names(topic_labels) <- c(1, 2)

# Replace the values in press$MainTopic with the corresponding labels
press$MainTopic <- factor(press$MainTopic, labels = topic_labels)

press$Topic <- as.character(press$Topic)

press$GeneralTopic <- case_when(
  startsWith(press$Topic, "1") ~ "UK Sovereignty",
  startsWith(press$Topic, "2") ~ "Economy / Business",
  startsWith(press$Topic, "3") ~ "Migration / Mobility",
  startsWith(press$Topic, "4") ~ "Terrorism and Security",
  startsWith(press$Topic, "5") ~ "Regulations, policies and standards",
  startsWith(press$Topic, "9") ~ "Discussion around the actual vote"
)

# To simplify we keep only the most relevant and mentioned spokespersons
press <- press %>% 
  mutate(Person = ifelse(Person == 1001, "David Cameron (Conservative)", 
                         ifelse(Person == 1002, "Boris Johnson (Conservative)", 
                         ifelse(Person == 1003, "George Osborne (Conservative)", 
                         ifelse(Person == 1005, "Michael Gove (Conservative)", 
                         ifelse(Person == 1055, "Nigel Farage (UKIP)", 
                         ifelse(Person == 1117, "Vote OUT Campaigners", NA)
                         ))))))

# Number of observations per newspaper
press %>% 
  group_by(medium) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  drop_na() 

# Table summarizing information based on Remain/Leave position
press_simple <- press %>% 
  mutate(Position = case_when(medium=="The Times" ~ "Pro-Remain",
                              medium=="The Daily Telegraph"~"Pro-Leave",
                              medium=="The Sun"~"Pro-Leave",
                              medium=="Daily Mirror"~"Pro-Remain",
                              medium=="Daily Mail"~"Pro-Leave",
                              medium=="The Guardian"~"Pro-Remain",
                              medium=="Daily Express"~"Pro-Leave",
                              medium=="Financial Times (UK Edition)"~"Pro-Remain",
                              medium=="The Daily Star"~"Pro-Leave"))


# Aggregate the linePTS (Probability to see)by Position
position_totals <- aggregate(linePTS ~ Position, data = press_simple, sum)
position_totals

# Add a column with the percentages
press_simple %>%
  group_by(Position) %>%
  summarise(total_linePTS = sum(linePTS),) %>%
  mutate(percentage = round(total_linePTS/sum(total_linePTS)*100,2))

# Table represents the top 5 individuals (with their respective positions) who were more mentioned
press_simple %>%
  filter(Person != -9) %>%
  group_by(Position, Person) %>%
  summarise(n = n()) %>%
  arrange(Position, desc(n)) %>%
  group_by(Position) %>%
  top_n(5, wt = n) %>%
  select(Position, Person, n)

# More information about the newspapers, grouped by general position
# In the paper this inforamtion is udes to build Table 3. Newspapers Information
# General topic
round(prop.table(table(press_simple$GeneralTopic, press_simple$Position), margin = 2) * 100,2)
# Spokesperson mentioned
round(prop.table(table(press_simple$Person, press_simple$Position), margin = 2) * 100,2)
# Tone of the article
round(prop.table(table(press_simple$Tone, press_simple$Position), margin = 2) * 100,2)
# Time 
round(prop.table(table(press_simple$Time, press_simple$Position), margin = 2) * 100,2)


# Brexit articles per newspaper
article <- press %>% 
  group_by(medium) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  drop_na() 
article %>% 
  kbl() %>%
  kable_classic(full_width = F, html_font = "Times New Roman")

# Add the percentage column
article <- press %>% 
  group_by(medium) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  drop_na() %>% 
  mutate(percentage=round(n/26890*100,2))


# The Brexit position of the articles published by each newspaper
media_position <- press %>% 
  group_by(medium, Position, art_position) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  drop_na()

# Overall position of each newspapers
p5 <- ggplot(media_position, 
             aes(x = reorder(medium, n), y = n, 
                 fill=Position)) + 
  geom_col(position = "dodge", alpha=0.9) +
  labs(x = "Newspaper", y = "Count", fill = "Position") +
  scale_fill_manual(values = c("#b3bfd1","#003883", "#E60000", "#834ba0")) +
  theme_bw() +
  coord_flip() 

# In the paper Figure 7. Articles Position
p6 <- ggplot(media_position, 
             aes(x = reorder(medium, -n), y = n, 
                 fill=art_position)) +
  geom_col(position = "dodge", alpha=0.9) +
  labs(x = "Newspaper", y = "Count", fill = "Article position") +
  scale_fill_manual(values = c("#b3bfd1", "#003883", "#E60000", "#834ba0")) +
  theme_bw() +
  coord_flip() 

ggsave("plot6.png", plot = p6)

# Topics discussed by each newspaper
media_topic <- press %>% 
  group_by(medium, GeneralTopic) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  drop_na()

# In the paper Figure 8. Topics of the Brexit Articles
p7 <- ggplot(media_topic, aes(x = reorder(medium, -n), y = n, fill = GeneralTopic)) +
  geom_col(position = "dodge", alpha = 0.9) +
  labs(x = "Newspaper", y = "Count", fill = "Topics") +
  scale_fill_manual(values = c("#5F4690", "#38A6A5", "#73AF48", "#EDAD08",
                               "#E17C05", "#CC503E")) +
  theme_bw() +
  coord_flip()

ggsave("plot7.png", plot = p7)

# Evolution in time of th topics discussed by each newspaper
press$date <- as.Date(as.character(press$date), format = "%Y%m%d")

topic_date <- press %>% 
  group_by(date,medium, GeneralTopic) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  drop_na()

# Filter the data frame for the last 10 weeks
last_10_weeks <- topic_date[topic_date$date >= (max(topic_date$date) - 69), ]

# In the paper Figure 10. Evolution of Articles Topics
p8 <- ggplot(last_10_weeks, aes(x = date, y = n, color = GeneralTopic)) +
  geom_line() +
  #facet_wrap(~ medium, ncol = 2) +
  labs(x = "Date", y = "Count", color = "Topics") +
  scale_color_manual(values =c("#5F4690", "#38A6A5", "#73AF48", "#EDAD08", 
                               "#E17C05", "#CC503E")) +
  theme_bw()

ggsave("plot8.png", plot = p8)

# The economy and migration were pivotal topics for the campaigns
# We subset the information to plot only these categories
subset_topic <- c("Economy / Business", "Migration / Mobility")
filtered_topic_date <- last_10_weeks[last_10_weeks$GeneralTopic %in% subset_topic, ]

# In the paper Figure 11. Evolution of Economy and Migration Topic Articles
p9 <- ggplot(filtered_topic_date, aes(x = date, y = n, color = GeneralTopic)) +
  geom_line() +
  #facet_wrap(~ medium, ncol = 2) + # By newspaper
  labs(x = "Date", y = "Count", color = "Topics") +
  scale_color_manual(values = c("#38A6A5", "#73AF48")) +
  theme_bw()

ggsave("plot9.png", plot = p9)


# linePTS 
# Probability to See is a calculated measurement of the number of readers exposed 
# to a given brand or product mention
# Probability for the message to be seen, based on audience data, 
# position and size of the article etc. ==\> article reach = sum of all lines (linePTS)
press %>%
  group_by(medium) %>%
  summarise(total_linePTS = sum(linePTS), .groups = 'drop') %>%
  mutate(percentage = total_linePTS / sum(total_linePTS) * 100)

# In the paper Figure 9. Newspaper Reach and Position
p10 <- ggplot(press_simple, aes(x = reorder(medium, linePTS), y = linePTS, fill = Position)) +
  geom_col(position = "dodge", alpha = 0.9) +
  scale_fill_manual(values = c("Pro-Remain" = "#003883", "Pro-Leave" = "#E60000")) +
  labs(x = "Newspaper", y = "Reach") +
  theme_bw() +
  coord_flip() +
  guides(fill = guide_legend(title = "Position"))

ggsave("plot10.png", plot = p10)


# Map data ----------------------------------------------------------------


# Available at: 
#nhttps://data.london.gov.uk/dataset/statistical-gis-boundary-files-london?resource=9ba8c833-6370-4b11-abdc-314aa020d5e0
# Statistical GIS Boundary Files for London
# London boroughs boundaries
# These geometries are sourced from the London Data Store
# GeoJSON file
library(viridis)
library(geojsonio)
map <- geojson_read("https://skgrange.github.io/www/data/london_boroughs.json",  
                    what = "sp")

# For an alternative visualization more simple that does not include river Thames
map2 <- geojson_read("https://skgrange.github.io/www/data/london_sport.json", 
                     what = "sp") 

# Map of London
ggplot() +
  geom_polygon(data = map, aes( x = long, y = lat, group = group), 
               fill="#69b3a2", 
               color="white") +
  theme_void() +
  coord_map()

# Choropleth map with R
# Attribute the appropriate color to each borough
# Assuming there is a unique identifier, such as "ID", that matches between the datasets
map@data$Pct_Remain <- london$Pct_Remain[match(map@data$name, london$Area)]
map@data$Pct_Remain <- as.numeric(map@data$Pct_Remain)

# Distribution of the Remain vote by borough
map@data %>% 
  ggplot(aes(x=as.numeric(Pct_Remain))) + 
  geom_histogram(bins=20, fill='#69b3a2', color='white') +
  theme_bw()

library(sp)
library(maps)
# Simple map, just blue or red depending on whether Remain or Leave won
num_colors <- 7  # Number of colors in the palette
palette <- brewer.pal(num_colors, "RdBu")
colors <- ifelse(map@data$Pct_Remain < 50, "#E60000", "#003883")

plot(map, col = colors, main = "Brexit vote by Borough")
legend("bottomleft", legend = c("Remain", "Leave"), fill = c("#003883", "#E60000"), 
       title = "Color Legend")

# The map usef in the paper is Figure 6. Brexit Vote by Borough and it is more detailed
# Blue as a gradient: Darker blue districts had higher percentage of Remain vote
# Number of subgroups because brewer.pal only has 9 blues
num_subgroups <- 9  
subgroup_indices <- cut(map@data$Pct_Remain, num_subgroups, labels = FALSE)

# Generate a palette of blue colors with num_subgroups shades
blue_palette <- brewer.pal(num_subgroups, "Blues")

# The loop assigns different shades of blue to each borough
for (i in 1:num_subgroups) {
  subgroup_areas <- map@data$name[subgroup_indices == i]
  subgroup_color <- blue_palette[i]
  colors[map@data$name %in% subgroup_areas] <- subgroup_color
}

# There are some boroughs that which not only has a very low percentage of remain votes 
# but also won the other position, leave
# So we want to highlight this boroughs by assigning them the color of leace, red
leave_regions <- c("Barking and Dagenham", "Bexley", "Havering", "Hillingdon", "Sutton")
leave_color<- "#E60000"
colors[map@data$name %in% leave_regions] <- leave_color # Include the different 

# The final map has a gradient of blue for the boroughs in which Remain won and red for 
# the few in which Leave won 
plot(map, col = colors, main = "Brexit vote by borough")
legend(x = "bottomright", # Position
       inset = 0.001, # Distance from the margin as a fraction of the plot region
       legend = c("Remain", "Leave"), # Legend texts
       fill = c("#003883", "#E60000"), # Colors
       title = "Referendum Vote",
       bty = "n",
       )


# Simulation data ---------------------------------------------------------

# Data generated from the Gama ABM
library(moments)
## Small-world networks
# Remain prototype small-world
remain_small <- read_csv("~/gama_tfm/models/remain_batch_small_wordl.csv")
remain_small <- remain_small %>%
  mutate(
    n_leave_voters = str_replace_all(n_leave_voters, "\\[|\\]", ""),
    n_remain_voters = str_replace_all(n_remain_voters, "\\[|\\]", ""),
    n_undecided_voters = str_replace_all(n_undecided_voters, "\\[|\\]", "")
  )

# Calculate variance and kurtosis
# Make sure the columns are numeric
remain_small$n_leave_voters <- as.numeric(remain_small$n_leave_voters)
remain_small$n_remain_voters <- as.numeric(remain_small$n_remain_voters)
remain_small$n_undecided_voters <- as.numeric(remain_small$n_undecided_voters)

# Variance
var(remain_small[,-1])

# Kurtosis
kurtosis(remain_small[,-1])

# Skewness
skewness(remain_small[,-1])

remain_small %>%
  summarise(
    variance_leave = var(n_leave_voters),
    variance_remain = var(n_remain_voters),
    variance_undecided = var(n_undecided_voters),
    kurtosis_leave = kurtosis(n_leave_voters),
    kurtosis_remain = kurtosis(n_remain_voters),
    kurtosis_undecided = kurtosis(n_undecided_voters),
    skewness_leave = skewness(n_leave_voters),
    skewness_remain = skewness(n_remain_voters),
    skewness_undecided = skewness(n_undecided_voters)
  ) %>%
  gather(Measure, Value) %>% 
  kbl() %>%
  kable_classic(full_width = F, html_font = "Times New Roman")


boxplot(remain_small[, -1])

# Density plot
pivot_remain <- remain_small %>%
  pivot_longer(cols = starts_with("n_"), names_to = "voter", values_to = "value")

# Figure 22. Density Plot Model 1-Remain in the paper
p11 <- ggplot(data = pivot_remain, aes(x = value, group = voter, fill = voter)) +
  geom_density(adjust = 1.5, alpha = 0.4) +
  scale_fill_manual(values = c("#E60000","#003883", "yellow"),
                    labels = c("Leave","Remain", "Undecided")) +
  theme_bw()
ggsave("plot11.png", plot = p11)


# Leave prototype small-world
leave_small <- read_csv("~/gama_tfm/models/leave_batch_small_wordl.csv")
leave_small <- leave_small %>%
  mutate(
    n_leave_voters = str_replace_all(n_leave_voters, "\\[|\\]", ""),
    n_remain_voters = str_replace_all(n_remain_voters, "\\[|\\]", ""),
    n_undecided_voters = str_replace_all(n_undecided_voters, "\\[|\\]", "")
  )

leave_small$n_leave_voters <- as.numeric(leave_small$n_leave_voters)
leave_small$n_remain_voters <- as.numeric(leave_small$n_remain_voters)
leave_small$n_undecided_voters <- as.numeric(leave_small$n_undecided_voters)

leave_small %>%
  summarise(
    variance_leave = var(n_leave_voters),
    variance_remain = var(n_remain_voters),
    variance_undecided = var(n_undecided_voters),
    kurtosis_leave = kurtosis(n_leave_voters),
    kurtosis_remain = kurtosis(n_remain_voters),
    kurtosis_undecided = kurtosis(n_undecided_voters),
    skewness_leave = skewness(n_leave_voters),
    skewness_remain = skewness(n_remain_voters),
    skewness_undecided = skewness(n_undecided_voters)
  ) %>%
  gather(Measure, Value)

boxplot(leave_small[, -1])

# Density plot
pivot_leave <- leave_small %>%
  pivot_longer(cols = starts_with("n_"), names_to = "voter", values_to = "value")

# Figure 23. Density Plot Model 2-Leave in the paper
p12 <- ggplot(data = pivot_leave, aes(x = value, group = voter, fill = voter)) +
  geom_density(adjust = 1.5, alpha = 0.4) +
  scale_fill_manual(values = c("#E60000","#003883", "yellow"),
                    labels = c("Leave","Remain", "Undecided")) +
  theme_bw()
ggsave("plot12.png", plot = p12)


# Tie prototype small-world
tie_small <- read_csv("~/gama_tfm/models/tie_batch_small_wordl.csv")
tie_small <- tie_small %>%
  mutate(
    n_leave_voters = str_replace_all(n_leave_voters, "\\[|\\]", ""),
    n_remain_voters = str_replace_all(n_remain_voters, "\\[|\\]", ""),
    n_undecided_voters = str_replace_all(n_undecided_voters, "\\[|\\]", "")
  )

tie_small$n_leave_voters <- as.numeric(tie_small$n_leave_voters)
tie_small$n_remain_voters <- as.numeric(tie_small$n_remain_voters)
tie_small$n_undecided_voters <- as.numeric(tie_small$n_undecided_voters)

tie_small %>%
  summarise(
    variance_leave = var(n_leave_voters),
    variance_remain = var(n_remain_voters),
    variance_undecided = var(n_undecided_voters),
    kurtosis_leave = kurtosis(n_leave_voters),
    kurtosis_remain = kurtosis(n_remain_voters),
    kurtosis_undecided = kurtosis(n_undecided_voters),
    skewness_leave = skewness(n_leave_voters),
    skewness_remain = skewness(n_remain_voters),
    skewness_undecided = skewness(n_undecided_voters)
  ) %>%
  gather(Measure, Value)

boxplot(tie_small[, -1])

# Density plot
pivot_tie <- tie_small %>%
  pivot_longer(cols = starts_with("n_"), names_to = "voter", values_to = "value")

# Figure 24. Density Plot Model 3-Tie in the paper
p13 <- ggplot(data = pivot_tie, aes(x = value, group = voter, fill = voter)) +
  geom_density(adjust = 1.5, alpha = 0.4) +
  scale_fill_manual(values = c("#E60000","#003883", "yellow"),
                    labels = c("Leave","Remain", "Undecided")) +
  theme_bw()
ggsave("plot13.png", plot = p13)


## Comparing model scenarios
boxplot(remain_small[, -1], col = "#003883", ylab = "Values")
boxplot(leave_small[, -1], col = "#E60000", add = TRUE)
boxplot(tie_small[, -1], col = "#834ba0", add = TRUE)

# Combine data for a final table summarizing the information
simulation <- bind_rows(
  mutate(pivot_leave, model = "Leave"),
  mutate(pivot_remain, model = "Remain"),
  mutate(pivot_tie, model = "Tie")
)

# Table 5. Dispersion Measures in the paper
simulation %>%
  group_by(model) %>% 
  summarise(
    variance = round(var(value),2),
    standard_deviation =round(sd(value),2),
    kurtosis = round(kurtosis(value),2),
    skewness = round(skewness(value),2)
  ) %>%
  gather(Measure, Value) %>% 
  kbl() %>%
  kable_classic(full_width = F, html_font = "Times New Roman")



