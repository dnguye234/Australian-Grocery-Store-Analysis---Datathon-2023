# Load dataframe
sales_df <- read.csv("~/Downloads/sales_data_2017_2018_for_tableau_with_new_date_columns.csv", stringsAsFactors = FALSE)

# Load libraries
library(dplyr)
library(stringr)
library(ggplot2)
library(scales)
library(forcats)

# How does the year 2017 compare to the year 2018? Were there any significant changes in purchasing habits of the customers? 
ggplot(data = sales_df) +
  geom_col(aes(x = quantity, 
               y = main_category,
               fill = year)) #overall proportion
# Fresh Produce outsell all other categories

sales_17_df <- sales_df %>%
  filter(year == 2017) %>% 
  group_by(main_category) %>%
  summarize(total_item_17 = sum(quantity))

sales_18_df <- sales_df %>%
  filter(year == 2018) %>%
  group_by(main_category) %>%
  summarize(total_item_18 = sum(quantity))

compare_df <- left_join(sales_17_df, sales_18_df)

ggplot(compare_df) +
  geom_segment(aes(x=main_category, xend=main_category, y=total_item_17, yend=total_item_18), color="black") +
  geom_point(aes(x = main_category, y = total_item_17, color = "2017"), size = 3 ) +
  geom_point(aes(x = main_category, y = total_item_18, color = "2018"), size = 3 ) +
  coord_flip() +
  scale_color_manual(values = c("#B970BB", "#70EBAE"),
                     guide = guide_legend(), 
                     name = "Year") + 
  labs(title = "Items sold in Main Category",
       caption = "Comparision of quantity sold in 2017 vs 2018", 
       x = "Category", 
       y = "Units sold") +
  scale_y_continuous(breaks = seq(0, 160000, 20000)) + 
  theme(legend.position = "bottom")
# Fresh produce clearly outsell all other categories

compare_df2 <- compare_df[-7, ] #Delete fresh produce to analyze others

ggplot(compare_df2) +
  geom_segment(aes(x=main_category, xend=main_category, y=total_item_17, yend=total_item_18), color="black") +
  geom_point(aes(x = main_category, y = total_item_17, color = "2017"), size = 3 ) +
  geom_point(aes(x = main_category, y = total_item_18, color = "2018"), size = 3 ) +
  scale_color_manual(values = c("#B970BB", "#70EBAE"),
                     guide = guide_legend(), 
                     name = "Year") + 
  coord_flip() + 
  labs(title = "Items sold in Main Category*",
       caption = "*excluding the Fresh Produce category for clearer comparision",
       x = "Category", 
       y = "Units sold") +
  scale_y_continuous(breaks = seq(0, 16000, 2000)) + 
  theme(legend.position = "bottom")

# 2018 sells more pantry, DCE, bag, flower, beverage
# 2017 sells more beverages, fresh produce, bakery, snacks
# There is only one category of beverage, while there is multiple for beverages

misc <- sales_df %>%
  filter(main_category == "Miscellaneous")
# Miscellaneous items were balms and candles, have only 9 sales recorded. All sales in 2018 were Siddhalepa balms (Sri Lankan product, relieve body aches), and all sales in 2017 were candles. Sales were mostly made toward the end of the week (Friday & Weekends). Both are considered stress-relieved type of product. 

------------------------------------------------------------------------------------------
  # Trend in main vs sub category
  # Hypothesis - items with most variety have the most sales - make the most profit
  main_to_sub_df_17 <- sales_df %>%
  filter(year == 2017) %>%
  group_by(main_category) %>%
  summarise(num_cat = paste(unique(sub_category), collapse = ", ")) %>%
  mutate(num_category = str_count(num_cat, ", ") + 1) %>%
  mutate(year = 2017)

main_to_sub_profit_17 <- sales_df %>%
  filter(year == 2017) %>%
  group_by(main_category) %>%
  summarize(total_profit= sum(total_profit))

main_to_sub_df_17 <- left_join(main_to_sub_df_17, main_to_sub_profit_17)

# For 2018
main_to_sub_df_18 <- sales_df %>%
  filter(year == 2018) %>%
  group_by(main_category) %>%
  summarise(num_cat = paste(unique(sub_category), collapse = ", ")) %>%
  mutate(num_category = str_count(num_cat, ", ") + 1) %>%
  mutate(year = 2018)

main_to_sub_profit_18 <- sales_df %>%
  filter(year == 2018) %>%
  group_by(main_category) %>%
  summarize(total_profit = sum(total_profit))

main_to_sub_df_18 <- left_join(main_to_sub_df_18, main_to_sub_profit_18)

main_to_sub_df <- rbind(main_to_sub_df_17, main_to_sub_df_18, by = "main_category")
main_to_sub_df <- main_to_sub_df[-21, ]

main_to_sub_df %>% 
  mutate(main_category = fct_reorder(main_category, as.numeric(num_category))) %>%
  ggplot(aes(x = main_category, y = as.numeric(num_category), fill = year)) +
  geom_bar(stat = "identity", position = "dodge", alpha = .6, width = .4) +
  coord_flip() +
  scale_y_continuous(labels = label_number_si()) +
  labs(title = "Variety for Each Category",
       x = "product category",
       y = "number of types") +
  theme_bw() 
# Ranking of category of sales stayed the same throughout 2017 - 2018
main_to_sub_df %>% 
  mutate(main_category = fct_reorder(main_category, as.numeric(total_profit))) %>%
  ggplot(aes(x = main_category, y = as.numeric(total_profit), fill = year)) +
  geom_bar(stat = "identity", position = "dodge", alpha = .6, width = .4) +
  coord_flip() +
  scale_y_continuous(labels = label_number_si()) +
  labs(title = "Profit for Each Category",
       x = "product category",
       y = "total profit") +
  theme_bw() 
# Should probably stop selling miscellaneous item, or expanding the variety for miscellaneous items. ALso, why separate beverage from beverages
# Correlation exists between product with more variety and more profit
# There was a significant change in total profit for Fresh Produce. Want to see if time is a factor
hour_17 <- sales_df %>%
  filter(year == 2017) 

hour_18 <- sales_df %>%
  filter(year == 2018) 

ggplot(data = hour_17) +
  geom_histogram(aes(x = hour), fill = "#6E20A4") +
  facet_wrap(~main_category)
ggplot(data = hour_18) +
  geom_histogram(aes(x = hour), fill = "#33FFDD") +
  facet_wrap(~main_category)

# Peak buying is still 


