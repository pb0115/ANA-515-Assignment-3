#step 1: Reading the dataset
storm_events_df <- read.csv("C:\\Users\\Dell\\Downloads\\StormEvents_details-ftp_v1.0_d2011_c20230417.csv")


# Step 2: Limit the dataframe to selected columns
storm_events_df <- storm_events_df[, c("BEGIN_YEARMONTH", "EPISODE_ID", "STATE", "STATE_FIPS", "CZ_NAME", "CZ_TYPE", "CZ_FIPS", "EVENT_TYPE")]

# Step 3: Arrange the data by state name
storm_events_df <- storm_events_df[order(storm_events_df$STATE), ]

# Step 4: Change state and county names to title case
storm_events_df$STATE <- tolower(storm_events_df$STATE)
storm_events_df$STATE <- tools::toTitleCase(storm_events_df$STATE)
storm_events_df$CZ_NAME <- tolower(storm_events_df$CZ_NAME)
storm_events_df$CZ_NAME <- tools::toTitleCase(storm_events_df$CZ_NAME)

# Step 5: Limit to events listed by county FIPS and remove CZ_TYPE column
storm_events_df <- storm_events_df[storm_events_df$CZ_TYPE == "C", ]
storm_events_df$CZ_TYPE <- NULL

# Step 6: Pad state and county FIPS with "0" and unite them into a single FIPS column
storm_events_df$STATE_FIPS <- sprintf("%02d", storm_events_df$STATE_FIPS)
storm_events_df$CZ_FIPS <- sprintf("%03d", storm_events_df$CZ_FIPS)
storm_events_df$FIPS <- paste0(storm_events_df$STATE_FIPS, storm_events_df$CZ_FIPS)

# Step 7: Change all column names to lower case
colnames(storm_events_df) <- tolower(colnames(storm_events_df))

# Step 8: Create a dataframe with state name, area, and region
state_info <- data.frame(state.name = state.name, area = state.x77[, "Area"], region = state.region)

# Step 9: Create a dataframe with the number of events per state in 2011 and merge with state_info
events_2011 <- storm_events_df[grepl("^2011", storm_events_df$begin_yearmonth), ]
events_per_state <- aggregate(event_type ~ state, data = events_2011, FUN = length)
merged_data <- merge(events_per_state, state_info, by.x = "state", by.y = "state.name")
merged_data <- merged_data[complete.cases(merged_data), ]

# Step 10: Create a plot
library(ggplot2)
plot <- ggplot(merged_data, aes(x = region, y = event_type, color = region)) +
  geom_point(aes(fill = region), shape = 21, size = 4) +
  labs(x = "Region", y = "Number of Events", title = "Number of Events per Region in 2011")

# Customize color palette
colors <- c("red", "blue", "green", "orange", "purple")
plot + scale_fill_manual(values = colors)

# Display the plot
print(plot)
