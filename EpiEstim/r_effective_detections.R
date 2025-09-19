library(EpiEstim)
library(ggplot2)
library(incidence)

setwd("/Users/claramal/Desktop/Moncla_Lab/H5-avian-ecology/metadata/detections_data/")

#data(Flu2009) #load data
#head(Flu2009$incidence) # view indicdence

#h5_detect = read.csv("./combinedNAm_detections_2021-2024_COUNTS.csv", header = TRUE, sep = ",")
h5_detect = read.csv("./combinedNAm_detections_2021-2024_COUNTS_fulldates.csv", header = TRUE, sep = ",")
# This dataset is fundamentally flawed bc it has non-avians from Canada

h5_detect = as.data.frame(h5_detect)
colnames(h5_detect) <- c("dates", "I")
h5_detect$dates <- as.Date(h5_detect$dates, format = "%Y-%m-%d")

View(h5_detect)

#plot(as.incidence(Flu2009$incidence$I, dates = Flu2009$incidence$dates))
plot(as.incidence(h5_detect$I, dates = h5_detect$dates))

# This checks for any cases where there aren't detections logged every day
if (any(unique(diff(h5_detect$dates)) != 1)) {
  print(index)
}

# Sliding monthly incidence windows starting 60 days after the onset of the outbreak and ending 30 days before the last day
# seq(from = START, to = END, by = STEP)
T <- nrow(h5_detect)
t_start <- seq(60, T-30)
print(length(t_start))
t_end <- t_start + 29
print(length(t_end))

# What if I don't want sliding windows...
t_start_2 <- seq(30, T-15, by = 30) # Set a sequences of start points every 30 days
print(length(t_start_2))
t_end_2 <- t_start_2 + 29 # Set an endpoint 29 days after the start point
print(length(t_end_2))

# Ok I do want sliding windows I think

# Set the config for a range of serial intervals
config <- make_config(list(t_start = t_start_2,
                           t_end = t_end_2,
                           mean_si = 4, std_mean_si = 2, 
                           min_mean_si = 1, max_mean_si = 7,
                           std_si = 3, std_std_si = 1, 
                           min_std_si = 1, max_std_si = 5))
# Play with these numbers a bit

h5_detect_allplots <- estimate_R(h5_detect, 
                                 #method = "parametric_si", 
                                 method = "uncertain_si",
                                 #config = make_config(list(mean_si = 2.6, std_si = 1.5))
                                 config = config)

R <- plot(h5_detect_allplots, "R", legend = FALSE) # use `type = "R"`, `type = "incid"` or `type = "SI"` to generate only one of the 3 plots
R

# I wonder if I could plot h5_detect_allplots$R by myself...
# Use median R or mean R?
# How to plot over time?
# Set continuous x-axis with xlim?

# Add months to the dataframe
nrow(h5_detect_allplots$R)
h5_detect_allplots$R$month <- c("December 2021",
                                   "January 2022", "February 2022", "March 2022", "April 2022", "May 2022", "June 2022",
                                   "July 2022", "August 2022", "September 2022", "October 2022", "November 2022", "December 2022",
                                   "January 2023", "February 2023", "March 2023", "April 2023", "May 2023", "June 2023",
                                   "July 2023", "August 2023", "September 2023", "October 2023", "November 2023", "December 2023",
                                   "January 2024", "February 2024", "March 2024", "April 2024", "May 2024", "June 2024",
                                   "July 2024", "August 2024", "September 2024", "October 2024", "November 2024", "December 2024"
                                   )
h5_detect_allplots$R$month <- as.Date(paste0("01 ", h5_detect_allplots$R$month), format = "%d %B %Y")

# Plot
R_smoothed <- ggplot(h5_detect_allplots$R, aes(x = month)) + 
  geom_line(aes(y = `Median(R)`), color = "#2B57B3", linewidth = 1) + 
  geom_ribbon(aes(ymin = `Quantile.0.05(R)`, ymax = `Quantile.0.95(R)`), fill = "#2B57B3", alpha = 0.3) + 
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") + 
  geom_vline(xintercept = as.Date(c("2022-03-20", "2022-06-20", "2022-09-20", "2022-12-20",
                            "2023-03-20", "2023-06-20", "2023-09-20", "2023-12-20",
                            "2024-03-20", "2024-06-20", "2024-09-20")),
             linetype = "dashed", color = "grey30", linewidth = 0.5) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

R_smoothed
