library(EpiEstim)
library(ggplot2)
library(incidence)

setwd("/Users/claramal/Desktop/Moncla_Lab/H5-avian-ecology/metadata/detections_data/")

#data(Flu2009) #load data
#head(Flu2009$incidence) # view indicdence

#h5_detect = read.csv("./combinedNAm_detections_2021-2024_COUNTS.csv", header = TRUE, sep = ",")
h5_detect = read.csv("./combinedNAm_detections_2021-2024_COUNTS_fulldates.csv", header = TRUE, sep = ",")
anseriformes_detect = read.csv("./anseriformes_detect_fullcount.csv", header = TRUE, sep = ",")
shorebirds_detect = read.csv("./shorebirds_detect_fullcount.csv", header = TRUE, sep = ",")
raptors_detect = read.csv("./raptors_detect_fullcount.csv", header = TRUE, sep = ",")

## Step 1 - read in CSVs as dataframes
h5_detect = as.data.frame(h5_detect)
duck_detect = as.data.frame(anseriformes_detect)
gull_detect = as.data.frame(shorebirds_detect)
rapt_detect = as.data.frame(raptors_detect)

## Step 2 - set column names and date formats
colnames(h5_detect) <- c("dates", "I")
h5_detect$dates <- as.Date(h5_detect$dates, format = "%Y-%m-%d")

colnames(duck_detect) <- c("dates", "I")
duck_detect$dates <- as.Date(duck_detect$dates, format = "%Y-%m-%d")

colnames(gull_detect) <- c("dates", "I")
gull_detect$dates <- as.Date(gull_detect$dates, format = "%Y-%m-%d")

colnames(rapt_detect) <- c("dates", "I")
rapt_detect$dates <- as.Date(rapt_detect$dates, format = "%Y-%m-%d")

View(h5_detect)

# Optional - plot incidence (but you will also get this plot later)
#plot(as.incidence(Flu2009$incidence$I, dates = Flu2009$incidence$dates))
plot(as.incidence(h5_detect$I, dates = h5_detect$dates))

## Step 3 - This checks for any cases where there aren't detections logged every day
if (any(unique(diff(h5_detect$dates)) != 1)) {
  print(index)
}

if (any(unique(diff(duck_detect$dates)) != 1)) {
  print(index)
}

if (any(unique(diff(gull_detect$dates)) != 1)) {
  print(index)
}

if (any(unique(diff(rapt_detect$dates)) != 1)) {
  print(index)
}

## Step 4 - Set timings for serial intervals

# Note to self: seq(from = START, to = END, by = STEP)

T <- nrow(h5_detect)
print(T)
D <- nrow(duck_detect)
print(D)
G <- nrow(gull_detect)
print(G)
P <- nrow(rapt_detect)
print(P)

# Sliding monthly incidence windows starting 60 days after the onset of the outbreak and ending 30 days before the last day
# Sliding windows are far better if you're going to use EpiEstim's graphing method
t_start <- seq(60, T-30)
print(length(t_start))
t_end <- t_start + 29
print(length(t_end))

# Non-sliding monthly incidence windows, starting 30 days after outbreak onset, each lasts about a month
# Non-sliding monthly incidence windows are better if you're going plot R effective yourself
# All detections
t_start_2 <- seq(30, T-30, by = 30) # Set a sequences of start points every 30 days
print(length(t_start_2))
t_end_2 <- t_start_2 + 29 # Set an endpoint 29 days after the start point
print(length(t_end_2))

# Duckies
t_start_duck <- seq(30, D-30, by = 30)
print(length(t_start_duck))
t_end_duck <- t_start_duck + 29
print(length(t_end_duck))

# Shorebirds
t_start_gull <- seq(30, G-20, by = 30)
print(length(t_start_gull))
t_end_gull <- t_start_gull + 29
print(length(t_end_gull))

# Raptors/owls/falcons
t_start_rapt <- seq(30, D-20, by = 30)
print(length(t_start_rapt))
t_end_rapt <- t_start_rapt + 29
print(length(t_end_rapt))


## Step 5 - Generate an R effective plot

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

# Duckies
duck_config <- make_config(list(t_start = t_start_duck,
                                t_end = t_end_duck,
                                mean_si = 4, std_mean_si = 2, 
                                min_mean_si = 1, max_mean_si = 7,
                                std_si = 3, std_std_si = 1, 
                                min_std_si = 1, max_std_si = 5))
anseriformes_R <- estimate_R(duck_detect, method = "uncertain_si", config = config)

# Gulls
gull_config <- make_config(list(t_start = t_start_gull,
                                t_end = t_end_gull,
                                mean_si = 3, std_mean_si = 1, 
                                min_mean_si = 1, max_mean_si = 5,
                                std_si = 3, std_std_si = 1, 
                                min_std_si = 1, max_std_si = 5))
shorebird_R <- estimate_R(gull_detect, method = "uncertain_si", config = config)

# Raptors
rapt_config <- make_config(list(t_start = t_start_rapt,
                                t_end = t_end_rapt,
                                mean_si = 2, std_mean_si = 1, 
                                min_mean_si = 0, max_mean_si = 4,
                                std_si = 3, std_std_si = 1, 
                                min_std_si = 1, max_std_si = 5))
raptors_R <- estimate_R(rapt_detect, method = "uncertain_si", config = config)

## Step 6 - Plot R effective by hand (optional)

# Add months to the dataframes
nrow(h5_detect_allplots$R)
nrow(anseriformes_R$R)
nrow(shorebird_R$R)
nrow(raptors_R$R)

month_vector_37 <- c("December 2021",
  "January 2022", "February 2022", "March 2022", "April 2022", "May 2022", "June 2022",
  "July 2022", "August 2022", "September 2022", "October 2022", "November 2022", "December 2022",
  "January 2023", "February 2023", "March 2023", "April 2023", "May 2023", "June 2023",
  "July 2023", "August 2023", "September 2023", "October 2023", "November 2023", "December 2023",
  "January 2024", "February 2024", "March 2024", "April 2024", "May 2024", "June 2024",
  "July 2024", "August 2024", "September 2024", "October 2024", "November 2024", "December 2024"
)

month_vector_35 <- c(#"December 2021",
                  "January 2022", "February 2022", "March 2022", "April 2022", "May 2022", "June 2022",
                  "July 2022", "August 2022", "September 2022", "October 2022", "November 2022", "December 2022",
                  "January 2023", "February 2023", "March 2023", "April 2023", "May 2023", "June 2023",
                  "July 2023", "August 2023", "September 2023", "October 2023", "November 2023", "December 2023",
                  "January 2024", "February 2024", "March 2024", "April 2024", "May 2024", "June 2024",
                  "July 2024", "August 2024", "September 2024", "October 2024", "November 2024"#, "December 2024"
                  )
# I'm not sure which months are not included, but I think this is right

h5_detect_allplots$R$month <- month_vector_37
h5_detect_allplots$R$month <- as.Date(paste0("01 ", h5_detect_allplots$R$month), format = "%d %B %Y")

anseriformes_R$R$month <- month_vector_35
anseriformes_R$R$month <- as.Date(paste0("01 ", anseriformes_R$R$month), format = "%d %B %Y")

shorebird_R$R$month <- month_vector_35
shorebird_R$R$month <- as.Date(paste0("01 ", shorebird_R$R$month), format = "%d %B %Y")

raptors_R$R$month <- month_vector_35
raptors_R$R$month <- as.Date(paste0("01 ", raptors_R$R$month), format = "%d %B %Y")


# Plot
R_smoothed <- ggplot() + 
  geom_line(data = h5_detect_allplots$R, aes(x = month, y = `Median(R)`), color = "#2B57B3", linewidth = 1) + 
  geom_line(data = anseriformes_R$R, aes(x = month, y = `Median(R)`), color = "#5E9B82", linewidth = 1) + 
  geom_line(data = shorebird_R$R, aes(x = month, y = `Median(R)`), color = "#C593D1", linewidth = 1) + 
  geom_line(data = raptors_R$R, aes(x = month, y = `Median(R)`), color = "#AA7722", linewidth = 1) +
  
  geom_ribbon(data = h5_detect_allplots$R, aes(x = month, ymin = `Quantile.0.05(R)`, ymax = `Quantile.0.95(R)`), fill = "#2B57B3", alpha = 0.3) + 
  geom_ribbon(data = anseriformes_R$R, aes(x = month,ymin = `Quantile.0.05(R)`, ymax = `Quantile.0.95(R)`), fill = "#5E9B82", alpha = 0.3) + 
  geom_ribbon(data = shorebird_R$R, aes(x = month,ymin = `Quantile.0.05(R)`, ymax = `Quantile.0.95(R)`), fill = "#C593D1", alpha = 0.3) + 
  geom_ribbon(data = raptors_R$R, aes(x = month,ymin = `Quantile.0.05(R)`, ymax = `Quantile.0.95(R)`), fill = "#AA7722", alpha = 0.3) + 
  
  scale_color_manual(name='Wild Birds', breaks=c('All', 'Anseriformes', 'Shorebirds', 'Raptors'), values=c('All' = "#2B57B3", 'Anseriformes' = "#5E9B82", 'Shorebirds' = "#C593D1", 'Raptors' = "#AA7722")) +  
  
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") + 
  geom_vline(xintercept = as.Date(c("2022-03-20", "2022-06-20", "2022-09-20", "2022-12-20",
                            "2023-03-20", "2023-06-20", "2023-09-20", "2023-12-20",
                            "2024-03-20", "2024-06-20", "2024-09-20")),
             linetype = "dashed", color = "grey30", linewidth = 0.5) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

R_smoothed
