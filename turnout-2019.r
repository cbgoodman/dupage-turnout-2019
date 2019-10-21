library(tidyverse)
library(readxl)

# Set working directory -- remove for final distro
setwd("~/Dropbox/Personal/website/dupage-turnout-2019")

# Read in data
turnout <- read_excel("Official Results April 2, 2019.xlsx")

# Data cleaning
turnout.clean <- distinct(turnout,Precinct, .keep_all= TRUE)
turnout.clean <- select(turnout.clean,-c(District, Contest, Candidate_Name,
    Write_In, Proposition, Candidate_Party, Candidate_Grand_Total, Votes,
    Number_to_Vote_For, Contest_Total_Ballots, Contest_Total_Registered_Voters,
    Number_of_Candidates))
attach(turnout.clean)

# Calculate precint level turnout data
turnout.clean$precinct.turnout <- Precinct_Contest_Total_Ballots/Precinct_Contest_Registered_Voters
turnout.clean$precinct.turnout[turnout.clean$precinct.turnout=="Inf"] <- NA
turnout.clean$precinct.turnout[turnout.clean$precinct.turnout=="NaN"] <- NA

# Initial Histogram
turnout <- ggplot(turnout.clean) +
  aes(x = precinct.turnout) +
  geom_histogram(binwidth=0.01, color = "white", fill = "#E69F00", na.rm=TRUE) +
  scale_x_continuous(breaks = seq(0,1, by = 0.1)) +
  scale_y_continuous(breaks = seq(0,60, by = 10)) +
  geom_vline(aes(xintercept = median(turnout.clean$precinct.turnout, na.rm=TRUE)),
            color = "#2B2B2B", linetype = "dashed", size = 0.5, alpha =0.75) +
  annotate("text", x = 0.125, y = 45, label = "Median Precinct Turnout, 11.7%",
    hjust = 0, size=3.5, family="Open Sans Condensed Light") +
  # Theming
  labs(
    title="Voter turnout in municipal elections in DuPage County, IL is low",
    subtitle="Precinct-level voter turnout, April 2, 2019 DuPage County, IL Consolidated General Election, n=923",
    caption="Author: Chris Goodman (@cbgoodman), Data: DuPage County (IL) Elections, https://www.dupageco.org/Election/.",
    y="Count",
    x="Turnout") +
  theme_minimal(base_family="Open Sans Condensed Light") +
  theme(
    legend.position = "bottom",
    legend.title.align = 0.5,
    legend.text.align = 0) +
  # light, dotted major y-grid lines only
  theme(panel.grid=element_line())+
  theme(panel.grid.major.y=element_line(color="#2b2b2b", linetype="dotted", size=0.15))+
  theme(panel.grid.major.x=element_blank())+
  theme(panel.grid.minor.x=element_blank())+
  theme(panel.grid.minor.y=element_blank())+
  # light x-axis line only
  theme(axis.line=element_line())+
  theme(axis.line.y=element_blank())+
  theme(axis.line.x=element_blank())+
  # tick styling
  theme(axis.ticks=element_line())+
  theme(axis.ticks.x=element_blank())+
  theme(axis.ticks.y=element_blank())+
  theme(axis.ticks.length=unit(5, "pt"))+
  # x-axis labels
  theme(axis.text.x=element_text(size=10, hjust=0.95,vjust=0.2))+
  # breathing room for the plot
  theme(plot.margin=unit(rep(0.5, 4), "cm"))+
  # move the y-axis tick labels over a bit
  theme(axis.text.y=element_text(margin=margin(r=-5)))+
  theme(axis.text.x=element_text(margin=margin(r=-5)))+
  # make the plot title bold and modify the bottom margin a bit
  theme(plot.title=element_text(family="Open Sans Condensed Bold", margin=margin(b=15)))+
  # make the subtitle italic
  theme(plot.subtitle=element_text(family="Open Sans Condensed Light Italic"))+
  theme(plot.caption=element_text(size=8, hjust=0, margin=margin(t=15)))
ggsave(plot = turnout, "turnout.png", width=10, height=8, units="in", dpi="retina")
