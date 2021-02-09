# Generate final exhibits
setwd("/Users/kevin/Dropbox/Users/Kevin-Mac-201808/Fracking studies")


library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(gridExtra)
library(ggpubr)
library(purrr)
library(tidyr)
# library(mapdata)

theme_set(theme_bw())
theme_update(
  text=element_text(size=8),
  plot.title=element_text(hjust=0.5),
  legend.title.align=0.5,
  axis.text=element_text(size=8)
)

L5_updated_master_table <- tbl_df(read.csv("8 Update Data/L5_updated_master_table.csv", 
                                           stringsAsFactors = FALSE, 
                                           colClasses = c("APINumber"="character"))) %>% select(-X)
 
# Only raw chemicals: not including supplier ingredients in Systems Approach
L5_chemicals <- L5_updated_master_table %>% filter(!SystemApproachFlag)

# Get form level database
L5_forms <- L5_chemicals %>%
  group_by(UploadKey) %>%
  mutate(WithholdingForm = sum(WithheldFlag) >= 1) %>%
  ungroup() %>%
  distinct(UploadKey, .keep_all=TRUE) %>%
  select(UploadKey, JobStartDate, JobEndDate, APINumber, StateNumber, CountyNumber, OperatorName, WellName,
         Latitude, Longitude, Projection, StateName, CountyName, FFVersion, LatitudeClean, LongitudeClean,
         StateNameFromCoords, StateAbbFromCoords, StateNumberFromCoords, CountyNameFromCoords, CountyCodeFromCoords,
         StateOK, CountyOK, FormUsedSystemApproach, SubmissionsAvailable, WithholdingForm, DownloadDate)


## Figure 1: Plot of withholding rate (WR), form-based, by time =======

dateFromJSD <- function (jsd) {
  strs <- word(jsd)
  # sep by /
  components <- strsplit(strs, "/")
  months = as.numeric(sapply(components, "[", 1))
  days = as.numeric(sapply(components, "[", 2))
  years = as.numeric(sapply(components, "[", 3))
  return (ISOdate(years, months, days))
}

L5_forms <- L5_forms %>%
  mutate(Date = dateFromJSD(JobStartDate))

by_quarter <- L5_forms %>%
  group_by(month=floor_date(Date, "quarter")) %>%
  summarize(NumWithholding = sum(WithholdingForm),
            NumForms = n()) %>%
  mutate(PctWithholding = NumWithholding / NumForms * 100)

by_quarter_SA <- L5_forms %>%
  filter(FormUsedSystemApproach) %>%
  group_by(month=floor_date(Date, "quarter")) %>%
  summarize(NumWithholding = sum(WithholdingForm),
            NumForms = n()) %>%
  mutate(PctWithholding = NumWithholding / NumForms * 100) %>%
  filter(NumForms >= 30)

by_quarter_traditional <- L5_forms %>%
  filter(!FormUsedSystemApproach) %>%
  group_by(month=floor_date(Date, "quarter")) %>%
  summarize(NumWithholding = sum(WithholdingForm),
            NumForms = n()) %>%
  mutate(PctWithholding = NumWithholding / NumForms * 100) %>%
  filter(NumForms >= 30)

# announced <- as_datetime("2015/02/26")
finished <- as_datetime("2016/04/12")    # news article 2016/04/12

# ggplot(by_quarter) + 
#   aes(x=month, y=PctWithholding) +
#   annotate("rect", xmin=announced, xmax=finished, ymin=-Inf, ymax=Inf, 
#            alpha=0.3, fill="lightblue") +
#   annotate("text", x=mean(c(announced, finished)), y=98, hjust=0.5, color="blue",
#            label="FracFocus 3.0\nannounced and\nrolled out", family="Times", size=3.5) +
#   geom_vline(xintercept=announced, color="blue") +
#   geom_vline(xintercept=finished, color="blue") +
#   geom_point(data=by_quarter_SA, mapping=aes(size=NumForms), color='red', alpha=0.75, shape=15) +
#   geom_line(data=by_quarter_SA, color='red', alpha=0.75) +
#   geom_point(data=by_quarter_traditional, mapping=aes(size=NumForms), color='darkgreen', alpha=0.75, shape=17) +
#   geom_line(data=by_quarter_traditional, color='darkgreen', alpha=0.75) +
#   geom_point(aes(size=NumForms)) +
#   geom_line() +
#   labs(x="Time", y="Withholding Rate (Pct of Forms)") +
#   annotate("text", x=as_datetime("2016/08/15"), y=73.5, label="Systems approach forms", hjust=0,
#            color="red", alpha=0.75, family="Times", size=3.5) +
#   annotate("text", x=as_datetime("2016/08/15"), y=93.5, label="\"Traditional\" forms", hjust=0,
#            color="darkgreen", alpha=0.75, family="Times", size=3.5) +
#   scale_size(breaks=c(1000, 3000, 5000, 7000), name="Quarterly Submissions") +
#   theme(legend.direction="horizontal",
#         legend.justification=c(0,0),
#         legend.position=c(0.05, 0.1),
#         legend.background=element_rect(fill=alpha("white", 0.9), color="grey")) +
#   ylim(50, 100)



# attempt to make legends clearer for shape

by_quarter_combined <- bind_rows(
  by_quarter_SA %>% mutate(FormType = "Systems app.", Alpha=0.75),
  by_quarter_traditional %>% mutate(FormType = "Traditional", Alpha=0.75),
  by_quarter %>% mutate(FormType = "All forms", Alpha=1)
)

ggplot(by_quarter_combined) + 
  aes(x=month, y=PctWithholding, color=FormType) +
  # annotate("rect", xmin=finished, xmax=as_datetime("2019/01/01"), ymin=-Inf, ymax=Inf, 
  #          alpha=0.3, fill="lightblue") +
  annotate("text", x=finished, y=1, hjust=0, vjust=-0.5,
           color="black", label="FracFocus 3.0 implemented", size=2.5, angle=90) +
  geom_vline(xintercept=finished, color="black", lty=2) +
  geom_line(aes(group=FormType)) +
  geom_point(aes(size=NumForms, shape=FormType)) +
  labs(x=NULL, y="Withholding Rate (Pct of Forms)") +
  # annotate("text", x=as_datetime("2016/08/15"), y=73.5, label="Systems approach forms", hjust=0,
  #          color="red", alpha=0.75, size=3.5) +
  # annotate("text", x=as_datetime("2016/08/15"), y=93.5, label="Traditional forms", hjust=0,
  #          color="darkgreen", alpha=0.75, size=3.5) +
  scale_color_manual(values=c("black", "red", "olivedrab"), 
                     breaks=c("All forms", "Traditional", "Systems app."),
                     name="  Form Type  ") +
  scale_shape_manual(values=c(16, 15, 17), 
                     breaks=c("All forms", "Traditional", "Systems app."),
                     name="  Form Type  ") +
  scale_size(breaks=c(1000, 4000, 7000), 
             range=c(0.5, 3),
             name="Forms") +
  theme(legend.justification=c(0,0),
        legend.position=c(0.03, 0.15),
        legend.background=element_rect(fill="grey97", color="grey"),
        legend.text=element_text(size=6), 
        legend.key.size = unit(0.2, "in"),
        legend.margin = margin(3, 3, 3, 3),
        legend.spacing = unit(0.1, "in"),
        legend.title=element_text(size=7)) +
  ylim(0, 100)



# ggsave("../Public Health Research/Environmental Health FracFocus Submission/Figure1.png",
#        width=85, height=95, units="mm")


## Figure 2: Flowchart. Won't generate actual figure, but the numbers are generated. ========

L5_FF2 <- L5_chemicals %>% filter(FFVersion == 2)
L5_FF3 <- L5_chemicals %>% filter(FFVersion == 3)

operators_FF2 <- L5_FF2 %>%
  group_by(UploadKey, OperatorName, FormUsedSystemApproach) %>%
  summarize(NumWithheld = sum(WithheldFlag),
            NumTotal = n(),
            WithheldOne = NumWithheld > 0) %>%
  ungroup() %>%
  group_by(OperatorName) %>%
  summarize(Num.FF2.Forms = n(),
            Num.FF2.SA.Forms = sum(FormUsedSystemApproach),
            Num.WithheldOne.FF2.SA = sum(WithheldOne & FormUsedSystemApproach),
            Num.WithheldOne.FF2 = sum(WithheldOne),
            Ingr.Withheld.FF2 = sum(NumWithheld),
            Ingr.Total.FF2 = sum(NumTotal)) %>%
  mutate(Ingr.WR.FF2 = Ingr.Withheld.FF2 / Ingr.Total.FF2 * 100,
         Num.WithheldOne.FF2.Trad = Num.WithheldOne.FF2 - Num.WithheldOne.FF2.SA)

operators_FF3 <- L5_FF3 %>%
  group_by(UploadKey, OperatorName, FormUsedSystemApproach) %>%
  summarize(NumWithheld = sum(WithheldFlag),
            NumTotal = n(),
            WithheldOne = NumWithheld > 0) %>%
  ungroup() %>%
  group_by(OperatorName) %>%
  summarize(Num.FF3.Forms = n(),
            Num.FF3.SA.Forms = sum(FormUsedSystemApproach),
            Num.WithheldOne.FF3.SA = sum(WithheldOne & FormUsedSystemApproach),
            Num.WithheldOne.FF3 = sum(WithheldOne),
            Ingr.Withheld.FF3 = sum(NumWithheld),
            Ingr.Total.FF3 = sum(NumTotal)) %>%
  mutate(Ingr.WR.FF3 = Ingr.Withheld.FF3 / Ingr.Total.FF3 * 100,
         Num.WithheldOne.FF3.Trad = Num.WithheldOne.FF3 - Num.WithheldOne.FF3.SA)

operators <- full_join(operators_FF2, operators_FF3, by="OperatorName") %>% 
  replace(., is.na(.), 0) %>%
  mutate(Num.FF2.Trad.Forms = Num.FF2.Forms - Num.FF2.SA.Forms,
         FF2.Pct.SA.Forms = Num.FF2.SA.Forms / Num.FF2.Forms * 100,
         FF2.Pct.Trad.Forms = 100 - FF2.Pct.SA.Forms,
         Num.FF3.Trad.Forms = Num.FF3.Forms - Num.FF3.SA.Forms,
         FF3.Pct.SA.Forms = Num.FF3.SA.Forms / Num.FF3.Forms * 100,
         FF3.Pct.Trad.Forms = 100 - FF3.Pct.SA.Forms,
         Num.Total.Forms = Num.FF2.Forms + Num.FF3.Forms,
         Change.In.Pct.SA = FF3.Pct.SA.Forms - FF2.Pct.SA.Forms) %>%
  arrange(desc(Num.Total.Forms))

operators_m_Pct <- operators %>%
  select(OperatorName, FF2.Pct.SA.Forms, FF3.Pct.SA.Forms) %>%
  rename(FF2 = FF2.Pct.SA.Forms, FF3 = FF3.Pct.SA.Forms) %>%
  melt(id="OperatorName", variable.name="FF.Version", value.name="PercentOfForms")
operators_m_Num <- operators %>%
  select(OperatorName, Num.FF2.Forms, Num.FF3.Forms) %>%
  rename(FF2 = Num.FF2.Forms, FF3 = Num.FF3.Forms) %>%
  melt(id="OperatorName", variable.name="FF.Version", value.name="NumForms")
operators_m <- tbl_df(full_join(operators_m_Pct, operators_m_Num, by=c("OperatorName", "FF.Version")))

# Weighted by size
# Use min percentile as weighting
FF2sizes <- sort(operators$Num.FF2.Forms)
FF3sizes <- sort(operators$Num.FF3.Forms)
operators <- operators %>%
  mutate(FF2.Size.Rank = match(Num.FF2.Forms, FF2sizes),
         FF3.Size.Rank = match(Num.FF3.Forms, FF3sizes),
         Min.Size.Rank = FF2.Size.Rank * (FF2.Size.Rank <= FF3.Size.Rank) +
           FF3.Size.Rank * (FF2.Size.Rank > FF3.Size.Rank),
         Weight.Coefficient = Min.Size.Rank / nrow(operators),
         Weighted.SA.Change = Change.In.Pct.SA * Weight.Coefficient)
mean(operators$Weighted.SA.Change[which(!is.na(operators$Weighted.SA.Change))])
# note: 28.81135 percentage-points increase in weighted SA usage

# Analyze corresponding changes in withholding rate
operators <- operators %>%
  # Change in total withholding rate from FF2 to FF3
  mutate(FF2.Pct.WithheldOne = Num.WithheldOne.FF2 / Num.FF2.Forms * 100,
         FF3.Pct.WithheldOne = Num.WithheldOne.FF3 / Num.FF3.Forms * 100,
         Change.In.Pct.WithheldOne = FF3.Pct.WithheldOne - FF2.Pct.WithheldOne) %>%
  # Change in SA form withholding rate from FF2 to FF3
  mutate(FF2.SA.Pct.WithheldOne = Num.WithheldOne.FF2.SA / Num.FF2.SA.Forms * 100,
         FF3.SA.Pct.WithheldOne = Num.WithheldOne.FF3.SA / Num.FF3.SA.Forms * 100,
         Change.In.SA.Pct.WithheldOne = FF3.SA.Pct.WithheldOne - FF2.SA.Pct.WithheldOne)

min_forms_in_each <- 5

top_operators <- operators %>%
  filter(Num.FF2.SA.Forms == 0) %>%
  filter(Num.FF2.Forms >= 5 & Num.FF3.Forms >= 5)

## Figure 2a ===========

# Still did not use SA in FF3
no_SA <- top_operators %>% filter(FF3.Pct.SA.Forms == 0)
x <- sum(no_SA$Num.WithheldOne.FF2)
n <- sum(no_SA$Num.FF2.Forms)           
x[2] <- sum(no_SA$Num.WithheldOne.FF3.Trad)
n[2] <- sum(no_SA$Num.FF3.Trad.Forms)   
prop.test(x=x, n=n)

# Partially used SA in FF3
some_SA <- top_operators %>% filter(FF3.Pct.SA.Forms > 0 & FF3.Pct.SA.Forms < 100)
sum(some_SA$Num.WithheldOne.FF2) / sum(some_SA$Num.FF2.Forms) 
sum(some_SA$Num.WithheldOne.FF3.SA) / sum(some_SA$Num.FF3.SA.Forms)
sum(some_SA$Num.WithheldOne.FF3.Trad) / sum(some_SA$Num.FF3.Trad.Forms)

# Converted completely to SA in FF3
all_SA <- top_operators %>% filter(FF3.Pct.SA.Forms == 100)
sum(all_SA$Num.WithheldOne.FF2) / sum(all_SA$Num.FF2.Forms)
sum(all_SA$Num.WithheldOne.FF3.SA) / sum(all_SA$Num.FF3.SA.Forms)


## Figure 2b Operators across FF2 and FF3, at least 5 forms in each version ===========

mean_among_no_SA <- mean(no_SA$Change.In.Pct.WithheldOne)


ggplot(top_operators) +    # should we overlay just the top 5% of operators above, in a different color?
  aes(x=Change.In.Pct.SA, y=Change.In.Pct.WithheldOne, color=Change.In.Pct.SA) +
  geom_hline(yintercept = 0, lty=2) +
  geom_vline(xintercept = 0, lty=2) +
  geom_point(alpha=0.2, size=4) +
  scale_color_gradient(low='blue', high='maroon') +
  #geom_smooth(method="lm") +
  geom_hline(yintercept=mean_among_no_SA, color="blue") +
  annotate("text", label="Mean line among\noperators without the\nsystems approach", hjust=0.5, vjust=0,
           color="blue", y=33, x=23, family="Times", size=3.5) +
  annotate("segment", x=6, y=mean_among_no_SA+1, xend=21, yend=31, 
           color="blue", size=0.25, linetype=1) +
  ylim(-100, 100) +
  labs(x="Operator Systems Approach Usage (% of FF 3.0 Forms)",
       y="Percentage-point Change in Withholding Rate\nFF 2.0 to FF 3.0") +
  guides(color=FALSE)



## Figure 3a ========

form_based_wrs <- L5_forms %>%
  group_by(FormUsedSystemApproach) %>%
  summarize(nWithholding = sum(WithholdingForm),
            n = n()) %>%
  mutate(WithholdingRate = nWithholding / n * 100,
         WRMethod = "form-based calculation")
ingr_based_wrs <- L5_chemicals %>%
  group_by(FormUsedSystemApproach) %>%
  summarize(nWithholding = sum(WithheldFlag),
            n = n()) %>%
  mutate(WithholdingRate = nWithholding / n * 100,
         WRMethod = "ingredient-based calculation")
wrs <- rbind(form_based_wrs, ingr_based_wrs) %>%
  group_by(FormUsedSystemApproach, WRMethod)

ggplot(wrs) +
  aes(x=WRMethod, y=WithholdingRate, fill=FormUsedSystemApproach) +
  geom_bar(stat="identity", position=position_dodge2(padding=0.2),
           width=0.7, color="black") +
  ylim(0, 100) +
  scale_fill_brewer(palette="Blues", labels=c("Traditional forms", "Systems approach forms"),
                    name="Submission Type") +
  theme(legend.justification=c(1,1),
        legend.position=c(0.9, 0.9),
        legend.background=element_rect(fill=alpha("white", 0.9), color="grey")) +
  labs(title="All Forms (FF 2.0 and FF 3.0)",
       x=NULL,
       y="Calculated Withholding Rate")

## Figure 3b; same as 3a but filtering to FF3.0 forms ======
form_based_wrs.FF3 <- L5_forms %>%
  filter(FFVersion == 3) %>%
  group_by(FormUsedSystemApproach) %>%
  summarize(nWithholding = sum(WithholdingForm),
            n = n()) %>%
  mutate(WithholdingRate = nWithholding / n * 100,
         WRMethod = "form-based calculation")
ingr_based_wrs.FF3 <- L5_chemicals %>%
  filter(FFVersion == 3) %>%
  group_by(FormUsedSystemApproach) %>%
  summarize(nWithholding = sum(WithheldFlag),
            n = n()) %>%
  mutate(WithholdingRate = nWithholding / n * 100,
         WRMethod = "ingredient-based calculation")
wrs.FF3 <- rbind(form_based_wrs.FF3, ingr_based_wrs.FF3) %>%
  group_by(FormUsedSystemApproach, WRMethod)

ggplot(wrs.FF3) +
  aes(x=WRMethod, y=WithholdingRate, fill=FormUsedSystemApproach) +
  geom_bar(stat="identity", position=position_dodge2(padding=0.2),
           width=0.7, color="black") +
  ylim(0, 100) +
  scale_fill_brewer(palette="Blues", labels=c("Traditional forms", "Systems approach forms"),
                    name="Submission Type") +
  theme(legend.justification=c(1,1),
        legend.position=c(0.9, 0.9),
        legend.background=element_rect(fill=alpha("white", 0.9), color="grey")) +
  labs(title="FF 3.0 Forms Only",
       x=NULL,
       y="Calculated Withholding Rate")


## Figure 3c Reasons for withholding =======

total = nrow(L5_chemicals)
CASLabels <- L5_chemicals %>%
  group_by(CASLabel) %>%
  summarize(NumIngrs = n()) %>%
  mutate(Pct = NumIngrs / total * 100) %>%
  filter(CASLabel != "Valid")
ggplot(CASLabels) + 
  aes(x=CASLabel, y=Pct) +
  geom_bar(stat="identity", color="black", fill="grey", width=0.8) +
  labs(x="Reason for Withholding", y="% of FF 3.0 Ingredients") +
  theme(axis.title.x=element_text(margin=margin(t=10,r=0,b=0,l=0)),
        axis.title.y=element_text(margin=margin(t=0,r=5,b=0,l=0)))





## Figure 4 state regulations =======

# Get lists of top states and operators
FF3_forms <- L5_forms %>% filter(FFVersion == 3)

operator_sizes <- FF3_forms %>%
  group_by(OperatorName) %>%
  summarize(NumForms = n(),
            NumStates = n_distinct(StateName)) %>%
  arrange(desc(NumForms))

state_sizes <- FF3_forms %>%
  group_by(StateName) %>%
  summarize(NumForms = n(),
            NumOperators = n_distinct(OperatorName)) %>%
  arrange(desc(NumForms))

top_operators_list <- operator_sizes[1:10,]$OperatorName
top_states_list <- state_sizes[1:10,]$StateName


# Get data sets; top states data set only includes operators in each state with >=10 forms

states_operators_wrs <- FF3_forms %>%
  group_by(StateName, OperatorName) %>%
  summarize(NumWithholding = sum(WithholdingForm),
            NumForms = n()) %>%
  mutate(WithholdingRate = NumWithholding / NumForms * 100) %>%
  arrange(desc(NumForms))



operators2display <- tibble(
  OperatorName=c("Cimarex Energy Co.",
                 "QEP Energy Company",
                 "EOG Resources, Inc.",
                 "ConocoPhillips Company/Burlington Resources",
                 "BP America Production Company",
                 "EnerVest, Ltd.",
                 "Chevron USA Inc.",
                 "XTO Energy/ExxonMobil",
                 "Anadarko Petroleum Corporation",
                 "Chesapeake Operating, Inc.",
                 "Marathon Oil ",
                 "Continental Resources, Inc",
                 "Occidental Oil and Gas",
                 "EP Energy",
                 "Pioneer Natural Resources"),
  DisplayName=c("Cimarex Energy",
                "QEP Energy",
                "EOG Resources",
                "ConocoPhillips/Burlington",
                "BP America",
                "EnerVest",
                "Chevron USA",
                "XTO Energy/ExxonMobil",
                "Anadarko Petroleum",
                "Chesapeake Operating",
                "Marathon Oil",
                "Continental Resources",
                "Occidental Oil and Gas",
                "EP Energy",
                "Pioneer Natural Resources")
)



# Get SA usage frequency for each of these top states and operators
total_SA_usage <- FF3_forms %>%
  group_by(StateName, OperatorName) %>%
  summarize(NumSAForms = sum(FormUsedSystemApproach),
            NumForms = n()) %>%
  ungroup()
states_SA_usage <- total_SA_usage %>%
  group_by(StateName) %>%
  summarize(NumSAForms = sum(NumSAForms),
            NumForms = sum(NumForms)) %>%
  mutate(PctSA = NumSAForms / NumForms * 100) %>%
  filter(StateName %in% top_states_list) %>%
  arrange(desc(PctSA)) %>%
  mutate(StateName = factor(StateName, levels=StateName))
operators_SA_usage <- total_SA_usage %>%
  group_by(OperatorName) %>%
  summarize(NumSAForms = sum(NumSAForms),
            NumForms = sum(NumForms)) %>%
  mutate(PctSA = NumSAForms / NumForms * 100) %>%
  filter(OperatorName %in% top_operators_list) %>%
  arrange(desc(PctSA)) %>%
  left_join(operators2display, by="OperatorName") %>%
  mutate(DisplayName = factor(DisplayName, levels=DisplayName))

# Plot these beneath the withholding rate graphs in order of decreasing SA usage
# fmt_dcimals <- function(decimals=0){
#   function(x) format(x,nsmall = decimals,scientific = FALSE)
# }

p2_states <- ggplot(states_SA_usage) +
  aes(x=StateName, y=PctSA) +
  geom_bar(stat="identity", fill="indianred", color="grey11", width=0.4) +
  # scale_y_continuous(labels=fmt_dcimals(2)) +
  labs(y="Pct Forms Using SA", x=NULL) +
  ylim(0, 100) +
  theme(axis.title.y=element_text(margin=margin(t=0,r=5,b=0,l=0)),
        axis.text.x=element_text(angle = 90, hjust = 1, vjust=0.5))

p2_operators <- ggplot(operators_SA_usage) +
  aes(x=DisplayName, y=PctSA) +
  geom_bar(stat="identity", fill="indianred", color="grey11", width=0.4) +
  # scale_y_continuous(labels=fmt_dcimals(2)) +
  labs(y="Pct Forms Using SA", x=NULL) +
  scale_x_discrete(labels = function(x) str_wrap(x, width=12)) +
  ylim(0, 100) +
  theme(
    # axis.title.y=element_text(margin=margin(t=0,r=5,b=0,l=0)),
    axis.title.y=element_blank(),
    axis.text.x=element_text(angle = 90, hjust = 1, vjust=0.5, size=6)
  )




states_operators_wrs.topStates <- states_operators_wrs %>%
  filter(StateName %in% top_states_list,
         NumForms >= 10) %>%
  ungroup() %>%
  mutate(StateName = factor(StateName, levels=states_SA_usage$StateName))

states_operators_wrs.topOps <- states_operators_wrs %>%
  filter(OperatorName %in% top_operators_list) %>%
  ungroup() %>%
  left_join(operators2display, by="OperatorName") %>%
  mutate(DisplayName = factor(DisplayName, levels=operators_SA_usage$DisplayName))





# Get standard error bars

se <- function(x) sqrt(var(x)/length(x))

gd.operators <- states_operators_wrs.topOps %>% 
  group_by(DisplayName) %>%
  summarize(NumWithholding = sum(NumWithholding),
            NumForms = sum(NumForms),
            FirstQuartile = quantile(WithholdingRate, 0.25),
            Median = quantile(WithholdingRate, 0.5),
            ThirdQuartile = quantile(WithholdingRate, 0.75)) %>%
  mutate(NumDisclosing = NumForms - NumWithholding) %>%
  mutate(Mean = NumWithholding / NumForms * 100,
         StdError = mapply(function (nw, nd) 100*se(c(rep(0, nd), rep(1, nw))),
                           NumWithholding,
                           NumDisclosing))

gd.states <- states_operators_wrs.topStates %>%
  group_by(StateName) %>%
  summarize(NumWithholding = sum(NumWithholding),
            NumForms = sum(NumForms),
            FirstQuartile = quantile(WithholdingRate, 0.25),
            Median = quantile(WithholdingRate, 0.5),
            ThirdQuartile = quantile(WithholdingRate, 0.75)) %>%
  mutate(NumDisclosing = NumForms - NumWithholding) %>%
  mutate(Mean = NumWithholding / NumForms * 100,
         StdError = mapply(function (nw, nd) 100*se(c(rep(0, nd), rep(1, nw))),
                           NumWithholding,
                           NumDisclosing))


# Make plots
ovr_wr <- sum(FF3_forms$WithholdingForm)/nrow(FF3_forms)*100

p1_operators <- ggplot(states_operators_wrs.topOps) +
  aes(x=DisplayName, y=WithholdingRate, size=NumForms) +
  geom_hline(yintercept=ovr_wr,
             color="darkred", lty=2) +
  geom_jitter(width=0.2, shape=1, color="grey37") +
  scale_size_continuous(name="Forms", range=c(0.5, 3), breaks=c(50, 500)) +
  scale_y_continuous(breaks=c(0,25,50,75,100), limits=c(-5,101)) +
  geom_errorbar(data=gd.operators, 
                aes(y=NULL, ymin=FirstQuartile, ymax=ThirdQuartile, size=NULL), 
                color="black", width=0.3, lwd=0.5) +
  geom_point(data=gd.operators, aes(y=Mean, size=NULL), color="black",
             shape=18, size=2.5) +
  annotate("text", x=0.75, y=74,
           vjust=1, hjust=0, color="darkred", size=2,
           label=paste0(round(ovr_wr,1), "% mean\nwithholding\nin FF 3.0")) +
  annotate("segment", x=0.95, y=76, xend=0.9, yend=ovr_wr-2,
           color="darkred", lwd=0.2, arrow=arrow(length=unit(0.05, "inches"))) +
  labs(x=NULL, y="Withholding Rate", 
       title="B",
       subtitle="Largest 10 Well Operators in FF 3.0") +
  guides(size=guide_legend(override.aes=list(linetype=0))) +
  theme(
    # axis.title.y=element_text(margin=margin(t=0,r=5,b=0,l=0)),
    axis.title.y=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    panel.border=element_rect(size=1),
    plot.title=element_text(hjust=-0.1, vjust=-3, size=12, face="bold"),
    plot.subtitle=element_text(hjust=0.5, size=9),
    legend.justification=c(0,0),
    legend.position=c(0.01, 0.01),
    legend.background=element_rect(fill="grey97", color="grey"),
    legend.text=element_text(size=5), 
    legend.key.size = unit(0.1, "in"),
    legend.margin = margin(1, 1, 1, 1),
    legend.title=element_text(size=6),
    legend.direction = "horizontal"
  )


p1_states <- ggplot(states_operators_wrs.topStates) +
  aes(x=StateName, y=WithholdingRate, size=NumForms) +
  geom_hline(yintercept=ovr_wr,
             color="darkred", lty=2) +
  geom_jitter(width=0.2, shape=1, color="grey37") +
  scale_size_continuous(name="Forms", range=c(0.5, 3), breaks=c(50, 500)) +
  scale_y_continuous(breaks=c(0,25,50,75,100), limits=c(-5,101)) +
  geom_errorbar(data=gd.states, 
                aes(y=NULL, ymin=FirstQuartile, ymax=ThirdQuartile, size=NULL),
                color="black", width=0.3, lwd=0.5) +
  geom_point(data=gd.states, aes(y=Mean, size=NULL), color="black", 
             shape=18, size=2.5) +
  annotate("text", x=4.1, y=65,
           vjust=1, hjust=0, color="darkred", size=2,
           label=paste0(round(ovr_wr,1), "% mean\nwithholding\nin FF 3.0")) +
  annotate("segment", x=4.6, y=67, xend=4.1, yend=ovr_wr-2,
           color="darkred", lwd=0.2, arrow=arrow(length=unit(0.05, "inches"))) +
  # annotate("text", x=10.2, y=-5, hjust=1, vjust=0.6, size=2.5,
  #          label="Mean and interquartile range shown") +
  labs(x=NULL, y="Withholding Rate", 
       title="A",
       subtitle="Largest 10 States in FF 3.0") +
  guides(size=guide_legend(override.aes=list(linetype=0))) +
  theme(axis.title.y=element_text(margin=margin(t=0,r=5,b=0,l=0)),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.border=element_rect(size=1),
        plot.title=element_text(hjust=-0.1, vjust=-3, size=12, face="bold"),
        plot.subtitle=element_text(hjust=0.5, size=9),
        legend.justification=c(0,0),
        legend.position=c(0.01, 0.01),
        legend.background=element_rect(fill="grey97", color="grey"),
        legend.text=element_text(size=5), 
        legend.key.size = unit(0.1, "in"),
        legend.margin = margin(1, 1, 1, 1),
        legend.title=element_text(size=6),
        legend.direction = "horizontal")


barplots <- ggarrange(p2_states, p2_operators, ncol=2, align="h")
jitterplots <- ggarrange(p1_states, p1_operators, ncol=2, align="h")
# combined <- grid.arrange(jitterplots, barplots, ncol=1)

combined <- ggarrange(jitterplots, barplots, nrow=2, align="v", heights=c(4,3))
combined

# ggarrange(p1_states, p1_operators, p2_states, p2_operators,
#           nrow=2, ncol=2, align="hv",
#           common.legend=T, legend="right")

# ggsave("Final Figures/Fig2V2.eps", plot=combined, width=7, height=5, units="in")
# ggsave("../Public Health Research/Environmental Health FracFocus Submission/Figure3.png",
#        plot=combined, width=170, height=122, units="mm")


## Pearson correlation coefficients for state WRs and SA usage ====

states_wrs_sa <- FF3_forms %>%
  group_by(StateName) %>%
  summarize(NumForms = n(),
            WithholdingRate = sum(WithholdingForm) / n(),
            SystemsApproachFreq = sum(FormUsedSystemApproach) / n()) %>%
  arrange(desc(NumForms)) %>%
  filter(StateName %in% top_states_list)
cor(states_wrs_sa$WithholdingRate, states_wrs_sa$SystemsApproachFreq)

ops_wrs_sa <- FF3_forms %>%
  group_by(OperatorName) %>%
  summarize(NumForms = n(),
            WithholdingRate = sum(WithholdingForm) / n(),
            SystemsApproachFreq = sum(FormUsedSystemApproach) / n()) %>%
  arrange(NumForms) %>% 
  filter(OperatorName %in% top_operators_list)
cor(ops_wrs_sa$WithholdingRate, ops_wrs_sa$SystemsApproachFreq)





## Figure 5: Chemicals disclosed before and after SA ========

# odds that a form will contain a specific chemical (e.g. Methanol or Benzene) 
# predictor is SA, with FF Version, state, and operator fixed effects

# Committee on Energy and Commerce Report on Chemicals Used in HF (Toxic chemicals identified):
# methanol
# ethylene glycol
# diesel           68476-34-6     37
# naphthalene      91-20-3        18513
# xylenes          1330-20-7      6188
# HCl
# Toluene          108-88-3       756
# ethylbenzene     100-41-4       1679
# diethanolamine   111-42-2       9985
# formaldehyde     50-00-0        13530
# sulfuric acid    7664-93-9      3569
# thiourea         62-56-6        1037
# benzyl chloride  100-44-7       3342
# cumene           98-82-8        69
# nitrilotriacetic acid    139-13-9   18
# dimethyl formamide       68-12-2    13131
# phenol           108-95-2       508
# benzene          71-43-2        22
# 
# 
# Most common:
# 1 NA             702301
# 2 7732-18-5      361874    water
# 3 14808-60-7     186897    sand - SiO2
# 4 67-56-1        140126    methanol
# 5 64742-47-8     113940    Hydrotreated light distillate (petroleum)
# 6 7647-01-0       91997    HCl
# 7 67-63-0         76377    2-propanol
# 8 7647-14-5       71390    NaCl
# 9 107-21-1        65344    Ethylene glycol
# 10 9000-30-0      56683    Guar gum



chemical_prevalence <- L5_chemicals %>%
  select(UploadKey, CASNumberClean) %>%
  group_by(UploadKey) %>%
  summarize(
    # Controls
    EntryError = sum(is.na(CASNumberClean)) > 0,
    Water = sum(CASNumberClean == "7732-18-5", na.rm=T) > 0,
    SiO2 = sum(CASNumberClean == "14808-60-7", na.rm=T) > 0,
    Petroleum = sum(CASNumberClean == "64742-47-8", na.rm=T) > 0,
    GuarGum = sum(CASNumberClean == "9000-30-0", na.rm=T) > 0,
    # Toxins
    Methanol = sum(CASNumberClean == "67-56-1", na.rm=T) > 0,
    EthyleneGlycol = sum(CASNumberClean == "107-21-1", na.rm=T) > 0,
    Diesel = sum(CASNumberClean == "68476-34-6", na.rm=T) > 0,
    Naphthalene = sum(CASNumberClean == "91-20-3", na.rm=T) > 0,
    Xylenes = sum(CASNumberClean == "1330-20-7", na.rm=T) > 0,
    HCl = sum(CASNumberClean == "7647-01-0", na.rm=T) > 0,
    Toluene = sum(CASNumberClean == "108-88-3", na.rm=T) > 0,
    Ethylbenzene = sum(CASNumberClean == "100-41-4", na.rm=T) > 0,
    Diethanolamine = sum(CASNumberClean == "111-42-2", na.rm=T) > 0,
    Formaldehyde = sum(CASNumberClean == "50-00-0", na.rm=T) > 0,
    SulfuricAcid = sum(CASNumberClean == "7664-93-9", na.rm=T) > 0,
    Thiourea = sum(CASNumberClean == "62-56-6", na.rm=T) > 0,
    BenzylChloride = sum(CASNumberClean == "100-44-7", na.rm=T) > 0,
    Cumene = sum(CASNumberClean == "98-82-8", na.rm=T) > 0,
    NitrilotriaceticAcid = sum(CASNumberClean == "139-13-9", na.rm=T) > 0,
    DimethylFormamide = sum(CASNumberClean == "68-12-2", na.rm=T) > 0,
    Phenol = sum(CASNumberClean == "108-95-2", na.rm=T) > 0,
    Benzene = sum(CASNumberClean == "71-43-2", na.rm=T) > 0)
logitObservations <- L5_forms %>%
  select(UploadKey, FormUsedSystemApproach, FFVersion, StateName, OperatorName) %>%
  mutate(FFVersion = factor(FFVersion),
         StateName = factor(StateName),
         OperatorName = factor(OperatorName)) %>%
  inner_join(chemical_prevalence, by="UploadKey")

# Each of the rows is one observation (one form). 
# Outcomes include whether they disclose various chemicals.
# Predictors are SA usage, FF version, State, and Operator.

# Before running the model, restrict to top 10 states and operators so that it doesn't take too long
top_operators_list
top_states_list

myLogitModel2 <- glm("Xylenes ~ FormUsedSystemApproach + FFVersion + StateName + OperatorName",
                    data = logitObservations %>% filter(StateName %in% top_states_list &
                                                          OperatorName %in% top_operators_list),
                    family = "binomial")

# pvals <- tibble(Outcome = character(0), SAEstimate = character(0),
                # SAPVal = character(0), FF3Estimate = character(0),
                # FF3PVal = character(0))


topstateslogit <- L5_FF3 %>%
  group_by(StateName) %>%
  summarize(n=n_distinct(UploadKey)) %>%
  filter(n >= 500) %>%
  pull(StateName)
topoperslogit <- L5_FF3 %>%
  group_by(OperatorName) %>%
  summarize(n=n_distinct(UploadKey)) %>%
  filter(n>=500) %>%
  pull(OperatorName)
filteredForms <- L5_forms %>% 
  filter(FFVersion == 3) %>% #, OperatorName %in% topoperslogit, StateName %in% topstateslogit) %>%
  pull(UploadKey)
filteredLogitObservations <- logitObservations %>% 
  filter(UploadKey %in% filteredForms)


pvals <- tibble()
for (chem in colnames(logitObservations)[6:ncol(logitObservations)]) {
  logitMod <- glm(paste0(chem, " ~ FormUsedSystemApproach + StateName + OperatorName"),
                  data = filteredLogitObservations,
                  family = "binomial")
  pvals <- pvals %>% bind_rows(c(Outcome = chem,
                                 SAEstimate = logitMod$coefficients["FormUsedSystemApproachTRUE"],
                                 SAPVal = summary(logitMod)$coefficients["FormUsedSystemApproachTRUE","Pr(>|z|)"]
                                 # FF3Estimate = logitMod$coefficients["FFVersion3"],
                                 # FF3PVal = summary(logitMod)$coefficients["FFVersion3","Pr(>|z|)"]
                                 ))
  print(chem)
}
for (c in 2:3) {
  pvals[,c] <- as.numeric(unlist(pvals[,c]))
}

SAcoefsplot <- ggplot(pvals) +
  aes(x=Outcome, y=SAEstimate.FormUsedSystemApproachTRUE) +
  geom_bar(stat="identity") +
  geom_text(aes(label=ifelse(SAPVal < 0.0001, ifelse(SAPVal < 0.00001, "***", "**"), ""))) +
  theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust=0.5))
SAcoefsplot

FFcoefsplot <- ggplot(pvals) +
  aes(x=Outcome, y=FF3Estimate.FFVersion3) +
  geom_bar(stat="identity") +
  geom_text(aes(label=ifelse(FF3PVal < 0.0001, ifelse(FF3PVal < 0.00001, "***", "**"), ""))) +
  theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5))
# FFcoefsplot
grid.arrange(SAcoefsplot, FFcoefsplot, ncol=1)





## Figure: map ===========

# Possible info: well locations, withholding well, SA well, FF3 well
# State withholding rates, state SA usage
# Also want to note the states that require FF, and restrict to submissions after effective date

statereqs <- tbl_df(read.csv("References/states-in-ff.csv", stringsAsFactors=F)) %>%
  mutate(EffectiveDate = as_datetime(as.character(strptime(EffectiveDate, "%b. %d, %Y")))) %>%
  mutate(ReqFF = str_trim(ReqFF) == "Yes")

states <- L5_forms %>% 
  filter(FFVersion == 3) %>%
  left_join(statereqs, by="StateAbbFromCoords") %>%
  mutate(RequiredLegally = Date > EffectiveDate & ReqFF) %>%
  filter(RequiredLegally | StateAbbFromCoords %in% c("LA", "OH", "WY")) %>%
  group_by(StateName) %>%
  summarize(WithholdingRate = sum(WithholdingForm) / n(),
            SystemsApproachRate = sum(FormUsedSystemApproach) / n(),
            NumForms = n()) %>%
  filter(NumForms >= 100) %>%
  mutate(region = tolower(StateName)) %>%
  right_join(tbl_df(map_data("state")), by = c("region"))

# ggplot(states) +
#   geom_polygon(aes(x=long, y=lat, group=group, fill=WithholdingRate)) +
#   coord_fixed(1.3) +
#   geom_point(data=filter(L5_forms, FFVersion == 3, StateOK, StateName != "Alaska"),
#              mapping=aes(x=LongitudeClean, y=LatitudeClean,
#                          color=FormUsedSystemApproach),
#              size=0.4, shape=2) +
#   scale_color_manual(values=c("green", "red"))

# Alternatively:

# for crosshatching:
source("https://raw.githubusercontent.com/imaddowzimet/drawcrosshatch/master/draw_crosshatch.R")

# Crosshatch on states that don't require disclosure on FF, but have > 100 FF3 forms:
# Louisiana, Ohio, Wyoming
crosshatched <- tbl_df(map_data("state")) %>%
  filter(region %in% c("louisiana", "ohio", "wyoming")) %>%
  group_by(group) %>%
  nest()
crosshatchlines <- map_df(crosshatched$data, draw.crosshatch, width=0.5, pattern="crosshatch")

ggplot(states) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=SystemsApproachRate)) +
  coord_fixed(1.3) +
  geom_segment(data=crosshatchlines, aes(x=x, y=y, xend=xend, yend=yend),
               inherit.aes=F, size=0.1, color="white") +
  geom_point(data=filter(L5_forms, FFVersion == 3, StateOK, StateName != "Alaska") %>% sample_frac(1),
             mapping=aes(x=LongitudeClean, y=LatitudeClean,
                         color=WithholdingForm),
             size=0.1, shape=17) +
  scale_color_manual(values=c("green2", "maroon1"), name=NULL, 
                     labels=c("Full chemical\ndisclosure", "Chemicals\nwithheld")) +
  scale_fill_continuous(name="State systems\napproach usage") +
  guides(colour = guide_legend(override.aes = list(size=2)),
         fill=guide_colorbar(barwidth=4, barheight=0.3)) +
  theme(legend.direction="horizontal",
        legend.title=element_text(vjust=0.8, size=7),
        legend.box="vertical", legend.box.spacing=unit(0,"mm"),
        legend.box.just=c(0,0),
        legend.justification=c(0,0),
        legend.position=c(0.02, 0.03),
        legend.background=element_rect(fill="grey97", color="grey"),
        # legend.spacing=unit(0,"mm"),
        axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank())


# ggsave("../Public Health Research/Environmental Health FracFocus Submission/Figure2.eps",
#        width=170, height=95, units="mm")




## Table describing FF Versions, SA usage, and Withholding rates =====

dispTbl <- L5_forms %>%
  group_by(FFVersion) %>%
  summarize(
    NumForms = n(),
    SAForms = sum(FormUsedSystemApproach),
    OvrPctWithholding = sum(WithholdingForm) / n() * 100,
    PctSAWithholding = sum(WithholdingForm & FormUsedSystemApproach) / sum(FormUsedSystemApproach) * 100,
    PctNonSAWithholding = sum(WithholdingForm & !FormUsedSystemApproach) / sum(!FormUsedSystemApproach) * 100
  )
dispTbl





## Hazardous chemical prevalence ======

# # Using House Report
# ggchems <- tbl_df(melt(chemical_prevalence, id.vars="UploadKey",
#                        variable.name="Chemical", value.name="Disclosed")) %>%
#   inner_join(L5_forms %>%
#                select(UploadKey, FormUsedSystemApproach, FFVersion, StateName, OperatorName) %>%
#                mutate(FFVersion = factor(FFVersion),
#                       StateName = factor(StateName),
#                       OperatorName = factor(OperatorName)), 
#              by="UploadKey") %>%
#   filter(FFVersion == "3") %>%
#   group_by(Chemical, FormUsedSystemApproach) %>%
#   summarize(NumForms = n(), 
#             NumDisclosingChemical = sum(Disclosed)) %>%
#   mutate(PctDisclosingChemical = NumDisclosingChemical / NumForms * 100)
# ggplot(ggchems) +
#   aes(x=Chemical, y=PctDisclosingChemical, fill=FormUsedSystemApproach) +
#   geom_bar(stat="identity", position="dodge") +
#   theme(axis.text.x=element_text(angle=90,hjust=1, vjust=0.5))



# Using EPA report + Yost et al 2016:
hazardchems <- tbl_df(read.csv("hazardous_chemicals.csv", stringsAsFactors = F))
# ff2denom <- sum(L5_forms$FFVersion == 2)
# ff3denom <- sum(L5_forms$FFVersion == 3)
SAdenom <- sum(FF3_forms$FormUsedSystemApproach)
traddenom <- sum(!FF3_forms$FormUsedSystemApproach)
fullhaztable <- L5_chemicals %>%
  filter(CASNumberClean %in% hazardchems$CAS.Number) %>%
  filter(FFVersion==3) %>%
  group_by(CASNumberClean, FormUsedSystemApproach) %>%
  summarize(FormsWithChem = n_distinct(UploadKey)) %>%
  mutate(PctWithChem = FormsWithChem / ifelse(FormUsedSystemApproach, SAdenom, traddenom) * 100) %>% 
  dcast(CASNumberClean ~ paste0("SA_", FormUsedSystemApproach), value.var = "PctWithChem") %>%
  mutate(SA_Overall = (SA_FALSE*traddenom + SA_TRUE*SAdenom) / (traddenom+SAdenom)) %>%
  right_join(hazardchems, by=c("CASNumberClean" = "CAS.Number")) %>%
  tbl_df()
fullhaztable
write.csv(fullhaztable, "fullhaztable.csv", row.names = F)

