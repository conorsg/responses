#!/usr/bin/Rscript

library(ggplot2)
library(plyr)

#load data
master <- read.csv(file="data.csv", sep=",", header=T)

#use only citizen generated calls
master <- master[master$SelfInitiated == "N", ]

#convert to time
master$TimeCreate <- as.POSIXct(master$TimeCreate, format="%d-%m-%Y %H:%M:%S")
master$TimeDispatch <- as.POSIXct(master$TimeDispatch, format="%d-%m-%Y %H:%M:%S")
master$TimeArrival <- as.POSIXct(master$TimeArrival, format="%d-%m-%Y %H:%M:%S")
master$TimeClosed <- as.POSIXct(master$TimeClosed, format="%d-%m-%Y %H:%M:%S")

#calculate time differences
master$TimeToDispatch <- difftime(master$TimeDispatch, master$TimeCreate, units="mins")
master$TimeToArrive <- difftime(master$TimeArrival, master$TimeDispatch, units="mins")

#list o' NOPD districts in dataset
districts <- c(levels(as.factor(master$District)))

#crime categories
PropTypes <- c("67P", "55", "67F", "67A", "67", "56", "62", "67S", "62C", "62R", "67B", "56D", "62B", "65", "67E", "67C", "65P", "65J")
ViolTypes <- c("35D", "35", "34", "34S", "37", "34D", "34C", "37D", "60", "38D")
HomicideTypes <- c("30", "30S", "30C", "30D")
RapeTypes <- c("42", "43")
GunTypes <- c("64G", "94", "95", "95G")

#signal priorities
PriorityZero <- c("0A", "0D", "0E", "0G", "0T")
PriorityOne <- c("1A", "1B", "1C", "1D", "1E", "1F", "1G", "1H", "1S")
PriorityTwo <- c("2A", "2B", "2C", "2D", "2E", "2F", "2H")
PriorityThree <-c("3A")

#assign crime type fn
assignType <- function(x) {
  ifelse(x %in% PropTypes,  "property",
    ifelse(x %in% ViolTypes, "violent",
      ifelse(x %in% HomicideTypes, "homicide",
        ifelse(x %in% RapeTypes, "rape",
          ifelse(x %in% GunTypes, "gun", "other")
        )
      )
    )
  )
}

#assign priority group fn
assignGroup <- function(x) {
  ifelse(x %in% PriorityZero, "zero",
    ifelse(x %in% PriorityOne, "one",
      ifelse(x %in% PriorityTwo, "two",
        ifelse(x %in% PriorityThree, "three", "error")
      )
    )
  )
}

#find signal priority, crime type, and NOPD district
master$PGroup <- lapply(master$Priority, assignGroup)
master$CrimeType <- lapply(master$Type, assignType)
master$District <- substring(as.character(master$Beat), 1, 1)

#type coercions for plots
master$PGroup <- as.character(master$PGroup)
master$CrimeType <- as.character(master$CrimeType)
master$District <-as.character(master$District)

#handy subsets of dispatch and arrival times without NAs and neg values
arrival_df <- master[master$TimeToArrive >0 & !is.na(master$TimeToArrive), ]
dispatch_df <- master[master$TimeToDispatch >0 & !is.na(master$TimeToDispatch), ]

#time to arrival density plots
arrv_signal_dist <- ggplot(arrival_df, aes(x = as.numeric(TimeToArrive), fill = PGroup)) +
                     geom_density(alpha = .3) +
                     geom_vline(xintercept = 5.73, linetype = "dashed", size = .6) +
                     scale_x_continuous(limits = c(0, 30), name = "Minutes between dispatch and arrival at scene") +
                     scale_fill_discrete(name = "Signal priority", breaks = c("zero", "one", "two")) +
                     geom_hline(yintercept = 0, colour = "#999999", size = .6) +
                     geom_vline(xintercept = 0, colour = "#999999", size = .6) +
                     ggsave("arrv-signal-dist.png", width = 12, height = 8)

arrv_crime_dist <- ggplot(arrival_df, aes(x=as.numeric(TimeToArrive), colour = CrimeType, fill = CrimeType)) +
                    geom_density(alpha = .2) +
                    scale_x_continuous(limits = c(0, 50), name = "Minutes between dispatch and arrival at scene") +
                    scale_colour_discrete(name = "Crime type") +
                    scale_fill_discrete(guide = F) +
                    geom_hline(yintercept = 0, colour = "#999999", size = .6) +
                    geom_vline(xintercept = 0, colour = "#999999", size = .6) +
                    ggsave("arrv-crime-dist.png", width = 12, height = 8)

arrv_district_dist <- ggplot(arrival_df, aes(x=as.numeric(TimeToArrive), colour = District, fill = District)) +
                       geom_density(alpha = .2) +
                       scale_x_continuous(limits = c(0, 50), name = "Minutes between dispatch and arrival at scene") +
                       scale_colour_discrete(name = "Police District") +
                       scale_fill_discrete(guide = F) +
                       geom_hline(yintercept = 0, colour = "#999999", size = .6) +
                       geom_vline(xintercept = 0, colour = "#999999", size = .6) +
                       ggsave("arrv-district-dist.png", width = 12, height = 8)

#time to dispatch density plots
disp_signal_dist <- ggplot(dispatch_df, aes(x = as.numeric(TimeToDispatch), colour = PGroup)) +
                    geom_density(alpha = .3) +
                    geom_vline(xintercept = 1.85, linetype = "dashed", size = .6) +
                    scale_x_continuous(limits = c(0, 50), name = "Minutes between call and officer dispatch") +
                    scale_colour_discrete(name = "Signal priority", breaks= c("zero", "one", "two")) +
                    geom_hline(yintercept = 0, colour = "#999999", size = .6) +
                    geom_vline(xintercept = 0, colour = "#999999", size = .6) +
                    ggsave("disp-signal-dist.png", width = 12, height = 8)

disp_crime_dist <- ggplot(dispatch_df, aes(x=as.numeric(TimeToDispatch), colour = CrimeType, fill = CrimeType)) +
                   geom_density(alpha = .2) +
                   scale_x_continuous(limits = c(0, 20), name = "Minutes between call and officer dispatch") +
                   scale_colour_discrete(name = "Crime type") +
                   scale_fill_discrete(guide = F) +
                   geom_hline(yintercept = 0, colour = "#999999", size = .6) +
                   geom_vline(xintercept = 0, colour = "#999999", size = .6) +
                   ggsave("disp-crime-dist.png", width = 12, height = 8)

disp_district_dist <- ggplot(dispatch_df, aes(x=as.numeric(TimeToDispatch), colour = District, fill = District)) +
                      geom_density(alpha = .2) +
                      scale_x_continuous(limits = c(0, 20), name = "Minutes between call and officer dispatch") +
                      scale_colour_discrete(name = "Police District") +
                      scale_fill_discrete(guide = F) +
                      geom_hline(yintercept = 0, colour = "#999999", size = .6) +
                      geom_vline(xintercept = 0, colour = "#999999", size = .6) +
                      ggsave("disp-district-dist.png", width = 12, height = 8)

#high priority signal benchmark comparison graph
ideal <- data.frame(x=rnorm(10000000, mean = 1.85))

benchmark_comp <- ggplot(top_priority_calls, aes(x = as.numeric(TimeToDispatch), fill = "red")) +
                  scale_x_continuous(limits = c(0, 20), name = "Minutes between call and officer dispatch") +
                  scale_fill_discrete(name = "", labels = c("Benchmark", "NOPD")) +
                  geom_density(alpha = .3) +
                  geom_density(data = ideal, mapping = aes(x = x, fill = "blue"), alpha = .3) +
                  geom_hline(yintercept = 0, colour = "#999999", size = .6) +
                  geom_vline(xintercept = 0, colour = "#999999", size = .6) +
                  ggsave("signal-dispatch-comp.png", width = 12, height = 8)

#benchmark analysis
fn_disp <- ecdf(dispatch_df[dispatch_df$PGroup == "two", ]$TimeToDispatch)
fn_arriv <- ecdf(arrival_df[arrival_df$PGroup == "two", ]$TimeToArrive)

fn_disp(1.85)
fn_arriv(5.73)

#summary tables
priority_summary_stats <- ddply(master, "as.character(PGroup)", summarise,
                          TimeToDispatch.median = median(TimeToDispatch[TimeToDispatch > 0], na.rm = T),
                          TimeToDispatch.mean = mean(TimeToDispatch[TimeToDispatch > 0], na.rm = T),
                          TimeToDispatch.n = sum(!is.na(TimeToDispatch[TimeToDispatch > 0])),
                          TimeToDispatch.sd = sd(TimeToDispatch[TimeToDispatch > 0], na.rm = T),
                          TimeToArrive.median = median(TimeToArrive[TimeToArrive > 0], na.rm = T),
                          TimeToArrive.mean = mean(TimeToArrive[TimeToArrive > 0], na.rm = T),
                          TimeToArrive.n = sum(!is.na(TimeToArrive[TimeToArrive > 0])),
                          TimeToArrive.sd = sd(TimeToArrive[TimeToArrive > 0], na.rm = T)
                          )

crime_summary_stats <- ddply(master, "as.character(CrimeType)", summarise,
                        TimeToDispatch.median = median(TimeToDispatch[TimeToDispatch > 0], na.rm = T),
                        TimeToDispatch.mean = mean(TimeToDispatch[TimeToDispatch > 0], na.rm = T),
                        TimeToDispatch.n = sum(!is.na(TimeToDispatch[TimeToDispatch > 0])),
                        TimeToDispatch.sd = sd(TimeToDispatch[TimeToDispatch > 0], na.rm = T),
                        TimeToArrive.median = median(TimeToArrive[TimeToArrive > 0], na.rm = T),
                        TimeToArrive.mean = mean(TimeToArrive[TimeToArrive > 0], na.rm = T),
                        TimeToArrive.n = sum(!is.na(TimeToArrive[TimeToArrive > 0])),
                        TimeToArrive.sd = sd(TimeToArrive[TimeToArrive > 0], na.rm = T)
                        )

district_summary_stats <- ddply(master, "as.character(District)", summarise,
                          TimeToDispatch.median = median(TimeToDispatch[TimeToDispatch > 0], na.rm = T),
                          TimeToDispatch.mean = mean(TimeToDispatch[TimeToDispatch > 0], na.rm = T),
                          TimeToDispatch.90 = quantile(TimeToDispatch[TimeToDispatch > 0], .9, na.rm = T),
                          TimeToDispatch.n = sum(!is.na(TimeToDispatch[TimeToDispatch > 0])),
                          TimeToDispatch.sd = sd(TimeToDispatch[TimeToDispatch > 0], na.rm = T),
                          TimeToArrive.median = median(TimeToArrive[TimeToArrive > 0], na.rm = T),
                          TimeToArrive.mean = mean(TimeToArrive[TimeToArrive > 0], na.rm = T),
                          TimeToArrive.90 = quantile(TimeToArrive[TimeToArrive > 0], .9, na.rm = T),
                          TimeToArrive.n = sum(!is.na(TimeToArrive[TimeToArrive > 0])),
                          TimeToArrive.sd = sd(TimeToArrive[TimeToArrive > 0], na.rm = T)
                          )

district_high_priority_summary_stats <- ddply(master[master$PGroup == "two", ], "as.character(District)", summarise,
                                        TimeToDispatch.median = median(TimeToDispatch[TimeToDispatch > 0], na.rm = T),
                                        TimeToDispatch.mean = mean(TimeToDispatch[TimeToDispatch > 0], na.rm = T),
                                        TimeToDispatch.90 = quantile(TimeToDispatch[TimeToDispatch > 0], .9, na.rm = T),
                                        TimeToDispatch.n = sum(!is.na(TimeToDispatch[TimeToDispatch > 0])),
                                        TimeToDispatch.sd = sd(TimeToDispatch[TimeToDispatch > 0], na.rm = T),
                                        TimeToArrive.median = median(TimeToArrive[TimeToArrive > 0], na.rm = T),
                                        TimeToArrive.mean = mean(TimeToArrive[TimeToArrive > 0], na.rm = T),
                                        TimeToArrive.90 = quantile(TimeToArrive[TimeToArrive > 0], .9, na.rm = T),
                                        TimeToArrive.n = sum(!is.na(TimeToArrive[TimeToArrive > 0])),
                                        TimeToArrive.sd = sd(TimeToArrive[TimeToArrive > 0], na.rm = T)
                                        )

write.csv(priority_summary_stats, "by-signal.csv", row.names = F)
write.csv(crime_summary_stats, "by-crime.csv", row.names = F)
write.csv(district_summary_stats, "by-district.csv", row.names = F)
write.csv(district_high_priority_summary_stats, "by-district-high-priority.csv", row.names = F)

#shim for fligner test
shim_disp <- data.frame(dispatch_df$District, dispatch_df$TimeToDispatch)
shim_arriv <- data.frame(arrival_df$District, arrival_df$TimeToArrive)

fligner_results_disp <- fligner.test(dispatch_df.TimeToDispatch ~ dispatch_df.District, data = shim_disp)
fligner_results_arriv <- fligner.test(arrival_df.TimeToArrive ~ arrival_df.District, data = shim_arriv)

if(fligner_results_disp$p.value < .05) {
  cat("Dispatch times between districts are heteroskedastic")
} else cat("Dispatch times between districts are homoskedastic")

if(fligner_results_arriv$p.value < .05) {
  cat("Arrival times between districts are heteroskedastic")
} else cat("Arrival times between districts are homoskedastic")

#k-s test to find samples with similar shape
get_each_district <- function(x, df, column) {
    subset(df, District == x, select = column)
}

disp_times_district <- sapply(districts, get_each_district, df = dispatch_df, column = "TimeToDispatch") #creates a recursive list. sub-lists are named. access like so: disp_times_district["1.TimeToDispatch"]
arriv_times_district <- sapply(districts, get_each_district, df = arrival_df, column = "TimeToArrive")

#verbose way of creating crosstable ks tests. probably a shortcut in plyr
crosstable_ks <- function(input, result) {
  result <- numeric()

  for (n in districts[districts != "N"]) {
    col <- numeric()
    for (i in districts[districts != "N"]) {
      col <- append(col, ks.test(as.numeric(input[[as.numeric(n)]]), as.numeric(input[[as.numeric(i)]]))$p.value)
      print(col)
    }
    result <- cbind(result, col)
  }

  colnames(result) <- districts[districts != "N"]
  result
}

ks_dispatch <- crosstable_ks(disp_times_district, ks_dispatch)
ks_arrival <- crosstable_ks(arriv_times_district, ks_arrival)

#find which districts have similar shapes
district_cluster_disp <- which(ks_dispatch > .05, arr.ind = T)
district_cluster_arriv <- which(ks_arrival > .05, arr.ind = T)

#panel charts of distributions
f_base <- ggplot(dispatch_df, aes(x=as.numeric(TimeToDispatch))) +
          geom_density(alpha = .3) +
          scale_x_continuous(limits = c(0, 20), name = "Minutes between call and officer dispatch")

f_base + facet_grid(District ~ .)

f_disp_1.5 <- ggplot(dispatch_df[dispatch_df$District == "1" | dispatch_df$District == "5", ], aes(x=as.numeric(TimeToDispatch))) +
          geom_density(alpha = .3) +
          scale_x_continuous(limits = c(0, 20), name = "Minutes between call and officer dispatch")

f_disp_1.5 + facet_grid(District ~ .)

f_disp_8.2 <- ggplot(dispatch_df[dispatch_df$District == "8" | dispatch_df$District == "2", ], aes(x=as.numeric(TimeToDispatch))) +
          geom_density(alpha = .3) +
          scale_x_continuous(limits = c(0, 20), name = "Minutes between call and officer dispatch")

f_disp_8.2 + facet_grid(District ~ .)
