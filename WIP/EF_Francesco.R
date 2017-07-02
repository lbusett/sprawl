require(tidyverse)
require(broom)
require(hydroGOF)

#   ____________________________________________________________________________
#   load the data --> Cycle on days                                         ####

inta <- get(load("/home/lb/Temp/buttami/fra/ta.Rdata"))
ints <- get(load("/home/lb/Temp/buttami/fra/ts.Rdata"))
innd <- get(load("/home/lb/Temp/buttami/fra/indata_ok$NDVI.Rdata"))

#   ____________________________________________________________________________
#   reshape the data and remove incomplete cases                            ####

pro           <- stack(inta, ints, innd)
indata        <- as_tibble(getValues(pro))
names(indata) <- c("Ta", "Ts", "NDVI")
dove_ok       <- which(complete.cases(indata) == TRUE)
indata_ok     <- indata[dove_ok,] %>%
  mutate(pix_num = dove_ok) %>%
  filter(Ts > -5)



#   ____________________________________________________________________________
#   Assign classes                                                          ####

ndvi_min <- quantile(indata_ok$NDVI, c(.005))[1]           # Minimum value NDVI (15% quantile)
ndvi_max <- max(indata_ok$NDVI, na.rm = TRUE)                                           # Maximum value NDVI

classLeft  <- ndvi_min
indata_ok  <- subset(indata_ok, NDVI >= classLeft)

classNum            <- 100 # Set the number of class (Tang et al., 2010)
classRange          <- (ndvi_max - ndvi_min) / classNum # Compute the class range
class_lims          <- c(ndvi_min + (classRange * (seq_len(classNum) - 1)),1)
indata_ok$NDVI_cut  <- cut(indata_ok$NDVI, class_lims, include.lowest = TRUE, ordered_result = TRUE)

classNum2              <- 20  # Set the number of class (Tang et al., 2010)
classRange2            <- (ndvi_max - ndvi_min) / classNum2  # Compute the class range
class_lims2            <- c(ndvi_min + (classRange2 * (seq_len(classNum2) - 1)),1)
class_centers          <- class_lims2 +  classRange2/2
indata_ok$NDVI_cut2    <- cut(indata_ok$NDVI, class_lims2, include.lowest = TRUE, ordered_result = TRUE)

#   ____________________________________________________________________________
#   cycle to remove "outliers"                                              ####


indata_summ <- indata_ok    %>%
  dplyr::group_by(NDVI_cut) %>%
  dplyr::summarise(max_Dt = max(Ts - Ta, na.rm = TRUE),
            NDVI_cut2 = first(NDVI_cut2))

#   ____________________________________________________________________________
#   cycle to remove "outliers"                                              ####


for (i in 1:10) {

  print(length(which(is.na(indata_summ$max_Dt)) == T))

indata_summ2 <- indata_summ %>%
  dplyr::select(max_Dt, NDVI_cut, NDVI_cut2) %>%
  dplyr::arrange(NDVI_cut2, NDVI_cut) %>%
  dplyr::group_by(NDVI_cut2) %>%
  dplyr::summarize(avg = mean(max_Dt, na.rm = T),
            std = sd(max_Dt, na.rm = T))


indata_summ <- indata_summ %>%
  dplyr::left_join(indata_summ2) %>%
  dplyr::mutate(check_dt = (max_Dt >= avg - std)) %>%
  dplyr::mutate(max_Dt = max_Dt * ifelse(check_dt, 1, NA)) %>%
  dplyr::select(max_Dt, NDVI_cut, NDVI_cut2) %>%
  dplyr::arrange(NDVI_cut2, NDVI_cut)


}

indata_final <- indata_summ %>%
  dplyr::group_by(NDVI_cut2) %>%
  dplyr::summarize(avg_dt = mean(max_Dt, na.rm = T)) %>%
  dplyr::mutate(NDVI_center = class_centers[1:20])


for (i in 1:5) {
regress      <-  lm(indata_final$avg_dt~indata_final$NDVI_center)
RMSE         <-  hydroGOF::gof(regress$fitted.values, indata_final$avg_dt)["RMSE",1]
indata_final <- indata_final %>%
  mutate(fitted = as.numeric(regress$fitted.values)) %>%
  mutate(delta = (avg_dt - fitted)) %>%
  mutate(check_delta = delta < -2*RMSE) %>%
  filter(check_delta == FALSE) %>%
  select(NDVI_cut2, avg_dt, NDVI_center)
}

final_regr <- tidy(regress)

intc <- final_regr$estimate[1]
slp  <- final_regr$estimate[2]


#   ____________________________________________________________________________
#   store results                                                           ####

# per ogni giorno, salva intc, slp, day + calcola EF e salva.
# out_ef <-  list()
# out_ef [[day]] = data.frame(day = day, EF = pippo)
#


#   ____________________________________________________________________________
#   Plot comparison

#
#
# p = ggplot() +
#   geom_point(aes(x = indata_ok$NDVI, y = (indata_ok$Ts - indata_ok$Ta)), size = 0.5, alpha = 0.5) + xlim(0,1) + ylim(0,20)
# p = p + geom_abline(intercept = intc, slope = slp, color = 'red')
# p = p + geom_abline(intercept = 14.75, slope = -10.27, color = 'blue')
#
# p + theme_bw()



