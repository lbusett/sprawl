
in_folder ="//10.0.1.252/nr_working/luigi/docs/Ermes/comparison_new_meteo/tests_rh/"

in_file_it = paste0(in_folder, 'it_out_risk.csv')
in_data = read.csv(in_file_it, header = T)
in_data$date = as.Date(lb_doytodate(in_data$doy, in_data$year))
#
# in_data$pot_infections_Interimv5[in_data$date < as.Date('2014-06-30')] = 0
# in_data$pot_infections_Interimv5[in_data$date > as.Date('2015-08-15')] = 0
# in_data$pot_infections_Interimv5[in_data$date > as.Date('2014-08-15') & in_data$date < as.Date('2015-06-30')] = 0

# in_datats_it = xts(data.frame(date = in_data$date, risk = in_data$pot_infections_Interimv5),in_data$date )
in_datats_it = data.frame(date = in_data$date, risk = in_data$pot_infections_Interimv5)
in_datats_it$country = 'IT'
weekly_data_it = apply.weekly(in_datats_it, mean)
plot(weekly_data_it)
plot(cumsum(weekly_data_it))

# Spain ----
in_folder ="//10.0.1.252/nr_working/luigi/docs/Ermes/comparison_new_meteo/tests_rh/"

in_file_es = paste0(in_folder, 'es_out_risk.csv')
in_data = read.csv(in_file_es, header = T)
in_data$date = as.Date(lb_doytodate(in_data$doy, in_data$year))
in_data = in_data [1:720,]
# in_data$pot_infections_Interimv5[in_data$date < as.Date('2014-06-15')] = 0
# in_data$pot_infections_Interimv5[in_data$date > as.Date('2015-08-30')] = 0
# in_data$pot_infections_Interimv5[in_data$date > as.Date('2014-08-30') & in_data$date < as.Date('2015-06-15')] = 0
in_datats_es = data.frame(date = in_data$date, risk = in_data$pot_infections_Interimv5)
in_datats_es$country = 'ES'
in_datats_es$Risk = as.numeric(as.character(in_datats_es$risk                                            ))
Date = as.Date(as.character(in_datats_es$date))
in_datats_es = xts(in_datats_es,order.by = Date)
weekly_data_es = apply.weekly(in_datats_es, mean)
plot(weekly_data_es)
plot(cumsum(in_datats_es))

# Greece ----
in_folder ="//10.0.1.252/nr_working/luigi/docs/Ermes/comparison_new_meteo/tests_rh/"

in_file_es = paste0(in_folder, 'gr_out_risk.csv')
in_data = read.csv(in_file_es, header = T)
in_data$date = as.Date(lb_doytodate(in_data$doy, in_data$year))
in_data = in_data [1:720,]
# in_data$pot_infections_Interimv5[in_data$date < as.Date('2014-06-30')] = 0
# in_data$pot_infections_Interimv5[in_data$date > as.Date('2015-08-15')] = 0
# in_data$pot_infections_Interimv5[in_data$date > as.Date('2014-08-15') & in_data$date < as.Date('2015-06-30')] = 0
in_datats_gr = data.frame(date = in_data$date, risk = in_data$pot_infections_Interimv5)
in_datats_gr$country = 'GR'
weekly_data_gr = apply.weekly(in_datats_gr, mean)
plot(weekly_data_gr)
plot(cumsum(weekly_data_gr))

in_datats = rbind(in_datats_it, in_datats_es,in_datats_gr)
in_datats$risk[in_datats$date < as.Date('2014-06-30')] = 0
in_datats$risk[in_datats$date > as.Date('2015-08-30')] = 0
in_datats$risk[in_datats$date > as.Date('2014-08-30') & in_datats$date < as.Date('2015-06-30')] = 0

in_datats$year = as.factor(year(in_datats$date))
in_datats$date [year(in_datats$date) == 2014] = in_datats$date [year(in_datats$date) == 2014]+365

p = ggplot(in_datats, aes(x = date, y = risk, color = country, group = country) ) + theme_bw()
p = p + geom_line()+scale_x_date(limits = c(as.Date('2014-05-01'),as.Date('2015-09-01')),date_breaks = "1 month", date_labels = "%m/%y")+facet_grid(country~year)
p

prots = xts(in_datats, in_datats$date)

proweek = ddply(in_datats, .(country, year), summarize, cum = cumsum(risk))
proweek$date= in_datats$date
p = ggplot(proweek, aes(x = date, y = cum))
p = p + geom_line(aes(color = year))+facet_wrap(~country)+scale_x_date(limits = c(as.Date('2015-06-01'),as.Date('2015-09-01')),date_breaks = "1 month", date_labels = "%m/%y")
p + theme_bw()

head(proweek)

plot(proweek)
