library(dplyr)
library (hash)
library(raster)
library(sprawl)
library(caret)
library(plyr)
library(reshape2)
library(gridExtra)


in_classrast = "/home/lb/projects/ermes/datasets/rs_products/RICE_map/2016/ITA/early_maps/Early_Rice_Map_IT_V2/asc+desc_MRA"     # input classified raster
in_shape = "/home/lb/Temp/Land_Cover_ERMES_AllData_07_07_14_asciutta.shp"        # input shapefile of ground truths
in_shape_grnd_field = "RICE"

out_shape = "/home/lb/Temp/Accuracy_2016_ALL_points_total.shp"

hash_rast = hash( "1" = "Rice",     # Hash table of raster: value to class
              "3" = "Rice",
              "4" = "Rice",
              "2" = "NoRice",
              "0" = "Unclassified")

hash_shape = hash("Rice" = "Rice",     # Hash table of shape: value to class
              "NoRIce" = "NoRice",
              "NoRice" = "NoRice",
              "Unknown" = "Unknown",
              "Unclassified" = "Unclassified")


acc_results = lb_class_accuracy_points(in_classrast, in_shape , in_shape_grnd_field , hash_rast , hash_shape, out_shape = out_shape)


in_multitemps = c("/home/lb/projects/ermes/datasets/rs_products/2016_S2/T15ascVV_mar2016jul_smoothed_dB_bsq",  #Asc vv
                  "/home/lb/projects/ermes/datasets/rs_products/2016_S2/T15ascVH_mar2016jul_smoothed_dB_bsq",   #Asc vh
                  "/home/lb/projects/ermes/datasets/rs_products/2016_S2/T66descVV_mar2016jun_smoothed_dB_bsq",  # desc vv
                  "/home/lb/projects/ermes/datasets/rs_products/2016_S2/T66descVH_mar2016jun_smoothed_dB_bsq") # desc vh

types = c("ASCENDING-VV", "ASCENDING-VH", "DESCENDING-VV", " DESCENDING-VH")
list_plots_scatter = list()
list_plots_scatter2 = list()
list_plots_scatter3 = list()
list_plots_box = list()
list_plots_box_asciutt = list()


for(in_multitemp in seq(along = in_multitemps)) {

  inrast = stack(in_multitemps[in_multitemp])
  dates = sapply(names(inrast), function(x) strsplit(x, "X")[[1]][2])
  b = as.Date(dates, format = "%Y%m%d")
  inrast = setZ(inrast, b)
  stats = lb_fastzonal(in_rast = inrast, sp_object = out_shape, id_field = "un_id", out_format = "dframe", verbose = TRUE)
  stats_melted = melt(stats, id.vars = 'date')
  names(stats_melted)[2] = "un_id"

  cumstats = colSums(stats[,2:2467])
  cumstatsdf = as.data.frame(cumstats)
  cumstatsdf$un_id = names(cumstats)

  cumstats1 = colSums(stats[1,2:2467])
  cumstats1df = as.data.frame(cumstats1)
  cumstats1df$un_id = names(cumstats)
  cumstats8= colSums(stats[8,2:2467])
  cumstats8df = as.data.frame(cumstats8)
  cumstats8df$un_id = names(cumstats)

  if (in_multitemp %in% c(1,2)) {
    cumstats9= colSums(stats[9,2:2467])
    cumstats9df = as.data.frame(cumstats9)
    cumstats9df$un_id = names(cumstats)
  }

  outshape = readshape(out_shape)
  totdata = join(outshape@data, cumstatsdf, by = 'un_id', type = 'left')
  totdata = join(totdata, cumstats1df, by = 'un_id', type = 'left')
  totdata = join(totdata, cumstats8df, by = 'un_id', type = 'left')
  totdata$RICE [which(totdata$RICE == 'NoRIce')] = "NoRice"
  totdata = droplevels(subset(totdata, RICE != "NoRICE"))
  # p = ggplot (subset(totdata, correct == "Error"), aes(x = cumstats1, y = cumstats8, colour = cmprfll))
  # p = p + geom_point()+ lb_theme_bw()
  # p
  #
  p = ggplot (totdata, aes(x = cumstats1, y = cumstats8, colour = compare, size = correct))
  p = p + geom_point(alpha = 0.5)+ lb_theme_bw()+ scale_size_manual(values = c(1,2))
  p = p + xlab(paste0("Backscatter - " , b[1]))
  p = p + ylab(paste0("Backscatter - " , b[8]))
  p = p + ggtitle(types[in_multitemp])
  list_plots_scatter [[in_multitemp]] = p

  p = ggplot (totdata, aes(x = cumstats1, y = cumstats8, colour = RICE, size = correct))
  p = p + geom_point(alpha = 0.5)+ lb_theme_bw()+ scale_size_manual(values = c(1,2))
  p = p + xlab(paste0("Backscatter - " , b[1]))
  p = p + ylab(paste0("Backscatter - " , b[8]))
  p = p + ggtitle(types[in_multitemp])
  list_plots_scatter2 [[in_multitemp]] = p

  if (in_multitemp %in% c(1,2)) {
    totdata = join(totdata, cumstats9df, by = 'un_id', type = 'left')
    totdata$RICE [which(totdata$RICE == 'NoRIce')] = "NoRice"
    totdata = droplevels(subset(totdata, RICE != "NoRICE"))

    p = ggplot (totdata, aes(x = cumstats1, y = cumstats9, colour = RICE, size = correct))
    p = p + geom_point(alpha = 0.5)+ lb_theme_bw()+ scale_size_manual(values = c(1,2))
    p = p + xlab(paste0("Backscatter - " , b[1]))
    p = p + ylab(paste0("Backscatter - " , b[10]))
    p = p + ggtitle(types[in_multitemp])
    list_plots_scatter3 [[in_multitemp]] = p
  }

  # p = ggplot(totdata, aes(x = RICE, y = cumstats8))
  # p = p + geom_boxplot() + lb_theme_bw() + geom_jitter(alpha = 0.1)
  # p
  #
  stats_melted_j = join (stats_melted,outshape@data , by = 'un_id', type = 'left')
  stats_melted_j$RICE [which(stats_melted_j$RICE == 'NoRIce')] = "NoRice"
  stats_melted_j = droplevels(subset(stats_melted_j, RICE != "NoRICE"))

  byrice = group_by(stats_melted_j, RICE, date)
  stats_melted_j_avg = dplyr::summarize(byrice, avg = mean(value, na.rm = T), stdev = sd(value, na.rm = T))
  p = ggplot(stats_melted_j_avg, aes(x = date, y = avg,color = RICE))
  p = p + geom_line() + lb_theme_bw() + facet_grid(~RICE)+ geom_line(aes(y = avg +stdev))+ geom_line(aes(y = avg -stdev))
  p = p + ggtitle(types[in_multitemp])

  list_plots_box [[in_multitemp]] = p
  levels(stats_melted_j$Asciutt) = c(levels(stats_melted_j$Asciutt), 'NoRice')
  stats_melted_j$Asciutt[is.na(stats_melted_j$Asciutt)] = 'NoRice'

  byrice = group_by(stats_melted_j, RICE, date, Asciutt)

  stats_melted_j_avg = dplyr::summarize(byrice, avg = mean(value, na.rm = T), stdev = sd(value, na.rm = T))
  p = ggplot(stats_melted_j_avg, aes(x = date, y = avg, color = Asciutt))
  p = p + geom_line() + lb_theme_bw() + facet_grid(~Asciutt)+ geom_line(aes(y = avg +stdev))+ geom_line(aes(y = avg -stdev))
  p = p + ggtitle(types[in_multitemp])

  list_plots_box_asciutt [[in_multitemp]] = p

}

grid.arrange(list_plots_box[[1]], list_plots_box[[3]],list_plots_box[[2]], list_plots_box[[4]])
grid.arrange(list_plots_scatter[[1]], list_plots_scatter[[3]],list_plots_scatter[[2]], list_plots_scatter[[4]])
grid.arrange(list_plots_scatter2[[1]], list_plots_scatter2[[3]],list_plots_scatter2[[2]], list_plots_scatter2[[4]])
grid.arrange(list_plots_scatter3[[1]], list_plots_scatter3[[2]])
grid.arrange(list_plots_box_asciutt[[1]], list_plots_box_asciutt[[3]], list_plots_box_asciutt[[2]], list_plots_box_asciutt[[4]])

lb_class_accuracy_points = function (in_classrast = in_classrast, in_shape = in_shape, in_shape_grnd_field = NULL , hash_rast = hash_rast,
                                 hash_shape = hash_shape, out_shape = NULL)  {



# Open raster and shape ----
inrast = stack(raster(in_classrast))
inrast = setZ(inrast, as.Date('2016-06-01'), name = 'time')
inshape = readshape(in_shape)
inshape$un_id = seq(1:length(inshape[,1]))

# Extract raster value for each point of the shape
rast_values = lb_fastzonal(inrast, inshape, out_format = 'dframe', id_field = "un_id")
class_values = data.frame(un_id = names(rast_values[2:length(rast_values)]), classvalue = as.character(rast_values[2:length(rast_values)]),
                      stringsAsFactors = FALSE)
class_values$classified = "NULL"

# Reassign raster classes on the basis of hash tables to have homogeneity ----

for(kk in keys (hash_rast)) {
class_values$classified[which(class_values$classvalue == kk)] =  hash::values(hash_rast[kk])
}

# Reassign shape classes on the basis of hash tables to have homogeneity ----
inshape$ground = "NULL"
for(kk in keys (hash_shape)) {
inshape@data$ground[which(inshape@data[,in_shape_grnd_field] == kk)] = hash::values(hash_shape[kk])
}
# Join datasets and create additional columns : correct/wrong and class to class ----
inshape@data = join(inshape@data, class_values, by = 'un_id', type = 'left')
inshape = subset(inshape, ground %in% unique(inshape$classified))   # Remove eventual "unknown" points
inshape$ground = factor(inshape$ground)    # transform to factors
inshape$classified = factor(inshape$classified)

inshape$correct = "Error"
inshape$correct [inshape$ground == inshape$classified] = "Correct"
inshape$correct = factor(inshape$correct)

inshape$compare = NULL
inshape$compare = paste0(inshape$ground, "->" , inshape$classified)
inshape$comparefull = paste0(inshape$COLTURA, "->" , inshape$classified)

confmatrix = table(inshape$ground, inshape$classified) # confusion matrix: CLASSIFIED IS ON COLUMNS , GROUND ON ROWS

# Compute Accuracy Metrics (Formulas from "http://blog.revolutionanalytics.com/2016/03/com_class_eval_metrics_r.html")
n = sum(confmatrix) # number of instances
nc = nrow(confmatrix) # number of classes
diag = diag(confmatrix) # number of correctly classified instances per class
rowsums = apply(confmatrix, 1, sum) # number of instances per class
colsums = apply(confmatrix, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes

OA = as.data.frame(sum(diag, na.rm = TRUE)/n)
user_acc = as.data.frame(t(diag / colsums ), row.names = "User's Accuracy")
comm_err = as.data.frame(1 - user_acc, row.names = "Commission Erros")
prod_acc = as.data.frame(t(diag / rowsums ), row.names = "Producer's Accuracy")
om_err = as.data.frame(1 - prod_acc, row.names = "Commission Erros")
f1 = as.data.frame(2 * user_acc * prod_acc / (user_acc + prod_acc) , row.names = "f1")

expAccuracy = sum(p*q)
kappa = (OA - expAccuracy) / (1 - expAccuracy)

acc_summary = list(OA = OA, kappa = kappa, user_acc = user_acc, prod_acc = prod_acc, om_err = om_err, comm_err = comm_err, confmatrix = confmatrix)
class(acc_summary)= 'acc.summary'


print.acc.summary = function(x){
message("Overall Accuracy: ", signif(as.numeric(x[["OA"]]),3))
message("Kappa: ", signif(as.numeric(x[["kappa"]]),3))
message("==============================================")
message("Confusion Matrix:")
print(x[["confmatrix"]], row.names = F)
message("==============================================")
message("User's Accuracies - per class")
print(x[["user_acc"]], row.names = F)
message("==============================================")
message("Producer's Accuracies - per class")
print(x[["prod_acc"]], row.names = F)
message("==============================================")
message("Omission Error - per class")
print(x[["om_err"]], row.names = F)
message("==============================================")
message("Commission Error - per class")
print(x[["comm_err"]], row.names = F)
}

if(!is.null(out_shape)) {
lb_writeshape(inshape,out_shape)
}

return(acc_summary)

}
