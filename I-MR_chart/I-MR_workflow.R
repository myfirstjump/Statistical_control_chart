# 程式: 
#   SNP array radiation monitor
# 版本:
#   2018-10-30
# 流程:
#   確認一段穩定的實驗時間區間為資料依據 ex. 0519變更試劑，則區間設定為0519~現在
# 確認新資料的時間區間 ex. 1020~1030
# produce control limit for "the individuals and moving range charts"
# initial data OOC -> 確認真因 -> remove the observation

library(xlsx)
library(dplyr)
library(qcc)

### Read data

rm(list=ls())

cat('\n\n\n')
cat('Results:\n\n')
cat('\n##############################\n')
cat('### Check data period:\n')
cat('##############################\n')
excel_file <- dir(pattern = "*\\.xlsx$", path = './data')
raw_data <- read.xlsx(paste0('./data/', excel_file), header = TRUE, sheetIndex = 1)
min_date <- as.character(min(as.Date(raw_data$upload_ts)))
max_date <- as.character(max(as.Date(raw_data$upload_ts)))
cat('Time of the raw data is between:', min_date, 'to', max_date, '\n')

config <- read.table('./data/qc_config.txt')
change_pts <- as.character(config$V2)[1]
change_pts <- strsplit(change_pts, ',')[[1]]
cat('Reagent or standard line change times:', change_pts, '\n')
last_change_pts <- change_pts[length(change_pts)]

new_data_date <- as.character(config$V2)[2]
cat('New data start at:', new_data_date, '\n')

### Get input data
# pass qc
radiation <- raw_data[which(raw_data$qc_abnormal=='pass'), c("upload_ts", "Radiation")]
# last reagent change
radiation <- radiation[as.Date(radiation$upload_ts) > last_change_pts, ]
radiation <- aggregate(radiation$Radiation, list(radiation$upload_ts), mean)

all_data <- radiation
new_data <- all_data[as.Date(all_data$Group.1) > new_data_date,]


### control chart
# Step1. Check moving range chart
cat('\n##############################\n')
cat('### Check moving range chart\n')
cat('##############################\n')
# Use type='R' to draw moving-range(MR) chart (qcc package no MR type setting)
get_moving_range_chart_input_from_one_at_time_data <- function(input_col){
  fool_qcc_data <- matrix(cbind(input_col[1:length(input_col)-1], input_col[2:length(input_col)]), ncol=2)
  return(fool_qcc_data)
}

mr_all_data <- get_moving_range_chart_input_from_one_at_time_data(all_data$x)
mr_old_data <- mr_all_data[1:(nrow(all_data)-nrow(new_data)-1), ]
mr_new_data <- mr_all_data[(nrow(all_data)-nrow(new_data)):nrow(mr_all_data), ]

R <- qcc(data=mr_old_data,
         newdata=mr_new_data, 
         type='R',
         title='Moving Range Chart',
         labels=as.character(all_data[1:(nrow(all_data)-nrow(new_data)-1), ]$Group.1),
         newlabels=as.character(new_data$Group.1),
         axes.las=2
         )

mr_violation_vec <- union(R$violations$beyond.limits, R$violations$violating.runs)
if(length(mr_violation_vec) > 0){
  cat('There is a least one violation in the moving range chart.\n')
  cat('Date', mr_violation_vec, '\n')
  
  
  # 如果 violation 發生在 new_data，則需確認實驗；發生在舊有data，則將該筆 data刪除
  if (dim(radiation)[1] < max(mr_violation_vec)){
    cat('MR chart has violation in new data, please check the data.\n')
    Sys.sleep(500)
  } else {
    radiation <- radiation[-mr_violation_vec]
  }
} else {
  cat('MR chart pass.\n')
}



### initial the process limit by calibration data.
#rownames(radiation) <- radiation$Group.1
tmp <- qcc(data=radiation$x,
         type='xbar.one', 
         plot=FALSE
         )
tmp <- qcc(data=radiation$x, 
         type='xbar.one',
         limits=c(0, tmp$limits[, 'UCL']),
         plot=FALSE
         )
violate_vec <- union(tmp$violations$beyond.limits, tmp$violations$violating.runs)

# remove beyond limits point for initial data

while(length(violate_vec) > 0){
  radiation <- radiation[-violate_vec, ]
  tmp <- qcc(data=radiation$x,
           type='xbar.one',
           plot=FALSE)
  tmp <- qcc(data=radiation$x, 
           type='xbar.one',
           limits=c(0, tmp$limits[, 'UCL']),
           plot=FALSE)
  violate_vec <- union(tmp$violations$beyond.limits, tmp$violations$violating.runs)
}


# Step2. Check individual chart
cat('\n##############################\n')
cat('### Check individual chart\n')
cat('##############################\n')
x <- qcc(data=radiation$x,
         newdata=new_data$x,
         type='xbar.one',
         chart.all=TRUE,
         fill=TRUE,
         axes.las=1,
         plot = FALSE)
x <- qcc(data=radiation$x,
         newdata=new_data$x,
         type='xbar.one',
         title='Individual Chart',
         chart.all=TRUE,
         labels=as.character(radiation$Group.1),
         newlabels=as.character(new_data$Group.1),
         axes.las=2,
         limits=c(0, x$limits[, 'UCL']),
         plot = TRUE) 
violate_vec <- union(x$violations$beyond.limits, x$violations$violating.runs)

if(length(violate_vec) > 0){
  combined_data <- rbind(radiation, new_data)
  cat('Exist violations in the control chart:\n')
  if(length(x$violations$beyond.limits > 0)){
    cat('Beyond control limit: ')
    beyond_idx <- x$violations$beyond.limits
    cat(as.character(combined_data[beyond_idx, "Group.1"]), '\n')
    
    if(length(x$violations$violating.runs > 0)){
      cat('Exist violating runs: ')
      violating_idx <- x$violations$violating.runs
      cat(as.character(combined_data[violating_idx, "Group.1"]), '\n')
    }
  } else {
    cat('Exist violating runs: ')
    violating_idx <- x$violations$violating.runs
    cat(as.character(combined_data[violating_idx, "Group.1"]), '\n')
  }
}

