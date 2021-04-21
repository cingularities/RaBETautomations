#' This function runs multiple 2 sample combinations of a multiple linear regressions model. Outmultiple iterations of linear model over as many amount of
#' @param rawdata Raw Data.
#' @return MAE, coefficient estimates and p.val
#' @export
#' @examples
#' model_MLRA(rawdata)

model_MLRA <- function(raw_data) { #filter list and raw data files
  
  group_MLRA <- raw_data %>% #created a grouped data frame from the tbl_df
    group_by(HI_RES_ID)
  
  split_MLRA <- group_split(group_MLRA) #split grouped_df by group
  bind_MLRA <- data.table::rbindlist(split_MLRA) #bind split tbl_df
  
  ##get class and combination of classes
  class <- distinct(raw_data,HI_RES_ID) #collecting class list
  
  get_combn <- function(x) { #function to produce class combination
    combn(x, 2, paste, collapse = ",")
  }
  
  combination_MLRA <- as.data.frame(lapply(class, get_combn)) %>% #applying get_combo function to class list
    separate(HI_RES_ID, c("A", "B")) #seperates
  
  
  mae_list <- list()
  coefficients_list <- list()
  
  for (i in 1:nrow(combination_MLRA)) {
    # Extract values from B column for this row
    A_class <- combination_MLRA$A[i]
    B_class <- combination_MLRA$B[i]
    # Pull out data from df_2 where the value in the HI_RES_ID column matches the 
    # value from df_1
    A_subset <- raw_data[raw_data$HI_RES_ID == A_class, ]
    B_subset <- raw_data[raw_data$HI_RES_ID == B_class, ]
    # Do linear regression for that subset of data
    filtered_MLRA <- rbind(A_subset,B_subset)
    
    linear_model <- lm(NAIP_WC ~ WVI + MSAVI + NDI5, data = filtered_MLRA)
    model_summary <- tidy(linear_model)
    model_coefficients<- coef(summary(linear_model), data=raw_data)
    coefficients_list[[i]] <- model_coefficients
    
    
    
    # Pull out the r.squared (as example)
    predict_MLRA <- add_predictions(raw_data, linear_model)
    mae <- function(error)
    {
      mean(abs(error))
    }
    error <- predict_MLRA$NAIP_WC - predict_MLRA$pred
    MAE <- mae(error)
    mae_list[[i]] <- MAE
  }
  
  MAE_df <- combination_MLRA %>% add_column(MAE = mae_list)
  MAE_final <- data.frame(lapply(MAE_df, as.character), stringsAsFactors=FALSE)
  coefficients_df <- data.frame(matrix(unlist(coefficients_list), nrow=length(coefficients_list), byrow=TRUE))
  coefficients_final <- coefficients_df %>% select(X1,X2,X3,X4,X13,X14,X15,X16) %>% 
    rename(est.int = X1, est.WVI = X2, est.MSAVI = X3, est.NDI5 = X4,
           p.int = X13, p.WVI = X14, p.MSAVI = X15, p.NDI5 = X16)
  
  final_file <- cbind(MAE_final,coefficients_final)
  #return(MAE_final)
  return(final_file)
}