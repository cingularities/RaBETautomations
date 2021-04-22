#' This function runs multiple 2 sample combinations of a multiple linear regressions model. Outmultiple iterations of linear model over as many amount of
#' @param rawdata Raw Data.
#' @return MAE, coefficient estimates and p.val
#' @export
#' @examples
#' model_MLRA(rawdata)

model_MLRA <- function(raw_data) { #filter list and raw data files and runs linear models on all all 2 samples combination dataset
  
  group_MLRA <- raw_data %>% #creates a grouped data frame from the tbl_df
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
  
  mae <- function(error){ #function for mean absolute error
    mean(abs(error))
  }
  
  mae_list <- list() #creates empty list for mae dataset
  coefficients_list <- list() #creates empty list for model coefficients 
  
  for (i in 1:nrow(combination_MLRA)) { #loop through combination list 
    # Extract values from from column 
    A_class <- combination_MLRA$A[i] #assign value of sample 1 to class var
    B_class <- combination_MLRA$B[i] #assign value of sample 2 to class var
    
    # Pull out data from  where the value in the HI_RES_ID column matches the 
    # Subset raw_data with selected combination_MLRA class
    A_subset <- raw_data[raw_data$HI_RES_ID == A_class, ] #subset sample 1
    B_subset <- raw_data[raw_data$HI_RES_ID == B_class, ] #subset sample 2
    # Do linear regression for that subset of data
    filtered_MLRA <- rbind(A_subset,B_subset) #bind filtered subsets
    
    linear_model <- lm(NAIP_WC ~ WVIplus + MSAVI + NDI5, data = filtered_MLRA) #run linear model
    model_coefficients<- coef(summary(linear_model), data=raw_data) #extract model coefficients
    coefficients_list[[i]] <- model_coefficients #input coefficient to list
    
    # Pull out the MAE
    predict_MLRA <- raw_data %>% add_predictions(linear_model)     #predict raw based on linear model
    error_subset<- predict_MLRA[predict_MLRA$HI_RES_ID != A_class, ]#removes samples used in linear model
    error_subset<- error_subset[error_subset$HI_RES_ID != B_class, ]#removes samples used in linear model
    error_estimate <- aggregate(NAIP_WC ~ HI_RES_ID, data = error_subset, FUN = mean) #aggretaes estimate by mean
    error_pred <- aggregate(pred ~ HI_RES_ID, data = error_subset, FUN = mean) #aggregate prediction by mean
    
    error <- error_estimate$NAIP_WC - error_pred$pred #calculate error
    MAE <- mae(error) #calculate MAE from function created
    mae_list[[i]] <- MAE #add mae values to list
  }
  
  MAE_df <- combination_MLRA %>% add_column(MAE = mae_list) #add MAE values to combination list
  MAE_final <- data.frame(lapply(MAE_df, as.character), stringsAsFactors=FALSE) #creates the right format for export MAE and sample combination
  coefficients_df <- data.frame(matrix(unlist(coefficients_list), nrow=length(coefficients_list), byrow=TRUE)) #creates the right format for export lm coefficients
  coefficients_final <- coefficients_df %>% select(X1,X2,X3,X4,X13,X14,X15,X16) %>% #select wanted coefficients and renaming columns
    rename(est.int = X1, est.WVIplus = X2, est.MSAVI = X3, est.NDI5 = X4,
           p.int = X13, p.WVIplus = X14, p.MSAVI = X15, p.NDI5 = X16)
  
  final_file <- cbind(MAE_final,coefficients_final) #binds MAE, sample combination and wanted coefficients
  return(final_file) #return final_file
}
