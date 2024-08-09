
combine_datasets<- function(datasets, outds) {
  # Filter out non-existing datasets first
  existing_datasets <- Filter(function(x) exists(x, where = .GlobalEnv), datasets)
  
  dataset_list <- lapply(existing_datasets, get)
  
  if (length(dataset_list) > 0) {
    
    combined_dataset <- do.call(rbind, dataset_list)
    
    # Remove duplicates
    combined_dataset <- combined_dataset[!duplicated(combined_dataset), ]
    
    assign(outds, combined_dataset, envir = .GlobalEnv)
    
    return(combined_dataset)
  } else {
    message("NOTE: No datasets to combine into ", outds)
  }
}