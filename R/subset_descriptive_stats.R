#' Descriptive Statistics for Subsets of a Dataframe
#'
#' @param dataframe A dataframe
#' @param group_var The name of a column in the dataframe
#' @param csv_file Logical, save results in a .csv file
#'
#' @return a dataframe or a .csv file with descriptive statistics for each subset based on the group_var.
#' @export
#'
#' @examples
#' sample_data.subset_descriptive = subset_descriptive_stats(sample_data, "sex")

subset_descriptive_stats <- function(dataframe,
                                     group_var,
                                     csv_file=FALSE){


  # Get correct class dataframe
  class_df = get_correct_class(dataframe)

  # Get group_var class and number of unique values
  group_var_type = class_df[class_df$name==group_var,"data_type"]
  group_var_unique = unique(stats::na.omit(dataframe[[group_var]]))

  # Stop if group_var is numeric or has too many / too few values
  if (group_var_type != "CAT") {
    stop("Selected column is not categorical; data not subsettable")}
  else if (length(group_var_unique) >= 5){
    stop("Selected column has more than 5 unique values; data not subsettable")}
  else if (length(group_var_unique) < 2){
    stop("Selected column has less than 2 unique values; data not subsettable")}

  # Get complete descriptive statistics for non-subset data
  complete_descr <- descriptive_stats(dataframe)

  # Get descriptive statistics for each subset
  subset_descr_list = lapply(group_var_unique, function(x){
    filtered_df = dplyr::filter(dataframe,
                                !!rlang::sym(group_var)==x)
    subset_descr_df = descriptive_stats(filtered_df,
                                        alternative.class_df=class_df)})

  # Initialize results dataframe
  result_dataframe = data.frame(matrix(nrow=nrow(complete_descr),
                                       ncol=length(group_var_unique)+2))
  result_dataframe[,1:2] = complete_descr[,1:2]
  colnames(result_dataframe) = c("name",
                                 "categ",
                                 as.character(group_var_unique))

  # Insert values into result dataframe
  for (row in seq_len(nrow(complete_descr))) {
    #name and single category of variable in complete descriptive
    row_name = complete_descr$name[row]
    row_categ = complete_descr$categ[row]
    #for each subset dataframe
    for (num_df in seq_along(subset_descr_list)) {
      sub_df = subset_descr_list[[num_df]]
      #filter subset df to contain only rows of interest
      row_sub_df = sub_df[sub_df$name==row_name,]
      #if single category is still present in the subset df
      if (row_categ %in% row_sub_df$categ) {
        #select row number corresponding to the single category values
        row_sub_df_name = rownames(row_sub_df[row_sub_df$categ==row_categ,])
        result_dataframe[row,num_df+2] = sub_df[row_sub_df_name,"count"]
      } else {
        result_dataframe[row,num_df+2] = "0 (0%)"
      }
    }
  }

  # Remove rows with only "0 (0%)", NA and 0
  result_dataframe = result_dataframe |>
    dplyr::filter(rowSums(dplyr::across(-c(1,2),
                                        ~ .!="0 (0%)" & !is.na(.) & .!= 0))>0)
  # Reset row names
  rownames(result_dataframe) <- seq_len(nrow(result_dataframe))

  # Write .csv file
  if (csv_file) {
    # Remove redundant name
    result_csv <- result_dataframe |>
      dplyr::group_by(name) |>
      dplyr::mutate(name_keep = if_else(row_number() == 1, name, "")) |>
      dplyr::ungroup() |>
      dplyr::select(name_keep, categ, group_var_unique) |>
      dplyr::rename(name = name_keep)

    # Make .csv filename
    file_name <- paste0("risultati_subset_", group_var, "_",
                        deparse(substitute(dataframe)), ".csv")

    write_csv2(result_csv, file_name)
    return(1)
  } else {
    return(result_dataframe)
  }
}

