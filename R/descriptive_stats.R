#' Complete Descriptive Statistics of Every Column in a Dataframe
#'
#' @param dataframe A dataframe
#' @param csv_file Logical, save results in a .csv file
#' @param alternative.class_df Dataframe used to assign a class to every column. Must be like get_correct_class output
#'
#' @return a dataframe or a .csv file with the complete descriptive statistics of the initial dataframe
#' @export
#'
#' @examples
#' sample_data.descriptive = descriptive_stats(sample_data)

descriptive_stats <- function(dataframe,
                              csv_file=FALSE,
                              alternative.class_df=NULL) {

  if (is.null(alternative.class_df)) {
    class_df = get_correct_class(dataframe) #get proper class df
  } else {
    class_df = alternative.class_df #or input one
  }

  #index empty df
  res_df = data.frame(name = character(),
                      categ = character(),
                      count = character(),
                      stringsAsFactors = FALSE)

  #for each variable of dataframe (= rows of class_df)
  for (num_var in seq_len(nrow(class_df))) {
    name_var <- class_df$name[num_var]      #name of column
    type_var <- class_df$data_type[num_var] #type of column

    #if type = discard -> c(name, NA, NA)
    if (type_var=="discard") {
      NA_df = data.frame(name = name_var, categ = NA, count = NA)
      ### ADD RESULTS TO res_df ###
      res_df = rbind(res_df, NA_df)

      #if type == CAT -> c(name, value, count(%))
    } else if (type_var=="CAT") {
      categories <- sort(na.omit(unique(dataframe[[name_var]])))
      CAT_df <- data.frame(name = name_var,
                           categ = c(as.character(categories), "NA"),
                           count = c(sapply(categories, function(cat) {
                             count <- sum(dataframe[[name_var]] == cat, na.rm = TRUE)
                             percent <- round(count / sum(!is.na(dataframe[[name_var]])) * 100, 1)
                             paste0(count, " (", percent, "%)")
                           }),
                           sum(is.na(dataframe[[name_var]]))),
                           stringsAsFactors = FALSE)
      ### ADD RESULTS TO res_df ###
      res_df <- rbind(res_df, CAT_df)

      #if type = NUM -> c(name, "Median [range]", Median [range])
    } else if (type_var=="NUM") {
     NUM_df=data.frame(name=name_var,
                        categ="Median [range]",
                        count=paste0(
                          round(median(dataframe[[num_var]],na.rm=TRUE),1),
                          " [",
                          round(min(dataframe[[num_var]],na.rm=TRUE),1),
                          "-",
                          round(max(dataframe[[num_var]],na.rm=TRUE),1),
                          "]"),
                       stringsAsFactors = FALSE)

      ### ADD RESULTS TO res_df ###
     res_df <- rbind(res_df, NUM_df)
    }
  }
  rownames(res_df) = NULL

  #write .csv file
  if (csv_file) {
    result <- res_df |>
      group_by(name) |>
      mutate(name_keep = if_else(row_number() == 1, name, "")) |>
      ungroup() |>
      select(name_keep, categ, count) |>
      rename(name = name_keep)

    file_name <- paste0("risultati_descr_",
                        deparse(substitute(dataframe)),
                        ".csv")
    write_csv2(result, file_name)
    return(1)
  } else {
    return(res_df)
  }
}
