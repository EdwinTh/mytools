#' Check if the id columns are unique
#'
#' @param x a data frame
#' @param ... the unquoted column names of the id variables
#'
#' @return
#' If the id variables are unique TRUE. Else, the rows in `x` for which the id
#' variables are not unique, arranged by id variables.
#'
#' @examples
#' mtcars %>% unique_id(cyl, disp)
#' mtcars %>% unique_id(cyl, wt)
#' mtcars %>% unique_id(cyl, qsec)
#' @export
unique_id <- function(x, ...) {
  vars <- as.character(match.call())[-(1:2)]
  vars_enq <- enquo(vars)
  id_set <- x %>% select(!!vars_enq)
  id_set_dist <- id_set %>% distinct
  if (nrow(id_set) == nrow(id_set_dist)) {
    TRUE
  } else {
    non_unique_ids <- id_set %>%
      filter(id_set %>% duplicated) %>%
      distinct()
    suppressMessages(
      inner_join(non_unique_ids, x) %>% arrange_(vars)
    )
  }
}

#' Frequencies Grouped by Another Column
#' 
#' Obtain the relative distribution of a categorical variable within the group
#' member of a different variable.
#' 
#' @param x a data frame
#' @param group_var the variable to group by
#' @param prop_var the variable over which to calculate the frequencies
#' @return 
#' A data frame with both the absolute and the relative distribution of `prop_var`
#' within `group_var`.
#' @seealso
#' Inspired by SO question 24576515
#' @examples
#' mtcars %>% freq_table(vs, cyl)
#' @export
freq_table <- function(x, 
                       group_var, 
                       prop_var) {
  group_var <- enquo(group_var)
  prop_var  <- enquo(prop_var)
  x %>% group_by(!!group_var, !!prop_var) %>% 
    summarise(n = n()) %>% 
    mutate(freq = n /sum(n)) %>% 
    ungroup
}
