#' Plot burndown
#'
#' @param d_times data frame of times from \code{\link{time_entries_all}} with \code{duration_hrs} field
#' @param beg beginning date
#' @param end end date to draw red dashed line from beginning \code{beg} to end \code{end} starting with hours \code{hrs} to zero
#' @param hrs hours estimated for burn down from begin
#'
#' @return ggplot2 plot
#' @export
#'
#' @examples
plot_burndown <- function(d_times, beg, end, hrs){
  library(ggplot2)

  stopifnot(all(is.Date(c(beg, end))))
  stopifnot(is.numeric(hrs))

  d <- tibble(
    # initial row with starting hours
    date_start   = beg,
    duration_hrs = hrs) %>%
    # rows with hours expended
    bind_rows(
      d_times %>%
        mutate(
          date_start = as.Date(time_start)) %>%
        group_by(date_start) %>%
        summarize(
          # set duration negative to deduct with cumsum()
          duration_hrs = -sum(duration_hrs))) %>%
    # ensure line goes to end
    bind_rows(
      tibble(
        date_start   = end,
        duration_hrs = 0)) %>%
    arrange(date_start) %>%
    mutate(
      task_hrs = cumsum(duration_hrs)) %>%
    select(-duration_hrs)

  d_areastep <- bind_rows(
    old = d,
    new = d %>%
      mutate(task_hrs = lag(task_hrs)),
    .id = "source") %>%
    arrange(date_start, source)

  slope     <- - hrs / as.numeric(end - beg)
  intercept <- - slope * as.numeric(beg)

  ggplot(data = d, aes(x = date_start, y = task_hrs)) +
    geom_ribbon(
      data = d_areastep,
      aes(x = date_start, ymax = task_hrs,
          ymin = 0),
      fill = "gray") +
    geom_step() +
    geom_line(
      data = tibble(
        date_start = c(beg, end),
        task_hrs   = c(hrs, 0)),
      aes(x = date_start, y = task_hrs),
      color = "red",
      linetype = "dashed") +
    scale_x_date(expand = c(0, 0)) +
    theme_bw() +
    xlab("Date") +
    ylab("Hours")
}
