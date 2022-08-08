# Libraries 
library(tidyverse) |> suppressPackageStartupMessages()
library(tidytext, include.only = c("reorder_within", "scale_y_reordered"))
library(scales)



# Data Cleaning 
clean_data <- function(df) {
  # Unique Date to for generating date in r -----------------------------------|
  unique_date <- df %>% 
    select(ddate) %>% 
    filter(!is.na(ddate)) %>%
    mutate(ddate = lubridate::as_date(ddate)) %>% pull()
  
  date_nm <- c("ItemLabels", unique_date, "State", "ddate")
  
  names(df) <- date_nm
  
  # Unique Geo-Political Zones in Nigeria -------------------------------------|
  north_central <- c("BENUE", "KOGI", "KWARA", "NASARAWA", "NIGER", "PLATEAU", "ABUJA")
  north_east <- c("ADAMAWA", "BAUCHI", "BORNO", "GOMBE", "TARABA", "YOBE")
  north_west <- c("JIGAWA", "KADUNA", "KANO", "KATSINA", "KEBBI", "SOKOTO", "ZAMFARA")
  south_east <- c("ABIA", "ANAMBRA", "EBONYI", "ENUGU", "IMO")
  south_south <- c("AKWA_IBOM", "BAYELSA", "CROSS_RIVER", "EDO", "RIVERS", "DELTA")
  south_west <- c("EKITI", "LAGOS", "OGUN", "ONDO", "OSUN", "OYO")
  
  # Cleaning ------------------------------------------------------------------|
  df <- df %>% 
    select(-c(ddate)) %>%
    relocate(State, .after = ItemLabels) %>% 
    pivot_longer(-c(ItemLabels, State), names_to = "Date", values_to = "Price") %>%
    mutate(State = if_else(State == "NASSARAWA", "NASARAWA", State)) %>% 
    mutate(zone = case_when(State %in% north_central ~ "North Central",
                            State %in% north_east  ~ "North East",
                            State %in% north_west  ~ "North West",
                            State %in% south_east  ~ "South East",
                            State %in% south_south ~ "South South",
                            State %in% south_west  ~ "South West",
                            TRUE ~ State),
           State = str_to_title(State)) %>% 
    mutate(Date  = as.numeric(Date) |> lubridate::as_date(),
           year  = lubridate::year(Date),
           month = lubridate::month(Date),
           month_label = lubridate::month(Date, label = TRUE, abbr = FALSE)) %>% 
    mutate(Price = replace_na(Price, 0)) %>% 
    janitor::clean_names() %>% 
    select(item_labels, state, zone, price, date, year, month, month_label)
  
  # output --------------------------------------------------------------------|
  return(df)
}


 

#' @description Return a vector of different lengths.
#'
#' @param cond the condition which can be coerced to logical mode.
#' @param true values when condition is TRUE.
#' @param false values when condition is FALSE
#'
#' @return
#' @export
#'
#' @examples
om_ifelse <- function(cond, true, false) {
  
  output <- ifelse(cond,
                   list(true),
                   list(false))[[1]]
  return(output)
}



#' Title
#'
#' @param x an expression.
#'
#' @return
#' @export
#'
#' @examples
sub_dep <- function(x) {
  
  newname <- substitute(x)
  
  for (i in seq_len(sys.nframe())) {
    oldname <- do.call("substitute", list(as.name(newname), parent.frame(i)))
    newname <- oldname
  }
  output <- deparse(newname)
  return(output)
}



#' Title
#'
#' @param r_df      check if data.frame
#' @param num_var 
#' @param in_loc    check if state or zone.
#' @param r_locs    check if part of states or zone.
#' @param btw       
#' @param btw_m     'list for btw': check if between a range of values.
#' @param len       list: check if length is valid.
#' @param unq_vals  check if values are unique.
#' @param r_typ     check for valid output type.
#' @param arg_nm     list: argument names
#' @param extra_info list: extra information for the error message.
#'
#' @return
#' @export
#'
#' @examples
right_arg <- function(r_df, 
                      num_var,
                      in_loc, 
                      r_locs, 
                      btw, btw_m, 
                      len, 
                      unq_vals,
                      r_typ, 
                      arg_nm, extra_info) {
  
  # data frame ----------------------------------------------------------------|
  if (!missing(r_df)) {
    if (!inherits(r_df, "data.frame")) {
      stop("argument `df` must be a data.frame")
    }
  }
  
  # numeric variable ----------------------------------------------------------|
  if (!missing(num_var)) {
    num_var <- sub_dep(num_var)
    
    if (! is.numeric(r_df[[num_var]])) {
      stop(paste0("argument `", arg_nm$num_var,"` must be numeric"))
    }
  }
  
  # location ------------------------------------------------------------------|
  # state zone 
  if (!missing(in_loc)) {
    if (!in_loc %in% c("state", "zone")) {
      stop("argument `by` must be either 'state' or 'zone'")
    }
  }
  
  # unique state zone
  if (!missing(r_locs)) {
    state_nm <- unique(r_df$state)
    zone_nm  <- unique(r_df$zone)
    
    if (any(! r_locs %in% state_nm) & any(! r_locs %in% zone_nm)) {
      stop(
        paste0('`',arg_nm$r_locs,
               '` can not be identified use any ',
               extra_info$r_locs,' in the df'))
    }
  }
  
  # between -------------------------------------------------------------------|
  if (!missing(btw)) {
    if (is.list(btw)) {
      from <- btw$between[1]
      to   <- btw$between[2]
      
      if (btw$check < from | btw$check > to) {
        stop(paste("argument", arg_nm$btw, "must be between", from, "&", to))
      }
    }
  }
  
  # month
  if (!missing(btw_m)) {
    if (btw_m < 1 | btw_m > 12) {
      stop("argument ", arg_nm$btw_m, " must be between 1 & 12")
    }
  }
  
  # unique values -------------------------------------------------------------|
 if (!missing(unq_vals)) {
   if (length(unique(unq_vals)) != length(unq_vals)) {
     stop(paste0("argument `", arg_nm$unq_vals, 
                "` should have ", length(unq_vals), " distinct values"))
   }
 }
  
  # length --------------------------------------------------------------------|
  if (!missing(len)) {
    if (is.list(len)) {
      if (! eval(len$is)) {
        stop(paste0("length of argument `",
                    arg_nm$len, "` must be ", 
                    extra_info$len))
      }
    } else {
      stop("argument `len` must be a list")
    }
  }
  # example:: list(check = arg_name, is = expression(length(len$check) == 2))
  # extra_info = list(len = "equal to 2")
  
  # output --------------------------------------------------------------------|
  if (!missing(r_typ)) {
    if (! r_typ %in% c("plt", "tbl")) {
      stop("argument `typ` must either be 'tbl' for table or 'plt' for plot.")
    }
  }
}



#' Title
#'
#'@description create a numeric data type summary.
#'
#' @param df  data.frame
#' @param only_average bool: TRUE to return only average else min, mean, max.
#' @param smy_var numeric: Variable from the data 'df' to summaries.
#' @param fun function: The average function to use either mean or median
#'
#' @return
#' @export
#'
#' @examples
numeric_smy <- function(df, 
                        only_average = TRUE, 
                        smy_var = price, 
                        fun = mean) {
  right_arg(r_df = df, num_var = smy_var, arg_nm = list(num_var = "smy_var"))
  
  if (only_average) {
    f_df <- df %>% 
      summarise(average_price = fun({{smy_var}}), .groups = "drop")
    
  } else {
    f_df <- df %>% 
      summarise(minimum_price = min({{smy_var}}),
                average_price = fun({{smy_var}}),
                maximum_price = max({{smy_var}}), .groups = "drop")
  }
  
  return(f_df)
}



#' Title
#'
#'@description create plot axis breakes using base::seq function.
#' @param df  data.frame
#' @param var numeric a variable from the data  'df'.
#'
#' @return
#' @export
#'
#' @examples
breakz <- function(df, var) {
  var_s <- ensym(var)
  
  stopifnot(inherits(df, "data.frame"))
  if (! is.numeric(df[[var_s]])) {
    stop("argument `var` must be a numeric variable.")
  }
  
  max_value <-  summarise(df, max = max({{ var }})) %>% pull()
  
  if (max_value <= 400) {
    in_by <- 20
    
  } else if (max_value > 400 & max_value <= 800) {
    in_by <- 50
    
  } else if (max_value > 800 & max_value < 1000) {
    in_by <- 100
    
  } else if (max_value >= 1000 & max_value <= 10000) {
    in_by <- 500
    
  } else if (max_value > 10000 & max_value <= 100000) {
    in_by <- 10000
    
  } else {
    in_by <- 100000
  }
  
  output <- list(to = max_value, by = in_by)
  
  return(output)
}



df_title <- function(df_name) {
  if (!is.character(df_name)) {
    stop("`df_name` must be character.")
  }
  
  if (df_name == "cookingOil_df") {
    title <- "Cooking Oil"
    
  } else {
    title <- df_name %>% 
      str_replace("_", " ") %>% 
      str_extract("[:alpha:]+") %>% 
      str_to_title()
  }
  return(title)
}




#' Title
#'
#'@description default colors to use when no color is specified.
#' @param num_colors numeric: number of colors.
#' @param paint bool: True to use default colors.
#'
#' @return
#' @export
#'
#' @examples
default_colors <- function(num_colors, paint = FALSE) {
  if (isFALSE(paint)) {
    if (num_colors < 2 | num_colors > 4) {
      stop("Number of colors must be from 2 to 4")
    }
    colr <- list('2' = c("plum", "plum2"), 
                 '3' = c("plum3", "plum", "plum2"),
                 '4' = c("plum4", "plum", "plum3", "plum2"))
    
    sl_color <- colr[[as.character(num_colors)]]
    
  } else {
    #c("gold3", "limegreen", "navy")
    paints <- c("gold3", "limegreen", "red3", "navy")
    sl_color <- paints[1:num_colors]
  }
  return(sl_color)
}




#' Location Summary Plot Layers
#'
#' @param type character: either 'all' for all main layers or 'some' for some layers.
#' @param p_by character: either 'state' or 'zone'.
#' @param lab  character: additional label to add to the plot title.
#' @param colors vector: max of 4 min of 2 colors for the plot e.g 'blue', 'red',..
#' @param bk   vector: a vector of 2 numbers for the scale breaks. e.g 70, 100
#' @param txt_pos position of the plot text.
#'
#' @return
#' @export
#'
#' @examples
plot_layer_ls <- function(type = "all", p_by, lab, colors, bk, txt_pos) {

  # plot layers ---------------------------------------------------------------|
  if (type == "all") {
    add_plot <- list(
      geom_col(aes(x = average_price), show.legend = FALSE),
      scale_y_reordered(),
      scale_x_continuous(breaks = seq(0, bk[1], bk[2])),
      scale_fill_manual(values = colors),
      labs(x = "", y = ""),
      theme_minimal()
    )
    
    # plot title --------------------------------------------------------------|
    add_plot[[length(add_plot)+1]] <- switch(
      p_by,
      state = ggtitle(paste("Average Price Of", lab, "Across All States & FCT")),
      zone  = ggtitle(paste("Average Price Of", lab, "Across All Geo-Political Zones"))
    )

  } else if (type == "some") {
    add_plot <- list(
      geom_text(aes(label = comma(average_price, 1), x = txt_pos), 
                size  = 2.5, 
                color = "grey15"),
      theme(axis.text.x = element_blank())
    )
    
  } else {
    stop("From plot_level_os:`type` can only be either 'all' or 'some'")
  }
  
  return(add_plot)
}



#' Location Summary
#'
#'@description calculate the average summary of the data commodity.
#' @param df data.frame
#' @param by character: either 'state' or 'zone'.
#' @param breakz vector of 2 numbers for the scale breaks.
#' @param color vector: max of 4 min of 2 colors for the plot e.g blue', 'red'...
#' @param typ  character: type of output 'plt' for plot or 'tbl' for table.
#' 
#' @note used right_arg, om_ifelse, plot_layer_ls & default_colors functions
#'
#' @return
#' @export
#'
#' @examples
loc_summary <- function(df, 
                        by = "state", 
                        breakz = c(700, 100), 
                        color,
                        typ = "plt") {
  right_arg(r_df  = df, 
            in_loc = by, 
            len = list(check = breakz, is = expression(length(len$check) == 2)),
            arg_nm = list(in_loc = "by", len = "breakz"),
            extra_info = list(len = "equal to 2"),
            r_typ = typ)

  if (by == "state") {
    f_tbl <- df %>%
      group_by(state, commodity) %>%
      numeric_smy()
    
  } else if(by == "zone") {
    f_tbl <- df %>%
      group_by(zone, commodity) %>%
      numeric_smy()
  }
  
  no_unique_commodity <- unique(f_tbl$commodity) |> length()
  
  label <- sub_dep(df) |> df_title()
  
  text_pos <- quantile(f_tbl$average_price, 0.15)
  
  sl_colr <- om_ifelse(missing(color),
                       default_colors(no_unique_commodity),
                       color)
  
  # plot ---------------------------------------------------------------------|
  if (by == "state") {
    f_plt <- f_tbl %>% 
      ggplot(aes(y = reorder_within(state, average_price, commodity), 
                 fill = commodity))
    if (no_unique_commodity == 2) {
      f_plt <- f_plt +
        plot_layer_ls("all", by, label, sl_colr, breakz) +
        facet_wrap(vars(commodity), ncol = 2, scales = "free")
      
    } else if (no_unique_commodity == 3) {
      f_plt <- f_plt +
        plot_layer_ls("all", by, label, sl_colr, breakz) +
        plot_layer_ls(type = "some", txt_pos = text_pos) +
        facet_wrap(vars(commodity), nrow = 1, scales = "free") 
        
    } else if (no_unique_commodity == 4) {
      f_plt <- f_plt +
        plot_layer_ls("all", by, label, sl_colr, breakz) +
        plot_layer_ls(type = "some", txt_pos = text_pos) +
        facet_wrap(vars(commodity), ncol = 4, scales = "free")
      
    } else {
      message('number of unique commodity is either `< 2 or > 4`')
    }
    
  } else if (by == "zone") {
    f_plt <- f_tbl %>% 
      ggplot(aes(y = reorder_within(zone, average_price, commodity), 
                 fill = commodity))
    if (no_unique_commodity == 2) {
      f_plt <- f_plt +
        plot_layer_ls("all", by, label, sl_colr, breakz) +
        plot_layer_ls(type = "some", txt_pos = text_pos) +
        facet_wrap(vars(commodity), ncol = 2, scales = "free")
      
    } else if (no_unique_commodity == 3) {
      f_plt <- f_plt +
        plot_layer_ls("all", by, label, sl_colr, breakz) +
        plot_layer_ls(type = "some", txt_pos = text_pos) +
        facet_wrap(vars(commodity), nrow = 1, scales = "free")
      
    } else if (no_unique_commodity == 4) {
      f_plt <- f_plt +
        plot_layer_ls("all", by, label, sl_colr, breakz) +
        plot_layer_ls(type = "some", txt_pos = text_pos) +
        facet_wrap(vars(commodity), ncol = 4, scales = "free")
      
    } else {
      message('number of unique commodity is either `< 2 or > 4`')
    }
    
  }

  output <- switch(typ,
                   plt = f_plt,
                   tbl = f_tbl)
  return(output)
}




#' Average Price By Year Plot layer.
#'
#' @param type  character: Either 'all' or 'some'.
#' @param p_by  character: Either 'state' or 'zone'.
#' @param nm    character: Name of the commodity.
#' @param lab   character: Additional label to add to the plot title.
#' @param colors vector: Max of 4 and min of 2 colors for the plot.
#' @param txt_pos numeric: A number to assign the geom text axis x position.
#'
#' @return
#' @export
#'
#' @examples
plot_layer_apy <- function(type = 'all', p_by, nm, lab, colors, txt_pos) {
  if (type == "all") {
    add_plot <- list(
      geom_col(show.legend = FALSE),
      scale_y_reordered(),
      labs(x = "", y = ""),
      scale_fill_manual(values = colors),
      theme_minimal()
    )
    
    add_plot[[length(add_plot)+1]] <- switch(
      p_by,
      state = ggtitle(
        paste("Average Price Of", nm, "In All State & FCT In", lab)
        ),
      zone = ggtitle(
        paste("Average Price Of", nm, "In All Geo-Political Zone In", lab)
        )
    )
    
  } else if (type == "some") {
    add_plot <- list(
      geom_text(aes(label = comma(round(average_price)), x = txt_pos),
                size = 2.5, color = "grey15"),
      theme(axis.text.x = element_blank())
    )
    
  } else {
    message("`type` can only be either 'all' or 'some'")
  }
  
  return(add_plot)
}



#' Calculate the average  price of commodity by year.
#'
#' @param df data.frame
#' @param by character: either 'state' or 'zone'.
#' @param p_year numeric: The year to filter by.
#' @param color vector max of 4 min of 2 colors for the plot e.g 'blue', 'red'...
#' @param typ character: type of output 'plt' for plot or 'tbl' for table.
#' 
#' @note Used right_arg, om_ifelse, numeric_smy, sub_dep, plot_layer_apy &
#'default_colors.
#'
#' @return
#' @export
#'
#' @examples
average_price_year <- function(df, 
                               by = "state", 
                               p_year, 
                               color, 
                               typ = "plt") {
  right_arg(r_df = df, 
            in_loc = by,
            btw = list(check = p_year, between = c(2017, 2021)),
            r_typ = typ,
            arg_nm = list(in_loc = "by", btw = "p_year"))
  
  if (by == "state") {
    f_tbl <- df %>% 
      group_by(state, year, commodity) %>% 
      numeric_smy()
    
  } else if (by == "zone") {
    f_tbl <- df %>% 
      group_by(zone, year, commodity) %>% 
      numeric_smy()
  }
  
  f_tbl <- f_tbl %>% dplyr::filter(year == p_year)

  no_unique_commodity <- unique(f_tbl$commodity) |> length()
  
  text_pos <- quantile(df$price, 0.05)
  label <- as.character(p_year)
  name  <- sub_dep(df) |> df_title()

  sl_colr <- om_ifelse(missing(color),
                       default_colors(no_unique_commodity),
                       color)
  
  num_cols <- list(`2` = 2, `3` = 3, `4` = 4)
  num_cols <- num_cols[[as.character(no_unique_commodity)]]
  
  if (by == "state") {
    f_plt <- f_tbl %>% 
      ggplot(aes(average_price, reorder_within(state, average_price, commodity),
                 fill = commodity))
    
    if (no_unique_commodity == 2) {
      f_plt <- f_plt +
        plot_layer_apy('all', by, name, label, sl_colr) +
        facet_wrap(vars(commodity), ncol = 2, scales = "free")
      
    } else {
      f_plt <- f_plt +
        plot_layer_apy('all', by, name, label, sl_colr) +
        plot_layer_apy('some', txt_pos = text_pos) +
        facet_wrap(vars(commodity), ncol = num_cols, scales = "free")
    } 
    
  } else if (by == "zone") {
    f_plt <- f_tbl %>% 
      ggplot(aes(average_price, reorder_within(zone, average_price, commodity),
                 fill = commodity))
    
    if (no_unique_commodity == 2) {
      f_plt <- f_plt +
        plot_layer_apy('all', by, name, label, sl_colr) +
        facet_wrap(vars(commodity), ncol = 2, scales = "free")
      
    } else {
      f_plt <- f_plt +
        plot_layer_apy('all', by, name, label, sl_colr) +
        plot_layer_apy('some', txt_pos = text_pos) +
        facet_wrap(vars(commodity), ncol = num_cols, scales = "free")
    } 
  }
    
  output <- switch(typ,
                   plt = f_plt,
                   tbl = f_tbl)
  return(output)
}




#' Price Over The Years Plot Layer
#'
#' @param by character: Either 'state' or 'zone'.
#' @param no numeric: Number of commodities for the color selection.
#' @param colrs vector: Colors for the plot, must be the same as number of lines.
#' @param max_p numeric: Maximum price of the commodity for the break.
#' @param increaseBy numeric: Number to increase the break seq by.
#' @param nm character: The name of the commodity.
#' @param l_loc character: The location to visualize.
#'
#' @return
#' @export
#'
#' @examples
plot_layer_poy <- function(by, no, colrs, max_p, increaseBy, nm, l_loc) {
  add_plot <- list(
    geom_line(),
    scale_y_continuous(breaks = seq(0, max_p+50, increaseBy),
                       labels = comma_format()),
    scale_color_manual(values = colrs, name = nm),
    scale_x_date(date_breaks = "12 months", date_labels = "%b %Y"),
    labs(x = "", y = ""),
    theme_minimal()
  )
  
  add_plot[[length(add_plot)+1]] <- switch(
    by,
    state = ggtitle(paste("Price Of", nm, "In", l_loc, "State")),
    zone = ggtitle(
        paste("Average Price Of", nm, "In", l_loc, "Geo-Political Zone"))
  )
  
  return(add_plot)
}



#' Price Of Commodities Over the Years
#'
#' @description create a line plot summary of price of commodity by selected
#' location
#' 
#' @param df data.frame
#' @param loc character: the location to visualize either from state or zone.
#' @param increase_by numeric: number to increase the break seq by.
#' @param color character: colors for the plot, must be the same as the number of
#' lines.
#' @param typ character:  the type of output either 'plt' for plot or 'tbl' for 
#' table.
#' 
#' @note Used om_ifelse, right_arg, plot_layer_poy & default_colors function
#' @return
#' @export
#'
#' @examples
price_over_years <- function(df, loc, increase_by, color, typ = "plt") {
  
  loc <- str_to_title(loc)

  right_arg(r_df = df,
            r_locs = loc,
            arg_nm = list(r_locs = "loc"), 
            extra_info = list(r_locs = "state or zone"),
            r_typ = typ)
  
  state_nm <- unique(df$state)
  zone_nm  <- unique(df$zone)
  
  if (loc %in% state_nm) {
    f_tbl <- df %>% dplyr::filter(state == loc)
    
    bk <- breakz(f_tbl, price)
    
  } else if (loc %in% zone_nm) {
    f_tbl <- df %>% dplyr::filter(zone == loc) %>% 
      group_by(zone, commodity, date) %>% 
      numeric_smy()
    
    bk <- breakz(f_tbl, average_price)
  }
  
  in_by <- ifelse(missing(increase_by),
                  bk$by,
                  increase_by)
  
  no_unique_commodity <- unique(f_tbl$commodity) |> length()
  name <- substitute(df) %>% deparse() %>% df_title()
  
  colour <- om_ifelse(missing(color),
                      default_colors(no_unique_commodity, TRUE),
                      color)
  
  if (loc %in% state_nm) {
    f_plt <- f_tbl %>% 
      ggplot(aes(date, price, color = commodity)) +
      plot_layer_poy("state", no_unique_commodity, colour, bk$to, in_by, 
                     name, loc)
    
  } else if (loc %in% zone_nm) {
    f_plt <- f_tbl %>% 
      ggplot(aes(date, average_price, color = commodity)) +
      plot_layer_poy("zone", no_unique_commodity, colour, bk$to, in_by, 
                     name, loc)
  }
  
  output <- switch(typ,
                   plt = f_plt,
                   tbl = f_tbl)
  return(output)
}



#' Average price by months
#'
#' @description create an average price of commodity plot by months for a
#' selected state.
#' 
#' @param df data.frame
#' @param loc character: the state location to visualize from e.g 'Oyo'.
#' @param yr_period vector: the range years to use e.g  2018, 2020.
#' @param color vector: colors for the plot, must be the same as the number of 
#' lines.
#' @param typ character: the type of output either 'plt' for plot or 'tbl' for table.
#'
#' @note Used right_arg, sub_dep, om_ifelse & default_colors functions
#' @return
#' @export
#'
#' @examples
average_price_month <- function(df, loc, yr_period, color, typ = "plt") {
  loc <- str_to_title(loc)

  right_arg(r_df = df,
            r_locs = loc,
            len= list(check= yr_period, is= expression(length(len$check) == 2)),
            arg_nm = list(r_locs = "loc", len = "yr_period"),
            extra_info = list(r_locs = "state", len = "equal to 2"),
            r_typ = typ)
  
  if (missing(yr_period)) {
    f_tbl <- df %>% 
      dplyr::filter(state == loc) %>%
      group_by(state, commodity, month_label) %>% 
      numeric_smy()  
    
  yr_label <- "2017-2021"
      
  } else {
    f_tbl <- df %>% 
      dplyr::filter(state == loc, 
                    between(year, min(yr_period), max(yr_period))) %>%
      group_by(state, commodity, month_label) %>% 
      numeric_smy() 
    
    yr_label <- str_glue("{min(yr_period)}-{max(yr_period)}")
  }
  no_unique_commodity <- unique(f_tbl$commodity) |> length()
  name <- sub_dep(df) |> df_title()
  
  bk <- breakz(f_tbl, average_price)
  
  colour <- om_ifelse(missing(color),
                      default_colors(no_unique_commodity, TRUE),
                      color)
  
  f_plt <- f_tbl %>% 
    ggplot(aes(month_label, average_price, group = commodity, color = commodity)) +
    geom_line() +
    scale_y_continuous(breaks = seq(0, bk$to+50, bk$by)) +
    scale_x_discrete(label = month.abb) +
    scale_color_manual(values = colour, name = name) +
    labs(x = "", y = "", title = paste(
      "Average Price Of", name, "In", loc, "State From", yr_label)) +
    theme_minimal()
  
  output <- switch(typ,
                   plt = f_plt,
                   tbl = f_tbl)
  return(output)
}




#' Comparing Commodity Price Plot Layout.
#'
#' @param by character: either 'state' or 'zone'
#' @param max_val numeric: max value for the breaks seq.
#' @param increaseBy numeric: by value for the breaks seq.
#' @param colour vector: of length not > 3 or < 2 colors.
#' @param nm character: name of the data.
#' @param lab vector: containing the states/zones to use as the plot label.
#'
#' @return
#' @export
#'
#' @examples
compare_plot_layer <- function(by, max_val, increaseBy, colour, nm, lab) {
  add_layer <- list(
    geom_line(),
    scale_x_date(date_breaks = "12 months", date_labels = "%b %Y"),
    scale_y_continuous(breaks = seq(0, max_val+50, increaseBy)),
    scale_color_manual(values = colour),
    labs(x = "", y = "", color = "State"),
    theme_minimal()
  )
  
  lab <- str_replace_all(lab, "_", " ")
  
  if (length(lab) == 2) label <- str_glue("{lab[1]} & {lab[2]}")
  else if (length(lab) == 3) label <- str_glue("{lab[1]}, {lab[2]} & {lab[3]}")
  
  add_layer[[length(add_layer)+1]] <- switch(
    by,
    state = ggtitle(paste(
      "Price Of", nm, "From 2017-2021 In", label, "State")),
    zone = ggtitle(paste(
      "Average Price Of", nm, "From 2017-2021 In", label, "Geo-political Zone"))
  )
  
  return(add_layer)
}



#' Comparing Commodity Price
#'
#' @description compare the price commodities for selected states over time.
#' 
#' @param df data.frame
#' @param locs vector: containing the states/zones to use.
#' @param color vector: of the same number of states/zones.
#' @param increaseBy numeric: by value for the breaks seq.
#' @param typ character: Output type, either 'plt' for plot or 'tbl' for table.
#' 
#' @note Used right_arg, breakz, om_ifelse & sub_dep functions
#' @return
#' @export
#'
#' @examples
compare_price <- function(df, locs, color, increaseBy, typ = "plt") {
  locs <- str_to_title(locs)

  right_arg(r_df = df,
            r_locs = locs, 
            len = list(check = locs, is = expression((length(len$check) == 2 | 
                                                      length(len$check) == 3))),
            unq_vals = locs,
            r_typ = typ,
            arg_nm = list(r_locs = "locs", len = "locs", unq_vals = "locs"),
            extra_info = list(r_locs = "2 or 3 states/zones", 
                              len = 'either 2 or 3'))

  state_nm <- unique(df$state)
  zone_nm  <- unique(df$zone)
  

  if (all(locs %in% state_nm)) {
    f_tbl <- df %>% 
      filter(state %in% locs)
    
    bk <- breakz(f_tbl, price)
    
  } else if (all(locs %in% zone_nm)) {
    f_tbl <- df %>% 
      filter(zone %in% locs) %>% 
      group_by(zone, commodity, date) %>% 
      numeric_smy()
    
    bk <- breakz(f_tbl, average_price)
  }
  
  no_unique_commodity <- unique(f_tbl$commodity) |> length()
  
  name <- sub_dep(df) |> df_title()
  
  in_by <- ifelse(missing(increaseBy),
                  bk$by,
                  increaseBy)
  
  colour <- om_ifelse(missing(color),
                      default_colors(length(locs), TRUE),
                      color)
  
  if (all(locs %in% state_nm)) {
    f_plt <- f_tbl %>% ggplot(aes(date, price, color = state))
    
    if (no_unique_commodity == 2) {
      f_plt <- f_plt +
        compare_plot_layer("state", bk$to, in_by, colour, name, locs) +
        facet_wrap(vars(commodity), ncol = 2, scale = "free") +
        theme(axis.text.x = element_text(angle = 45))
      
    } else if (no_unique_commodity == 3) {
      f_plt <- f_plt +
        compare_plot_layer("state", bk$to, in_by, colour, name, locs) +
        facet_wrap(vars(commodity), nrow = 3, scale = "free_y")
      
    } else if (no_unique_commodity == 4) {
      f_plt <- f_plt +
        compare_plot_layer("state", bk$to, in_by, colour, name, locs) +
        facet_wrap(vars(commodity), nrow = 2, scale = "free_y")+
        theme(axis.text.x = element_text(angle = 45))
      
    } else {
      stop("Number of Unique commodity should be between 2 & 4")
    }
    
  } else if (all(locs %in% zone_nm)) {
    f_plt <- f_tbl %>% ggplot(aes(date, average_price, color = zone))
    
    if (no_unique_commodity == 2) {
      f_plt <- f_plt +
        compare_plot_layer("zone", bk$to, in_by, colour, name, locs) +
        facet_wrap(vars(commodity), ncol = 2, scale = "free")+
        theme(axis.text.x = element_text(angle = 45))
      
    } else if (no_unique_commodity == 3) {
      f_plt <- f_plt +
        compare_plot_layer("zone", bk$to, in_by, colour, name, locs) +
        facet_wrap(vars(commodity), nrow = 3, scale = "free_y")
      
    } else if (no_unique_commodity == 4) {
      f_plt <- f_plt +
        compare_plot_layer("zone", bk$to, in_by, colour, name, locs) +
        facet_wrap(vars(commodity), nrow = 2, scale = "free_y")+
        theme(axis.text.x = element_text(angle = 45))
      
    } else {
      stop("Number of Unique commodity should be between 2 & 4")
    }
  }
  
  output <- switch(typ,
                   plt = f_plt,
                   tbl = f_tbl)
  return(output)
}




#' Comparing Average Price by Months Plot Layer
#'
#' @param by character: either 'state' or 'zone'
#' @param max_val numeric: price max value.
#' @param increaseBy numeric: value to increase the plot axis breaks.
#' @param colour vector: of not > 3 or < 2 colors.
#' @param nm character: name of the data.
#' @param lab vector: containing the states/zones to use for the plot label.
#'
#' @return
#' @export
#'
#' @examples
compare_plot_layer_m <- function(by, max_val, increaseBy, colour, nm, lab) {
  
  add_layer <- list(
    geom_line(),
    scale_x_discrete(labels = month.abb),
    scale_y_continuous(breaks = seq(0, max_val+50, increaseBy)),
    scale_color_manual(values = colour),
    labs(x = "", y = "", color = "State"),
    theme_minimal()
  )
  
  lab <- str_replace_all(lab, "_", " ")
  
  if (length(lab) == 2) label <- str_glue("{lab[1]} & {lab[2]}")
  else if (length(lab) == 3) label <- str_glue("{lab[1]}, {lab[2]} & {lab[3]}")
  
  # title ---------------------------------------------------------------------|
  add_layer[[7]] <- switch(
    by,
    state = ggtitle(paste(
      "Average Price Of", nm, "From 2017-2021 For", label, "State")),
    zone = ggtitle(paste(
      "Average Price Of", nm, "From 2017-2021 For", label, "Geo-political Zone"))
  )
  
  return(add_layer)
}



#' Comparing Average Price by Months.
#' 
#' @description compare the average price of commodity by month for all selected
#' state or zone.
#' 
#' @param df data.frame
#' @param locs vector: containing the states/zones to use.
#' @param colors vector: of the same number of states/zones.
#' @param increaseBy numeric: value to increase the breaks by.
#' @param typ character: the type of output either 'plt' for plot or 'tbl' for 
#' table.
#'
#' @note breakz, right_arg, om_ifelse, default_colors & compare_plot_layer_m 
#' @return
#' @export
#'
#' @examples
compare_price_month <- function(df, locs, colors, increaseBy, typ = "plt") {
  locs <- str_to_title(locs)

  right_arg(r_df = df,
            r_locs = locs, 
            len = list(check = locs, is = expression((length(len$check) == 2 | 
                                                      length(len$check) == 3))),
            unq_vals = locs,
            r_typ = typ,
            arg_nm = list(r_locs = "locs", len = "locs", unq_vals = "locs"),
            extra_info = list(r_locs = "2 or 3 states/zones", 
                              len = 'either 2 or 3'))

  state_nm <- unique(df$state)
  zone_nm  <- unique(df$zone)
  
  if (all(locs %in% state_nm)) {
    f_tbl <- df %>% 
      group_by(state, commodity, month_label) %>% 
      numeric_smy() %>% 
      dplyr::filter(state %in% locs)
    
  } else if (all(locs %in% zone_nm)) {
    f_tbl <- df %>% 
      group_by(zone, commodity, month_label) %>%
      numeric_smy() %>% 
      dplyr::filter(zone %in% locs)
  }
  
  bk <- breakz(f_tbl, average_price)
  
  no_unique_commodity <- unique(f_tbl$commodity) |> length()
  name <- sub_dep(df) |> df_title()
  
  in_by <- ifelse(missing(increaseBy), bk$by, increaseBy)
  
  colour <- om_ifelse(missing(colors),
                      default_colors(length(locs), TRUE),
                      colors)
  
  if (all(locs %in% state_nm)) {
    f_plt <- f_tbl %>% 
      ggplot(aes(month_label, average_price, group = state, color = state))
    
    if (no_unique_commodity == 2) {
      f_plt <- f_plt +
        compare_plot_layer_m("state", bk$to, in_by, colour, name, locs) +
        facet_wrap(vars(commodity), ncol = 2, scale = "free") +
        theme(axis.text.x = element_text(angle = 45))
        
    } else if (no_unique_commodity == 3) {
      f_plt <- f_plt +
        compare_plot_layer_m("state", bk$to, in_by, colour, name, locs) +
        facet_wrap(vars(commodity), nrow = 3, scale = "free_y")
      
    } else if (no_unique_commodity == 4) {
      f_plt <- f_plt +
        compare_plot_layer_m("state", bk$to, in_by, colour, name, locs) +
        facet_wrap(vars(commodity), nrow = 2, scale = "free_y")+
        theme(axis.text.x = element_text(angle = 45))
      
    } else {
      stop("Number of Unique commodity should be between 2 & 4")
    }
    
  } else if (all(locs %in% zone_nm)) {
    f_plt <- f_tbl %>% 
      ggplot(aes(month_label, average_price, group = zone, color = zone))
    
    if (no_unique_commodity == 2) {
      f_plt <- f_plt +
        compare_plot_layer_m("zone", bk$to, in_by, colour, name, locs) +
        facet_wrap(vars(commodity), ncol = 2, scale = "free")+
        theme(axis.text.x = element_text(angle = 45))
      
    } else if (no_unique_commodity == 3) {
      f_plt <- f_plt +
        compare_plot_layer_m("zone", bk$to, in_by, colour, name, locs) +
        facet_wrap(vars(commodity), nrow = 3, scale = "free_y")
      
    } else if (no_unique_commodity == 4) {
      f_plt <- f_plt +
        compare_plot_layer_m("zone", bk$to, in_by, colour, name, locs) +
        facet_wrap(vars(commodity), nrow = 2, scale = "free_y")+
        theme(axis.text.x = element_text(angle = 45))
      
    } else {
      stop("Number of Unique commodity should be between 2 & 4")
    }
  }
 
  output <- switch(typ,
                   plt = f_plt,
                   tbl = f_tbl)
  return(output)
}




month_name <- function(month_number) {
  
  month_nm <- list()
  
  for (i in seq_along(month.name)) {
    month_nm <- append(month_nm, month.name[[i]])
  }
  
  month_nm[[month_number]]
}



#' Filter price by date plot layout
#'
#' @param by character: either 'state' or 'zone'
#' @param txt_pos where to position the text.
#' @param nm character: name of the data.
#' @param colr vector: a list of colors to use.
#' @param month.nm character: the name of the month for the plot title.
#' @param year numeric: the year of the analysis.
#'
#' @return
#' @export
#'
#' @examples
add_plot_layer_fd <- function(by, txt_pos, nm, colr, month.nm, year) {
  add_layer <- list(
    geom_col(show.legend = FALSE),
    scale_y_reordered(),
    geom_text(aes(label = switch(by, 
                                 state = comma(price, 1), 
                                 zone  = comma(average_price, 1)), x = txt_pos), 
              size = 2.5, color = "grey15"),
    scale_fill_manual(values = colr),
    labs(x = "", y = ""),
    theme_minimal(),
    theme(axis.text.x = element_blank())
  )
  
  add_layer[[length(add_layer)+1]] <- switch(
    by,
    state = ggtitle(
      paste("Price of", nm, "Across All State In", month.nm, year)),
    zone  = ggtitle(
      paste("Average Price of", nm, "For Each Zone In", month.nm, year))
  )
  
  return(add_layer)
}



#' Title
#' @description create the price of commodity across all state/zone for the 
#' selected month and year.
#' 
#' @param df data.frame.
#' @param f_year numeric: Year to use for the analysis from 2017 to 2021.
#' @param month.no numeric: A month number from 1-12.
#' @param by character: Run analysis for either 'state' or 'zone'.
#' @param color vector: colors for the plot.
#' @param typ character: The kind of output either 'plt' for plot or 'tbl' for table
#'
#' @note Used right_arg, om_ifelse, default_colors, add_plot_layer_fd & month_name
#' @return
#' @export
#'
#' @examples
filter_date <- function(df, f_year, month.no, by, color, typ = "plt") {

  right_arg(r_df = df, 
            btw = list(check = f_year, between = c(2017, 2021)),
            btw_m = month.no,
            in_loc = by,
            r_typ = typ,
            arg_nm = list(btw = "f_year", btw_m = "month.no", in_loc = "by"))

  if (by == "state") {
    f_tbl <- df %>% 
      dplyr::filter(year == f_year, month == month.no)
    
  } else if (by == "zone") {
    f_tbl <- df %>% 
      dplyr::filter(year == f_year, month == month.no) %>% 
      group_by(zone, commodity) %>% 
      numeric_smy()
  }
  
  no_unique_commodity <- unique(f_tbl$commodity) |> length()
  name <- sub_dep(df) |> df_title()
  
  text_pos <- switch(by,
                     state = quantile(f_tbl$price, 0.10),
                     zone  = quantile(f_tbl$average_price, 0.10))
  
  month_nm <- month_name(month.no)
  
  colour <- om_ifelse(missing(color),
                      default_colors(no_unique_commodity),
                      color)
  
  if (by == "state") {
    f_plt <- f_tbl %>% 
     ggplot(aes(price, reorder_within(state, price, commodity), fill = commodity))
    
    if (no_unique_commodity == 2) {
      f_plt <- f_plt +
        add_plot_layer_fd(by, text_pos, name, colour, month_nm, f_year) +
        facet_wrap(vars(commodity), ncol = 2, scales = "free") 
      
    } else if (no_unique_commodity == 3) {
      f_plt <- f_plt +
        add_plot_layer_fd(by, text_pos, name, colour, month_nm, f_year) +
        facet_wrap(vars(commodity), ncol = 3, scales = "free")
      
    } else if (no_unique_commodity == 4) {
      f_plt <- f_plt +
        add_plot_layer_fd(by, text_pos, name, colour, month_nm, f_year) +
        facet_wrap(vars(commodity), ncol = 4, scales = "free")
    }
    
  } else if (by == "zone") {
    f_plt <- f_tbl %>% 
      ggplot(aes(average_price, reorder_within(zone, average_price, commodity), 
                 fill = commodity))
    
    if (no_unique_commodity == 2) {
      f_plt <- f_plt +
        add_plot_layer_fd(by, text_pos, name, colour, month_nm, f_year) +
        facet_wrap(vars(commodity), ncol = 2, scales = "free")
      
    } else if (no_unique_commodity == 3) {
      f_plt <- f_plt +
        add_plot_layer_fd(by, text_pos, name, colour, month_nm, f_year) +
        facet_wrap(vars(commodity), ncol = 3, scales = "free")
      
    } else if (no_unique_commodity == 4) {
      f_plt <- f_plt +
        add_plot_layer_fd(by, text_pos, name, colour, month_nm, f_year) +
        facet_wrap(vars(commodity), ncol = 2, scales = "free_y")
    }
  }

  output <- switch(typ,
                   plt = f_plt,
                   tbl = f_tbl)
  return(output)
}




#' Minimum & Maximum Commodity Price Plot Layout
#'
#' @param l_by character: the variable to plot by either 'state' or 'zone'.
#' @param nm character: name of the data.
#' @param colr vector: of length 2, colors for the points.
#' @param max_val numeric: max value for the breaks seq.
#' @param in_by numeric: value to increase the break seq.
#' @param rm_bk bool: whether to use custom breaks or not.
#' @param yr_l year label.
#'
#' @return
#' @export
#'
#' @examples
min_max_plot_layer <- function(l_by, nm, colr, max_val, in_by, rm_bk, yr_l) {

  state_lst <- list(
    geom_segment(aes(x = minimum_price, xend = maximum_price, 
                     y = state, yend = state), color = "grey"),
    geom_point(aes(x = minimum_price, state), color = colr[1], size = 3),
    geom_point(aes(x = maximum_price, state), color = colr[2], size = 3),
    labs(x = "", y = "", 
         title = str_glue("<span style = 'font-size:13pt'> 
                           <span style='color:{colr[1]};'>Minimum</span> &
                           <span style='color:{colr[2]};'>Maximum</span>
                           Price Of {nm} In All States {yr_l} </span>")),
    theme_minimal(),
    theme(plot.title = ggtext::element_markdown(lineheight = 1.1))
  )
  
  zone_lst <- list(
    geom_segment(aes(x = minimum_price, xend = maximum_price, 
                     y = zone, yend = zone), color = "grey"),
    geom_point(aes(x = minimum_price, zone), color = colr[1], size = 3),
    geom_point(aes(x = maximum_price, zone), color = colr[2], size = 3),
    labs(x = "", y = "",
         title = str_glue("<span style = 'font-size:11pt'> 
                           <span style='color:{colr[1]};'>Minimum</span> &
                           <span style='color:{colr[2]};'>Maximum</span>
                           Price Of {nm} In All Geo-political Zones {yr_l}
                           </span>")),
    theme_minimal(),
    theme(plot.title = ggtext::element_markdown(lineheight = 1.1))
  )
  
  if (rm_bk == FALSE) {
    state_lst[[length(state_lst)+1]] <- 
      scale_x_continuous(breaks = seq(0, max_val+50, in_by))
    zone_lst[[length(zone_lst)+1]]  <-
      scale_x_continuous(breaks = seq(0, max_val+50, in_by))
  }
  
  add_layer <- switch(l_by,
                      state = state_lst,
                      zone  = zone_lst)
  return(add_layer)
}



#' Minimum & Maximum Commodity Price
#'
#' @description create a minimum and maximum plot of price for all state or
#' zone within a specified year if given.
#' 
#' @param df data.frame
#' @param by character: either zone or state. 'not quoted'
#' @param f_year 
#' @param rm_breaks  bool: whether to use custom breaks or not.
#' @param increaseBy numeric: value to increase the break seq.
#' @param color vector: of length 2, colors for the points.
#' @param typ character: the type of output either 'plt' or 'tbl'.
#'
#' @note Used right_arg, sub_dep, numeric_smy, min_max_plot_layer
#' @return
#' @export
#'
#' @examples
min_max_price <- function(df,
                          by, 
                          f_year, 
                          rm_breaks = FALSE, 
                          increaseBy, 
                          color,
                          typ = "plt") {
  
  right_arg(r_df = df, 
            in_loc = sub_dep(by), 
            btw = list(check = f_year, between = c(2017, 2021)),
            r_typ = typ,
            arg_nm = list(in_loc = "by", btw = "f_year"))
  
  if (missing(f_year)) {
    f_tbl <- df %>% 
      dplyr::filter(price != 0) %>% 
      group_by({{ by }}, commodity) %>% 
      numeric_smy(FALSE)
    
  } else {
    f_tbl <- df %>% 
      dplyr::filter(price != 0, year == f_year) %>%
      group_by({{ by }}, commodity) %>% 
      numeric_smy(FALSE)
  }
  
  no_unique_commodity <- unique(f_tbl$commodity) |> length()
  
  name <- sub_dep(df) |> df_title()
  
  bk <- breakz(f_tbl, maximum_price)
  
  increaseBy <- ifelse(missing(increaseBy),
                       bk$by,
                       increaseBy)
  
  yr_label <- ifelse(missing(f_year),
                     "From 2017-2021",
                     paste("In", f_year))

  colour <- om_ifelse(missing(color),
                      c("#33b864", "#ca0147"),
                      color)
  
  f_plt <- ggplot(data = f_tbl)
  
  if (sub_dep(by) == "state") {
    if (no_unique_commodity == 2) {
      f_plt <- f_plt +
        min_max_plot_layer("state", name, colour, bk$to, increaseBy, rm_breaks,
                           yr_label) +
        facet_wrap(vars(commodity), ncol = 2, scales = "free_x")
      
    } else if (no_unique_commodity == 3) {
      f_plt <- f_plt +
        min_max_plot_layer("state", name, colour, bk$to, increaseBy, rm_breaks,
                           yr_label) +
        facet_wrap(vars(commodity), ncol = 3, scales = "free_x")
      
    } else if (no_unique_commodity == 4) {
      f_plt <- f_plt +
        min_max_plot_layer("state", name, colour, bk$to, increaseBy, rm_breaks,
                           yr_label) +
        facet_wrap(vars(commodity), ncol = 4, scales = "free_x")
      
    } else {
      stop("Number of unique commodity must not be < 2 or > 4")
    }
    
  } else if (sub_dep(by) == "zone") {
    if (no_unique_commodity == 2) {
      f_plt <- f_plt +
        min_max_plot_layer("zone", name, colour, bk$to, increaseBy, rm_breaks,
                           yr_label) +
        facet_wrap(vars(commodity), ncol = 2, scales = "free_x")
      
    } else if (no_unique_commodity == 3) {
      f_plt <- f_plt +
        min_max_plot_layer("zone", name, colour, bk$to, increaseBy, rm_breaks,
                           yr_label) +
        facet_wrap(vars(commodity), ncol = 3, scales = "free_x")
      
    } else if (no_unique_commodity == 4) {
      f_plt <- f_plt +
        min_max_plot_layer("zone", name, colour, bk$to, increaseBy, rm_breaks,
                           yr_label) +
        facet_wrap(vars(commodity), ncol = 4, scales = "free_x")
      
    } else {
      stop("Number of unique commodity must not be < 2 or > 4")
    }
  }
  
  output <- switch(typ,
                   tbl = f_tbl,
                   plt = f_plt)
  return(output)
}




#' Price by Zone Group Plot Layout
#'
#' @param nm character: name of the data.
#' @param max_val numeric: max value for the break seq.
#' @param increseBy numeric: value to increase the break seq by.
#' @param color vector: colors for the plot depending on the number of state.
#' @param z 
#'
#' @return
#' @export
#'
#' @examples
plot_layer_zg <- function(nm, max_val, increseBy, color, z) {
  add_layer <- list(
    geom_line(size = 0.6),
    labs(x = "", y = "", color = "State",
         title = paste("Average Price Of", nm, "In", z, "Zone")),
    scale_y_continuous(breaks = seq(0, max_val+50, increseBy)),
    scale_color_manual(values = color),
    theme_minimal()
  )
  
  return(add_layer)
}



#' Price by Zone Group
#'
#' @description create an average price plot of a commodity for all states 
#' in a selected zone.
#' 
#' @param df data.frame.
#' @param gpz character: Unique zone in the data  'df'.
#' @param colors vector: Colors for the plot depending on the number of state.
#' @param increaseBy numeric: Value to increase the break seq by.
#' @param typ character: Output type
#'
#' @note Used right_arg, sub_dep, plot_layer_zg.
#' @return
#' @export
#'
#' @examples
zone_group <- function(df, gpz, colors, increaseBy, typ = "plt") {
  right_arg(r_df = df, 
            r_locs = gpz,
            r_typ = typ,
            arg_nm = list(r_locs = "gpz"))
  
  f_tbl <- df %>% 
    dplyr::filter(zone == gpz) %>% 
    group_by(state, commodity, year) %>% 
    numeric_smy()
  
  # number of unique commodity, data name, max break seq value, value to increase
  # break seq by, color.
  no_unique_commodity <- unique(f_tbl$commodity) |> length()
  
  name <- sub_dep(df) |> df_title()
  
  bk <- breakz(f_tbl, average_price)
  
  in_by <- ifelse(missing(increaseBy), bk$by, increaseBy)
  
  no_color <- unique(f_tbl$state) |> length()
  
  colr <- c("#bf9005", "#ec2d01", "#4da409", "#24bca8", "#0203e2", "#f504c9",
            "#88b378")
  colour <- om_ifelse(missing(colors),
                      sample(colr, no_color),
                      colors)

  f_plt <- f_tbl %>% 
    ggplot(aes(year, average_price, color = state, group = state))
  
  if (no_unique_commodity == 2) {
    f_plt <- f_plt +
      plot_layer_zg(name, bk$to, in_by, colour, gpz) +
      facet_wrap(vars(commodity), ncol = 2, scales = "free")
    
  } else if (no_unique_commodity == 3) {
    f_plt <- f_plt +
      plot_layer_zg(name, bk$to, in_by, colour, gpz) +
      facet_wrap(vars(commodity), ncol = 1, scales = "free_y")
    
  } else if (no_unique_commodity == 4) {
    f_plt <- f_plt +
      plot_layer_zg(name, bk$to, in_by, colour, gpz) +
      facet_wrap(vars(commodity), ncol = 2, scales = "free_y")
    
  } else {
    stop("number of unique commodity should be between 2 & 4")
  }
  
  output <- switch(typ,
                   plt = f_plt,
                   tbl = f_tbl)
  return(output)
}




