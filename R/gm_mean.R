#' Calculate the geometric mean
#'
#' \code{gm_mean} takes as input a numeric vector and returns the geometric mean
#'
#' When there are no zeroes in a vector of numbers, then the geometric mean is
#' defined as the nth root of the product of all the values \emph{and is
#' equivalently defined as} exp(1/n * the sum of the logs of each value).
#' However, those two are not equivalent if there are zeroes!
#'
#' This function gives the option of propagating zeroes. If you want to get a 0
#' when the vector includes 0 values, that's the definition of the nth root of
#' the product of all values, so that would be zero.propagate = TRUE.
#'
#' Alternatively, if you want to just ignore 0 values, you can choose
#' zero.propagate = FALSE. Then, either definition works, and, since R has a
#' built-in sum(...) function but not a built-in product(...) function, you use
#' the definition where the geometric \eqn{mean = exp(1/n * the sum of the logs of
#' each value)} and that will give you an actual number if you ignore the zeroes.
#'
#' Note that the geometric mean is undefined for negative numbers.
#'
#' @param x A vector of numbers
#' @param na.rm Should NA values be removed? (logical)
#' @param zero.propagate Should zeroes be propagated? (logical)
#'
#' @return Returns a number
#'
#' @examples
#' gm_mean(rnorm(10, 5, 1))
#'
#' @export

gm_mean <- function(x, na.rm=TRUE, zero.propagate = FALSE){
   
   # If any values are negative, return NaN.
   if(any(x < 0, na.rm = TRUE)){
      return(NaN)
   }
   
   if(zero.propagate){
      if(any(x == 0, na.rm = TRUE)){
         return(0)
      }
   } else {
      # If user doesn't want to propagate zeroes, then remove them from
      # the vector.
      x <- x[x > 0]
   }
   
   return(exp(mean(log(x), na.rm = na.rm)))
   
}


#' Calculate the geometric standard deviation
#'
#' \code{gm_sd} takes as input a numeric vector and returns the geometric
#' standard deviation
#'
#' Per Wikipedia's entry on the geometric standard deviation,
#' \href{https://en.wikipedia.org/wiki/Geometric_standard_deviation}{https://en.wikipedia.org/wiki/Geometric_standard_deviation},
#' the geometric standard deviation is dimensionless. See that entry for the
#' formula used.
#'
#' Also, note that the way you determine the range of the data for the geometric
#' mean and one geometric standard deviation is NOT \eqn{geometric mean value
#' +/- the geometric standard deviation}! Instead, it is \eqn{geometric mean /
#' geometric standard deviation} to \eqn{geometric mean * geometric standard
#' deviation}!
#'
#'
#' @param x A vector of numbers
#' @param na.rm Should NA values be removed? (logical)
#' @param zero.propagate Should zeroes be propagated? (logical)
#'
#' @examples
#'
#' gm_sd(rnorm(10, 5, 1))
#' gm_sd(c(5, 3, 6, 10, 2, 0), zero.propagate = TRUE)
#'
#' @export

gm_sd <- function(x, na.rm = TRUE, zero.propagate = FALSE) {
   
   if(na.rm){
      x <- x[complete.cases(x)]
   }
   
   # If any values are negative, return NaN.
   if(any(x < 0, na.rm = TRUE)){
      return(NaN)
   }
   
   if(zero.propagate){
      if(any(x == 0, na.rm = TRUE)){
         return(0)
      }
   } else { # If you don't want to propagate zeroes, then remove them from the vector.
      x <- x[x > 0]
   }
   
   # Now, proceed with whatever your vector is after removing zeroes (or not
   # removing them if zero.propagate was TRUE but there weren't any zeroes
   # to start with anyway.)
   ToSum <- rep(NA, length(x))
   # There's probably a way to do this next bit without actually writing
   # a loop, but I'm not sure how.
   for(i in 1:length(x)){
      ToSum[i] <- (log(x[i]/gm_mean(x)))^2
   }
   
   return(exp(sqrt(sum(ToSum)/(length(x))))) # This is population geometric
   # standard deviation. For *sample* geometric sd, you'd want the denominator
   # to be N-1 instead of N.
}



#' Calculate the geometric coefficient of variation (CV)
#'
#' \code{gm_CV} takes as input a numeric vector and returns the geometric
#' CV, calculated as: sqrt(exp(sd(log(x))^2)-1)
#'
#' @param x A vector of numbers
#' @param na.rm Should NA values be removed? (logical)
#' @param zero.propagate Should zeroes be propagated? (logical)
#'
#' @examples
#'
#' gm_CV(rnorm(10, 5, 1))
#' gm_CV(c(5, 3, 6, 10, 2, 0), zero.propagate = TRUE)
#'
#' @export

gm_CV <- function(x, na.rm = TRUE, zero.propagate = FALSE) {
   
   if(na.rm){
      x <- x[complete.cases(x)]
   }
   
   # If any values are negative, return NaN.
   if(any(x < 0, na.rm = TRUE)){
      return(NaN)
   }
   
   if(zero.propagate){
      if(any(x == 0, na.rm = TRUE)){
         return(0)
      }
   } else { # If you don't want to propagate zeroes, then remove them from the vector.
      x <- x[x > 0]
   }
   
   # Now, proceed with whatever your vector is after removing zeroes (or not
   # removing them if zero.propagate was TRUE but there weren't any zeroes
   # to start with anyway.)
   
   return( sqrt(exp(sd(log(x))^2)-1) )
}



#' Calculate the X percent confidence interval
#'
#' \code{confInt} takes as input a numeric vector, the desired confidence
#' interval, and returns the lower and upper values for the confidence interval.
#' Note that, because calculating a confidence interval relies on an accurate
#' estimate of the mean and standard deviation, your data should be normally
#' distributed.
#'
#' @param x A vector of numbers
#' @param CI The confidence interval desired; default is 90%. Enter this
#'   value as a decimal, e.g., 0.95.
#' @param na.rm Should NA values be removed? (logical)
#' @param distribution_type use a "t" distribution (default) or a "Z"
#'   distribution. Note: The Simcyp Simulator calculates geometric confidence
#'   intervals with a t distribution.
#'
#' @examples
#' x <- rnorm(100, 5, 1)
#' confInt(x)
#' confInt(x, CI = 0.9)
#'
#' @export

confInt <- function(x, 
                    CI = 0.9, 
                    na.rm = TRUE, 
                    distribution_type = "t"){
   
   if(tolower(distribution_type) %in% c("z", "t")){
      distribution_type <- ifelse(tolower(distribution_type) == "z", 
                                  "Z", "t")
   } else {
      stop("You have supplied a value for distribution_type that doesn't work. It must be either `t` (default and what the Simulator uses) or `Z`.\n", 
           call. = FALSE)
   }
   
   if(na.rm){
      x <- x[complete.cases(x)]
   }
   
   # Now, proceed with whatever your vector is after removing zeroes (or not
   # removing them if zero.propagate was TRUE but there weren't any zeroes
   # to start with anyway.)
   
   alpha <- 1-CI
   
   Up <- switch(distribution_type, 
                "Z" = mean(x) + qnorm(1-alpha/2)*sd(x)/sqrt(length(x)),
                "t" = mean(x) + qt(p = 1-alpha/2, df = (length(x) - 1)) * 
                   sd(x)/sqrt(length(x)))
   
   Low <- switch(distribution_type, 
                 "Z" = mean(x) - qnorm(1-alpha/2)*sd(x)/sqrt(length(x)),
                 "t" = mean(x) - qt(p = 1-alpha/2, df = (length(x) - 1)) * 
                    sd(x)/sqrt(length(x)))
   
   Out <- c("lower" = Low, "upper" = Up)
   
   return(Out)
}


#' Calculate the geometric X percent confidence interval
#'
#' \code{gm_conf} takes as input a numeric vector, the desired confidence
#' interval, and returns the lower and upper values for the confidence interval.
#' Note that, because calculating a confidence interval relies on an accurate
#' estimate of the mean and standard deviation, your data should be log-normally
#' distributed.
#'
#' @param x A vector of numbers
#' @param CI The confidence interval desired; default is 90%. Enter this value
#'   as a decimal, e.g., 0.95.
#' @param na.rm Should NA values be removed? (logical)
#' @param zero.propagate Should zeroes be propagated? (logical)
#' @param distribution_type use a "t" distribution (default) or a "Z"
#'   distribution. Note: The Simcyp Simulator calculates geometric confidence
#'   intervals with a t distribution.
#'
#' @examples
#' x <- rnorm(100, 5, 1)
#' gm_conf(x)
#' gm_conf(x, CI = 0.9)
#'
#' # PK data are often log-normally distributed, so try this function with
#' # some example concentration-time data. (For this, we're not worried
#' # about independence; we just need some example data to work with.)
#' data(MDZConcTime)
#' # Making values larger just so we're not dealing w/tiny decimals and
#' # removing 0's for simplicity.
#' x <- MDZConcTime$Conc[MDZConcTime$Conc != 0]*100
#'
#' # Compare the distributions of the untransformed vs. log-transformed data:
#' ggplot2::qplot(x, bins = 15)
#' ggplot2::qplot(x, bins = 15) + ggplot2::scale_x_log10()
#' gm_conf(x)
#' # Compare the results with confInt()
#' confInt(x)
#'
#' @export

gm_conf <- function(x, 
                    CI = 0.9, 
                    na.rm = TRUE, 
                    zero.propagate = FALSE, 
                    distribution_type = "t") {
   
   if(tolower(distribution_type) %in% c("z", "t")){
      distribution_type <- ifelse(tolower(distribution_type) == "z", 
                                  "Z", "t")
   } else {
      stop("You have supplied a value for distribution_type that doesn't work. It must be either `t` (default and what the Simulator uses) or `Z`.\n", 
           call. = FALSE)
   }
   
   if(na.rm){
      x <- x[complete.cases(x)]
   }
   
   # If any values are negative, return NaN.
   if(any(x < 0, na.rm = TRUE)){
      return(NaN)
   }
   
   if(zero.propagate){
      if(any(x == 0, na.rm = TRUE)){
         return(0)
      }
   } else { # If you don't want to propagate zeroes, then remove them from the vector.
      x <- x[x > 0]
   }
   
   # Now, proceed with whatever your vector is after removing zeroes (or not
   # removing them if zero.propagate was TRUE but there weren't any zeroes
   # to start with anyway.)
   
   alpha <- 1-CI
   
   logx <- log(x)
   
   Up <- switch(distribution_type, 
                "Z" = exp(mean(logx) + qnorm(1-alpha/2)*sd(logx)/sqrt(length(logx))),
                "t" = exp(mean(logx) + qt(p = 1-alpha/2, df = (length(logx) - 1)) * 
                             sd(logx)/sqrt(length(logx))))
   
   Low <- switch(distribution_type, 
                 "Z" = exp(mean(logx) - qnorm(1-alpha/2)*sd(logx)/sqrt(length(logx))), 
                 "t" = exp(mean(logx) - qt(p = 1-alpha/2, df = (length(logx) - 1)) * 
                              sd(logx)/sqrt(length(logx))))
   
   Out <- c("lower" = Low, "upper" = Up)
   
   return(Out)
   
}



