#' Finds open threads in a forum
#'
#' @returns A vector of the open threads
#'
openThreads <- function(forum){
  open <-
    forum %>%
    rvest::html_elements(".topic-row") %>%
    rvest::html_elements(".t_img [src]") %>%
    rvest::html_attr("src") %>%
    stringr::str_detect("topic_locked") %>%
    !.

  recent <-
    forum %>%
    rvest::html_elements(".topic-row") %>%
    rvest::html_elements(".row4 [href]") %>%
    rvest::html_attr("href") %>%
    stringr::str_remove_all(pattern = "s=[0-9a-z]+")

  current <-
    recent[
      recent %>%
        stringr::str_detect(
          pattern =
            if(any(open)){
              recent %>%
                stringr::str_extract(pattern = "&showtopic=[0-9]+") %>%
                unique() %>%
                .[open] %>%
                paste0(collapse = "|")
            } else {
              " "
            }
        )
    ]

  current <-
    split(current, f = stringr::str_extract(current, pattern = "showtopic=[0-9]+"))

  links <- function(current){
    if(any(current %>% stringr::str_detect("st=[0-9]+"))){
      currentPages <-
        current %>%
        stringr::str_extract_all(pattern = "st=[0-9]+", simplify = TRUE) %>%
        stringr::str_extract_all(pattern = "[0-9]+", simplify = TRUE) %>%
        stringi::stri_remove_empty_na() %>%
        as.numeric()

      paste(
        current[1], "&st=",
        seq(
          from = currentPages[1],
          to = currentPages[length(currentPages)],
          by = 15
        ),
        sep = ""
      ) %>%
        return()
    } else {
      return(current)
    }
  }

  lapply(current, FUN = links) %>%
    unlist() %>%
    unname() %>%
    return()
}


#' Scrapes most recent AC Thread
#'
#' @export
#'
#' @returns
#' Returns a vector of all the AC links
#'

activityCheckLinks <-
  function() {
    ## The url to the Activity Check forum
    forum <-
      xml2::read_html("https://simsoccer.jcink.net/index.php?showforum=7")

    return(openThreads(forum))
  }

#' Scrapes all ACs and
#'
#' @param AC A link to a specific AC page
#'
#' @export
#'
#' @returns
#' Returns a data frame of all the AC posts, by whom and when
#'

activityCheckPosts <-
  function(AC) {

    ## Reads the current AC link
    current <- xml2::read_html(AC)

    nr <-
      current %>%
      rvest::html_elements(".topic-title") %>%
      rvest::html_text2()

    users <-
      current %>%
      rvest::html_elements(".normalname") %>%
      rvest::html_text2()

    post <-
      current %>%
      rvest::html_elements(".postcolor") %>%
      rvest::html_text2() %>%
      stringr::str_remove_all(pattern = "emo&:[a-z]+:endemo") %>%
      stringr::str_squish()

    time <-
      current %>%
      rvest::html_elements(".row4 .postdetails") %>%
      rvest::html_text2() %>%
      stringr::str_remove_all("Posted: ")

    link <-
      current %>%
      rvest::html_elements(".row4 .postdetails a") %>%
      rvest::html_attr("onclick") %>%
      stringr::str_extract(pattern = "[0-9]+") %>%
      paste(
        AC,
        "&view=findpost&p=",
        .,
        sep = ""
      )

    data.frame(
      AC = rep(nr, times = length(users)),
      User = users,
      Post = post,
      Time = time,
      Link = link
    )
  }

#' Scrapes most recent Affiliate thread
#'
#' @export
#'
#' @returns
#' Returns a vector of all the Affiliate links
#'

affiliateLinks <-
  function() {
    forum <-
      xml2::read_html("https://simsoccer.jcink.net/index.php?showforum=34")

    return(openThreads(forum))
  }


#' Scrapes all the posts from a thread page
#'
#' @param page A link to a specific forum thread page
#'
#' @export
#'
#' @returns Returns a data frame of all the posts in the thread, by whom and when

readPosts <-
  function(page){
    ## Reads the current AC link
    current <- xml2::read_html(page)

    nr <-
      current %>%
      rvest::html_elements(".topic-title") %>%
      rvest::html_text2()

    users <-
      current %>%
      rvest::html_elements(".normalname") %>%
      rvest::html_text2()

    post <-
      current %>%
      rvest::html_elements(".postcolor") %>%
      rvest::html_text2() %>%
      stringr::str_remove_all(pattern = "emo&:[a-z]+:endemo") %>%
      stringr::str_squish()

    time <-
      current %>%
      rvest::html_elements(".row4 .postdetails") %>%
      rvest::html_text2() %>%
      stringr::str_remove_all("Posted: |Posted on:|,") %>%
      stringr::str_trim() %>%
      lubridate::as_datetime(format = "%b %d %Y %H:%M %p")

    link <-
      current %>%
      rvest::html_elements(".row4 .postdetails a") %>%
      rvest::html_attr("onclick") %>%
      stringr::str_extract(pattern = "[0-9]+") %>%
      paste(
        page,
        "&view=findpost&p=",
        .,
        sep = ""
      )

    data.frame(
      Thread = rep(nr, times = length(users)),
      User = users,
      Post = post,
      Time = time,
      Link = link
    )
  }

#' Scrapes all Affiliates and
#'
#' @param Affiliate A link to a specific Affiliate page
#'
#' @export
#'
#' @returns
#' Returns a data frame of all the Affiliate posts, by whom and when
#'

affiliatePosts <-
  function(Affiliate) {

    ## Reads the current AC link
    current <- xml2::read_html(Affiliate)

    nr <-
      current %>%
      rvest::html_elements(".topic-title") %>%
      rvest::html_text2()

    users <-
      current %>%
      rvest::html_elements(".normalname") %>%
      rvest::html_text2()

    post <-
      current %>%
      rvest::html_elements(".postcolor") %>%
      rvest::html_text2() %>%
      stringr::str_remove_all(pattern = "emo&:[a-z]+:endemo") %>%
      stringr::str_squish()

    time <-
      current %>%
      rvest::html_elements(".row4 .postdetails") %>%
      rvest::html_text2() %>%
      stringr::str_remove_all("Posted: ")

    link <-
      current %>%
      rvest::html_elements(".row4 .postdetails a") %>%
      rvest::html_attr("onclick") %>%
      stringr::str_extract(pattern = "[0-9]+") %>%
      paste(
        Affiliate,
        "&view=findpost&p=",
        .,
        sep = ""
      )

    data.frame(
      Affiliate = rep(nr, times = length(users)),
      User = users,
      Post = post,
      Time = time,
      Link = link
    )
  }

#' Scrapes weekly articles
#'
#' @export
#'
#' @returns
#' Returns a data frame of all the article links
#'

articleLinks <-
  function() {
    monday <-
      lubridate::now() %>%
      lubridate::with_tz(tzone = "America/Los_Angeles") %>%
      lubridate::floor_date("week", 1)

    articles <-
      xml2::read_html("https://simsoccer.jcink.net/index.php?showforum=31") %>%
      openThreads()

    articles <-
      articles %>%
      lapply(
        X = .,
        FUN = function(x){
          readPosts(x)
        }
      ) %>%
      do.call(
        what = plyr::rbind.fill,
        args = .
      ) %>%
      dplyr::mutate(
        Time = lubridate::force_tz(Time, tzone = "America/Los_Angeles")
      ) %>%
      dplyr::filter(
        Time >= monday,
        stringr::str_count(Post, "\\w+") > 200
      )

    return(articles)
  }

#' Scrapes weekly graphics
#'
#' @export
#'
#' @returns
#' Returns a data frame of all the graphics links
#'

graphicsLinks <-
  function() {
    monday <-
      lubridate::now() %>%
      lubridate::with_tz(tzone = "America/Los_Angeles") %>%
      lubridate::floor_date("week", 1)

    graphics <-
      xml2::read_html("https://simsoccer.jcink.net/index.php?showforum=33") %>%
      rvest::html_elements(".topic-row") %>%
      rvest::html_elements(".row4 [href]") %>%
      rvest::html_attr("href") %>%
      stringr::str_remove_all(pattern = "s=[0-9a-z]+")

    graphics <-
      graphics %>%
      lapply(
        X = .,
        FUN = function(x){
          readPosts(x)
        }
      ) %>%
      do.call(
        what = plyr::rbind.fill,
        args = .
      ) %>%
      dplyr::mutate(
        Time = lubridate::force_tz(Time, tzone = "America/Los_Angeles")
      ) %>%
      dplyr::filter(
        Time >= monday
      )

    return(graphics)
  }

#' Scrapes weekly podcasts
#'
#' @export
#'
#' @returns
#' Returns a data frame of all the graphics links
#'

podcastLinks <-
  function() {
    monday <-
      lubridate::now() %>%
      lubridate::with_tz(tzone = "America/Los_Angeles") %>%
      lubridate::floor_date("week", 1)

    podcasts <-
      xml2::read_html("https://simsoccer.jcink.net/index.php?showforum=32") %>%
      rvest::html_elements(".topic-row") %>%
      rvest::html_elements(".row4 [href]") %>%
      rvest::html_attr("href") %>%
      stringr::str_remove_all(pattern = "s=[0-9a-z]+")

    podcasts <-
      podcasts %>%
      lapply(
        X = .,
        FUN = function(x){
          readPosts(x)
        }
      ) %>%
      do.call(
        what = plyr::rbind.fill,
        args = .
      ) %>%
      dplyr::mutate(
        Time = lubridate::force_tz(Time, tzone = "America/Los_Angeles")
      ) %>%
      dplyr::filter(
        Time >= monday
      )

    return(podcasts)
  }

#' Scrapes the training camp
#'
#' @export
#'
#' @returns
#' Returns a data frame of the predictions

trainingCampLinks <-
  function() {
    ## The url to the Training Camp forum
    forum <-
      xml2::read_html("https://simsoccer.jcink.net/index.php?showforum=9")

    return(openThreads(forum))
  }

#' Scrapes the predictions
#'
#' @export
#'
#' @returns
#' Returns a data frame of the predictions

predictionLinks <-
  function() {
    ## The url to the Predictions forum
    forum <-
      xml2::read_html("https://simsoccer.jcink.net/index.php?showforum=10")

    openThreads(forum) %>%
      return()

  }

