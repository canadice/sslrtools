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
    recentAC <-
      xml2::read_html("https://simsoccer.jcink.net/index.php?showforum=7") %>%
      rvest::html_elements(".topic-row") %>%
      rvest::html_elements(".row4 [href]") %>%
      rvest::html_attr("href")

    firstCut <-
      recentAC %>%
      stringr::str_extract(pattern = "&showtopic=[0-9]+") %>%
      unique() %>%
      dplyr::nth(3)

    maxLink <-
      recentAC[
        stringr::str_detect(recentAC, firstCut) %>%
          which() %>%
          dplyr::nth(1) - 1
      ] %>%
      stringr::str_extract(pattern = "&showtopic.+")

    minLink <-
      recentAC[2] %>%
      stringr::str_extract(pattern = "&showtopic.+")

    if(minLink == maxLink){
      recentAC <- paste(
        "https://simsoccer.jcink.net/index.php?",
        minLink,
        sep = ""
      )
    } else {
      recentAC <-
        paste(
          "https://simsoccer.jcink.net/index.php?",
          minLink,
          paste(
            "&st=",
            seq(0, 300, by = 15),
            sep = ""
          ),
          sep = ""
        ) %>%
        .[
          1:((stringr::str_detect(
            string = .,
            pattern = paste(maxLink,"$", sep = "")
          )
          ) %>%
            which()
          )
        ]

    }


    return(recentAC)
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
    recentAffiliate <-
      xml2::read_html("https://simsoccer.jcink.net/index.php?showforum=34") %>%
      rvest::html_elements(".topic-row") %>%
      rvest::html_elements(".row4 [href]") %>%
      rvest::html_attr("href")

    firstCut <-
      recentAffiliate %>%
      stringr::str_extract(pattern = "&showtopic=[0-9]+") %>%
      unique() %>%
      dplyr::nth(3)

    maxLink <-
      recentAffiliate[
        stringr::str_detect(recentAffiliate, firstCut) %>%
          which() %>%
          dplyr::nth(1) - 1
      ] %>%
      stringr::str_extract(pattern = "&showtopic.+")

    minLink <-
      recentAffiliate[2] %>%
      stringr::str_extract(pattern = "&showtopic.+")

    if(minLink == maxLink){
      recentAffiliate <- paste(
        "https://simsoccer.jcink.net/index.php?",
        minLink,
        sep = ""
      )
    } else {
      recentAffiliate <-
        paste(
          "https://simsoccer.jcink.net/index.php?",
          minLink,
          paste(
            "&st=",
            seq(0, 300, by = 15),
            sep = ""
          ),
          sep = ""
        ) %>%
        .[
          1:((stringr::str_detect(
            string = .,
            pattern = paste(maxLink,"$", sep = "")
          )
          ) %>%
            which()
          )
        ]

    }

    return(recentAffiliate)
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
      rvest::html_elements(".topic-row") %>%
      rvest::html_elements(".row4 [href]") %>%
      rvest::html_attr("href") %>%
      stringr::str_remove_all(pattern = "s=[0-9a-z]+")

    articles <-
      articles %>%
      lapply(
        X = .,
        FUN = function(x){
          current <- xml2::read_html(x)

          title <-
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
            stringr::str_remove_all("Posted on: |,") %>%
            lubridate::as_datetime(format = "%b %d %Y %H:%M %p")

          link <-
            current %>%
            rvest::html_elements(".row4 .postdetails a") %>%
            rvest::html_attr("onclick") %>%
            stringr::str_extract(pattern = "[0-9]+") %>%
            paste(
              x,
              "&view=findpost&p=",
              .,
              sep = ""
            )

          data.frame(
            Title = rep(title, times = length(users)),
            User = users,
            Post = post,
            Time = time,
            Link = link
          )
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
          current <- xml2::read_html(x)

          title <-
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
            stringr::str_remove_all("Posted on: |,") %>%
            lubridate::as_datetime(format = "%b %d %Y %H:%M %p")

          link <-
            current %>%
            rvest::html_elements(".row4 .postdetails a") %>%
            rvest::html_attr("onclick") %>%
            stringr::str_extract(pattern = "[0-9]+") %>%
            paste(
              x,
              "&view=findpost&p=",
              .,
              sep = ""
            )

          data.frame(
            Title = rep(title, times = length(users)),
            User = users,
            Post = post,
            Time = time,
            Link = link
          )
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
          current <- xml2::read_html(x)

          title <-
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
            stringr::str_remove_all("Posted on: |,") %>%
            lubridate::as_datetime(format = "%b %d %Y %H:%M %p")

          link <-
            current %>%
            rvest::html_elements(".row4 .postdetails a") %>%
            rvest::html_attr("onclick") %>%
            stringr::str_extract(pattern = "[0-9]+") %>%
            paste(
              x,
              "&view=findpost&p=",
              .,
              sep = ""
            )

          data.frame(
            Title = rep(title, times = length(users)),
            User = users,
            Post = post,
            Time = time,
            Link = link
          )
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
    recentTC <-
      xml2::read_html("https://simsoccer.jcink.net/index.php?showforum=9") %>%
      rvest::html_elements(".topic-row") %>%
      rvest::html_elements(".row4 [href]") %>%
      rvest::html_attr("href")

    firstCut <-
      recentTC %>%
      stringr::str_extract(pattern = "&showtopic=[0-9]+") %>%
      unique() %>%
      dplyr::nth(2)

    maxLink <-
      recentTC[
        stringr::str_detect(recentTC, firstCut) %>%
          which() %>%
          dplyr::nth(1) - 1
      ] %>%
      stringr::str_extract(pattern = "&showtopic.+")

    minLink <-
      recentTC[1] %>%
      stringr::str_extract(pattern = "&showtopic.+")

    if(minLink == maxLink){
      recentTC <- paste(
        "https://simsoccer.jcink.net/index.php?",
        minLink,
        sep = ""
      )
    } else {
      recentTC <-
        paste(
          "https://simsoccer.jcink.net/index.php?",
          minLink,
          paste(
            "&st=",
            seq(0, 300, by = 15),
            sep = ""
          ),
          sep = ""
        ) %>%
        .[
          1:((stringr::str_detect(
            string = .,
            pattern = paste(maxLink,"$", sep = "")
          )
          ) %>%
            which()
          )
        ]

    }

    return(recentTC)
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

    openPT <-
      forum %>%
      rvest::html_elements(".topic-row") %>%
      rvest::html_elements(".t_img [src]") %>%
      rvest::html_attr("src") %>%
      stringr::str_detect("topic_locked") %>%
      !.

    recentPT <-
      forum %>%
      rvest::html_elements(".topic-row") %>%
      rvest::html_elements(".row4 [href]") %>%
      rvest::html_attr("href") %>%
      stringr::str_remove_all(pattern = "s=[0-9a-z]+")

    recentPT[
      recentPT %>%
        stringr::str_detect(
          pattern =
            if(any(openPT)){
              recentPT %>%
                stringr::str_extract(pattern = "&showtopic=[0-9]+") %>%
                unique() %>%
                .[openPT]
            } else {
              " "
            }

        )
    ] %>%
      return()

  }

