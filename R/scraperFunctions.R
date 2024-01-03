require(dplyr)

#' Function for all links to players
#'
#' @export
#'

teamLinks <- function(){

  prospectForum <-
    "https://forum.simulationsoccer.com/forumdisplay.php?fid=76" %>%
    c(
      .,
      paste(., "&page=2", sep = ""),
      paste(., "&page=3", sep = ""),
      paste(., "&page=4", sep = ""),
      paste(., "&page=5", sep = ""),
      paste(., "&page=6", sep = "")
    )

  faForum <-
    "https://forum.simulationsoccer.com/forumdisplay.php?fid=78" %>%
    c(
      .,
      paste(., paste("&page=", 2:20, sep = ""), sep = "")
    )

  teamForums <-
    c(
      ## Romana
      "https://forum.simulationsoccer.com/forumdisplay.php?fid=148",
      ## Inter London
      "https://forum.simulationsoccer.com/forumdisplay.php?fid=59",
      ## Buenos Aires
      "https://forum.simulationsoccer.com/forumdisplay.php?fid=113",
      ## Athenai
      "https://forum.simulationsoccer.com/forumdisplay.php?fid=60",
      ## Catalunya
      "https://forum.simulationsoccer.com/forumdisplay.php?fid=105",
      ## Seoul
      "https://forum.simulationsoccer.com/forumdisplay.php?fid=81",
      ## Hollywood
      "https://forum.simulationsoccer.com/forumdisplay.php?fid=66",
      ## Kaapstad
      "https://forum.simulationsoccer.com/forumdisplay.php?fid=139",
      ## Reykjavik
      "https://forum.simulationsoccer.com/forumdisplay.php?fid=116",
      ## Laos
      "https://forum.simulationsoccer.com/forumdisplay.php?fid=152",
      ## Schwarzwälder
      "https://forum.simulationsoccer.com/forumdisplay.php?fid=135",
      ## Montreal
      "https://forum.simulationsoccer.com/forumdisplay.php?fid=63",
      ## Tokyo
      "https://forum.simulationsoccer.com/forumdisplay.php?fid=68",
      ## Cairo
      "https://forum.simulationsoccer.com/forumdisplay.php?fid=70",
      ## Sao Paulo
      "https://forum.simulationsoccer.com/forumdisplay.php?fid=141",
      ## Paris
      "https://forum.simulationsoccer.com/forumdisplay.php?fid=132",
      ## Academy
      "https://forum.simulationsoccer.com/forumdisplay.php?fid=159",
      "https://forum.simulationsoccer.com/forumdisplay.php?fid=160",
      "https://forum.simulationsoccer.com/forumdisplay.php?fid=158",
      "https://forum.simulationsoccer.com/forumdisplay.php?fid=161"
    ) %>%
    c(
      .,
      paste(., "&page=2", sep = "")
    )

  retiredForum <-
    "https://forum.simulationsoccer.com/forumdisplay.php?fid=119" %>%
    c(
      .,
      paste(., paste("&page=", seq(2, 50), sep = ""), sep = "")
    )


  forumsToScrape <-
    c(
      # createdForum,
      prospectForum,
      teamForums,
      faForum,
      retiredForum
    )

  return(forumsToScrape)
}

#' Scrapes player links from teams
#'
#' @param forum The forum link to the team
#' @export
#'

playerLinkScraper <-
  function(forum){
    ### Takes the player link scraped from the team pages
    ##  If it is a complete link with the base url there it scrapes it directly
    ##  For use with teamLinkScraper and playerLinkScraper then only the endings are used, requiring the baseLink addition
    if(stringr::str_detect(forum, "simulationsoccer")){

    } else{
      baseLink <- "https://forum.simulationsoccer.com/"

      forum <- paste(baseLink, forum, sep = "")

    }

    ### Reads the information
    topic <- xml2::read_html(forum)

    ### Reads all topics
    playerLinks <-
      topic %>%
      rvest::html_elements(".inline_row .subject_new") %>%
      rvest::html_elements("a") %>%
      rvest::html_attr("href") %>%
      .[
        all(stringr::str_detect(string = ., pattern = "showthread"))
      ]

    return(playerLinks)
  }

#' Scrapes all player page data
#'
#' @param player Player page link
#'
#' @export
#'
#' @returns
#' Returns a data.frame with all information from a player page
#'
playerScraper <-
  function(player){
    ### Takes the player link scraped from the team pages
    ##  If it is a complete link with the base url there it scrapes it directly
    ##  For use with teamLinkScraper and playerLinkScraper then only the endings are used, requiring the baseLink addition
    if(stringr::str_detect(player, "simulationsoccer")){

    } else{
      baseLink <- "https://forum.simulationsoccer.com/"

      player <- paste(baseLink, player, sep = "")

    }

    ### Reads the information
    topic <- xml2::read_html(player)

    postData <-
      topic %>%
      rvest::html_elements(".post_body") %>%
      .[1] %>%
      # ## Changes to dplyr 1.1.0 removes this functionality.
      # dplyr::nth(1) %>%
      # rvest::html_elements(".postcolor") %>%
      rvest::html_text2() %>%
      stringr::str_split(pattern = "\\n|\\r") %>%
      unlist() %>%
      .[stringr::str_detect(string = ., pattern = ":")] %>%
      .[!stringr::str_detect(string = ., pattern = "edited by")] %>%
      stringr::str_split(pattern = ":", simplify = TRUE) %>%
      matrix(ncol = 2) %>%
      data.frame() %>%
      dplyr::mutate(
        X2 = stringr::str_squish(X2)
      ) %>%
      tidyr::pivot_wider(
        names_from = X1,
        values_from = X2
      )

    postData$Created <-
      topic %>%
      rvest::html_elements(".post_date") %>%
      rvest::html_text() %>%
      dplyr::nth(1) %>%
      stringr::str_split(pattern = ": |,", simplify = TRUE) %>%
      .[,1] %>%
      # ## Changes in dplyr 1.1.0
      # dplyr::nth(2) %>%
      # {
      #   if(packageVersion("lubridate") == '1.9.0') {
      #     lubridate::as_date(., format = "b-d-Y")
      #   } else {
      #     lubridate::as_date(., format = "%d-%b-%Y")
      #   }
      # }
      lubridate::ymd()

    postData$Class <-
      topic %>%
      rvest::html_elements(".thead") %>%
      rvest::html_text() %>%
      stringr::str_extract_all(pattern = "(?<=\\().*?(?=\\))", simplify = TRUE) %>%
      c() %>%
      stringi::stri_remove_empty_na()

    if(!("Preferred Position" %in% colnames(postData))){
      postData$`Preferred Position` <- postData$Position

      postData <-
        postData %>%
        dplyr::relocate(
          `Preferred Position`,
          .after = Position
        )
    }

    postData$Position <-
      topic %>%
      rvest::html_elements(".thead strong") %>%
      .[3] %>%
      rvest::html_text() %>%
      stringr::str_split(pattern = " - ", simplify = TRUE) %>%
      .[,2] %>%
      # ## Changes in dplyr 1.1.0
      # dplyr::nth(2) %>%
      c()

    postData$TPE <-
      topic %>%
      rvest::html_elements(".thead small") %>%
      rvest::html_text() %>%
      stringr::str_split(pattern = ":", simplify = TRUE) %>%
      .[,2] %>%
      # ## Changes in dplyr 1.1.0
      # dplyr::nth(2) %>%
      stringr::str_squish() %>%
      as.numeric()

    playerTeam <-
      teamData %>%
      select(
        team
      ) %>%
      dplyr::slice(
        topic %>%
          rvest::html_elements(".navigation") %>%
          rvest::html_text2() %>%
          stringr::str_split(pattern = "nav_bit", simplify = TRUE) %>%
          stringr::str_remove_all(pattern = "start:|nav_sep|end:|_active|nav_dropdown") %>%
          stringr::str_squish() %>%
          stringi::stri_remove_empty_na() %>%
          .[length(.)-1] %>%
          stringr::str_detect(
            ## Takes team information from a separate data set
            pattern = teamData$team
          ) %>%
          which()
      )

    if((playerTeam %>% nrow()) == 0){
      forum <- topic %>%
        rvest::html_elements(".navigation") %>%
        rvest::html_text2() %>%
        stringr::str_split(pattern = "nav_bit", simplify = TRUE) %>%
        stringr::str_remove_all(pattern = "start:|nav_sep|end:|_active|nav_dropdown") %>%
        stringr::str_squish() %>%
        stringi::stri_remove_empty_na()

      playerTeam <-
        playerTeam %>%
        dplyr::add_row() %>%
        dplyr::mutate(
          team =
            dplyr::case_when(
              any(stringr::str_detect(forum, pattern = "Retired")) ~ "Retired",
              any(stringr::str_detect(forum, pattern = "Academy")) ~ "Prospect",
              TRUE ~ "FA"
            )
          )
    }

    postData$Team <- playerTeam %>% unname() %>% unlist()

    ## Finds the date of the user's last post on the forum
    userData <-
      topic %>%
      rvest::html_elements(".post_author strong a") %>%
      .[1] %>%
      # ## Changes in dplyr 1.1.0
      # dplyr::nth(1) %>%
      rvest::html_attr("href") %>%
      xml2::read_html() %>%
      rvest::html_elements(".tborderposts a") %>%
      rvest::html_attr("href") %>%
      .[
        stringr::str_detect(., pattern = "finduser&")
      ] %>%
      paste(
        "https://forum.simulationsoccer.com/",
        .,
        sep = ""
      ) %>%
      xml2::read_html() %>%
      rvest::html_elements(".inline_row") %>%
      .[1] %>%
      rvest::html_elements(".smalltext") %>%
      .[3] %>%
      rvest::html_text() %>%
      stringr::str_split(pattern = ": |,", simplify = TRUE) %>%
      .[,1]

    postData$lastPost <- userData

    postData <-
      postData %>%
      dplyr::mutate(
        lastPost =
          dplyr::case_when(
            stringr::str_detect(lastPost, pattern = "minute") ~ lubridate::today(),
            stringr::str_detect(lastPost, pattern = "hour") ~ lubridate::today(),
            stringr::str_detect(lastPost, pattern = "Today") ~ lubridate::today(),
            stringr::str_detect(lastPost, pattern = "Yesterday") ~ lubridate::today()-1,
            TRUE ~ lastPost %>% lubridate::ymd()
          ),
        Active =
          dplyr::case_when(
            lubridate::today() - (lastPost %>% unlist()) > 21 ~ "IA",
            TRUE ~ "Active"
          )
      )

    postData$Username <-
      topic %>%
      rvest::html_elements(".author_information a") %>%
      .[1] %>%
      # ## Changes in dplyr 1.1.0
      # dplyr::nth(1) %>%
      rvest::html_text()

    postData$Userlink <-
      topic %>%
      rvest::html_elements(".post_author strong a") %>%
      .[1] %>%
      # ## Changes in dplyr 1.1.0
      # dplyr::nth(1) %>%
      rvest::html_attr("href")

    postData$Playerlink <-
      player

    postData$`All Traits` <-
      paste(
        postData %>%
          dplyr::select(
            tidyselect::contains("Trait")
          ),
        collapse = " \\ "
      )


    # postData$lastPost

    postData <-
      postData %>%
      dplyr::select(
        !tidyselect::starts_with("Trait")
      ) %>%
      dplyr::relocate(
        c(
          Class,
          TPE,
          Created,
          Team
        ),
        .after = Username
      ) %>%
      dplyr::relocate(
        Position,
        .before = `Preferred Position`
      ) %>%
      dplyr::relocate(
        `All Traits`,
        .before = lastPost
      )

    return(postData)
  }

#'
#' #' Scrapes the claim threads
#' #'
#' #' @param thread A link to the claim thread
#' #'
#' #' @export
#' #'
#' #' @returns
#' #' Returns a data frame of all the AC posts, by whom and when
#' #'
#'
#' claimThreadPost <-
#'   function(thread) {
#'
#'     ## Reads the current AC link
#'     current <- read_html(thread)
#'
#'     threads <-
#'       current %>%
#'       rvest::html_elements(".postcolor") %>%
#'       rvest::html_text2()
#'
#'
#'
#'
#'
#'   }
