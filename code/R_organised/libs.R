librarian::shelf("groundhog")
pkgs <- c("dataverse", "tidyverse", "curl", "jsonlite", "glue", "stringdist", "stringi", "janitor", "lubridate", "here", "fst", "data.table", "htm2txt")
# groundhog.library(pkgs, "2023-12-09")
librarian::shelf(pkgs)
here <- here()
`%notlike%` <- Negate(`%like%`)
`%notchin%` <- Negate(`%chin%`)
options(datatable.prettyprint.char = 40L)
Sys.setenv("DATAVERSE_SERVER" = "dataverse.harvard.edu")





# Rselenium up in here.
getSeleniumDriver <- function() {
    print("need a new cookie, rebooting selenium etc.")
    print("first open docker, this will take a minute.........")
    try(docker_running <- system("pgrep -x docker", intern = T))
    if (length(docker_running) == 0) {
        system("open -a docker")
        Sys.sleep(30)
    }
    selenium_id <- system("docker run -d -p 5901:5900 -p 4445:4444 edwaldoalmeida/selenium-standalone-chromium-debug:3.141.59-iron-aarch64", intern = T)

    # The whole 'edwaldoal' bit changes to selenium/standalone-chrome-debug when using intel architecture.
    driver <- RSelenium::remoteDriver(remoteServerAddr = "localhost", port = 4445L, version = "latest", browser = c("chrome"))
    # driver$open()
    # driver$navigate("https://www.doi.org/")
    return(driver)
}

# getting that handle
getHandle <- function() {
    h <- new_handle()

    handle_setheaders(h,
        "authority" = "journals.sagepub.com",
        "cache-control" = "max-age=0",
        "upgrade-insecure-requests" = "1",
        "user-agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/96.0.4664.110 Safari/537.36",
        "accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9",
        "sec-gpc" = "1",
        "sec-fetch-site" = "none",
        "sec-fetch-mode" = "navigate",
        "sec-fetch-user" = "?1",
        "sec-fetch-dest" = "document",
        "accept-language" = "en-GB,en-US;q=0.9,en;q=0.8"
    )

    handle_setopt(h,
        "timeout" = 300,
        "connecttimeout" = 10,
        "ssl_verifyhost" = 0L,
        "ssl_verifypeer" = 0L
    )
    return(h)
}
