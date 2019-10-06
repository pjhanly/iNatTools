#' @title iNaturalist Observation Import
#'
#' @description Core function for pulling observation data from the iNaturalist API
#'
#' @param per_page Results requested per API request page. Cannot exceed 200
#'
#' @param order Query sort order. "desc" (default) for descending and "asc" for ascending
#'
#' @param order_by Sorting field for order paramter. Options are "created_at" (default), "observed_on", "species_guess", "votes", and "id"
#'
#' @param acc Positional accuracy is specified: "true" or "false"
#'
#' @param captive Fetch captive/cultivated observations: "true" or "false"
#'
#' @param endemic Fetch taxa endemic to location: "true" or "false"
#'
#' @param geo Fetch georeferenced observations: "true" or "false"
#'
#' @param identified Fetch observations with community IDs: "true" or "false"
#'
#' @param introduced Fetch taxa introduced to location: "true" or "false"
#'
#' @param mappaple Fetch observations that can show on map tiles: "true" or "false"
#'
#' @param native Fetch taxa native to location: "true" or "false"
#'
#' @param only_id Fetch only the record ID: "true" or "false"
#'
#' @param out_of_range Fetch taxa outside of known range: "true" or "false"
#'
#' @param pcid Fetch observations identified by project curators: "true" or "false"
#'
#' @param photos Fetch observations with photos: "true" or "false"
#'
#' @param popular Fetch only favorited observations: "true" or "false"
#'
#' @param taxon_is_active Fetch only taxa with active concepts: "true" or "false"
#'
#' @param threatened Fetch taxa threatened in their location: "true" or "false"
#'
#' @param verifiable Fetch only observations classified as Needs ID or are Research Grade: "true" or "false". Default = "true"
#'
#' @param id Fetch a specific observation ID number
#'
#' @param not_id Exclude a specific observation ID number from a fetch
#'
#' @param place_id Fetch observations based on place ID
#'
#' @param project_id Fetch observations from "[project name]"
#'
#' @param rank Fetch observations with a specific taxon rank. Example options are: "kingdom", "phylum", "order", "family", "genus", "species"
#'
#' @param site_id Fetch observations from iNaturalist affiliate site ID
#'
#' @param taxon_id Fetch observations of taxon ID and descendants
#'
#' @param without_taxon_id Exclude fetching of observations of a taxon ID and descendants
#'
#' @param taxon_name Fetch observations matching "[common name]" or "[scientific name]"
#'
#' @param user_id Fetch observations from user ID number
#'
#' @param user_login Fetch observations by "[user name]"
#'
#' @param day Fetch observation from a given day
#'
#' @param month Fetch observations from a given month
#'
#' @param year Fetch observations from a given year
#'
#' @param term_id Control term ID for annotations
#'
#' @param term_value_id Combined with the term_id paramter to fetch observations with specific annotations
#'
#' @param without_term_value_id Combined with the term_id parameter to exclude observations with specific annotations
#'
#' @param acc_above Positional accuracy must be above this value (meters)
#'
#' @param acc_below Positional accuracy must be below this value (meters)
#'
#' @param d1 Fetch observations created on or after a specific date: "YYYY-MM-DD"
#'
#' @param d2 Fetch observations on or before a specific date: "YYYY-MM-DD"
#'
#' @param created_d1 Fetch observations created on or after a specific date: "YYYY-MM-DD"
#'
#' @param created_d2 Fetch observations created on or before a specific date: "YYYY-MM-DD"
#'
#' @param created_on Fetch observations created on a specific date: "YYYY-MM-DD"
#'
#' @param observed_on Fetch observations from a specific date: "YYYY-MM-DD"
#'
#' @param unobserved_by_user_id Fetch observations of taxa never observed by a user ID number
#'
#' @param apply_project_rules_for Fetch observations that match rules of "[project name]"
#'
#' @param cs Fetch observations of taxa with a specific conservation status code. When the place_id parameter is specified, the status code will be specific to that location
#'
#' @param csa Fetch observations of taxa with a conservation status code from a specific authority
#'
#' @param csi Fetch observations of taxa with a specific IUCN conservation status. Options are "LC", "NT", "VU", "EN", "CR", "EW", "EX"
#'
#' @param geoprivacy Fetch observations with this geoprivacy setting: "obscured", "obscured_private", "open", or "private
#'
#' @param taxon_geoprivacy Fetch observations with the specified most conservative geoprivacy applied by a conservation status: "obscured", "obscured_private", "open", or "private
#'
#' @param hrank Fetch observations with this taxon rank or lower: Example options are: "kingdom", "phylum", "order", "family", "genus", "species"
#'
#' @param lrank Fetch observations with this taxon rank or higher: Example options are: "kingdom", "phylum", "order", "family", "genus", "species"
#'
#' @param id_above Fetch observations above this ID number
#'
#' @param id_below Fetch observations below this ID number
#'
#' @param identifications Fetch observations with identifications where: "most_agree", "most_disagree", "some_agree"
#'
#' @param lat Fetch observations within a specified radius of this latitude (decimal degrees)
#'
#' @param lng Fetch observations within a specified radius of this longitude (decimal degrees)
#'
#' @param radius Fetch observations within a kilometer radius of specified variables lat and lng
#'
#' @param nelat Set northeast latitude (decimal degrees) of bounding box to fetch observations within
#'
#' @param nelng Set norteast longitude (decimal degrees) of bounding box to fetch observations within
#'
#' @param swlat Set southwest latitude (decimal degrees) of bounding box to fetch observations within
#'
#' @param swlng Set southwest longitude (decimal degrees) of bounding box to fetch observations within
#'
#' @param list_id Fetch taxon in list of IDs
#'
#' @param not_in_project Fetch observations that are not in a given project ID
#'
#' @param not_matching_project_rules_for Fetch observations that do not match the rules of a project ID
#'
#' @param q String search of observation properties that can be combined with paramter search_on
#'
#' @param search_on Properties to search on based on parameter q string query: "names", "tags", "description", "place"
#'
#' @param quality_grade Fetch observations of this quality grade: "casual", "needs_id", "research"
#'
#' @param updated_since String query for fetching observations updated since a given time
#'
#' @param viewer_id Fetch observations reviewed by a user ID
#'
#' @param reviewed Fetch observation that have ("true") or have no ("false") been reviewed by the user specified in the viewer_id parameter
#'
#' @param locale Specify locale preference for taxon common names
#'
#' @param preferred_place_id Specify place preference for regional common names
#'
#' @param ttl Specify the Cache-Control HTTP header in seconds
#'
#' @return NULL
#'
#' @examples
#'
#' Fetch all observations of Gastropoda (47114) for a user_id (473359):
#' df <- iNat(user_id = 473359, taxon_id = 47114)
#'
#' Fetch all observations of flowering dicots for a project:
#' df <- iNat(project="golden-ears-provincial-park", taxon_id = 47124, term_id = 12, term_value_id = 13)
#'
#' Fetch deer (Cervidae) observed on 2019-06-19
#' df <- iNat(taxon_id = 42158, observed_on = "2019-06-19")
#'
#' Fetch all observations created for a project for the first 3 days of May 2019:
#' df <- iNat(project="bowerbird",created_d1="2019-05-01",created_d2="2019-05-03")
#'
#' Fetch all observations of Felidae that have IUCN endangered status and are Research Grade:
#' df <- iNat(csi = "EN", taxon_id = 41944, quality_grade = "research")
#'
#' Fetch all monarch butterfly observations within a specific bounding box of latitude and longitude:
#' df <- iNat(taxon_id = 48662, nelat = 40, nelng = -94, swlat = 39, swlng = -95)
#'
#' Fetch all bird observations within a kilometer of the Empire State Building:
#' df <- iNat(taxon_id = 3, lat = 40.748424, lng = -73.985698, radius = 1)
#'
#' @export iNat

iNat <- function(per_page = 200, order = "desc", order_by = "created_at", acc = NULL,
                 captive = NULL, endemic = NULL, geo = NULL, identified = NULL,
                 introduced = NULL, mappable = NULL, native = NULL,  only_id = NULL,
                 out_of_range = NULL, pcid = NULL, photos = NULL, popular = NULL,
                 taxon_is_active = NULL, threatened = NULL, verifiable = "true",
                 id = NULL, not_id = NULL, place_id = NULL, project_id = NULL,
                 rank = NULL, site_id = NULL, taxon_id = NULL, without_taxon_id = NULL,
                 taxon_name = NULL, user_id = NULL, user_login = NULL, day = NULL,
                 month = NULL, year = NULL, term_id = NULL, term_value_id = NULL,
                 without_term_value_id = NULL, acc_above = NULL, acc_below = NULL,
                 d1 = NULL, d2 = NULL, created_d1 = NULL, created_d2 = NULL,
                 created_on = NULL, observed_on = NULL, unobserved_by_user_id = NULL,
                 apply_project_rules_for = NULL, cs = NULL, csa = NULL, csi = NULL,
                 geoprivacy = NULL, taxon_geoprivacy = NULL, hrank = NULL, lrank = NULL,
                 id_above = NULL, id_below = NULL, identifications = NULL, lat = NULL,
                 lng = NULL, radius = NULL, nelat = NULL, nelng = NULL, swlat = NULL,
                 swlng = NULL, list_id = NULL, not_in_project = NULL,
                 not_matching_project_rules_for = NULL, q = NULL, search_on = NULL,
                 quality_grade = NULL, updated_since = NULL, viewer_id = NULL,
                 reviewed = NULL, locale = NULL, preferred_place_id = NULL, ttl = NULL) {

  options(stringsAsFactors = FALSE)
  api <- "https://api.inaturalist.org/v1/observations"







  fetch <- list(per_page, order, order_by, acc, captive, endemic, geo, identified, introduced,
                mappable, native, only_id, out_of_range, pcid, photos, popular, taxon_is_active,
                threatened, verifiable, id, not_id, place_id, project_id, rank, site_id,
                taxon_id, without_taxon_id, taxon_name, user_id, user_login, day, month, year,
                term_id, term_value_id, without_term_value_id, acc_above, acc_below, d1, d2, created_d1,
                created_d2, created_on, observed_on, unobserved_by_user_id, apply_project_rules_for,
                cs, csa, csi, geoprivacy, taxon_geoprivacy, hrank, lrank, id_above, id_below, identifications,
                lat, lng, radius, nelat, nelng, swlat, swlng, list_id, not_in_project, not_matching_project_rules_for,
                q, search_on, quality_grade, updated_since, viewer_id, reviewed, locale, preferred_place_id, ttl)

  names(fetch) <- c("per_page", "order", "order_by", "acc", "captive", "endemic", "geo", "identified", "introduced",
                    "mappable", "native", "only_id", "out_of_range", "pcid", "photos", "popular", "taxon_is_active",
                    "threatened", "verifiable", "id", "not_id", "place_id", "project_id", "rank", "site_id",
                    "taxon_id", "without_taxon_id", "taxon_name", "user_id", "user_login", "day", "month", "year",
                    "term_id", "term_value_id", "without_term_value_id", "acc_above", "acc_below", "d1", "d2", "created_d1",
                    "created_d2", "created_on", "observed_on", "unobserved_by_user_id", "apply_project_rules_for",
                    "cs", "csa", "csi", "geoprivacy", "taxon_geoprivacy", "hrank", "lrank", "id_above", "id_below", "identifications",
                    "lat", "lng", "radius", "nelat", "nelng", "swlat", "swlng", "list_id", "not_in_project", "not_matching_project_rules_for",
                    "q", "search_on", "quality_grade", "updated_since", "viewer_id", "reviewed", "locale", "preferred_place_id", "ttl")



  res <- GET(api, query=c(fetch,list(page=1)))
  resDF <- fromJSON(httr::content(res, as = "text"),flatten=TRUE)

  if (resDF$total_results < 10001 & resDF$total_results > 200) {

    for (i in 2:(ceiling(resDF$total_results/resDF$per_page))) {
      res.t <- GET(api, query=c(fetch,list(page=i)))
      resDF.t <- fromJSON(httr::content(res.t, as = "text"),flatten=TRUE)
      resDF$results <- bind_rows(resDF$results,resDF.t$results)
      Sys.sleep(1)
    }
    iNatDF <- resDF$results
  }

  iNatDF <- resDF$results

  if (resDF$total_results > 10000) {
    stop("This query exceeds 10,000 records. Please narrow down your search terms or consider
breaking your search into multiple queries")
  }

  if (is.data.frame(iNatDF)=="TRUE") {
  cat(nrow(iNatDF), "records fetched")
  }

  if (resDF[1]=="Error") {
    cat("Error code",resDF$status, "- no results generated from query")
  }

return(iNatDF)

}
