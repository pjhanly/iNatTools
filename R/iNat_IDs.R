#' @title iNaturalist Identifications Import
#'
#' @description Function for pulling identification data from the iNaturalist API
#'
#' @param per_page Results requested per API request page. Cannot exceed 200
#'
#' @param order Query sort order. "desc" (default) for descending and "asc" for ascending
#'
#' @param order_by Sorting field for order paramter. Options are "created_at" (default) and "id"
#'
#' @param current_taxon Whether or not the current taxon is identical to the observation's taxon: "true" or "false"
#'
#' @param own_observation Whether or not the identification is for an observation of the identifier: "true" or "false"
#'
#' @param is_change Whetheror not the identification is the automatic result of a taxonomic change in the iNaturalist database: "true" or "false"
#'
#' @param taxon_active Whether or not the taxon ID is active in the iNaturalist database: "true" or "false"
#'
#' @param observation_taxon_active Whether or not the taxon of the observation is active in the iNaturalist database: "true" or "false"
#'
#' @param id Fetch a specific identification ID number
#'
#' @param rank Specify the ID taxonomic rank: "kingdom", "phylum", "subphylum", "superclass", "class", "subclass", "superorder", "order", "suborder", "infraorder", "superfamily", "epifamily", "family", "subfamily", "supertribe", "tribe", "subtribe", "genus", "genushybrid", "species", "hybrid", "subspecies", "variety", "form",  
#'
#' @param observation_rank Specify the observation taxonomic rank: "kingdom", "phylum", "subphylum", "superclass", "class", "subclass", "superorder", "order", "suborder", "infraorder", "superfamily", "epifamily", "family", "subfamily", "supertribe", "tribe", "subtribe", "genus", "genushybrid", "species", "hybrid", "subspecies", "variety", "form",  
#'
#' @param user_id Fetch identifications from user ID number
#'
#' @param user_login Fetch identifications by "[user name]"
#'
#' @param current Fetch identifications that are the current, active ones made by a user (i.e., have not been retracted): "true" (default) or "false"
#'
#' @param category Identification type: "improving" (brings the observation ID to a more specific level and has community agreement), "supporting" (confirms an existing level of observation ID), "leading" (brings the observation IF to a more specific level but has not yet been confirmed by the community), and "maverick" (an ID that conflicts with the community consensus ID for an observation; not necessarily a wrong ID)
#'
#' @param place_id Fetch identifications based on place ID
#'
#' @param quality_grade Current quality grade of the observation being ID'ed: "casual", "needs_id", "research"
#'
#' @param taxon_id Fetch identifications that are of taxon ID and descendants
#'
#' @param observation_taxon_id Fetch identifications that are for an observation of a taxon and descendants
#'
#' @param iconic_taxon_id Fetch identifications that are of an iconic taxon ID
#'
#' @param observation_iconic_taxon_id Fetch identifications that are for an observation of an iconic taxon
#'
#' @param lrank Fetch identifications with this taxon rank or higher. Example options are: "kingdom", "phylum", "order", "family", "genus", "species"
#'
#' @param hrank Fetch identifications with this taxon rank or lower. Example options are: "kingdom", "phylum", "order", "family", "genus", "species"
#'
#' @param observation_lrank Fetch identifications for observations with this taxon rank or higher. Example options are: "kingdom", "phylum", "order", "family", "genus", "species"
#'
#' @param observation_hrank Fetch identifications for observations with this taxon rank or lower. Example options are: "kingdom", "phylum", "order", "family", "genus", "species"
#'
#' @param without_taxon_id Fetch identifications that are not of a taxon or its descendants
#'
#' @param d1 Fetch identifications on or after a specific date: "YYYY-MM-DD"
#'
#' @param d2 Fetch identifications on or before a specific date: "YYYY-MM-DD"
#'
#' @param observation_created_d1 Fetch identifications for observations created on or after a specific date: "YYYY-MM-DD"
#'
#' @param observation_created_d2 Fetch identifications for observations created on or before a specific date: "YYYY-MM-DD"
#'
#' @param observed_d1 Fetch identifications for observations that were observed on or after a specific date: "YYYY-MM-DD"
#'
#' @param observed_d2 Fetch identifications for observations that were observed on or before a specific date: "YYYY-MM-DD"
#'
#' @param id_above Fetch identifications above this ID number
#'
#' @param id_below Fetch identifications below this ID number
#'
#' @return NULL
#'
#' @examples
#'
#' Fetch species counts for Baker Woodlot at Michigan State University:
#' df <- iNat_IDs(place_id=136412)
#'
#' Fetch species counts of insects for a specific user at a specific location (Baker Woodlot):
#' df <- iNat_IDs(user_id="hanly", taxon_id=47158, place_id=136412)
#' @export iNat_IDs

iNat_IDs <- function(per_page = 200, order = "desc", order_by = NULL, current_taxon = NULL, own_observation = NULL, is_change = NULL,
			taxon_active = NULL, observation_taxon_active = NULL, id = NULL, rank = NULL, observation_rank = NULL,
			user_id = NULL, user_login = NULL, current = NULL, category = NULL, place_id = NULL, quality_grade = NULL,
			taxon_id = NULL, observation_taxon_id = NULL, iconic_taxon_id = NULL, observation_iconic_taxon_id = NULL,
			lrank = NULL, hrank = NULL, observation_lrank = NULL, observation_hrank = NULL, without_taxon_id = NULL,
			d1 = NULL, d2 = NULL, observation_created_d1 = NULL, observation_created_d2 = NULL, observed_d1 = NULL,
			observed_d2 = NULL, id_above = NULL, id_below = NULL, page = NULL) {

  options(stringsAsFactors = FALSE)
  api <- "https://api.inaturalist.org/v1/identifications"

  fetch <- list(per_page, order, order_by, current_taxon, own_observation, is_change,
			taxon_active, observation_taxon_active, id, rank, observation_rank,
			user_id, user_login, current, category, place_id, quality_grade,
			taxon_id, observation_taxon_id, iconic_taxon_id, observation_iconic_taxon_id,
			lrank, hrank, observation_lrank, observation_hrank, without_taxon_id,
			d1, d2, observation_created_d1, observation_created_d2, observed_d1,
			observed_d2, id_above, id_below, page)

  names(fetch) <- c("per_page", "order", "order_by", "current_taxon", "own_observation", "is_change",
			"taxon_active", "observation_taxon_active", "id", "rank", "observation_rank",
			"user_id", "user_login", "current", "category", "place_id", "quality_grade",
			"taxon_id", "observation_taxon_id", "iconic_taxon_id", "observation_iconic_taxon_id",
			"lrank", "hrank", "observation_lrank", "observation_hrank", "without_taxon_id",
			"d1", "d2", "observation_created_d1", "observation_created_d2", "observed_d1",
			"observed_d2", "id_above", "id_below", "page")

  res <- GET(api, query=c(fetch,list(page=1)))
  resDF <- fromJSON(httr::content(res, as = "text"),flatten=TRUE)
    for (i in 2:(ceiling(resDF$total_results/resDF$per_page))) {
      res.t <- GET(api, query=c(fetch,list(page=i)))
      resDF.t <- fromJSON(httr::content(res.t, as = "text"),flatten=TRUE)
      resDF$results <- bind_rows(resDF$results,resDF.t$results)
      Sys.sleep(1)
    
    iNatDF <- resDF$results
	}

  iNatDF <- resDF$results

return(iNatDF)

}