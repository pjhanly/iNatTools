% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iNat_sc.R
\name{iNat_sc}
\alias{iNat_sc}
\title{iNaturalist Species Counts Import}
\usage{
iNat_sc(
  per_page = 200,
  order = "desc",
  order_by = "created_at",
  acc = NULL,
  captive = NULL,
  endemic = NULL,
  geo = NULL,
  identified = NULL,
  introduced = NULL,
  mappable = NULL,
  native = NULL,
  only_id = NULL,
  out_of_range = NULL,
  pcid = NULL,
  photos = NULL,
  popular = NULL,
  taxon_is_active = NULL,
  threatened = NULL,
  verifiable = "true",
  id = NULL,
  not_id = NULL,
  place_id = NULL,
  project_id = NULL,
  rank = NULL,
  site_id = NULL,
  taxon_id = NULL,
  without_taxon_id = NULL,
  taxon_name = NULL,
  user_id = NULL,
  user_login = NULL,
  day = NULL,
  month = NULL,
  year = NULL,
  term_id = NULL,
  term_value_id = NULL,
  without_term_value_id = NULL,
  acc_above = NULL,
  acc_below = NULL,
  d1 = NULL,
  d2 = NULL,
  created_d1 = NULL,
  created_d2 = NULL,
  created_on = NULL,
  observed_on = NULL,
  unobserved_by_user_id = NULL,
  apply_project_rules_for = NULL,
  cs = NULL,
  csa = NULL,
  csi = NULL,
  geoprivacy = NULL,
  taxon_geoprivacy = NULL,
  hrank = NULL,
  lrank = NULL,
  id_above = NULL,
  id_below = NULL,
  identifications = NULL,
  lat = NULL,
  lng = NULL,
  radius = NULL,
  nelat = NULL,
  nelng = NULL,
  swlat = NULL,
  swlng = NULL,
  list_id = NULL,
  not_in_project = NULL,
  not_matching_project_rules_for = NULL,
  q = NULL,
  search_on = NULL,
  quality_grade = NULL,
  updated_since = NULL,
  viewer_id = NULL,
  reviewed = NULL,
  locale = NULL,
  preferred_place_id = NULL,
  ttl = NULL
)
}
\arguments{
\item{per_page}{Results requested per API request page. Cannot exceed 200}

\item{order}{Query sort order. "desc" (default) for descending and "asc" for ascending}

\item{order_by}{Sorting field for order paramter. Options are "created_at" (default), "observed_on", "species_guess", "votes", and "id"}

\item{acc}{Positional accuracy is specified: "true" or "false"}

\item{captive}{Fetch captive/cultivated observations: "true" or "false"}

\item{endemic}{Fetch taxa endemic to location: "true" or "false"}

\item{geo}{Fetch georeferenced observations: "true" or "false"}

\item{identified}{Fetch observations with community IDs: "true" or "false"}

\item{introduced}{Fetch taxa introduced to location: "true" or "false"}

\item{native}{Fetch taxa native to location: "true" or "false"}

\item{only_id}{Fetch only the record ID: "true" or "false"}

\item{out_of_range}{Fetch taxa outside of known range: "true" or "false"}

\item{pcid}{Fetch observations identified by project curators: "true" or "false"}

\item{photos}{Fetch observations with photos: "true" or "false"}

\item{popular}{Fetch only favorited observations: "true" or "false"}

\item{taxon_is_active}{Fetch only taxa with active concepts: "true" or "false"}

\item{threatened}{Fetch taxa threatened in their location: "true" or "false"}

\item{verifiable}{Fetch only observations classified as Needs ID or are Research Grade: "true" or "false". Default = "true"}

\item{id}{Fetch a specific observation ID number}

\item{not_id}{Exclude a specific observation ID number from a fetch}

\item{place_id}{Fetch observations based on place ID}

\item{project_id}{Fetch observations from "[project name]"}

\item{rank}{Fetch observations with a specific taxon rank. Example options are: "kingdom", "phylum", "order", "family", "genus", "species"}

\item{site_id}{Fetch observations from iNaturalist affiliate site ID}

\item{taxon_id}{Fetch observations of taxon ID and descendants}

\item{without_taxon_id}{Exclude fetching of observations of a taxon ID and descendants}

\item{taxon_name}{Fetch observations matching "[common name]" or "[scientific name]"}

\item{user_id}{Fetch observations from user ID number}

\item{user_login}{Fetch observations by "[user name]"}

\item{day}{Fetch observation from a given day}

\item{month}{Fetch observations from a given month}

\item{year}{Fetch observations from a given year}

\item{term_id}{Control term ID for annotations}

\item{term_value_id}{Combined with the term_id paramter to fetch observations with specific annotations}

\item{without_term_value_id}{Combined with the term_id parameter to exclude observations with specific annotations}

\item{acc_above}{Positional accuracy must be above this value (meters)}

\item{acc_below}{Positional accuracy must be below this value (meters)}

\item{d1}{Fetch observations on or after a specific date: "YYYY-MM-DD"}

\item{d2}{Fetch observations on or before a specific date: "YYYY-MM-DD"}

\item{created_d1}{Fetch observations created on or after a specific date: "YYYY-MM-DD"}

\item{created_d2}{Fetch observations created on or before a specific date: "YYYY-MM-DD"}

\item{created_on}{Fetch observations created on a specific date: "YYYY-MM-DD"}

\item{observed_on}{Fetch observations from a specific date: "YYYY-MM-DD"}

\item{unobserved_by_user_id}{Fetch observations of taxa never observed by a user ID number}

\item{apply_project_rules_for}{Fetch observations that match rules of "[project name]"}

\item{cs}{Fetch observations of taxa with a specific conservation status code. When the place_id parameter is specified, the status code will be specific to that location}

\item{csa}{Fetch observations of taxa with a conservation status code from a specific authority}

\item{csi}{Fetch observations of taxa with a specific IUCN conservation status. Options are "LC", "NT", "VU", "EN", "CR", "EW", "EX"}

\item{geoprivacy}{Fetch observations with this geoprivacy setting: "obscured", "obscured_private", "open", or "private}

\item{taxon_geoprivacy}{Fetch observations with the specified most conservative geoprivacy applied by a conservation status: "obscured", "obscured_private", "open", or "private}

\item{hrank}{Fetch observations with this taxon rank or lower: Example options are: "kingdom", "phylum", "order", "family", "genus", "species"}

\item{lrank}{Fetch observations with this taxon rank or higher: Example options are: "kingdom", "phylum", "order", "family", "genus", "species"}

\item{id_above}{Fetch observations above this ID number}

\item{id_below}{Fetch observations below this ID number}

\item{identifications}{Fetch observations with identifications where: "most_agree", "most_disagree", "some_agree"}

\item{lat}{Fetch observations within a specified radius of this latitude (decimal degrees)}

\item{lng}{Fetch observations within a specified radius of this longitude (decimal degrees)}

\item{radius}{Fetch observations within a kilometer radius of specified variables lat and lng}

\item{nelat}{Set northeast latitude (decimal degrees) of bounding box to fetch observations within}

\item{nelng}{Set norteast longitude (decimal degrees) of bounding box to fetch observations within}

\item{swlat}{Set southwest latitude (decimal degrees) of bounding box to fetch observations within}

\item{swlng}{Set southwest longitude (decimal degrees) of bounding box to fetch observations within}

\item{list_id}{Fetch taxon in list of IDs}

\item{not_in_project}{Fetch observations that are not in a given project ID}

\item{not_matching_project_rules_for}{Fetch observations that do not match the rules of a project ID}

\item{q}{String search of observation properties that can be combined with paramter search_on}

\item{search_on}{Properties to search on based on parameter q string query: "names", "tags", "description", "place"}

\item{quality_grade}{Fetch observations of this quality grade: "casual", "needs_id", "research"}

\item{updated_since}{String query for fetching observations updated since a given time}

\item{viewer_id}{Fetch observations reviewed by a user ID}

\item{reviewed}{Fetch observation that have ("true") or have no ("false") been reviewed by the user specified in the viewer_id parameter}

\item{locale}{Specify locale preference for taxon common names}

\item{preferred_place_id}{Specify place preference for regional common names}

\item{ttl}{Specify the Cache-Control HTTP header in seconds}

\item{mappaple}{Fetch observations that can show on map tiles: "true" or "false"}
}
\description{
Function for pulling summary species counts data from the iNaturalist API
}
\examples{

Fetch species counts for Baker Woodlot at Michigan State University:
df <- iNat_sc(place_id=136412)

Fetch species counts of insects for a specific user:
df <- iNat_sc(user_id="hanly", taxon_id=47158)
}
