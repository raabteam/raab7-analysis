#v. 5th May 2021
#v. 19th July 2021 - RB - updated to take input in RAAB7 format
#v. 7th December 2021 - RB - updated to include functions for LCIs and UCIs

#For binomial outcomes only

#CIs derived from standard errors in equation 6 from Bennett et al. (1991). A Simplified general method for cluster-sample surveys of health in developing countries. World Health Statistics Quarterly; 44(3): 98-106
#Age and sex adjustment for RAAB proportion estimates

#Usage: outcome <- bennett.uci(proportion,numerator,denominator,clusters)    
#proportion = crude/adjusted estimate of proportion you need CI for
#numerator = RAAB survey variable containing cases as 1, others as 0
#denominator = RAAB survey variable containing people examined for outcome as 1, others as 0
#clusters = RAAB survey variable with list of which cluster each person examined lives in

bennett.uci<-function(proportion,numerator,denominator,clusters) 

{
  
  p<-proportion
  xi<-aggregate(denominator,by=list(clusters),FUN=sum)
  yi<-aggregate(numerator,by=list(clusters),FUN=sum)
  c<-length(unique(clusters))
  se<-(c/sum(xi$x)) * sqrt( (sum(yi$x^2) - (2 * p * sum(xi$x*yi$x)) + (p^2 * sum(xi$x^2))) / (c*(c-1)) )
  p.uci <- p + (1.96* se)
  
}



#Usage: outcome <- bennett.lci(proportion,numerator,denominator,clusters)    
#proportion = crude/adjusted estimate of proportion you need CI for
#numerator = RAAB survey variable containing cases as 1, others as 0
#denominator = RAAB survey variable containing people examined for outcome as 1, others as 0
#clusters = RAAB survey variable with list of which cluster each person examined lives in

bennett.lci<-function(proportion,numerator,denominator,clusters) 
  
{
  
  p<-proportion
  xi<-aggregate(denominator,by=list(clusters),FUN=sum)
  yi<-aggregate(numerator,by=list(clusters),FUN=sum)
  c<-length(unique(clusters))
  se<-(c/sum(xi$x)) * sqrt( (sum(yi$x^2) - (2 * p * sum(xi$x*yi$x)) + (p^2 * sum(xi$x^2))) / (c*(c-1)) )
  p.lci <- p - (1.96* se)
  
}



# Usage: prop.age.sex.adjust(pop.tab,raab.tab,numerator,denominator)
# pop.tab = RAAB7 format population file for RAAB being analysed with male and female 5-year age bands
# raab.tab = table of individual-level RAAB data
# numerator = variable from raab.tab with cases coded as 1, others as 0
# denominator =  variable from raab.tab with those examined coded as 1, others as 0

prop.age.sex.adjust<-function(pop.tab,raab.tab,numerator,denominator)
  
{
  
  age.groups.tens<-c("50-59","60-69","70-79","80+")
  
  pop.tab$age.groups.tens<-cut(pop.tab$ageStart,breaks=c(49,59,69,79,110),labels=age.groups.tens)
  
  MPOP<-pop.tab[pop.tab$gender=="male",]
  mpop<-aggregate(MPOP$population,by=list(MPOP$age.groups.tens),FUN=sum)
  names(mpop)[1:2]<-c("age.groups.tens","n")
  mpop$gender<-"male"
  
  FPOP<-pop.tab[pop.tab$gender=="female",]  
  fpop<-aggregate(FPOP$population,by=list(FPOP$age.groups.tens),FUN=sum)
  names(fpop)[1:2]<-c("age.groups.tens","n")
  fpop$gender<-"female"
  
  fpop$age.groups.tens<-as.numeric(as.factor(age.groups.tens))
  mpop$age.groups.tens<-as.numeric(as.factor(age.groups.tens))
  raab.tab$age.groups.tens<-as.numeric(as.factor(raab.tab$age.groups.tens))
  
  for (j in 1:length(age.groups.tens))
    
  {
    
    mpop$examined[mpop$age.groups.tens==j] <- sum(raab.tab$exam_status[raab.tab$age.groups.tens==j & raab.tab$gender=="male"]=="exam_status_examined",na.rm=T)
    fpop$examined[fpop$age.groups.tens==j] <- sum(raab.tab$exam_status[raab.tab$age.groups.tens==j & raab.tab$gender=="female"]=="exam_status_examined",na.rm=T)
    
    mpop$numerator[mpop$age.groups.tens==j] <- sum(numerator[raab.tab$age.groups.tens==j & raab.tab$gender=="male"],na.rm=T) 
    mpop$denominator[mpop$age.groups.tens==j] <- sum(denominator[raab.tab$age.groups.tens==j & raab.tab$gender=="male"],na.rm=T)
    
    fpop$numerator[fpop$age.groups.tens==j] <- sum(numerator[raab.tab$age.groups.tens==j & raab.tab$gender=="female"],na.rm=T) 
    fpop$denominator[fpop$age.groups.tens==j] <- sum(denominator[raab.tab$age.groups.tens==j & raab.tab$gender=="female"],na.rm=T)
    
  }  
  
  mpop$exam.infl.fact<-mpop$n/mpop$examined    
  fpop$exam.infl.fact<-fpop$n/fpop$examined
  
  p.age.sex.adj<- (sum(fpop$numerator * fpop$exam.infl.fact) + sum(mpop$numerator * mpop$exam.infl.fact))/(sum(fpop$denominator * fpop$exam.infl.fact) + sum(mpop$denominator * mpop$exam.infl.fact))
  
}


# Usage: prop.age.adjust(pop.subtab,raab.subtab,numerator.subpop,denominator.subpop)
# pop.subtab = RAAB7-format population file for RAAB being analysed with 5-year age bands for the subpopulation you are analysing (i.e., males or females)
# raab.subtab = table of individual-level RAAB data from subpopulation you are analysing (i.e., males or females)
# numerator.subtab = variable from raab.subtab with cases coded as 1, others as 0
# denominator.subtab =  variable from raab.subtab with those examined coded as 1, others as 0

prop.age.adjust<-function(pop.subtab,raab.subtab,numerator.subpop,denominator.subpop)
  
{
  
  age.groups.tens<-c("50-59","60-69","70-79","80+")
  
  pop.subtab$age.groups.tens<-cut(pop.subtab$ageStart,breaks=c(49,59,69,79,110),labels=age.groups.tens)
  
  subpop<-aggregate(pop.subtab$population,by=list(pop.subtab$age.groups.tens),FUN=sum)
  
  names(subpop)<-c("age.groups.tens","n")
  subpop$age.groups.tens<-as.numeric(as.factor(age.groups.tens))
  raab.subtab$age.groups.tens<-as.numeric(as.factor(raab.subtab$age.groups.tens))
  
  for (j in 1:length(age.groups.tens))
    
  {
    
    subpop$examined[subpop$age.groups.tens==j] <- sum(raab.subtab$exam_status[raab.subtab$age.groups.tens==j]=="exam_status_examined",na.rm=T)
    
    subpop$numerator[subpop$age.groups.tens==j] <- sum(numerator.subpop[raab.subtab$age.groups.tens==j],na.rm=T) 
    subpop$denominator[subpop$age.groups.tens==j] <- sum(denominator.subpop[raab.subtab$age.groups.tens==j],na.rm=T)
    
  }  
  
  subpop$exam.infl.fact<-subpop$n/subpop$examined    
  
  p.age.adj<-sum(subpop$numerator * subpop$exam.infl.fact)/sum(subpop$denominator * subpop$exam.infl.fact)
  
}

#Where a confidence interval is over 100, replace with 100; where it is under 0, replace with 0.

lci_0<-function(x){if(!is.na(x) & x < 0){x <- 0} else {x <- x}}
hci_100<-function(x){if(!is.na(x) & x > 100){x <- 100} else {x <- x}}

# Function to generate the auth token

generateAuthToken <- function(username, password, basic_auth) {
  # Headers for the authentication request
  headers <- c(
    "Authorization" = paste("Basic", basic_auth),
    "Content-Type" = "application/json"
  )
  
  # Parameters for the authentication request
  params <- toJSON(list(
    grant_type = "password",
    username = username,
    password = password
  ), auto_unbox = TRUE)
  
  # Debugging: Print the params and headers for visibility
  print("Headers for authentication:")
  print(headers)
  print("Parameters for authentication:")
  print(params)
  
  # Perform the authentication request with error handling
  tryCatch({
    # Perform the POST request to obtain the authentication token
    res <- postForm("https://www.raab.world/api/access_token", 
                    .opts = list(postfields = params, 
                                 httpheader = headers, 
                                 followlocation = TRUE), 
                    style = "httppost")
    
    # Debugging: Check the response
    print("Response from authentication request:")
    print(res)
    
    # Parse the response (assuming it's JSON)
    res_parsed <- fromJSON(res)
    
    # Check if the access_token exists in the response
    if (!is.null(res_parsed$access_token)) {
      return(res_parsed$access_token)
    } else {
      stop("Authentication failed: No access token returned. Please check your credentials.", call. = FALSE)
    }
    
  }, error = function(e) {
    # Check if it's a 401 error (unauthorized)
    if (grepl("401", e$message)) {
      stop("Authentication failed: Invalid credentials. Please check your username, password, or basic authentication token.", call. = FALSE)
    } else if (grepl("403", e$message)) {
      # Handle Forbidden error (e.g., insufficient permissions)
      stop("Authentication failed: You do not have permission to access this resource. Please check your credentials or permissions.", call. = FALSE)
    } else {
      # Handle other errors
      stop(paste("An unexpected error occurred during authentication. Please check your credentials."), call. = FALSE)
    }
  })
}

# Function to perform the API request using the generated auth token
performApiRequest <- function(auth_token, raabID) {
  # Headers for the API request
  headers <- c(
    "Authorization" = paste("Bearer", auth_token),
    "Content-Type" = "application/json"
  )
  
  # Parameters for the API request (GraphQL query)
  params <- sprintf(
    '{"query":"query readSurveyByRaabID($raabID: String!) {\\n    readSurveyByRaabID(raab_id: $raabID) {\\n        title\\n        page_url\\n    }   \\n}","variables":{"raabID":"%s"}}', 
    raabID
  )
  
  # Debugging: Print the params and headers for visibility
  print("Headers for API request:")
  print(headers)
  print("Parameters for API request:")
  print(params)
  
  # Perform the API request
  res <- getURL(
    "https://www.raab.world/api/graphql", 
    .opts = list(httpheader = headers, postfields = params, followlocation = TRUE)
  )
  
  # Debugging: Check the response
  print("Response from API request:")
  print(res)
  
  # Parse the JSON response
  res_parsed <- fromJSON(res)  # This assumes the response is in JSON format
  
  # Extract the page_url from the response
  if (!is.null(res_parsed$data$readSurveyByRaabID$page_url)) {
    page_url <- res_parsed$data$readSurveyByRaabID$page_url
  } else {
    stop("Error: The expected 'page_url' was not found in the response.", call. = FALSE)
  }
  
  # Return the page_url
  return(page_url)
}
