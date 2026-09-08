
#' Calculate Canopy Storiedness (BA_STORY)
#'
#' Calculates canopy layers/storiedness (BA_STORY) for a stand/plot in
#' accordance with the Region 3 ruleset from Vandendriesche, D., 2013. A
#' Compendium of NFS Regional Vegetation Classification Algorithms. USDA Forest
#' Service. Fort Collins, CO. (2013 pg. R3-4 - R3-5).
#'
#' @param data Data frame containing tree records from a single stand or plot.
#'   Must contain stand/plot ID, DBH, and expansion factor columns.
#' @param stand Character string for stand/plot ID column name. Default is
#'   "StandID".
#' @param dbh Character string for DBH column name. Default is "DBH".
#' @param expf Character string for TPA column name. Default is "TPA".
#' @param BA Basal area per acre of plot/stand.
#' @param TPA Trees per acre of plot/stand.
#' @param CC Percent canopy cover corrected for overlap of plot/stand.
#' @param debug Logical; if TRUE, prints debug output to R console.
#'
#' @returns Storiedness value (integer value from 0 - 3)
#'
#' @examples
#'  Example usage:
#'  baStory(data, stand = "StandID", dbh = "DBH", expf = "TPA", BA = 120, TPA = 300, CC = 45)
#'@export
baStory<-function(data,
                  stand = "StandID",
                  dbh = "DBH",
                  expf = "TPA",
                  BA = 0,
                  TPA = 0,
                  CC = 0,
                  debug = F)
{
  #Initialize variable to return
  story <- NA

  if(debug)
  {
    cat("In function baStory", "\n")
    cat("Stand:", unique(data[[stand]]), "\n")
    cat("Columns:", "\n",
        "Stand:", stand, "\n",
        "dbh:", dbh, "\n",
        "expf:", expf, "\n", "\n")
  }

  #if TPA is 0, return
  if(TPA <= 0){
    story<-0
    return(story)
  }

  #If data has no rows, return
  if(nrow(data) <= 0){
    story<-0
    return(story)
  }

  #Check for missing columns in data
  missing <- c(stand, dbh, expf) %in% colnames(data)

  #If name of columns provided in stand, dbh, and expf are not found in data
  #warning message is issued and NA value is returned.
  if(F %in% missing)
  {
    story<-0
    cat("One or more input arguments not found in data. Check spelling.", "\n")
    return(story)
  }

  #Calculate BA of each tree record if TREEBA does not exist in data.
  if(! "TREEBA" %in% colnames(data))
  {
    data$TREEBA <- data[[dbh]]^2 * data[[expf]] * 0.005454
  }

  #Print BA, CC, and TPA of plot
  if(debug) cat("BA of plot is", BA, "\n")
  if(debug) cat("CC of plot is", CC, "\n")
  if(debug) cat("TPA of plot is", TPA, "\n")

  #Test if plot is below minimum CC (10) and TPA (100). If so, then set story to
  #0.
  if(CC < 10 & TPA < 100)
  {
    story<-0
    if(debug) cat("Total CC:",CC, " LT 10 and TPA:", TPA,
                                "LT 100.", "\n")
  }

  #Test if plot is below minimum CC (10) and above minimum TPA (100). If so,
  #then set story to 1.
  else if(CC < 10 & TPA >= 100)
  {
    story<-1
    if(debug) cat("Total CC:",CC, " LT 10 and TPA:", TPA,
    "GE 100.", "\n")
  }

  #Else calculate story using canopy layers/ R3 storiedness algorithm
  else
  {
    #Set story to 3 (multistory). This value will be returned if plot is not
    #determined to be single or two story in the logic below.
    story<- 3

    #Test if sum of BA for trees greater than 24 inches is greater than 70% of
    #total stand basal area. If this criteria is true then stand is single story.
    if((sum(data$TREEBA[data[[dbh]] >= 24]) / BA )>= 0.7)
    {
      if(debug) cat("BA for trees >= 24 inches:",
                    sum(data$TREEBA[data[[dbh]] >= 24]),
                    "GE", "total BA:", BA, "\n")
      story = 1
    }

    else
    {
      #Initialize bottom variable (moving lower limit of diameter range being
      #evaluated)
      bottom = 0

      #Initialize boolean variable used to determine if story was found in
      #while loop below
      storyFound = F

      while(bottom < 24 & !storyFound)
      {
        #Determine top variable (moving upper limit of diameter range being
        #evaluated)
        top = bottom + 8

        #Calculate BA in current 8" DBH range
        baSlide<-sum(data$TREEBA[data[[dbh]] >= bottom & data[[dbh]] < top])
        if(debug) cat("baSlide", "for trees between", bottom, "and",
                      top, "DBH:", baSlide, "\n")

        #Test if plot is two story based on value of baSlide. If baSlide is
        #greater than or equal to 60% of stand BA and less than 70% of stand BA,
        #then stand is considered to have two stories.
        if((baSlide/BA) >= 0.6 & (baSlide/BA) < 0.7)
        {
          if(debug) cat("baSlide for trees between", bottom, "and", top, "DBH:",
                         baSlide, "GE 60% of total plot BA:", BA, "\n")
          story = 2
          # storyFound = T
        }

        #Test if plot is single story based on value of baSlide. If baSlide is
        #greater than or equal to 70% of stand BA, then stand is considered to
        #have one story.
        if((baSlide/BA) >= 0.7)
        {
          if(debug) cat("baSlide for trees between", bottom, "and", top, "DBH:",
                        baSlide, "GE 70% of total plot BA:", BA, "\n")
          story = 1
          storyFound = T
        }

        #increment bottom by 1"
        bottom = bottom + 1
      }
    }
  }

  return(story)
}
