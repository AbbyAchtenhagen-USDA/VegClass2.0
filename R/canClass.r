################################################################################
#Function canClass
#
#This function calculates canopy classes based on the canopy cover percent
#corrected for overlap. Canopy classes can be calculated using USFS Region 1,
#Region 2, Region 3, Region 4, and Region 6 rule sets described in
#Vandendriesche, D., 2013. A Compendium of NFS Regional Vegetation
#Classification Algorithms. USDA Forest Service. Fort Collins, CO
#
#Arguments
#
#cc:     Percent Canopy cover corrected for overlap.
#
#region: USFS region code. Valid values are 1, 2, 3, 4, 5, 6, 8, 9, or 10.
#
#debug:  logical variable indicating if debug statements should be printed. By
#        default this value is set to FALSE.
################################################################################

##'
#' Calculate Canopy Class (canClass)
#'
#' Calculates canopy class based on the canopy cover percent corrected for overlap.
#' Canopy classes can be calculated using USFS Region 1, Region 2, Region 3, Region 4, and Region 6 rule sets described in:
#' Vandendriesche, D., 2013. A Compendium of NFS Regional Vegetation Classification Algorithms. USDA Forest Service. Fort Collins, CO.
#'
#' @param cc Percent canopy cover corrected for overlap.
#' @param region USFS region code. Valid values are 1, 2, 3, 4, 5, 6, 8, 9, or 10.
#' @param debug Logical; if TRUE, prints debug output to R console. Default is FALSE.
#'
#' @returns Integer code for canopy class (region-specific meaning).
#'
#' @examples
#' canClass(cc = 45, region = 3)
#'
#' @export
canClass <- function(cc,
                     region,
                     debug = F)
{
  #Do debug
  if(debug)
  {
    cat("In function canClass", "\n",
        "CC:", cc, "\n",
        "USFS Region:", region, "\n", "\n")
  }

  #Call function for calculated R1 canopy class
  if(region == 1)
  {
    CAN_CLASS <- canClassR1(cc,
                            debug)
  }

  #Call function for calculating R3 canopy class
  else if(region == 3)
  {
    CAN_CLASS <- canClassR3(cc,
                            debug)
  }

  #Call function for calculating R2, R4, R6. This is the default if region is
  #not 1 or 3.
  else
  {
    CAN_CLASS <- canClassR246(cc,
                              debug)
  }

  #Return CAN_CLASS
  return(CAN_CLASS)
}

################################################################################
#canClassR1
#
#This function calculates canopy class from percent canopy cover corrected for
#overlap using USGS R1 rule sets. Vandendriesche, D., 2013. A Compendium of NFS
#Regional Vegetation Classification Algorithms. USDA Forest Service.
#Fort Collins, CO R1-9.
#
#Arguments:
#
#cc:    percent canopy cover corrected for overlap.
#
#debug: logical variable indicating if debug statements should be printed. By
#       default this value is set to FALSE.
#
#value:
#0: 0  - 10%  CC (Sparse)
#1: 10 - 25%  CC (Low)
#2: 25 - 40%  CC (Open)
#3: 40 - 60%  CC (Moderate)
#4: 60 - 100% CC (Closed)
################################################################################

##'
#' Calculate Canopy Class for Region 1 (canClassR1)
#'
#' Calculates canopy class from percent canopy cover corrected for overlap using USFS R1 rule sets.
#' Vandendriesche, D., 2013. A Compendium of NFS Regional Vegetation Classification Algorithms. USDA Forest Service. Fort Collins, CO R1-9.
#'
#' @param cc Percent canopy cover corrected for overlap.
#' @param debug Logical; if TRUE, prints debug output to R console. Default is FALSE.
#'
#' @returns Integer code for canopy class (0: Sparse, 1: Low, 2: Open, 3: Moderate, 4: Closed).
#' @returns 0: 0  - 10%  CC (Sparse)
#' @returns 1: 10 - 25%  CC (Low)
#' @returns 3: 40 - 60%  CC (Moderate)
#' @returns 4: 60 - 100% CC (Closed)
#'
#' @examples
#' canClassR1(cc = 45)
#'
#' @export
canClassR1 <- function(cc,
                     debug = F)
{

  #Do debug
  if(debug)
  {
    cat("In function canClassR1", "\n", "\n")
  }

  #Sparse
  if(cc < 10)
  {
    CAN_CLASS = 0
  }

  #Low
  else if(cc >= 10 & cc < 25)
  {
    CAN_CLASS = 1
  }

  #Open
  else if(cc >= 25 & cc < 40)
  {
    CAN_CLASS = 2
  }

  #Moderate
  else if(cc >= 40 & cc < 60)
  {
    CAN_CLASS = 3
  }

  #Closed
  else
  {
    CAN_CLASS = 4
  }

  #Do debug
  if(debug)
  {
    cat("CC:", cc, "\n")
    cat("CAN_CLASS:", CAN_CLASS, "\n")
  }

  return(CAN_CLASS)
}

################################################################################
#canClassR246
#
#This function calculates canopy class from percent canopy cover corrected for
#overlap using USFS R2, R4, R6 rule sets. Vandendriesche, D., 2013. A Compendium
#of NFS Regional Vegetation Classification Algorithms. USDA Forest Service.
#Fort Collins, CO R2-4, R4-6, R6-5.
#
#Arguments:
#
#cc:    percent canopy cover corrected for overlap.
#
#debug: logical variable indicating if debug statements should be printed. By
#       default this value is set to FALSE.
#
#value:
#0: 0  - 10%  CC (Sparse)
#1: 10 - 40%  CC (Low)
#2: 40 - 70%  CC (Moderate)
#3: 70 - 100% CC (Closed)
################################################################################

##'
#' Calculate Canopy Class for Regions 2, 4, 6 (canClassR246)
#'
#' Calculates canopy class from percent canopy cover corrected for overlap using USFS R2, R4, R6 rule sets.
#' Vandendriesche, D., 2013. A Compendium of NFS Regional Vegetation Classification Algorithms. USDA Forest Service. Fort Collins, CO R2-4, R4-6, R6-5.
#'
#' @param cc Percent canopy cover corrected for overlap.
#' @param debug Logical; if TRUE, prints debug output to R console. Default is FALSE.
#'
#' @returns Integer code for canopy class (0: Sparse, 1: Low, 2: Moderate, 3: Closed).
#'
#' @examples
#' canClassR246(cc = 45)
#'
#' @export
canClassR246 <- function(cc,
                       debug = F)
{

  #Do debug
  if(debug)
  {
    cat("In function canClassR246", "\n", "\n")
  }

  #Sparse
  if(cc < 10)
  {
    CAN_CLASS = 0
  }

  #Open
  else if(cc >= 10 & cc < 40)
  {
    CAN_CLASS = 1
  }

  #Moderate
  else if(cc >= 40 & cc < 70)
  {
    CAN_CLASS = 2
  }

  #Closed
  else
  {
    CAN_CLASS = 3
  }

  #Do debug
  if(debug)
  {
    cat("CC:", cc, "\n")
    cat("CAN_CLASS:", CAN_CLASS, "\n")
  }

  return(CAN_CLASS)
}

################################################################################
#canClassR3
#
#This function calculates canopy class from percent canopy cover corrected for
#overlap using USFS R3 rule sets. Vandendriesche, D., 2013. A Compendium
#of NFS Regional Vegetation Classification Algorithms. USDA Forest Service.
#Fort Collins, CO R3-6.
#
#Arguments:
#
#cc:    percent canopy cover corrected for overlap.
#
#debug: logical variable indicating if debug statements should be printed. By
#       default this value is set to FALSE.
#
#value:
#0: 0  - 10%  CC (Sparse)
#1: 10 - 30%  CC (Low)
#2: 30 - 60%  CC (Moderate)
#3: 60 - 100% CC (Closed)
################################################################################

##'
#' Calculate Canopy Class for Region 3 (canClassR3)
#'
#' Calculates canopy class from percent canopy cover corrected for overlap using USFS R3 rule sets.
#' Vandendriesche, D., 2013. A Compendium of NFS Regional Vegetation Classification Algorithms. USDA Forest Service. Fort Collins, CO R3-6.
#'
#' @param cc Percent canopy cover corrected for overlap.
#' @param debug Logical; if TRUE, prints debug output to R console. Default is FALSE.
#'
#' @returns Integer code for canopy class (0: Sparse, 1: Low, 2: Moderate, 3: Closed).
#'
#' @examples
#' canClassR3(cc = 45)
#'
#' @export
canClassR3 <- function(cc,
                       debug = F)
{

  #Do debug
  if(debug)
  {
    cat("In function canClassR3", "\n", "\n")
  }

  #Sparse
  if(cc < 10)
  {
    CAN_CLASS = 0
  }

  #Open
  else if(cc >= 10 & cc < 30)
  {
    CAN_CLASS = 1
  }

  #Moderate
  else if(cc >= 30 & cc < 60)
  {
    CAN_CLASS = 2
  }

  #Closed
  else
  {
    CAN_CLASS = 3
  }

  #Do debug
  if(debug)
  {
    cat("CC:", cc, "\n")
    cat("CAN_CLASS:", CAN_CLASS, "\n")
  }

  return(CAN_CLASS)
}
