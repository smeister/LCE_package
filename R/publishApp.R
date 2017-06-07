


#rsconnect::setAccountInfo(name='smeister',
#                          token='C445EF079A35EB73CDA602B72E14C86E',
                          #secret='7ZCX/llRXBvQt/T0t4pAerTVbOnca6cXowTe4UmC')

#library(rsconnect)
#rsconnect::deployApp('/Users/smeister/Dropbox/EPFL/Lab book/R/packages/LCE.Package2/LCE.Package/virus')
#library(LCE.Package)

### 1 ### Install the package on the shiny server directly
#su - -c "R -e \"devtools::install_github('user/shinypackage')\""

### 2 ### Change the owner of the package folder
#chown -R shiny:shiny /usr/local/lib/R/site-library/shinypackage/

### 3 ### Restart the system
#systemctl restart shiny-server


# devtools::install_github("username/packagename")
