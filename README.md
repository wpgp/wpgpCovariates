wpgpCovariates
===================
wpgpCovariates is an R Package interface for downloading raster datasets from [WorldPop](http://www.worldpop.org.uk/) FTP.

What is WorldPop?
High spatial resolution, contemporary data on human population distributions are a prerequisite for the accurate measurement of the impacts of population growth, for monitoring changes and for planning interventions. The WorldPop project aims to meet these needs through the provision of detailed and open access population distribution datasets built using transparent approaches.

Installation
------------

**Installation**
wpgpCovariates isn't available from CRAN yet, but you can get it from github with:

    install.packages("devtools")
    devtools::install_github("worldpopglobal/wpgpCovariates")
    
    # load package
    library(wpgpCovariates)
    
**Basic usage**

After installation you should be able to use three main functions from the library:

 - wpgpListCountries
 - wpgpListCountryCovariates
 - wpgpGetCountryCovariate

----------

**wpgpListCountries** will return a dataframe with all ISO3 available on WorldPop ftp server.
```
wpgpListCountries(username = "ftpUsername", password = "ftpPassword")
      
      
      ISO3 ISOnumber                                  NameEnglish
1      ABW       533                                        Aruba
51     AFG         4                                  Afghanistan
101    AGO        24                                       Angola
151    AIA       660                                     Anguilla
201    ALA       248                                land Islands
251    ALB         8                                      Albania
301    AND        20                                      Andorra
.....
```


----------

**wpgpListCountryCovariates** will return a dataframe of available covariates to download from WorldPop FTP for a country. This function could be used to query the name of the dataset which then could be downloaded for a country.
```
wpgpListCountryCovariates(ISO="NPL", 
			  username = "ftpUsername", 
			  password = "ftpPassword")

ISO3 ISOnumber     CvtName           Year    Description
NPL  524           ccidadminl0       2000    Mastergrid ISO 
NPL  524           ccilc_dst011_2000 2000    Distance to cultivated ..
....
```

----------

**wpgpGetCountryCovariate** will download a raster dataset based on ISO and covariate name.

```
> df <- wpgpGetCountryCovariate(ISO3 = "NPL",
                         covariate = "guf2012_ghsl2000_dst190_2012",
                         year = 2012,
                         destDir ="G:\\WorldPop_Data",
			 username = "ftpUsername", 
			 password = "ftpPassword")
						 
> df
$ISO3
[1] "NPL"

$CvtName
[1] "guf2012_ghsl2000_dst190_2012"

$RstName
[1] "npl_grid_100m_guf2012_ghsl2000_dst190_2012"

$filepath
[1] "G:\\WorldPop_Data/npl_grid_100m_guf2012_ghsl2000_dst190_2012.tif"      

```
wpgpGetCountryCovariate will return a dataframe with *filepath* where raster was downloaded, *RstName*  full name of the raster, *CvtName* covariate name and *ISO*
        
You can also download multiple raster by using a list as an input
```
wpgpGetCountryCovariate(ISO3 = c("NPL", "BTN"),
			covariate = c("guf2012_ghsl2000_dst190_2012","px_area"),
			year = c("2012","2000")
			destDir ="D:\\WorldPop_Data",
			username = "ftpUsername", 
			password = "ftpPassword")
```
