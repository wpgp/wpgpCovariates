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
    devtools::install_github("wpgp/wpgpCovariates")
    
    # load package
    library(wpgpCovariates)
    
**Basic usage**

After installation you should be able to use five main functions from the library:

 - wpgpListCountries
 - wpgpListCountryCovariates
 - wpgpGetCountryCovariate
 - wpgpGetPOPTable
 - wpgpGetZonalStats

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
wpgpListCountryCovariates(ISO3="NPL", 
			  username = "ftpUsername", 
			  password = "ftpPassword")

ISO3 ISOnumber     CvtName           Year    Description
NPL  524           ccidadminl0       2000    Mastergrid ISO 
NPL  524           ccilc_dst011_2000 2000    Distance to cultivated ..
....
```

Multiple countries can also be supplied to the function.
```
wpgpListCountryCovariates(ISO3=c("NPL","BTN"), 
			  username = "ftpUsername", 
			  password = "ftpPassword")

ISO3 ISOnumber     CvtName           Year    Description
BTN   64           ccidadminl0       2000    Mastergrid ISO 
BTN   64           ccilc_dst011_2000 2000    Distance to cultivated ..
...
NPL  524           ccidadminl0       2000    Mastergrid ISO 
NPL  524           ccilc_dst011_2000 2000    Distance to cultivated ..
....
```

----------

**wpgpGetCountryCovariate** will download a raster dataset based on ISO and covariate name.

```
> df <- wpgpGetCountryCovariate(ISO3 = "NPL",
                                covariate = "ccilc_dst011_2000"
                                destDir ="G:\\WorldPop_Data",
				username = "ftpUsername", 
				password = "ftpPassword")
						 
> df
$ISO3
[1] "NPL"

$CvtName
[1] "ccilc_dst011_2000"

$RstName
[1] "npl_grid_100m_ccilc_dst011_2000"

$filepath
[1] "G:\\WorldPop_Data/npl_grid_100m_ccilc_dst011_2000.tif"      

```
wpgpGetCountryCovariate will return a list with the *filepath* where the raster was downloaded, *RstName* the full name of the raster, *CvtName* covariate name and the *ISO*
        
You can also download multiple covariates, for multiple years, or countries.
```
wpgpGetCountryCovariate(ISO3 = c("NPL", "BTN"),
                  			covariate = c("ccilc_dst011_2000","px_area")
                  			destDir ="D:\\WorldPop_Data",
                  			username = "ftpUsername", 
                  			password = "ftpPassword")
```
Note that the above command will attempt to retrieve the full combination of ISO3, covariate, and year. If a user has a specific list of files to download, this can be supplied to the function as a dataframe instead of querying by ISO3, covariate, and year. This information is available by returning the detailed results from *wpgpListCountryCovariates* or from a previous return value from *wpgpGetCountryCovariate*.

```
# start by getting the available files
allcovariates <- wpgpListCountryCovariates(ISO3=c("NPL","BTN"), 
					    detailed=T, 
					    username = "ftpUsername", 
					    password = "ftpPassword")

# example: only want to retrieve 5 specific covariates from Nepal and Bhutan
data_example <- allcovariates[c(1,25,50,75,100), c("ISO3","Folder","RstName")]

# download the files to the tempdir() location and save the list to inspect
file_list <- wpgpGetCountryCovariate(df.user=data_example, 
				     username = "ftpUsername",
				     password = "ftpPassword")

data.frame(file_list)
ISO3          CvtName                         RstName   filepath
BTN       ccidadminl0       btn_grid_100m_ccidadminl0   C:\\Temp\\btn_grid_100m_ccidadminl0.tif
BTN ccilc_dst160_2014 btn_grid_100m_ccilc_dst160_2014   C:\\Temp\\btn_grid_100m_ccilc_dst160_2014.tif
BTN     wdpa_dst_2014     btn_grid_100m_wdpa_dst_2014   C:\\Temp\\btn_grid_100m_wdpa_dst_2014.tif
NPL ccilc_dst160_2014 npl_grid_100m_ccilc_dst160_2014   C:\\Temp\\npl_grid_100m_ccilc_dst160_2014.tif
NPL     wdpa_dst_2014     npl_grid_100m_wdpa_dst_2014   C:\\Temp\\npl_grid_100m_wdpa_dst_2014.tif

```

----------

**wpgpGetPOPTable** will download a CSV file of population based on ISO and covariate name. Function will return a dataframe with two columes "ADMINID", "ADMINPOP"

```
> df <- wpgpCovariates::wpgpGetPOPTable("AGO",2000,"G:/WorldPop_Data/",
			  username = "ftpUsername", 
			  password = "ftpPassword")
						 
> df
 ADMINID    ADMINPOP
1    241457   34970.426
2    241458  349494.179
3    241459    7977.856
4    241460    2681.160
5    241461   97864.592
6    241462   77712.153
```

----------

**wpgpGetZonalStats** will download a CSV file of ZonalStats based on ISO and covariate name. Function will return a dataframe with two columes "ADMINID", and name of the covariate 

```
> df <- wpgpGetZonalStats("AGO","ccilc_dst011_2000", stat = "mean" ,"G:/WorldPop_Data/",
			  username = "ftpUsername", 
			  password = "ftpPassword")
						 
> df
 ADMINID    ccilc_dst011_2000
1    241457  1.56037065
2    241458  1.56037065
3    241459  1.61910718
4    241460  1.19869991
5    241461  0.85845653
6    241462  2.56389180
```

