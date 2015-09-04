Configuration
=============

## Registration
The first step of working with OpenML should be to register yourself at the [OpenML website](http://www.openml.org). Most of the package's functions require a API authentication key which is only accessible with a (free) account. You can access that key after logging into your account when you go to http://www.openml.org/u/YOUR_USER_ID#!api.

## Permanently setting configuration
After the registration you should create a configuration file. You can easily create this file using the command `saveOMLConfig`.

The `config` file may contain the following information:
* apikey
    * required to access the server
* server
    * default: `http://www.openml.org`
* verbosity:
    * `0`: normal output
    * `1`: info output (default)
    * `2`: debug output
* cachedir:
    * directory where current cache contents ist stored, the default cache directory can be obtained 
      by the R command `file.path(tempdir(), "cache")`
* arff.reader:
    * `RWeka`
    * `farff`

An exemplary configuration can look as follows 

```r
saveOMLConfig(apikey = "ADD_THE_PUBLIC_READONLY_API_KEY_HERE")
```
    
The configuration file is not mandatory. Permanently setting your API key via the `config` is recommended, 
as this key is required to access the OpenML server. Note however, that basically everybody who has access 
to your computer can read the configuration file and see your password, so please use this password **only** for OpenML.

### Manually creating the config file
Alternatively you can manually create a file `~/.openml/config` in your home directory (you can use the R command 
`path.expand("~/.openml/config")` to get the full path to the configuration file on your operating system).

An exemplary minimal `config` file may look like


```r
apikey = "ADD_THE_PUBLIC_READONLY_API_KEY_HERE"
```

If one manually modifies the `config` file  one needs to reload the modified `config` file to your current R session 
using `loadOMLConf()`.

### Temporarily changing the configuration
If you want to modify your configuration only for the current R session and without changing the `config` file, 
you should have a look at `setOMLConfig`. 

If you have done these steps, you are ready to go. Have fun!

----------------------------------------------------------------------------------------------------
Jump to:    
[Introduction](1-Introduction.md)  
Configuration  
[Stage 0 - Listing](3-Stage-0-Listing.md)  
[Stage 1 - Downloading](4-Stage-1-Downloading.md)  
[Stage 2 - Running models on tasks](5-Stage-2-Running.md)  
[Stage 3 - Uploading](6-Stage-3-Uploading.md)  
[Example workflow with mlr](8-Example-workflow-with-mlr.md)
