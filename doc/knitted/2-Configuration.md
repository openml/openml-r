Configuration
=============




## Registration
The first step of working with OpenML should be to register yourself at the [OpenML website](http://www.openml.org).
Most of the package's functions require a API authentication key which is only accessible with a (free) account.
To access the API key

* log into your account
* and then go to <http://www.openml.org/u#!api>.

For demonstration purposes, we have created a public, read-only API key (`"c1994bdb7ecb3c6f3c8f3b35f4b47f1f"`), which will be used in the following to make the examples executable.

## Permanently setting configuration
After registration you should create a configuration file. 
The `config` file may contain the following information:

* `apikey`:
    * required to access the server
* `server`:
    * default: `http://www.openml.org`
* `verbosity`:
    * `0`: normal output
    * `1`: info output (default)
    * `2`: debug output
* `cachedir`:
    * directory where current cache contents are stored; the default cache directory can be obtained
      by the R command `file.path(tempdir(), "cache")`.
* `arff.reader`:
    * `RWeka`: This is the standard Java parser used in Weka.
    * `farff`: The [farff package](http://www.github.com/mlr-org/farff) lives below the mlr-org and is a newer, faster parser without Java.

The configuration file is not mandatory. Yet, permanently setting your API key via a `config` file is recommended,
as this key is required to access the OpenML server. Note however, that basically everybody who has access
to your computer can read the configuration file and see your API key. With your API key other users have full
access to your account via the API, so please handle the API key with care.

The configuration file and some related things are also explained in the [OpenML Wiki](https://github.com/openml/OpenML/wiki/Client-API).     

### Creating the configuration file in R

You can easily create a configuration file using the command `saveOMLConfig`.
An standard configuration might look as follows

```r
saveOMLConfig(apikey = "c1994bdb7ecb3c6f3c8f3b35f4b47f1f")
```
where `"c1994bdb7ecb3c6f3c8f3b35f4b47f1f"` should be replaced by your personal API key. 

### Manually creating the configuration file
Alternatively, you can manually create a file `~/.openml/config` in your home directory (you can use the R command
`path.expand("~/.openml/config")` to get the full path to the configuration file on your operating system). The
`config` file consists of `key = value` pairs. An exemplary minimal `config` file might look as follows

```r
apikey = c1994bdb7ecb3c6f3c8f3b35f4b47f1f
```
Note that values are not quoted.

If one manually modifies the `config` file  one needs to reload the modified `config` file to your current R session using `loadOMLConfig()`.
To query the current configuration, one can use

```r
getOMLConfig()
```

```
## OpenML configuration:
##   server           : http://api_new.openml.org/v1
##   cachedir         : L:\Users\Admin2\AppData\Local\Temp\Rtmp2VANdO
##   verbosity        : 0
##   arff.reader      : farff
##   apikey           : ***************************47f1f
```


### Temporarily changing the configuration
If you want to modify your configuration only for the current R session and without changing the `config` file,
you can use `setOMLConfig()` with the same key/value pairs as above.

If you have done these steps, you are ready to go. Have fun!

----------------------------------------------------------------------------------------------------
Jump to:

- [Introduction](1-Introduction.md)
- Configuration
- [Listing](3-Listing.md)
- [Downloading](4-Downloading.md)
- [Running models on tasks](5-Running.md)
- [Uploading](6-Uploading.md)
- [Example workflow with mlr](7-Example-workflow-with-mlr.md)
