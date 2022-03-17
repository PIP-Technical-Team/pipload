# pipload 0.1.20

# pipload 0.1.19
* add parameter `suffix` to `pip_load_aux()` to retrieve complementary data of measures. For instance `pip_load_aux("ppp", suffix = "vintage")`

* Add new way to create vintage folders in `pip_create_globals()` using the new function  `pip_create_vintage()`. **This is still work in progress**. 


# pipload 0.1.18
* Add code coverage `covr`



# pipload 0.1.17
* remove deprecated messages. They will be used in the future. 

# pipload 0.1.16

* fix issue with self reference in data.table when creating classed

* add pip_class tests

# pipload 0.1.15

* Change creation of pip classes in S3 mode. now, the main function is `as_pip`

# pipload 0.1.14

* make functions consistent with output from `suyrvey_id_to_vars()`

* modify `pip_class()` functions to make sure they always return data.tables

* create `assign_pipclass()` function to make it work easily with pip classes

* create classes with structures for future development

# pipload 0.1.13
* read most recent dir in each object of list `create_dir()` inside `pip_create_globals()`

* Create `pip_load_dlw_inventory()`, `pip_find_dlw()`, and `pip_load_dlw()`

* add `lifecycle` badges


# pipload 0.1.12
* Allow "all" in `pip_load_all_aux()`

* messages for failure running into errors

# pipload 0.1.11
* add function `pip_load_dlw_inventory()`

* fix bugs with installation. 

# pipload 0.1.10
* make sure `pipload` work in RS connect server. 

* `pip_create_globals()` vignette.

# pipload 0.1.9
* add max years for countries and aggregates in `pip_create_globals()` this 
option should be moved somewhere else. Maybe the pipeline, but for now it is 
included here. 

* Allow user to use `version = 0` in `pip_load_aux()` to refer to the current 
version

* allow users to select output directory in `pip_create_globals()`  and vintage
name.

# pipload 0.1.8
* fix issue with several version in pip_find_data

# pipload 0.1.7
* implement function `pip_load_all_aux()`

# pipload 0.1.6
* Fix messaging in `pip_load_results()`
* Add function `read_by_format()` to `pip_load_aux()`

# pipload 0.1.5
* add `verbose` parameter

# pipload 0.1.4
* Improve messages when `root_dir` is not available
* Add function `add add_gls_to_env()` to be used by other PIP packages
* add old povcalnet paths to retrieve master file vintages.

# pipload 0.1.3
* replace global variables in .Renviron, so  PIP_DATA_ROOT_FOLDER for PIP_ROOT_DIR

# pipload 0.1.2
* Add function `pip_load_results()` to load results from precalculated indicators

# pipload 0.1.1
* make sure we return `survey_id` properly in `pip_find_data()`
* replace directory paths in zzz.R for objects from `pip_create_globals()`

# pipload 0.1.0
* load data with pip classes

# pipload 0.0.1

* Added a `NEWS.md` file to track changes to the package.
* Added function `pip_find_cache()` to find cache data
* Added function `pip_load_cache()` to load cache data
* All functions work now with tool "PC" or "TB"
