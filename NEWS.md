# pipload (development version)
* add max years for countries and aggregates in `pip_create_globals()` this 
option should be moved somewhere else. Maybe the pipeline, but for now it is 
included here. 

* Allow user to use `version = 0` in `pip_load_aux()` to refer to the current 
version

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
