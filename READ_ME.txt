vegClass package update draft and user interface (app) draft

Featured updates:

Parallel processing in the main function to speed up processing
Automatically index the input database to speed up processing
Input number of cores used for processing in the main function
Adding Stand_CN to output .csv
Option to remove database indices after processing
Custom classification script option
Option to remove attributes from final .csv
If a custom attribute has a duplicate name, the script will still run but rename your attribute with a "_2" 

Open vegClass2.0.Rproj to have all the necessary scripts and description file open in one project space. Scripts can be opened and ran in other project spaces as well. 

This folder contains 2 R scripts to use for running vegClass. The run_vegClass.R is the basic main function and the app.R is a draft of a vegClass user interface which runs the main function from the run_vegClass.R script. The app will autofill all of the inputs that the run_vegClass.R script, but they can all be changed in the interface. Output .csv files can be viewed directly in the user interface along with flowcharts on how values are classified by clicking on a value in the table. A different vegClass output .csv that was not run with the app can also be inputted into the view output page to see flowcharts or generate graphs. 

The custom_project_attr_template.r is located inside the R folder. It can be a good starting point for what is needed in a function to have vegClass run it. 

attribute_logic_dictionary_detailed.md is a markdown file that can be opened in R. It will have a description on how each attribute is generated. 

A FVS output database has also been provided in this folder space for convenience. 

The CustomVars_vegClass_BKNF.xlsx is the "order form" from the original vegClass package. It can be used to set some custom variables and the file path should be set in customVars if you want to use it. 

Notes:

A couple of warnings will pop up in the R console when running the app script but the app still runs.

If a syntax error pops up after clicking on a value, close out of the popup and try clicking again.

This app is still a work in progress, any thoughts on how it can be made better are appreciated! 

Sometimes plots take a while to render if a large number of variables are selected. 

Any questions email Abby at abby.acthenhagen@usda.gov