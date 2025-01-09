<h1>Balance Analysis</h1>
This code runs analysis on human balance data providing various parameters relating to the centre of force movement. The following parameters will be saved as a csv file;
<ul>
  <li><b>file_name</b>: The name of the file that the data are based on</li>
  <li><b>Data_read</b>: Whether the data were successfully read from the file (normally 'Success')</li>
  <li><b>trial</b>: This is used to group data by different trial types in the descriptive statistics (see below). This is extracted from the file name using Regex. The Regex expression is set in the analysis settings file. See SettingsFiles/AnalysisSpecDescriptor.txt for further details.</li>
  <li><b>No_frames</b>: The number of frames of data in the time-series</li>
  <li><b>sample_rate</b>: The sample rate of the data. This is read in from the header of the source data</li>
  <li><b>sample_t</b>: The sample time, i.e. the duration of the data analysed</li>
  <li><b>Interp</b>: Whether missing values have been interpolated (see below)</li>
  <li><b>CoF_x_Range / CoF_y_Range</b>: This is simply the maximum - minimum in the Ax / Ay data</li>
  <li><b>CoF_x_pl / CoF_y_pl</b>: The total path length in the Ax / Ay data calculated as the sum of the absolute differences in consecutive points in the time-series</li>
  <li><b>CoF_xy_pl</b>: The total path length in xy calculated as the square root of the sum of the vectors between consecutive x, y pairs squared. For further detials, see;  See Prieto et al (1996) Measures of Postural Steadiness: Differences Between Healthy Young and Elderly Adults, IEEE TRANSACTIONS ON BIOMEDICAL ENGINEERING, VOL. 43, # NO. 9, SEPTEMBER 1996, 956- 966</li>
  <li><b>CoF_x_speed / CoF_y_speed / CoF_xy_speed</b>: The respective path lengths divided by sample time</li>
  <li><b>Ellipse_area</b>: 95% confidence ellipse area. For full details, see; Prieto et al (1996) Measures of Postural Steadiness: Differences Between Healthy Young and Elderly Adults, IEEE TRANSACTIONS ON BIOMEDICAL ENGINEERING, VOL. 43, # NO. 9, SEPTEMBER 1996, 956- 966</li>
</ul>

<h2>Handling missing data</h2>
If there may be frames of data missing from the time serires, there is also the option to return an analysis of these and optionally interpolate the gaps. 
<h3>Frame gap analysis</h3>
If 'fgap' is set to 'T' (TRUE) in settings, these data will be included in the results saved to .csv file separated by an empty column headed 'FrameGaps'. The following columns contain counts of gaps in the time-series of the indicated number of frames.
</ul>
  By calling the main function as <i>(run_it(miss = TRUE)</i>, data describing the profile of missing data will be calculated. These are exported as a text file as well as creating global variables. These are used by the <i></i>FrameSkippingErrorAnalysis</i> script and further details can be found in the guide for this. 
<h3>Interpolation</h3>
Gaps in data can optionally be interpolated using splines (using the na. spline function from the zoo library). Whether interpolation is to be used prior to analysis can be set in the analysis specifications file. It is possible to run the analysis both with and without interpolation of missing data. See <i>SettingsFiles/AnalysisSpecDescriptor.txt</i> for further details. If only interpolated data are to be used, 'Interp' will still be returned as FALSE if there are no gaps in the data set.

<h2>Descriptive Statistics</h2>
If there are more than 1 sets of results in the returned file, the descriptive statistics listed below will also be returned as a separate csv file. If both unfilled and interpolated data are used, then the statistics will be grouped by these (indicated by the 'Interp' column). If there are more than 1 unique values found in the 'trial' column (as extracted from the file names), descriptive statistics will be returned grouped by trial as well as for all data sets.
<ul>
  <li><b>n</b>: The number of datasets included in the statistics</li>
  <li><b>Mean</b>: Mean of included values</li>
  <li><b>SD</b>: Standard deviation of included values</li>
  <li><b>Median</b>: Median of included values</li>
  <li><b>Trimmed_Mean</b>: The mean value calculated after a fraction (0.1) of observations have been trimmed from each end of the data</li>
  <li><b>MAD</b>: Median Absolute Deviation: the median of the absolute deviations from the median</li>
  <li><b>Minimum / Maximum</b>: Minimum / maximum of values included</li>
  <li><b>Range</b>: The difference between the maximum and mnimum of the values included</li>
  <li><b>Standard_Error</b>: standard error of included values calcualted as the standard deviaiton divided by the square root of the number of data points</li>
  <li><b>Skew</b>: the type of skewness calculations used can be set in the analysis settings file (ee SettingsFiles/AnalysisSpecDescriptor.txt for further details). Details of the different calculations can be found in the documentation of the library used: e1071</li>
  <li><b>Kurtosis</b>: the type of kurtosis calculations used can be set in the analysis settings file (ee SettingsFiles/AnalysisSpecDescriptor.txt for further details). Details of the different calculations can be found in the documentation of the library used: e1071</li>
</ul>
<h2>Data file compatibility</h2>
The required specifications to use text files exported with data from the Kistler Bioware software or the Tekscan Fscan research software (SAM) are included in the default DataFileSpec.csv. It would also be possible to use any other deliminated text file that also contain:
<ol>
  <li>Time and x/y locations of centre of force.</li>
  <li>The sample rate in the header of the file.</li>
  <li>For descriptive statistics to be returned grouped by trial type, these must be coded within the file name and the Regex expression in the analysis specifications file edited to extract these.</li>
</ol>
<h2>Running the source code</h2>
The main function is called as 'run_it()'. Passing the following values as TRUE will return additional behaviours;
<ul>
  <li><i>miss</i>: details of missing frames are returned (see Missing Values section)</li>
  <li><i>data_out</i>: Data sets will be returned as global variables with the file names as the variable name. If both filled and interpolated data have been included in the analysis, calling the plot_interpolate function as plot_interpolate([filename)] will generate an overlay plot of the two sets of data</li>
  <li><i>check_flname</i>: If true: The names of the data files are checked against that in the files' headers and the user is given the option to rename the file to match the information in the header (with file type extension removed). If this is set to TRUE, <i>chkskp</i>, <i>chkcol</i> should also be passed if these are different from the default values (4 and 2 respecively). <i>chkskp</i> denotes the number of header rows to skip and <i></i>chkcol</i>the column in which the file name appears respectively.
  <i>analspecfile</i>: the path and names of the analysis specification settings files with respect to the script path. By default this is '<i>/SettingsFiles/AnalysisSpec.csv</i>'.
  
</ul>
<h3>Dependencis</h3>
<ul>
  <li>R version 4+</li>
</ul>
