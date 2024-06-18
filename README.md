<h1>Balance Analysis</h1>
This code runs analysis on human balance data providing various parameters relating to the centre of force movement. The following parameters will be saved as a csv file;
<ul>
  <li>file_name: The name of the file that the data are based on</li>
  <li>Data_read: Whether the data were successfully read from the file (normally 'Success')</li>
  <li>trial: This is used to group data by different trial types in the descriptive statistics (see below). This is extracted from the file name using Regex. The Regex expression is set in the analysis settings file. See SettingsFiles/AnalysisSpecDescriptor.txt for further details.</li>
  <li>No_frames: The number of frames of data in the time-series</li>
  <li>sample_rate: The sample rate of the data. This is read in from the header of the source data</li>
  <li>sample_t: The sample time, i.e. the duration of the data analysed</li>
  <li>Interp: Whether missing values have been interpolated (see below)</li>
  <li>CoF_x_Range / CoF_y_Range: This is simply the maximum - minimum in the Ax / Ay data</li>
  <li>CoF_x_pl / CoF_y_pl: The total path length in the Ax / Ay data calculated as the sum of the absolute differences in consecutive points in the time-series</li>
  <li>CoF_xy_pl: The total path length in xy calculated as the sqaure root of the sum of the vectors between consecutive x, y pairs squared. For further detials, see;  See Prieto et al (1996) Measures of Postural Steadiness: Differences Between Healthy Young and Elderly Adults, IEEE TRANSACTIONS ON BIOMEDICAL ENGINEERING, VOL. 43, # NO. 9, SEPTEMBER 1996, 956- 966</li>
  <li>CoF_x_speed / CoF_y_speed / CoF_xy_speed: The respective path lengths divided by sample time</li>
  <li>Ellipse_area: 95% confidence ellipse area. For full details, see; Prieto et al (1996) Measures of Postural Steadiness: Differences Between Healthy Young and Elderly Adults, IEEE TRANSACTIONS ON BIOMEDICAL ENGINEERING, VOL. 43, # NO. 9, SEPTEMBER 1996, 956- 966</li>
</ul>

<h2>Handling missing data</h2>
If there may be frames of data missing from the time serires, there is also the option to return an analysis of these and optionally interpolate the gaps. 
<h3>Frame gap analysis</h3>
<ul>
  <li>FG01... FGn: The count of gaps in the time-series of the indicated number of frames. The maximum number of potential frames missing in any single gap to consider in the analysis can be set in the analysis specification file.</li>
  <li>Sum_Exceed: This is the count of frames where the size of the gaps were greater than the maximum value set in the settings (if any). See SettingsFiles/AnalysisSpecDescriptor.txt for further details.</li>
  <li>Max_Exceed: This is the size of the largest number of frames in any gap greater than the maximum value set in the settings (if any). See SettingsFiles/AnalysisSpecDescriptor.txt for further details. </li>
</ul>
  By calling the main function (run_it()) with miss = TRUE, a list of all the missing times found in the data analysed can also be returned as a global variable. The mean and standard deviation of these data will also be returned in the console. This is intended to facilitate modelling of errors associated with missing data.
<h3>Interpolation</h3>
Whether any missing data are to be interpolated prior to analysis can be set in the analysis specifications file. It is possible to run the analysis both with and without interpolation of missing data. See SettingsFiles/AnalysisSpecDescriptor.txt for further details. If only interpolated data are to be used, 'Interp' will still be returned as FALSE if there are no gaps in the data set.

<h2>Descriptive Statistics</h2>
If there are more than 1 sets of results in the returned file, the descriptive statistics listed below will also be returned as a separate csv file. If both unfilled and interpolated data are used, then the statistics will be grouped by these (indicated by the 'Interp' column). If there are more than 1 unique values found in the 'trial' column (as extracted from the file names), descriptive statistics will be returned grouped by trial as well as for all data sets.
<ul>
  <li>n: The number of datasets included in the statistics</li>
  <li>Mean: Mean of included values</li>
  <li>SD: Standard deviation of included values</li>
  <li>Median: Median of included values</li>
  <li>Trimmed_Mean: The mean value calculated after a fraction (0.1) of observations have been trimmed from each end of the data</li>
  <li>MAD: Median Absolute Deviation: the median of the absolute deviations from the median</li>
  <li>Minimum / Maximum: Minimum / maximum of values included</li>
  <li>Range: The difference between the maximum and mnimum of the values included</li>
  <li>Standard_Error: standard error of included values calcualted as the standard deviaiton divided by the square root of the number of data points</li>
  <li>Skew: the type of skewness calculations used can be set in the analysis settings file (ee SettingsFiles/AnalysisSpecDescriptor.txt for further details). Details of the different calculations can be found in the documentation of the library used: e1071</li>
  <li>Kurtosis: the type of kurtosis calculations used can be set in the analysis settings file (ee SettingsFiles/AnalysisSpecDescriptor.txt for further details). Details of the different calculations can be found in the documentation of the library used: e1071</li>
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
  <li>miss: details of missing frames are returned (see Missing Values section)</li>
  <li>data_out: Data sets will be returned as global variables with the file names as the variable name. If both filled and interpolated data have been included in the analysis, calling the plot_interpolate function as plot_interpolate([filename)] will generate an overlay plot of the two sets of data</li>
</ul>
<h3>Dependencis</h3>
<ul>
  <li>R version 4+</li>
</ul>
