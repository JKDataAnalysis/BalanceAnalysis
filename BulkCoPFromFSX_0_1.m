% Read_FSX_Demo.m
% Tekscan, Inc.
% Copyright September, 2018
%
% Rev 2.0
% September 13, 2018

%
%
% This sample code demonstrates how the you can read and modify .FSX files
% using the OEM Toolkit. Please see product documentation for full 
% descriptions of all available functions.
%
% If you using a 32-bit Matlab installation, please use the set of DLLs in the 
% root directory of the installation. If you using a 64-bit installation
% of Matlab, please use the set of DLLs in the x64 folder within the TekAPI
% directory. This example code is configured for 32-bit, but the DLL file 
% path can be modified accordingly.
%
% MATLAB 2011a or later recommended due to .NET Framework compatibility. 
% http://www.mathworks.com/support/solutions/en/data/1-CNPYZV/index.html
% For earlier versions: The LabVIEW help thread - 
% http://digital.ni.com/public.nsf/allkb/32B0BA28A72AA87D8625782600737DE9
% describes a process that will also work for MATLAB. However, for
% MATLAB, this config file must be placed in the bin/Win32 or bin/Win64 
% folder in the MATLAB installation rather than the bin directory.
%
% All file paths included below assume that the files reside in the
% default installation directory. If the files are moved, or different
% .cal, .equ, or .fsx files are used, the paths must be changed
% accordingly.
%
% Many functions return error codes as defined in the documentation. We
% recommend using these error codes to verify that operations complete
% successfully. The TekEnableLogging and TekGetLastError functions can be
% used for debugging.


%% Clean-up
close all; clear all; clc;

%% Import OEM Toolkit functions as .NET assembly
% Tekscan SDK and DRT require .Net version 4.0 and Matlab 2011 and higher
%assembly = NET.addAssembly('C:\Tekscan\TekAPI\TekAPIRead.dll');
% Use the following dll for Matlab 64-bit
assembly = NET.addAssembly('C:\Tekscan\TekAPI\x64\TekAPIRead64.dll');

CTekAPI = TekAPI.CTekAPI;

%% Display available methods and their signatures (optional)
% methodsview(CTekAPI)

%% Open .FSX file
% Set the location to look for .mp files
mapFileDirectory = 'C:\Tekscan\TekAPI\Samples';
% CTekAPI.TekSetMapFileDirectory(mapFileDirectory);

%% Get lis of .fsx files in user selected folder
sourcepath = getdir('Select folder containing .fsx files (will inlcude subfolders)');
if isequal(sourcepath,0)
%uigetdir(title = 'Select folder containing .fsx files (will inlcude subfolders)'); % choose path
if isequal(sourcepath,0)
   disp('User selected Cancel');
else
    % >>> select destination folder
    filelist = dir(fullfile(sourcepath, '**/*.fsx')); % get list of all .fsx files in that path
    if isequal(height(filelist), 0)
        disp('No .fsx files found in that folder or subfolders')
    else
        for f = 1:height(filelist) % iterate through list of files
            % load file
            recording = CTekAPI.TekLoadRecording(fullfile(filelist(f).folder, filelist(f).name)) % load CTekFile object
            %% Get details about the recording
            rows = recording.TekGetRows();
            columns = recording.TekGetColumns();
            numberOfFrames = recording.TekGetFrameCount();
            rowSpacing = recording.TekGetRowSpacing();
            columnSpacing = recording.TekGetColumnSpacing();

            tmp = sprintf('rows: %d, columns: %d, NoFrames: %d', rows, columns, numberOfFrames)
            disp(tmp)

            % >>> iterate through each frame
                % >>> calculate CoP
                % >>> append to array
            % >>> Check file name doesn't already exist in destination folder
            % >>> write array as csv file
        end
    end
    
    % recording = CTekAPI.TekLoadRecording(fullfile(path,file)); % returns CTekFile object
   % disp(['User selected ', fullfile(path,file)]);
end

% recordingPath = 'C:\Tekscan\TekAPI\Samples\SampleRecording.fsx';

% recording = CTekAPI.TekLoadRecording(recordingPath); % returns CTekFile object

%% Display available methods and their signatures (optional)
% methodsview(recording);

%% Get details about the recording
rows = recording.TekGetRows();
columns = recording.TekGetColumns();
numberOfFrames = recording.TekGetFrameCount();
rowSpacing = recording.TekGetRowSpacing();
columnSpacing = recording.TekGetColumnSpacing();

%% Get data from file
% Return value of data is System.Byte[], must use int8(), double(), etc.
% to convert to MATLAB matrix/vector.
frameNumber = 100; % 0-based frame counting (first frame of the recording)
[error, recordingData] = recording.TekGetRawFrameData(frameNumber);
[error, peakFrameData] = recording.TekGetPeakFrameData();

recordingData = double(recordingData);
peakFrameData = double(peakFrameData);


Frame = transpose(reshape(recordingData, columns, rows));

% Reshape Peak frame data to 2D array
peakFrame = transpose(reshape(peakFrameData, columns, rows));


% Will only return calibrated data if recording has a calibration applied.
% Both raw and calibrated data will be equilibrated if an equilibration is 
% applied to the recording.
calibratedRecordingData = double(recording.TekGetCalibratedFrameData(frameNumber)); 


%% Plot the frame data
imagesc(Frame);
title(num2str(frameNumber));
%% Spy plot frame
spy(Frame);
%% Spy plot peak frame
spy(peakFrame);



%% Load a calibration/equilibration (optional)
% Return types are CTekEquilibration and CTekCalibration objects,
% respectively. These objects provide a function allowing for calibration
% and equilibration of arrays of captured frame data and can also be passed 
% as parameters to functions that save recordings or apply calibrations and 
% equilibrations to .fsx files.
equilibrationFilePath = 'C:\Tekscan\TekAPI\Samples\SampleEquil.equ';
calibrationFilePath = 'C:\Tekscan\TekAPI\Samples\SampleCal.cal';
equilibration = CTekAPI.TekLoadEquilibration(equilibrationFilePath);
calibration = CTekAPI.TekLoadCalibration(calibrationFilePath);

%% Apply calibrations and equilibrations to files
% While these operations will complete even if the sensitivity of the 
% recording and calibration do not match (not recommended), an error code 
% will be produced when saving recordings or applying calibrations to recordings.
recording.TekApplyCalibration(calibration);
recording.TekApplyEquilibration(equilibration);

%% Undo calibrations and equilibrations in files
recording.TekClearCalibration();
recording.TekClearEquilibration();
%%


dirchosen = getdir('Test');

function [chosendir] = getdir(dlgttl)
    gotdir = false;
    while ~gotdir
        chosendir = uigetdir(title = dlgttl); % choose path
        if isequal(chosendir,0)
           answer = questdlg('No folder selected, try again?','Source folder not selected','Yes','No','Yes');
           if isequal(answer,'No')
               gotdir = true;
           end
        else
            gotdir = true;
        end
    end
end
