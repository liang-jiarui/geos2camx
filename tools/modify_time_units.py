import sys
import netCDF4 as nc
import os
import re

def modify_time_units(input_dir, output_dir, start_time=None, end_time=None):
    """
    Modify the time units in NetCDF files based on the date in the filename.

    Parameters:s
    - input_dir (str): Directory containing the input NetCDF files.
    - output_dir (str): Directory where the modified NetCDF files will be saved.
    - start_time (str, optional): Start date in the format 'YYYYMMDD'. If None, process all files.
    - end_time (str, optional): End date in the format 'YYYYMMDD'. If None, process all files.
    """
    # Ensure the output directory exists
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)

    # Regular expression to extract date from filename
    date_pattern = re.compile('GEOSChem\.SpeciesConc\.(\d{8})\.nc')

    # Iterate over all files in the input directory
    for filename in os.listdir(input_dir):
        match = date_pattern.match(filename)
        if match:
            file_date = match.group(1)
            
            # If start_time and end_time are specified, check if the file date is within the range
            if start_time and end_time:
                if file_date < start_time or file_date > end_time:
                    continue  # Skip files outside the specified date range
            
            # Construct the input file path
            input_file = os.path.join(input_dir, filename)
            
            # Open the file in read-write mode
            with nc.Dataset(input_file, 'r+') as readfile:
                # Modify the time units
                print('before modify:', readfile.variables['time'].units)
                # time:units = "minutes since 2023-04-11 00:00:00 UTC" ;
                readfile.variables['time'].units = f'minutes since {file_date[:4]}-{file_date[4:6]}-{file_date[6:8]} 00:00:00'
                print('after modify:', readfile.variables['time'].units)
                # time:units = "minutes since 2023-04-11 00:00:00" ;
            
            # Construct the output file path
            output_file = os.path.join(output_dir, filename)
            
            # Save the modified file to the output directory
            os.rename(input_file, output_file)

        print(f"Modified file units to {output_file}")


if __name__ == "__main__":

    # input_dir = "/home/camx/ljr/result/1_GC_merge"
    # output_dir = "/home/camx/ljr/result/1_GC_merge"
    # start_time = "20230401"
    # end_time = "20230430"
    # modify_time_units(input_dir, output_dir, start_time, end_time)


    # Parse command line arguments
    if len(sys.argv) < 3:
        print("Usage: python modify_time_units.py <input_dir> <output_dir> [start_time] [end_time]")
        sys.exit(1)
    
    input_dir = sys.argv[1]
    output_dir = sys.argv[2]
    start_time = sys.argv[3] if len(sys.argv) > 3 else None
    end_time = sys.argv[4] if len(sys.argv) > 4 else None
    
    # Call the function
    modify_time_units(input_dir, output_dir, start_time, end_time)
    