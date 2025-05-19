#!/bin/bash
# filepath: /scratch1/RDARCH/rda-arl-gpu/Barry.Baker/models/nxs_config/utils/python/inventory/process_ceds_files.sh
#
# This script processes CEDS emission files in a directory
# and converts them to HEMCO-compatible format using ceds_preprocess.py
#
# Usage:
#   Interactive mode: ./process_ceds_files.sh <input_dir> <output_dir>
#   SLURM batch mode: sbatch process_ceds_files.sh <input_dir> <output_dir>
#
# Example:
#   ./process_ceds_files.sh /path/to/raw/ceds /path/to/processed/ceds
#   sbatch process_ceds_files.sh /path/to/raw/ceds /path/to/processed/ceds

# SLURM directives (used only when submitted with sbatch)
#SBATCH --job-name=ceds_proc
#SBATCH --account=rda-arl-gpu
#SBATCH --time=04:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=96G
#SBATCH --output=ceds_proc_%A_%a.log
#SBATCH --array=1-23%10  # Process up to 23 files, 10 at a time (adjust based on file count)

# Exit on error
set -e

# Check if we have the right number of arguments
if [ $# -ne 2 ]; then
  echo "Usage: $0 <input_dir> <output_dir>"
  echo "Example: $0 /path/to/raw/ceds /path/to/processed/ceds"
  exit 1
fi

# Get input and output directories
INPUT_DIR="$1"
OUTPUT_DIR="$2"

# Detect if running under SLURM
RUNNING_UNDER_SLURM=0
if [ -n "$SLURM_ARRAY_JOB_ID" ]; then
  RUNNING_UNDER_SLURM=1
  echo "Running as SLURM array job: ${SLURM_ARRAY_JOB_ID}_${SLURM_ARRAY_TASK_ID}"
fi

# Check if input directory exists
if [ ! -d "$INPUT_DIR" ]; then
  echo "Error: Input directory $INPUT_DIR does not exist"
  exit 1
fi

# Create output directory if it doesn't exist
mkdir -p "$OUTPUT_DIR"

# Find the path to the script
if [ $RUNNING_UNDER_SLURM -eq 1 ]; then
  # When running as a SLURM job, use the absolute path to the script
  SCRIPT_DIR="/scratch1/RDARCH/rda-arl-gpu/Barry.Baker/models/nxs_config/utils/python/inventory"
else
  # In interactive mode, determine path from script location
  SCRIPT_DIR="$(dirname "$(readlink -f "$0")")"
fi

PREPROCESS_SCRIPT="${SCRIPT_DIR}/ceds_preprocess.py"

# Check if the preprocessing script exists
if [ ! -f "$PREPROCESS_SCRIPT" ]; then
  echo "Error: Preprocessing script $PREPROCESS_SCRIPT not found"
  echo "Looked in: $SCRIPT_DIR"
  exit 1
fi

# Counter for processed files
COUNT=0

# Find all NetCDF files in the input directory that match CEDS emission patterns
echo "Searching for CEDS emission files in $INPUT_DIR..."

# Create a temporary file list
FILE_LIST=$(mktemp)
find "$INPUT_DIR" -name "*-em-anthro*.nc" > "$FILE_LIST"
TOTAL_FILES=$(wc -l < "$FILE_LIST")

if [ $TOTAL_FILES -eq 0 ]; then
  echo "No matching CEDS emission files found in $INPUT_DIR"
  rm "$FILE_LIST"
  exit 0
fi

echo "Found $TOTAL_FILES files to process"

# Process files differently based on whether we're in SLURM mode or not
if [ $RUNNING_UNDER_SLURM -eq 1 ]; then
  # Get the file to process based on SLURM_ARRAY_TASK_ID
  if [ "$SLURM_ARRAY_TASK_ID" -le "$TOTAL_FILES" ]; then
    input_file=$(sed -n "${SLURM_ARRAY_TASK_ID}p" "$FILE_LIST")
    base_filename=$(basename "$input_file")
    output_filename="${base_filename%.nc}_processed.nc"
    output_path="$OUTPUT_DIR/$output_filename"

    echo "Processing: $base_filename (File $SLURM_ARRAY_TASK_ID of $TOTAL_FILES)"
    echo "  Input: $input_file"
    echo "  Output: $output_path"

    # Process the file using the Python script
    python "$PREPROCESS_SCRIPT" "$input_file" "$output_path"

    echo "Completed processing file $SLURM_ARRAY_TASK_ID: $base_filename"
  else
    echo "No file to process for task ID $SLURM_ARRAY_TASK_ID (only $TOTAL_FILES files available)"
  fi

  # Clean up
  rm "$FILE_LIST"

else
  # Non-SLURM (interactive) mode - process files sequentially
  while read -r input_file; do
    # Get the base filename
    base_filename=$(basename "$input_file")

    # Construct the output filename by adding "_processed" before the extension
    output_filename="${base_filename%.nc}_processed.nc"
    output_path="$OUTPUT_DIR/$output_filename"

    echo "Processing: $base_filename"
    echo "  Input: $input_file"
    echo "  Output: $output_path"

    # Process the file using the Python script
    python "$PREPROCESS_SCRIPT" "$input_file" "$output_path"

    # Increment the counter
    COUNT=$((COUNT + 1))
    echo "Completed $COUNT of $TOTAL_FILES files"
    echo "----------------------------------------"
  done < "$FILE_LIST"

  # Clean up
  rm "$FILE_LIST"

  echo "Processing complete. Processed $COUNT files"
  echo "Output files are in $OUTPUT_DIR"
fi
