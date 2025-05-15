#!/bin/bash
# Script to test nexus_nei2019_linker.py with all days in a year

# Default settings
YEAR=2023
WORK_DIR="./test_output"
SRC_DIR="../emissions/nexus"
VERSION="v2023-03"
NEI_SCRIPT_PATH="../python/nexus_nei2019_linker.py"

# Parse command line arguments
while [[ $# -gt 0 ]]; do
  key="$1"

  case $key in
    -y|--year)
      YEAR="$2"
      shift 2
      ;;
    -w|--work-dir)
      WORK_DIR="$2"
      shift 2
      ;;
    -s|--src-dir)
      SRC_DIR="$2"
      shift 2
      ;;
    -v|--version)
      VERSION="$2"
      shift 2
      ;;
    -p|--script-path)
      NEI_SCRIPT_PATH="$2"
      shift 2
      ;;
    -h|--help)
      echo "Usage: $0 [options]"
      echo "Options:"
      echo "  -y, --year YEAR          Year to test (default: $YEAR)"
      echo "  -w, --work-dir DIR       Working directory (default: $WORK_DIR)"
      echo "  -s, --src-dir DIR        Source directory (default: $SRC_DIR)"
      echo "  -v, --version VERSION    NEI version (default: $VERSION)"
      echo "  -p, --script-path PATH   Path to nexus_nei2019_linker.py (default: $NEI_SCRIPT_PATH)"
      echo "  -h, --help               Show this help message and exit"
      exit 0
      ;;
    *)
      echo "Unknown option: $1"
      echo "Use --help for usage information"
      exit 1
      ;;
  esac
done

# Create work directory if it doesn't exist
mkdir -p "$WORK_DIR"

# Check if Python script exists
if [ ! -f "$NEI_SCRIPT_PATH" ]; then
  echo "Error: NEI2019 linker script not found at $NEI_SCRIPT_PATH"
  exit 1
fi

echo "Testing NEI2019 linker for all days in year $YEAR"
echo "Source directory: $SRC_DIR"
echo "Work directory: $WORK_DIR"
echo "NEI version: $VERSION"

# Create a log directory for storing outputs
LOG_DIR="${WORK_DIR}/logs"
mkdir -p "$LOG_DIR"
SUMMARY_LOG="${LOG_DIR}/summary.log"
echo "Date,Status,Error" > "$SUMMARY_LOG"

# Generate dates for the entire year
START_DATE="${YEAR}-01-01"
END_DATE="${YEAR}-12-31"

# Function to get the next date
next_date() {
  date -j -v+1d -f "%Y-%m-%d" "$1" "+%Y-%m-%d" 2>/dev/null || \
  date -d "$1 + 1 day" "+%Y-%m-%d"  # Linux fallback
}

# Function to compare dates (returns 0 if date1 <= date2)
compare_dates() {
  local date1=$(date -d "$1" +%s 2>/dev/null || date -j -f "%Y-%m-%d" "$1" "+%s")
  local date2=$(date -d "$2" +%s 2>/dev/null || date -j -f "%Y-%m-%d" "$2" "+%s")

  if [[ $date1 -le $date2 ]]; then
    return 0
  else
    return 1
  fi
}

# Testing each date
current_date="$START_DATE"
success_count=0
failure_count=0

echo "Starting tests on $(date)"
echo "=============================================="

while compare_dates "$current_date" "$END_DATE"; do
  echo -n "Testing $current_date... "

  # Clean previous test output to ensure fresh environment for each date
  clean_dir="${WORK_DIR}/NEMO/NEI2019/${VERSION}"
  [ -d "$clean_dir" ] && rm -rf "$clean_dir"

  # Run the NEI2019 linker for the current date
  log_file="${LOG_DIR}/test_${current_date}.log"
  python "$NEI_SCRIPT_PATH" --date "$current_date" --work-dir "$WORK_DIR" --src-dir "$SRC_DIR" --nei-version "$VERSION" --no-read-hemco-time > "$log_file" 2>&1

  if [ $? -eq 0 ]; then
    # Check for broken links - find all symlinks in the output directory and verify they point to existing files
    broken_links=0
    echo "Verifying links..." >> "$log_file"

    while IFS= read -r link; do
      target=$(readlink "$link")
      if [ ! -e "$target" ]; then
        echo "BROKEN LINK: $link -> $target" >> "$log_file"
        broken_links=$((broken_links + 1))
      else
        echo "VALID LINK: $link -> $target" >> "$log_file"
      fi
    done < <(find "${WORK_DIR}/NEMO" -type l 2>/dev/null)

    if [ $broken_links -gt 0 ]; then
      echo "FAILED - Found $broken_links broken links"
      echo "$current_date,FAILED,\"Found $broken_links broken links\"" >> "$SUMMARY_LOG"
      ((failure_count++))
    else
      echo "SUCCESS"
      echo "$current_date,SUCCESS," >> "$SUMMARY_LOG"
      ((success_count++))
    fi
  else
    error_msg=$(grep -i "error" "$log_file" | head -1 | sed 's/.*error[^:]*: //')
    echo "FAILED - $error_msg"
    echo "$current_date,FAILED,\"$error_msg\"" >> "$SUMMARY_LOG"
    ((failure_count++))
  fi

  # Move to next date
  current_date=$(next_date "$current_date")
done

echo "=============================================="
echo "Testing completed on $(date)"
echo "Results: $success_count dates succeeded, $failure_count dates failed"
echo "Detailed logs are available in $LOG_DIR"
echo "Summary log is available at $SUMMARY_LOG"

exit 0
