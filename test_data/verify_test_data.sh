#!/bin/bash
# Verify test data directory setup

set -e

echo "=========================================="
echo "Test Data Directory Verification"
echo "=========================================="
echo ""

TEST_DIR="/Users/barry/Documents/NEXUS/test_data"
ERRORS=0

echo "Checking directory structure..."
for dir in cdeps hemco mesh streams; do
    if [ -d "$TEST_DIR/$dir" ]; then
        echo "  ✓ $dir/ directory exists"
    else
        echo "  ✗ $dir/ directory missing"
        ERRORS=$((ERRORS + 1))
    fi
done

echo ""
echo "Checking HEMCO configuration files..."
for file in HEMCO_sa_Config.rc HEMCO_Diagn.rc HEMCO_sa_Spec.rc HEMCO_sa_Time.rc; do
    if [ -f "$TEST_DIR/hemco/$file" ]; then
        echo "  ✓ $file exists"
    else
        echo "  ✗ $file missing"
        ERRORS=$((ERRORS + 1))
    fi
done

echo ""
echo "Checking HEMCO grid files..."
shopt -s nullglob
grid_files=("$TEST_DIR/hemco"/HEMCO_sa_Grid.*.rc)
if [ ${#grid_files[@]} -gt 0 ]; then
    for grid in "${grid_files[@]}"; do
        echo "  ✓ $(basename $grid) exists"
    done
else
    echo "  ✗ HEMCO_sa_Grid.*.rc missing"
    ERRORS=$((ERRORS + 1))
fi
shopt -u nullglob

echo ""
echo "Checking CDEPS configuration..."
if [ -f "$TEST_DIR/cdeps/cdeps_streams.conf" ]; then
    echo "  ✓ cdeps_streams.conf exists"
else
    echo "  ✗ cdeps_streams.conf missing"
    ERRORS=$((ERRORS + 1))
fi

echo ""
echo "Checking Python helper scripts..."
if [ -f "$TEST_DIR/cdeps/create_sample_data.py" ]; then
    echo "  ✓ create_sample_data.py exists"
else
    echo "  ✗ create_sample_data.py missing"
    ERRORS=$((ERRORS + 1))
fi
if [ -f "$TEST_DIR/mesh/create_sample_mesh.py" ]; then
    echo "  ✓ create_sample_mesh.py exists"
else
    echo "  ✗ create_sample_mesh.py missing"
    ERRORS=$((ERRORS + 1))
fi

echo ""
echo "=========================================="
if [ $ERRORS -eq 0 ]; then
    echo "All test data files verified!"
    echo "=========================================="
    echo ""
    echo "Test data directory structure:"
    echo "  /Users/barry/Documents/NEXUS/test_data/"
    echo "  ├── cdeps/"
    echo "  │   ├── cdeps_streams.conf"
    echo "  │   ├── create_sample_data.py"
    echo "  │   └── create_sample_mesh.py"
    echo "  ├── hemco/"
    echo "  │   ├── HEMCO_sa_Config.rc"
    echo "  │   ├── HEMCO_Diagn.rc"
    echo "  │   ├── HEMCO_sa_Spec.rc"
    echo "  │   ├── HEMCO_sa_Time.rc"
    echo "  │   └── HEMCO_sa_Grid.*.rc"
    echo "  ├── mesh/"
    echo "  │   └── create_sample_mesh.py"
    echo "  └── streams/"
    echo ""
    echo "To create sample data files:"
    echo "  cd /Users/barry/Documents/NEXUS/test_data/cdeps"
    echo "  python3 create_sample_data.py"
    echo "  cd /Users/barry/Documents/NEXUS/test_data/mesh"
    echo "  python3 create_sample_mesh.py"
    exit 0
else
    echo "$ERRORS error(s) found"
    echo "=========================================="
    exit 1
fi
