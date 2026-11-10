#!/bin/sh

# Exclude reclaimable cache from Linux memory usage.
if [ -r /proc/meminfo ]; then
    awk '
        /^MemTotal:/ { total = $2 }
        /^MemAvailable:/ { available = $2; found = 1 }
        END {
            if (total > 0 && found)
                printf "%.0f%%", 100 * (total - available) / total
            else
                printf "N/A"
        }
    ' /proc/meminfo
elif [ "$(uname -s)" = Darwin ]; then
    # Active, wired and compressed pages, accounting for Apple Silicon page size.
    total=$(sysctl -n hw.memsize) || exit 1
    vm_stat | awk -v total="$total" '
        /page size of/ { page_size = $8 }
        /Pages active:|Pages wired down:|Pages occupied by compressor:/ {
            used += $NF
        }
        END {
            if (total > 0 && page_size > 0)
                printf "%.0f%%", 100 * used * page_size / total
            else
                printf "N/A"
        }
    '
else
    printf 'N/A'
fi
