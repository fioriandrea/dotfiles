icon="💾"
location="/"
printf "%s: %s\n" "$icon" "$(df -h "$location" | awk ' /[0-9]/ {print $3 "/" $2}')"
