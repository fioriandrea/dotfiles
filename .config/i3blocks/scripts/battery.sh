#! /bin/bash

discharging_icon="🔋"
charging_icon="🔌"
full_icon="⚡"
not_charging_icon="🛑"
unknown_icon="♻"

for battery in /sys/class/power_supply/BAT?
do
	# Get its remaining capacity and charge status.
	capacity=$(cat "$battery"/capacity 2>/dev/null) || break
    status_string="$(< "$battery"/status)"

    discharging_re="[Dd]ischarging"
    charging_re="[Cc]harging"
    full_re="[Ff]ull"
    not_charging_re="[Nn]ot charging"

    if [[ $status_string =~ $discharging_re ]]
    then
        icon="$discharging_icon"
    elif [[ $status_string =~ $charging_re ]]
    then
        icon="$charging_icon"
    elif [[ $status_string =~ $full_re ]]
    then
        icon="$full_icon"
    elif [[ $status_string =~ $not_charging_re ]]
    then
        icon="$not_charging_icon"
    else
        icon="$unknown_icon"
    fi

    echo "$icon $capacity"
done | sed 's/ *$//'
