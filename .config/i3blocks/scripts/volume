#! /bin/bash

muteicon="🔇"
zeroicon="🔈"
mediumicon="🔉"
highicon="🔊"

# vol=$(pactl list sinks | grep '^[[:space:]]Volume:' | grep -oE '[0-9]{1,3}%')
# muted=$(pactl list sinks | grep '^[[:space:]]Mute:' | awk '{print $2 == "yes" ? 1 : 0}')


[ $(pamixer --get-mute) = true ] && echo "$muteicon" && exit

vol="$(pamixer --get-volume)"

if [ "$vol" -gt "70" ]; then
	icon="$highicon"
elif [ "$vol" -lt "30" ]; then
	icon="$zeroicon"
else
	icon="$mediumicon"
fi

echo "$icon $vol%"
