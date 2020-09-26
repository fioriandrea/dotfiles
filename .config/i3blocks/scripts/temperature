#! /bin/bash

thermometericon=🌡

temp=$(sensors | grep "Core 0" | awk '{print $3}')

echo "$thermometericon $temp"
