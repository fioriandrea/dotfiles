[[ -e /etc/arch-release ]] || return

alias cleanup='sudo pacman -Rns $(pacman -Qtdq)'

alias mirror_country='sudo reflector --age 12 --number 10 --protocol https --sort rate --verbose --save /etc/pacman.d/mirrorlist --country'
alias mirror_delay='sudo reflector --latest 50 --number 20 --sort delay --verbose --save /etc/pacman.d/mirrorlist'
alias mirror_score='sudo reflector --latest 50 --number 20 --sort score --verbose --save /etc/pacman.d/mirrorlist'
alias mirror_age='sudo reflector --latest 50 --number 20 --sort age --verbose --save /etc/pacman.d/mirrorlist'
