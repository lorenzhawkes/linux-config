#!/bin/bash

xsetroot -cursor_name left_ptr

# Load resources

# xrdb -merge .Xresources

# export XDG_DATA_DIRS="$XDG_DATA_DIRS:/var/lib/flatpak/exports/share:/home/lorenz/.local/share/flatpak/exports/share"

# source /etc/X11/Xsession.d/20flatpak

if [ -x /usr/lib/policykit-1-gnome/polkit-gnome-authentication-agent-1 ] ; then
   /usr/lib/policykit-1-gnome/polkit-gnome-authentication-agent-1 &
fi

if [[ $(type -P dunst) ]] ; then 
   dunst &
elif [[ $(type -P deadd-notification-center) ]] ; then # /home/lorenz/dev/phuhl/linux_notification_center
   deadd-notification-center &
fi

if [[ $(type -P picom) ]] ; then # /home/lorenz/dev/yshui/picom
   picom --config $HOME/.config/picom/picom.conf &
fi

if [[ $(type -P polybar) ]] ; then # from aur
   polybar -r &
fi

if [[ $(type -P xss-lock) ]] ; then
   xss-lock --transfer-sleep-lock -- lock --nofork &
fi

if [[ $(type -P feh) ]] ; then
   feh --bg-scale $HOME/backgrounds/family.jpg &
fi

if [[ $(type -P xfce4-power-manager) ]] ; then
   xfce4-power-manager &
fi

if [[ $(type -P nm-applet) ]] ; then
   nm-applet --sm-disable &
fi

exec xmonad # /home/lorenz/.local/bin/xmonad
