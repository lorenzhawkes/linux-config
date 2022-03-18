function angband
i3-msg "workspace 4; append_layout /home/lorenz/angband-workspace.json"
/usr/bin/angband -mx11 -- -n6 &
disown (jobs -lp)
end
